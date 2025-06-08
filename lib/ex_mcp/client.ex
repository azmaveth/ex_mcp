defmodule ExMCP.Client do
  @moduledoc """
  MCP client for connecting to Model Context Protocol servers.

  This module provides both MCP specification features and ExMCP extensions.

  ## MCP Specification Features

  Core protocol operations that are portable across all MCP implementations:

  - `initialize/2` - Initialize connection with server
  - `list_tools/2` - List available tools
  - `call_tool/4` - Execute a tool
  - `list_resources/2` - List available resources  
  - `read_resource/3` - Read resource content
  - `list_prompts/2` - List available prompts
  - `get_prompt/3` - Get a specific prompt
  - `list_roots/2` - List server roots
  - `subscribe/3` - Subscribe to resource changes
  - `unsubscribe/3` - Unsubscribe from resources
  - `create_message/3` - Request LLM sampling
  - `send_cancelled/3` - Cancel an in-flight request
  - `ping/2` - Keep-alive ping

  ## ExMCP Extensions

  > #### Extension Features {: .warning}
  > These features are specific to ExMCP and not part of the official MCP specification.

  - **Automatic Reconnection** - Reconnects on connection failure
  - **Batch Operations** - `batch_request/3` for efficient multi-request handling
  - **Process Monitoring** - Integration with OTP supervision trees
  - **Request Tracking** - `get_pending_requests/1` for debugging
  - **Connection Management** - `disconnect/1`

  ## Basic Example

      # Connect to a server
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: ["npx", "-y", "@modelcontextprotocol/server-filesystem", "/tmp"],
        name: :fs_client
      )
      
      # List available tools
      {:ok, %{"tools" => tools}} = ExMCP.Client.list_tools(client)
      
      # Call a tool
      {:ok, result} = ExMCP.Client.call_tool(client, "read_file", %{
        "path" => "/tmp/example.txt"
      })

  ## Progress Tracking

  For tools that support progress notifications:

      # Call with progress token
      {:ok, result} = ExMCP.Client.call_tool(
        client,
        "process_large_file",
        %{"path" => "/tmp/large.csv"},
        progress_token: "process-123"
      )
      
      # The server will send progress notifications that are logged

  ## Sampling/LLM Integration

  If the server supports sampling (LLM integration):

      {:ok, response} = ExMCP.Client.create_message(client, %{
        messages: [
          %{
            role: "user",
            content: %{type: "text", text: "Explain quantum computing"}
          }
        ],
        modelPreferences: %{
          hints: [%{name: "claude-3-sonnet"}],
          temperature: 0.7,
          maxTokens: 1000
        }
      })
      
      # Response includes the generated content
      IO.puts(response["content"]["text"])

  ## Notifications

  The client automatically handles server notifications:

  - Resource changes (`notifications/resources/list_changed`)
  - Resource updates (`notifications/resources/updated`)
  - Tool changes (`notifications/tools/list_changed`) 
  - Prompt changes (`notifications/prompts/list_changed`)
  - Progress updates (`notifications/progress`)

  These are logged by default. To handle them programmatically,
  implement a custom client that processes notifications.
  """

  use GenServer
  require Logger

  alias ExMCP.{Protocol, Transport, VersionRegistry}
  alias ExMCP.Authorization.{ErrorHandler, TokenManager}

  @reconnect_interval 5_000
  @request_timeout 30_000

  # Client state
  defstruct [
    :transport_mod,
    :transport_opts,
    :transport_state,
    :server_info,
    :server_capabilities,
    :negotiated_version,
    :protocol_version,
    :pending_requests,
    :initialized,
    :client_info,
    :handler,
    :handler_state,
    :auth_config,
    :token_manager
  ]

  # Public API

  @doc """
  Starts an MCP client.

  ## Options

  - `:transport` - Transport type (:stdio, :http, or module)
  - `:name` - GenServer name (optional)
  - `:client_info` - Client information map with :name and :version
  - `:handler` - Module implementing `ExMCP.Client.Handler` behaviour (optional)
  - `:handler_state` - Initial state for the handler (optional)
  - `:auth_config` - Authorization configuration (optional)

  Transport-specific options are passed through.

  ## Authorization

  To enable automatic authorization handling:

      {:ok, client} = ExMCP.Client.start_link(
        transport: :http,
        url: "https://api.example.com",
        auth_config: %{
          client_id: "my-client",
          client_secret: "secret",
          token_endpoint: "https://auth.example.com/token",
          initial_token: %{
            "access_token" => "...",
            "refresh_token" => "...",
            "expires_in" => 3600
          }
        }
      )

  The client will automatically:
  - Add Authorization headers to all requests
  - Refresh tokens before expiration
  - Handle 401/403 responses appropriately

  ## Bi-directional Communication

  If a handler is provided, the client can respond to requests from the server.
  This enables bi-directional communication where the server can:
  - Ping the client
  - Request the client's file system roots
  - Request the client to sample an LLM

  ## Example

      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        handler: MyClientHandler,
        handler_state: %{roots: [%{uri: "file:///home", name: "Home"}]}
      )
  """
  def start_link(opts) do
    {name, opts} = Keyword.pop(opts, :name)
    gen_opts = if name, do: [name: name], else: []
    GenServer.start_link(__MODULE__, opts, gen_opts)
  end

  @doc """
  Lists available tools from the server.

  ## Options
  - `:cursor` - Optional cursor for pagination
  - `:timeout` - Request timeout (default: 30 seconds)

  ## Return value
  Returns a tuple with:
  - `:ok` tuple containing tools list and optional nextCursor
  - `:error` tuple with error reason

  ## Example
      {:ok, %{tools: tools, nextCursor: cursor}} = Client.list_tools(client, cursor: "page2")
  """
  @spec list_tools(GenServer.server(), keyword()) ::
          {:ok, %{tools: [ExMCP.Types.tool()], nextCursor: String.t() | nil}} | {:error, any()}
  def list_tools(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @request_timeout)
    cursor = Keyword.get(opts, :cursor)
    GenServer.call(client, {:list_tools, cursor}, timeout)
  end

  @doc """
  Calls a tool with the given arguments.

  ## Options
  - `:progress_token` - Optional token for progress tracking
  """
  @spec call_tool(GenServer.server(), String.t(), map(), keyword() | timeout()) ::
          {:ok, ExMCP.Types.tool_result()} | {:error, any()}
  def call_tool(client, name, arguments, opts \\ []) do
    {timeout, opts} =
      if is_integer(opts) or opts == :infinity do
        {opts, []}
      else
        Keyword.pop(opts, :timeout, @request_timeout)
      end

    progress_token = Keyword.get(opts, :progress_token)
    GenServer.call(client, {:call_tool, name, arguments, progress_token}, timeout)
  end

  @doc """
  Lists available resources from the server.

  ## Options
  - `:cursor` - Optional cursor for pagination
  - `:timeout` - Request timeout (default: 30 seconds)

  ## Return value
  Returns a tuple with:
  - `:ok` tuple containing resources list and optional nextCursor
  - `:error` tuple with error reason

  ## Example
      {:ok, %{resources: resources, nextCursor: cursor}} = Client.list_resources(client, cursor: "page2")
  """
  @spec list_resources(GenServer.server(), keyword()) ::
          {:ok, %{resources: [ExMCP.Types.resource()], nextCursor: String.t() | nil}}
          | {:error, any()}
  def list_resources(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @request_timeout)
    cursor = Keyword.get(opts, :cursor)
    GenServer.call(client, {:list_resources, cursor}, timeout)
  end

  @doc """
  Reads a resource by URI.
  """
  @spec read_resource(GenServer.server(), String.t(), timeout()) ::
          {:ok, ExMCP.Types.read_resource_result()} | {:error, any()}
  def read_resource(client, uri, timeout \\ @request_timeout) do
    GenServer.call(client, {:read_resource, uri}, timeout)
  end

  @doc """
  Lists available prompts from the server.

  ## Options
  - `:cursor` - Optional cursor for pagination
  - `:timeout` - Request timeout (default: 30 seconds)

  ## Return value
  Returns a tuple with:
  - `:ok` tuple containing prompts list and optional nextCursor
  - `:error` tuple with error reason

  ## Example
      {:ok, %{prompts: prompts, nextCursor: cursor}} = Client.list_prompts(client, cursor: "page2")
  """
  @spec list_prompts(GenServer.server(), keyword()) ::
          {:ok, %{prompts: [ExMCP.Types.prompt()], nextCursor: String.t() | nil}}
          | {:error, any()}
  def list_prompts(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @request_timeout)
    cursor = Keyword.get(opts, :cursor)
    GenServer.call(client, {:list_prompts, cursor}, timeout)
  end

  @doc """
  Gets a prompt with the given arguments.
  """
  @spec get_prompt(GenServer.server(), String.t(), map(), timeout()) ::
          {:ok, ExMCP.Types.prompt_message()} | {:error, any()}
  def get_prompt(client, name, arguments \\ %{}, timeout \\ @request_timeout) do
    GenServer.call(client, {:get_prompt, name, arguments}, timeout)
  end

  @doc """
  Creates a message using the server's LLM sampling capability.
  """
  @spec create_message(GenServer.server(), ExMCP.Types.create_message_params(), timeout()) ::
          {:ok, ExMCP.Types.create_message_result()} | {:error, any()}
  def create_message(client, params, timeout \\ @request_timeout) do
    GenServer.call(client, {:create_message, params}, timeout)
  end

  @doc """
  Gets server information.
  """
  @spec server_info(GenServer.server()) ::
          {:ok, ExMCP.Types.server_info()} | {:error, :not_initialized}
  def server_info(client) do
    GenServer.call(client, :server_info)
  end

  @doc """
  Gets server capabilities.
  """
  @spec server_capabilities(GenServer.server()) ::
          {:ok, ExMCP.Types.server_capabilities()} | {:error, :not_initialized}
  def server_capabilities(client) do
    GenServer.call(client, :server_capabilities)
  end

  @doc """
  Gets the negotiated protocol version.

  Returns the protocol version that was negotiated during initialization.
  This is the version that both client and server agreed to use.

  ## Examples

      {:ok, "2025-03-26"} = ExMCP.Client.negotiated_version(client)

  """
  @spec negotiated_version(GenServer.server()) ::
          {:ok, String.t()} | {:error, :not_initialized}
  def negotiated_version(client) do
    GenServer.call(client, :negotiated_version)
  end

  @doc """
  Lists available roots from the server.
  """
  @spec list_roots(GenServer.server(), timeout()) ::
          {:ok, %{roots: [ExMCP.Types.root()]}} | {:error, any()}
  def list_roots(client, timeout \\ @request_timeout) do
    GenServer.call(client, :list_roots, timeout)
  end

  @doc """
  Subscribes to resource updates.
  """
  @spec subscribe_resource(GenServer.server(), String.t(), timeout()) ::
          {:ok, map()} | {:error, any()}
  def subscribe_resource(client, uri, timeout \\ @request_timeout) do
    GenServer.call(client, {:subscribe_resource, uri}, timeout)
  end

  @doc """
  Alias for subscribe_resource/3 for backward compatibility.
  """
  @spec subscribe(GenServer.server(), String.t(), timeout()) ::
          {:ok, map()} | {:error, any()}
  def subscribe(client, uri, timeout \\ @request_timeout) do
    subscribe_resource(client, uri, timeout)
  end

  @doc """
  Unsubscribes from resource updates.
  """
  @spec unsubscribe_resource(GenServer.server(), String.t(), timeout()) ::
          {:ok, map()} | {:error, any()}
  def unsubscribe_resource(client, uri, timeout \\ @request_timeout) do
    GenServer.call(client, {:unsubscribe_resource, uri}, timeout)
  end

  @doc """
  Lists available resource templates.
  """
  @spec list_resource_templates(GenServer.server(), keyword()) ::
          {:ok,
           %{resourceTemplates: [ExMCP.Types.resource_template()], nextCursor: String.t() | nil}}
          | {:error, any()}
  def list_resource_templates(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @request_timeout)
    cursor = Keyword.get(opts, :cursor)
    GenServer.call(client, {:list_resource_templates, cursor}, timeout)
  end

  @doc """
  Sends a ping request to the server.
  """
  @spec ping(GenServer.server(), timeout()) :: :ok | {:error, any()}
  def ping(client, timeout \\ @request_timeout) do
    GenServer.call(client, :ping, timeout)
  end

  @doc """
  Sets the server's log level.

  Valid levels are: "debug", "info", "warning", "error".

  > #### Draft Feature {: .info}
  > This implements a draft MCP specification feature (`logging/setLevel`) that may change.

  ## Examples

      {:ok, %{}} = ExMCP.Client.set_log_level(client, "debug")
      {:ok, %{}} = ExMCP.Client.set_log_level(client, "error")

  @doc api: :draft
  """
  @spec set_log_level(GenServer.server(), String.t(), timeout()) :: {:ok, map()} | {:error, any()}
  def set_log_level(client, level, timeout \\ @request_timeout) do
    GenServer.call(client, {:set_log_level, level}, timeout)
  end

  @doc """
  Sends a completion request.
  """
  @spec complete(
          GenServer.server(),
          ExMCP.Types.complete_ref(),
          ExMCP.Types.complete_argument(),
          timeout()
        ) ::
          {:ok, ExMCP.Types.complete_result()} | {:error, any()}
  def complete(client, ref, argument, timeout \\ @request_timeout) do
    GenServer.call(client, {:complete, ref, argument}, timeout)
  end

  @doc """
  Sends a batch of requests to the server.

  JSON-RPC batching is available in 2025-03-26 and removed in draft.
  The initialize request MUST NOT be included in a batch.

  ## Examples

      # Create batch requests
      batch = [
        ExMCP.Protocol.encode_request("tools/list", %{}),
        ExMCP.Protocol.encode_request("resources/list", %{})
      ]
      
      {:ok, responses} = ExMCP.Client.send_batch(client, batch)

  """
  @spec send_batch(GenServer.server(), [map()], timeout()) ::
          {:ok, [map()]} | {:error, any()}
  def send_batch(client, batch, timeout \\ @request_timeout) do
    GenServer.call(client, {:send_batch, batch}, timeout)
  end

  @doc """
  Sends a cancellation notification for a request.

  The request ID should be from a currently in-progress request.
  Cancellation notifications can only be sent in the same direction
  as the original request (e.g., if client made request, client can cancel).

  ## Examples

      # Cancel a specific request
      :ok = ExMCP.Client.send_cancelled(client, "req_123", "User cancelled")

      # Cancel without reason
      :ok = ExMCP.Client.send_cancelled(client, "req_123")

  """
  @spec send_cancelled(GenServer.server(), ExMCP.Types.request_id(), String.t() | nil) :: :ok
  def send_cancelled(client, request_id, reason \\ nil) do
    GenServer.cast(client, {:send_cancelled, request_id, reason})
  end

  @doc """
  Gets the list of currently pending request IDs.

  > #### Extension Feature {: .warning}
  > This is an ExMCP extension for debugging and monitoring, not part of the MCP specification.

  This can be useful for debugging or implementing cancellation UIs.

  ## Examples

      pending_ids = ExMCP.Client.get_pending_requests(client)
      # => ["req_123", "req_124"]

      # Cancel all pending requests
      Enum.each(pending_ids, fn id ->
        ExMCP.Client.send_cancelled(client, id, "Shutdown")
      end)

  """
  @spec get_pending_requests(GenServer.server()) :: [ExMCP.Types.request_id()]
  def get_pending_requests(client) do
    GenServer.call(client, :get_pending_requests)
  end

  @doc """
  Sends a log message notification.
  """
  @spec log_message(GenServer.server(), ExMCP.Types.log_level(), String.t(), any()) :: :ok
  def log_message(client, level, message, data \\ nil) do
    GenServer.cast(client, {:log_message, level, message, data})
  end

  @doc """
  Sends a batch of requests to the server.

  This uses standard JSON-RPC batch processing which is only available
  in protocol version 2025-03-26. The server must support this version
  for batch requests to work.

  Takes a list of request specifications and returns a list of results
  in the same order. Each request spec is a tuple of the function name
  and arguments.

  ## Example

      requests = [
        {:list_tools, []},
        {:list_resources, []},
        {:read_resource, ["file:///data.txt"]}
      ]
      
      {:ok, [tools, resources, content]} = ExMCP.Client.batch_request(client, requests)

  ## Request Specifications

  - `{:list_tools, []}`
  - `{:call_tool, [name, arguments]}` or `{:call_tool, [name, arguments, progress_token]}`
  - `{:list_resources, []}`
  - `{:read_resource, [uri]}`
  - `{:list_prompts, []}`
  - `{:get_prompt, [name, arguments]}`
  - `{:create_message, [messages, options]}`
  - `{:list_roots, []}`
  - `{:list_resource_templates, [cursor]}`
  - `{:ping, []}`
  - `{:complete, [ref, argument]}`
  """
  @spec batch_request(GenServer.server(), list({atom(), list()}), timeout() | nil) ::
          {:ok, list(any())} | {:error, any()}
  def batch_request(client, requests, timeout \\ nil) do
    actual_timeout = timeout || @request_timeout * length(requests)
    GenServer.call(client, {:batch_request, requests}, actual_timeout)
  end

  @doc """
  Disconnects from the MCP server gracefully.

  > #### Extension Feature {: .warning}
  > This is an ExMCP extension for clean disconnection, following draft specification guidelines.

  Performs a clean shutdown by:
  1. Stopping message reception
  2. Closing the transport connection
  3. Cleaning up resources

  ## Example

      :ok = ExMCP.Client.disconnect(client)
  """
  @spec disconnect(GenServer.server()) :: :ok
  def disconnect(client) do
    GenServer.call(client, :disconnect)
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    transport_type = Keyword.fetch!(opts, :transport)
    transport_mod = Transport.get_transport(transport_type)

    client_info =
      Keyword.get(opts, :client_info, %{
        name: "ex_mcp",
        version: ExMCP.version()
      })

    # Get protocol version from options or use preferred
    protocol_version =
      Keyword.get(opts, :protocol_version, ExMCP.VersionRegistry.preferred_version())

    # Initialize handler if provided
    {handler, handler_state} =
      case Keyword.get(opts, :handler) do
        nil ->
          {nil, nil}

        {handler_mod, handler_args} when is_atom(handler_mod) ->
          case handler_mod.init(handler_args) do
            {:ok, initial_state} ->
              {handler_mod, initial_state}

            other ->
              Logger.warning("Handler init returned unexpected value: #{inspect(other)}")
              {nil, nil}
          end

        handler_mod when is_atom(handler_mod) ->
          handler_args = Keyword.get(opts, :handler_state, %{})

          case handler_mod.init(handler_args) do
            {:ok, initial_state} ->
              {handler_mod, initial_state}

            other ->
              Logger.warning("Handler init returned unexpected value: #{inspect(other)}")
              {nil, nil}
          end
      end

    # Initialize authorization if configured
    {auth_config, token_manager} = init_authorization(opts)

    state = %__MODULE__{
      transport_mod: transport_mod,
      transport_opts: opts,
      transport_state: nil,
      pending_requests: %{},
      initialized: false,
      client_info: client_info,
      protocol_version: protocol_version,
      handler: handler,
      handler_state: handler_state,
      auth_config: auth_config,
      token_manager: token_manager
    }

    {:ok, state, {:continue, :connect}}
  end

  @impl true
  def handle_continue(:connect, state) do
    case connect_and_initialize(state) do
      {:ok, new_state} ->
        {:noreply, new_state}

      {:error, reason} ->
        Logger.error("Failed to connect to MCP server: #{inspect(reason)}")
        Process.send_after(self(), :reconnect, @reconnect_interval)
        {:noreply, state}
    end
  end

  @impl true
  def handle_call({:list_tools, cursor}, from, state) do
    send_request(Protocol.encode_list_tools(cursor), from, state)
  end

  def handle_call({:call_tool, name, arguments}, from, state) do
    send_request(Protocol.encode_call_tool(name, arguments), from, state)
  end

  def handle_call({:call_tool, name, arguments, progress_token}, from, state) do
    send_request(Protocol.encode_call_tool(name, arguments, progress_token), from, state)
  end

  def handle_call({:list_resources, cursor}, from, state) do
    send_request(Protocol.encode_list_resources(cursor), from, state)
  end

  def handle_call({:read_resource, uri}, from, state) do
    send_request(Protocol.encode_read_resource(uri), from, state)
  end

  def handle_call({:list_prompts, cursor}, from, state) do
    send_request(Protocol.encode_list_prompts(cursor), from, state)
  end

  def handle_call({:get_prompt, name, arguments}, from, state) do
    send_request(Protocol.encode_get_prompt(name, arguments), from, state)
  end

  def handle_call({:create_message, params}, from, state) do
    send_request(Protocol.encode_create_message(params), from, state)
  end

  def handle_call(:server_info, _from, state) do
    if state.initialized do
      {:reply, {:ok, state.server_info}, state}
    else
      {:reply, {:error, :not_initialized}, state}
    end
  end

  def handle_call(:server_capabilities, _from, state) do
    if state.initialized do
      {:reply, {:ok, state.server_capabilities}, state}
    else
      {:reply, {:error, :not_initialized}, state}
    end
  end

  def handle_call(:negotiated_version, _from, state) do
    if state.initialized do
      {:reply, {:ok, state.negotiated_version}, state}
    else
      {:reply, {:error, :not_initialized}, state}
    end
  end

  def handle_call(:get_pending_requests, _from, state) do
    pending_ids = Map.keys(state.pending_requests)
    {:reply, pending_ids, state}
  end

  def handle_call(:list_roots, from, state) do
    send_request(Protocol.encode_list_roots(), from, state)
  end

  def handle_call({:subscribe_resource, uri}, from, state) do
    send_request(Protocol.encode_subscribe_resource(uri), from, state)
  end

  def handle_call({:unsubscribe_resource, uri}, from, state) do
    send_request(Protocol.encode_unsubscribe_resource(uri), from, state)
  end

  def handle_call({:list_resource_templates, cursor}, from, state) do
    send_request(Protocol.encode_list_resource_templates(cursor), from, state)
  end

  def handle_call(:ping, from, state) do
    send_request(Protocol.encode_ping(), from, state)
  end

  def handle_call({:set_log_level, level}, from, state) do
    send_request(Protocol.encode_set_log_level(level), from, state)
  end

  def handle_call({:complete, ref, argument}, from, state) do
    send_request(Protocol.encode_complete(ref, argument), from, state)
  end

  def handle_call({:send_batch, batch}, from, state) do
    # Check if batch requests are supported in negotiated version
    if state.negotiated_version == "draft" do
      {:reply, {:error, "JSON-RPC batching not supported in draft version"}, state}
    else
      if state.initialized && state.transport_state do
        case Protocol.encode_to_string(batch) do
          {:ok, json} ->
            case send_with_auth(json, state) do
              {:ok, new_transport_state} ->
                # Generate a batch ID to track this batch request
                batch_id = Protocol.generate_id()
                # Store the batch request with a special marker
                pending =
                  Map.put(state.pending_requests, batch_id, {from, :batch_simple, length(batch)})

                {:noreply,
                 %{state | transport_state: new_transport_state, pending_requests: pending}}

              {:error, reason} ->
                {:reply, {:error, reason}, state}
            end

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end
      else
        {:reply, {:error, :not_connected}, state}
      end
    end
  end

  def handle_call(:disconnect, _from, state) do
    # Perform clean shutdown
    if state.transport_state && state.transport_mod do
      # Close the transport connection
      case state.transport_mod.close(state.transport_state) do
        :ok ->
          {:reply, :ok, %{state | transport_state: nil, initialized: false}}

        {:error, reason} ->
          Logger.warning("Error closing transport: #{inspect(reason)}")
          {:reply, :ok, %{state | transport_state: nil, initialized: false}}
      end
    else
      {:reply, :ok, state}
    end
  end

  def handle_call({:batch_request, requests}, from, state) do
    # Check if batch requests are supported in negotiated version
    if state.negotiated_version != "2025-03-26" do
      {:reply,
       {:error,
        "Batch requests only supported in protocol version 2025-03-26, current: #{state.negotiated_version}"},
       state}
    else
      # Build batch of encoded requests
      {batch_messages, request_map} =
        Enum.reduce(requests, {[], %{}}, fn request_spec, {messages, req_map} ->
          {message, id} = encode_request_spec(request_spec, state)
          {[message | messages], Map.put(req_map, id, request_spec)}
        end)

      batch = Protocol.encode_batch(Enum.reverse(batch_messages))

      # Generate a batch ID to track this batch request
      batch_id = Protocol.generate_id()

      case Protocol.encode_to_string(batch) do
        {:ok, json} ->
          case send_with_auth(json, state) do
            {:ok, new_transport_state} ->
              # Store the batch request
              pending = Map.put(state.pending_requests, batch_id, {from, :batch, request_map})

              {:noreply,
               %{state | transport_state: new_transport_state, pending_requests: pending}}

            {:error, reason} ->
              {:reply, {:error, reason}, state}
          end

        {:error, reason} ->
          {:reply, {:error, reason}, state}
      end
    end
  end

  @impl true
  def handle_cast({:send_cancelled, request_id, reason}, state) do
    case Protocol.encode_cancelled(request_id, reason) do
      {:ok, notification} ->
        # Send cancellation notification
        send_notification(notification, state)

        # Remove the request from pending if it exists
        case Map.pop(state.pending_requests, request_id) do
          {nil, _pending} ->
            # Request not found in pending - that's okay
            {:noreply, state}

          {from, new_pending} ->
            # Request was pending, reply with cancellation error and remove it
            GenServer.reply(from, {:error, :cancelled})
            {:noreply, %{state | pending_requests: new_pending}}
        end

      {:error, :cannot_cancel_initialize} ->
        Logger.warning("Cannot cancel initialize request as per MCP specification")
        {:noreply, state}
    end
  end

  def handle_cast({:log_message, level, message, data}, state) do
    send_notification(Protocol.encode_log_message(level, message, data), state)
  end

  @impl true
  def handle_info(:reconnect, state) do
    handle_continue(:connect, state)
  end

  # Handle port messages that might arrive before ownership transfer
  def handle_info({port, {:data, _data}}, state) when is_port(port) do
    # Ignore port messages - they should be handled by the transport
    {:noreply, state}
  end

  def handle_info({port, {:exit_status, _status}}, state) when is_port(port) do
    # Port exited - treat as transport closed
    {:noreply, state}
  end

  def handle_info({:transport_message, message}, state) do
    case Protocol.parse_message(message) do
      {:request, method, params, id} ->
        # Server is making a request to us
        handle_server_request(method, params, id, state)

      {:result, result, id} ->
        handle_response(result, id, state)

      {:error, error, id} ->
        handle_error_response(error, id, state)

      {:notification, method, params} ->
        handle_notification(method, params, state)

      {:batch, messages} ->
        handle_batch_response(messages, state)

      _ ->
        Logger.warning("Unexpected message from server: #{inspect(message)}")
        {:noreply, state}
    end
  end

  def handle_info({:transport_closed, reason}, state) do
    Logger.warning("Transport closed: #{inspect(reason)}")
    Process.send_after(self(), :reconnect, @reconnect_interval)
    {:noreply, %{state | transport_state: nil, initialized: false}}
  end

  # Private functions

  defp connect_and_initialize(state) do
    with {:ok, transport_state} <- state.transport_mod.connect(state.transport_opts),
         :ok <- start_receiver(transport_state, state.transport_mod),
         {:ok, init_result} <- initialize_connection(transport_state, state) do
      new_state = %{
        state
        | transport_state: transport_state,
          server_info: init_result["serverInfo"],
          server_capabilities: init_result["capabilities"],
          negotiated_version: init_result["protocolVersion"],
          initialized: true
      }

      {:ok, new_state}
    end
  end

  defp initialize_connection(transport_state, state) do
    # Use the protocol version from client state
    # Build capabilities based on handler and version
    capabilities = build_client_capabilities(state.handler, state.protocol_version)
    init_msg = Protocol.encode_initialize(state.client_info, capabilities, state.protocol_version)

    with {:ok, json} <- Protocol.encode_to_string(init_msg),
         {:ok, _} <- state.transport_mod.send_message(json, transport_state) do
      # Wait for initialize response
      receive do
        {:transport_message, message} ->
          case Protocol.parse_message(message) do
            {:result, result, _id} ->
              # Validate protocol version
              server_version = result["protocolVersion"]

              # Validate protocol version (currently accepts all for compatibility)
              validate_protocol_version(server_version)
              # Send initialized notification
              notif = Protocol.encode_initialized()
              {:ok, json} = Protocol.encode_to_string(notif)
              state.transport_mod.send_message(json, transport_state)
              {:ok, result}

            {:error, error, _id} ->
              {:error, error}

            _ ->
              {:error, :unexpected_response}
          end
      after
        10_000 ->
          {:error, :initialize_timeout}
      end
    end
  end

  defp start_receiver(transport_state, transport_mod) do
    parent = self()

    spawn_link(fn ->
      receive_loop(transport_state, transport_mod, parent)
    end)

    :ok
  end

  defp receive_loop(transport_state, transport_mod, parent) do
    case transport_mod.receive_message(transport_state) do
      {:ok, message, new_state} ->
        send(parent, {:transport_message, message})
        receive_loop(new_state, transport_mod, parent)

      {:error, reason} ->
        send(parent, {:transport_closed, reason})
    end
  end

  defp send_request(message, from, state) do
    if state.initialized && state.transport_state do
      id = message["id"]

      case Protocol.encode_to_string(message) do
        {:ok, json} ->
          case send_with_auth(json, state) do
            {:ok, new_transport_state} ->
              pending = Map.put(state.pending_requests, id, from)

              new_state = %{
                state
                | transport_state: new_transport_state,
                  pending_requests: pending
              }

              {:noreply, new_state}

            {:error, reason} ->
              {:reply, {:error, reason}, state}
          end

        {:error, reason} ->
          {:reply, {:error, reason}, state}
      end
    else
      {:reply, {:error, :not_connected}, state}
    end
  end

  defp send_notification(message, state) do
    if state.initialized && state.transport_state do
      case Protocol.encode_to_string(message) do
        {:ok, json} ->
          case send_with_auth(json, state) do
            {:ok, new_transport_state} ->
              {:noreply, %{state | transport_state: new_transport_state}}

            {:error, reason} ->
              Logger.error("Failed to send notification: #{inspect(reason)}")
              {:noreply, state}
          end

        {:error, reason} ->
          Logger.error("Failed to encode notification: #{inspect(reason)}")
          {:noreply, state}
      end
    else
      {:noreply, state}
    end
  end

  defp handle_response(result, id, state) do
    case Map.pop(state.pending_requests, id) do
      {nil, _} ->
        Logger.warning("Received response for unknown request #{id}")
        {:noreply, state}

      {from, pending} ->
        # Atomize keys in result for consistent API
        atomized_result = atomize_keys(result)
        GenServer.reply(from, {:ok, atomized_result})
        {:noreply, %{state | pending_requests: pending}}
    end
  end

  defp handle_error_response(error, id, state) do
    case Map.pop(state.pending_requests, id) do
      {nil, _} ->
        Logger.warning("Received error for unknown request #{id}")
        {:noreply, state}

      {from, pending} ->
        GenServer.reply(from, {:error, error})
        {:noreply, %{state | pending_requests: pending}}
    end
  end

  defp handle_batch_response(messages, state) do
    parsed_responses = Protocol.parse_batch_response(messages)
    {results, state} = process_batch_responses(parsed_responses, state)

    {batch_requests, simple_batch_requests} = split_pending_batches(state.pending_requests)
    state = handle_complex_batch_requests(batch_requests, results, state)
    state = handle_simple_batch_requests(simple_batch_requests, parsed_responses, state)

    {:noreply, state}
  end

  defp process_batch_responses(parsed_responses, state) do
    Enum.reduce(parsed_responses, {%{}, state}, fn parsed, {results_acc, state_acc} ->
      case parsed do
        {:result, result, id} ->
          atomized_result = atomize_keys(result)
          {Map.put(results_acc, id, {:ok, atomized_result}), state_acc}

        {:error, error, id} ->
          {Map.put(results_acc, id, {:error, error}), state_acc}

        {:notification, method, params} ->
          {:noreply, new_state} = handle_notification(method, params, state_acc)
          {results_acc, new_state}

        _ ->
          {results_acc, state_acc}
      end
    end)
  end

  defp split_pending_batches(pending_requests) do
    Enum.split_with(pending_requests, fn {_id, req} ->
      match?({_from, :batch, _map}, req)
    end)
  end

  defp handle_complex_batch_requests(batch_requests, results, state) do
    Enum.reduce(batch_requests, state, fn {batch_id, {from, :batch, request_map}}, acc_state ->
      all_responses = Enum.map(request_map, fn {id, _spec} -> Map.get(results, id) end)

      if Enum.all?(all_responses, &(&1 != nil)) do
        GenServer.reply(from, {:ok, all_responses})
        %{acc_state | pending_requests: Map.delete(acc_state.pending_requests, batch_id)}
      else
        acc_state
      end
    end)
  end

  defp handle_simple_batch_requests(simple_batch_requests, parsed_responses, state) do
    Enum.reduce(simple_batch_requests, state, fn {batch_id, req}, acc_state ->
      case req do
        {from, :batch_simple, expected_count} ->
          if length(parsed_responses) == expected_count do
            response_list = convert_simple_batch_responses(parsed_responses)
            GenServer.reply(from, {:ok, response_list})
            %{acc_state | pending_requests: Map.delete(acc_state.pending_requests, batch_id)}
          else
            acc_state
          end

        _ ->
          acc_state
      end
    end)
  end

  defp convert_simple_batch_responses(parsed_responses) do
    Enum.map(parsed_responses, fn parsed ->
      case parsed do
        {:result, result, _id} -> atomize_keys(result)
        {:error, error, _id} -> %{"error" => error}
        _ -> %{}
      end
    end)
  end

  defp handle_notification(method, params, state) do
    case method do
      "notifications/resources/list_changed" ->
        handle_resources_list_changed_notification(state)

      "notifications/resources/updated" ->
        handle_resource_updated_notification(params, state)

      "notifications/tools/list_changed" ->
        handle_tools_list_changed_notification(state)

      "notifications/prompts/list_changed" ->
        handle_prompts_list_changed_notification(state)

      "notifications/progress" ->
        handle_progress_notification(params, state)

      "notifications/roots/list_changed" ->
        handle_roots_list_changed_notification(state)

      "notifications/cancelled" ->
        handle_cancellation_notification(params, state)

      _ ->
        Logger.debug("Received notification: #{method} #{inspect(params)}")
        {:noreply, state}
    end
  end

  # Individual notification handlers
  defp handle_resources_list_changed_notification(state) do
    Logger.info("Resources list changed")
    # Could emit a telemetry event or update a cache here
    {:noreply, state}
  end

  defp handle_resource_updated_notification(params, state) do
    uri = Map.get(params, "uri", "unknown")
    Logger.info("Resource updated: #{uri}")
    {:noreply, state}
  end

  defp handle_tools_list_changed_notification(state) do
    Logger.info("Tools list changed")
    {:noreply, state}
  end

  defp handle_prompts_list_changed_notification(state) do
    Logger.info("Prompts list changed")
    {:noreply, state}
  end

  defp handle_progress_notification(params, state) do
    token = Map.get(params, "progressToken", "unknown")
    progress = Map.get(params, "progress", 0)
    total = Map.get(params, "total")
    message = Map.get(params, "message")

    log_message = format_progress_message(token, progress, total, message)
    Logger.info(log_message)
    {:noreply, state}
  end

  defp handle_roots_list_changed_notification(state) do
    Logger.info("Roots list changed")
    {:noreply, state}
  end

  defp handle_cancellation_notification(params, state) do
    case Map.get(params, "requestId") do
      nil ->
        Logger.warning("Ignoring malformed cancellation notification: #{inspect(params)}")
        {:noreply, state}

      request_id when is_binary(request_id) or is_integer(request_id) ->
        handle_valid_cancellation(request_id, params, state)

      _ ->
        Logger.warning("Ignoring cancellation with invalid requestId type: #{inspect(params)}")
        {:noreply, state}
    end
  end

  defp handle_valid_cancellation(request_id, params, state) do
    request_id_str = to_string(request_id)
    reason = Map.get(params, "reason")
    Logger.info("Request #{request_id_str} cancelled: #{reason || "no reason given"}")

    # Check if this is a valid in-progress request (try both formats)
    cond do
      Map.has_key?(state.pending_requests, request_id) ->
        cancel_pending_request(request_id, state)

      Map.has_key?(state.pending_requests, request_id_str) ->
        cancel_pending_request(request_id_str, state)

      true ->
        Logger.debug("Ignoring cancellation for unknown request #{request_id_str}")
        {:noreply, state}
    end
  end

  defp cancel_pending_request(request_id, state) do
    from = Map.get(state.pending_requests, request_id)
    Logger.debug("Cancelling in-progress request #{request_id}")
    new_pending = Map.delete(state.pending_requests, request_id)
    GenServer.reply(from, {:error, :cancelled})
    {:noreply, %{state | pending_requests: new_pending}}
  end

  defp format_progress_message(token, progress, total, message) do
    cond do
      total && message -> "Progress [#{token}]: #{progress}/#{total} - #{message}"
      total -> "Progress [#{token}]: #{progress}/#{total}"
      message -> "Progress [#{token}]: #{progress} - #{message}"
      true -> "Progress [#{token}]: #{progress}"
    end
  end

  # Helper functions for batch requests

  defp encode_request_spec({:list_tools, []}, _state) do
    request = Protocol.encode_list_tools()
    {request, request["id"]}
  end

  defp encode_request_spec({:call_tool, [name, arguments]}, _state) do
    request = Protocol.encode_call_tool(name, arguments)
    {request, request["id"]}
  end

  defp encode_request_spec({:call_tool, [name, arguments, progress_token]}, _state) do
    request = Protocol.encode_call_tool(name, arguments, progress_token)
    {request, request["id"]}
  end

  defp encode_request_spec({:list_resources, []}, _state) do
    request = Protocol.encode_list_resources()
    {request, request["id"]}
  end

  defp encode_request_spec({:read_resource, [uri]}, _state) do
    request = Protocol.encode_read_resource(uri)
    {request, request["id"]}
  end

  defp encode_request_spec({:list_prompts, []}, _state) do
    request = Protocol.encode_list_prompts()
    {request, request["id"]}
  end

  defp encode_request_spec({:get_prompt, [name, arguments]}, _state) do
    request = Protocol.encode_get_prompt(name, arguments)
    {request, request["id"]}
  end

  defp encode_request_spec({:create_message, [params]}, _state) do
    request = Protocol.encode_create_message(params)
    {request, request["id"]}
  end

  defp encode_request_spec({:list_roots, []}, _state) do
    request = Protocol.encode_list_roots()
    {request, request["id"]}
  end

  defp encode_request_spec({:list_resource_templates, [cursor]}, _state) do
    request = Protocol.encode_list_resource_templates(cursor)
    {request, request["id"]}
  end

  defp encode_request_spec({:ping, []}, _state) do
    # ping is not implemented in Protocol yet
    request = %{
      "jsonrpc" => "2.0",
      "method" => "ping",
      "id" => Protocol.generate_id()
    }

    {request, request["id"]}
  end

  defp encode_request_spec({:complete, [ref, argument]}, _state) do
    request = Protocol.encode_complete(ref, argument)
    {request, request["id"]}
  end

  # Helper functions for server requests

  defp build_client_capabilities(nil, _version), do: %{}

  defp build_client_capabilities(handler, version) do
    version = version || VersionRegistry.preferred_version()
    base = %{}

    # Add roots capability if handler implements list_roots
    base =
      if function_exported?(handler, :handle_list_roots, 1) do
        Map.put(base, "roots", %{})
      else
        base
      end

    # Add sampling capability if handler implements create_message
    base =
      if function_exported?(handler, :handle_create_message, 2) do
        sampling_caps = %{}

        # Add elicitation support for draft version
        sampling_caps =
          if version == "draft" && function_exported?(handler, :handle_elicitation_create, 3) do
            Map.put(sampling_caps, "elicitation", %{})
          else
            sampling_caps
          end

        Map.put(base, "sampling", sampling_caps)
      else
        base
      end

    # Always include experimental capabilities for future extensions
    Map.put(base, "experimental", %{})
  end

  defp validate_protocol_version(server_version) do
    # Currently we support these protocol versions
    supported_versions = ["2025-03-26", "2024-11-05"]

    cond do
      server_version in supported_versions ->
        true

      is_nil(server_version) or server_version == "" ->
        Logger.warning("Server returned empty protocol version")
        # Accept for backwards compatibility
        true

      true ->
        Logger.warning("Server returned unsupported protocol version: #{server_version}")
        # For now, we'll be lenient and accept unknown versions
        # In production, you might want to be more strict
        true
    end
  end

  defp handle_server_request(method, params, id, state) do
    if state.handler do
      case method do
        "ping" ->
          handle_ping_request(id, state)

        "roots/list" ->
          handle_list_roots_request(id, state)

        "sampling/createMessage" ->
          handle_create_message_request(params, id, state)

        "elicitation/create" ->
          handle_elicitation_create_request(params, id, state)

        _ ->
          # Unknown method
          error =
            Protocol.encode_error(
              Protocol.method_not_found(),
              "Method not supported: #{method}",
              nil,
              id
            )

          send_message(error, state)
      end
    else
      # No handler configured, reject all requests
      error =
        Protocol.encode_error(
          Protocol.method_not_found(),
          "Client does not support server requests",
          nil,
          id
        )

      send_message(error, state)
    end
  end

  defp handle_ping_request(id, state) do
    case state.handler.handle_ping(state.handler_state) do
      {:ok, result, new_handler_state} ->
        response = Protocol.encode_response(result, id)
        send_message(response, %{state | handler_state: new_handler_state})

      {:error, reason, new_handler_state} ->
        error =
          Protocol.encode_error(
            Protocol.internal_error(),
            format_error(reason),
            nil,
            id
          )

        send_message(error, %{state | handler_state: new_handler_state})
    end
  end

  defp handle_list_roots_request(id, state) do
    case state.handler.handle_list_roots(state.handler_state) do
      {:ok, roots, new_handler_state} ->
        response = Protocol.encode_response(%{"roots" => roots}, id)
        send_message(response, %{state | handler_state: new_handler_state})

      {:error, reason, new_handler_state} ->
        error =
          Protocol.encode_error(
            Protocol.internal_error(),
            format_error(reason),
            nil,
            id
          )

        send_message(error, %{state | handler_state: new_handler_state})
    end
  end

  defp handle_create_message_request(params, id, state) do
    case state.handler.handle_create_message(params, state.handler_state) do
      {:ok, result, new_handler_state} ->
        response = Protocol.encode_response(result, id)
        send_message(response, %{state | handler_state: new_handler_state})

      {:error, reason, new_handler_state} ->
        error =
          Protocol.encode_error(
            Protocol.internal_error(),
            format_error(reason),
            nil,
            id
          )

        send_message(error, %{state | handler_state: new_handler_state})
    end
  end

  defp handle_elicitation_create_request(params, id, state) do
    case state.handler.handle_elicitation_create(
           params["message"],
           params["requestedSchema"],
           state.handler_state
         ) do
      {:ok, result, new_handler_state} ->
        response = Protocol.encode_response(result, id)
        send_message(response, %{state | handler_state: new_handler_state})

      {:error, reason, new_handler_state} ->
        error =
          Protocol.encode_error(
            Protocol.internal_error(),
            format_error(reason),
            nil,
            id
          )

        send_message(error, %{state | handler_state: new_handler_state})
    end
  end

  defp send_message(message, state) do
    if state.initialized && state.transport_state do
      case Protocol.encode_to_string(message) do
        {:ok, json} ->
          case state.transport_mod.send_message(json, state.transport_state) do
            {:ok, new_transport_state} ->
              {:noreply, %{state | transport_state: new_transport_state}}

            {:error, reason} ->
              Logger.error("Failed to send message: #{inspect(reason)}")
              {:noreply, state}
          end

        {:error, reason} ->
          Logger.error("Failed to encode message: #{inspect(reason)}")
          {:noreply, state}
      end
    else
      Logger.warning("Cannot send message - not connected")
      {:noreply, state}
    end
  end

  defp format_error(reason) when is_binary(reason), do: reason
  defp format_error(reason), do: inspect(reason)

  # Helper to atomize keys in response maps
  defp atomize_keys(map) when is_map(map) do
    Map.new(map, fn
      {k, v} when is_binary(k) ->
        atom_key = safe_string_to_atom(k)
        {atom_key, atomize_keys(v)}

      {k, v} ->
        {k, atomize_keys(v)}
    end)
  end

  defp atomize_keys(list) when is_list(list) do
    Enum.map(list, &atomize_keys/1)
  end

  defp atomize_keys(value), do: value

  # Safe string to atom conversion for known MCP protocol keys
  defp safe_string_to_atom(key)
       when key in [
              "mimeType",
              "nextCursor",
              "tools",
              "resources",
              "prompts",
              "contents",
              "uri",
              "text",
              "blob",
              "name",
              "description",
              "version",
              "capabilities",
              "protocolVersion",
              "serverInfo",
              "clientInfo",
              "messages",
              "role",
              "content",
              "type",
              "inputSchema",
              "outputSchema",
              "annotations",
              "arguments",
              "required",
              "listChanged",
              "subscribe",
              "hasArguments",
              "values",
              "experimental",
              "batchProcessing",
              "completion",
              "temperature",
              "conditions",
              "humidity",
              "wind",
              "speed",
              "direction",
              "structuredContent",
              "isError",
              "meta",
              "status",
              "elicitationId",
              "requiresAuth",
              "scopes"
            ] do
    String.to_atom(key)
  end

  defp safe_string_to_atom(key) do
    try do
      String.to_existing_atom(key)
    rescue
      ArgumentError -> key
    end
  end

  # Authorization helpers

  defp init_authorization(opts) do
    case Keyword.get(opts, :auth_config) do
      nil ->
        {nil, nil}

      auth_config ->
        # Start token manager
        {:ok, token_manager} =
          TokenManager.start_link(
            auth_config: auth_config,
            initial_token: auth_config[:initial_token]
          )

        {auth_config, token_manager}
    end
  end

  defp send_with_auth(json, state) do
    # Check if we have authorization configured
    if state.token_manager && state.transport_mod == ExMCP.Transport.HTTP do
      # Get current token
      case TokenManager.get_token(state.token_manager) do
        {:ok, token} ->
          # Add authorization header to transport state
          enhanced_transport_state = add_auth_to_transport(state.transport_state, token)

          # Send with enhanced transport state
          case state.transport_mod.send_message(json, enhanced_transport_state) do
            {:ok, new_transport_state} ->
              # Extract the base transport state without auth header for storage
              {:ok, restore_transport_state(new_transport_state)}

            {:error, {:http_error, 401, body}} ->
              # Token might be expired, try refreshing
              handle_auth_error_with_retry({:error, {:http_error, 401, body}}, json, state)

            {:error, {:http_error, 403, body}} ->
              # Forbidden - check if we need different permissions
              handle_auth_error_with_retry({:error, {:http_error, 403, body}}, json, state)

            error ->
              error
          end

        {:error, :no_token} ->
          # No token available, proceed without auth
          state.transport_mod.send_message(json, state.transport_state)

        {:error, reason} ->
          {:error, {:auth_error, reason}}
      end
    else
      # No auth configured or not HTTP transport
      state.transport_mod.send_message(json, state.transport_state)
    end
  end

  defp add_auth_to_transport(%ExMCP.Transport.HTTP{headers: headers} = transport_state, token) do
    # Add or update Authorization header
    auth_header = {"Authorization", "Bearer #{token}"}
    updated_headers = update_header(headers, "Authorization", auth_header)
    %{transport_state | headers: updated_headers}
  end

  defp restore_transport_state(%ExMCP.Transport.HTTP{headers: headers} = transport_state) do
    # Remove Authorization header to avoid storing it in state
    filtered_headers =
      Enum.reject(headers, fn {name, _} ->
        String.downcase(name) == "authorization"
      end)

    %{transport_state | headers: filtered_headers}
  end

  defp update_header(headers, header_name, new_header) do
    # Remove existing header (case-insensitive) and add new one
    filtered =
      Enum.reject(headers, fn {name, _} ->
        String.downcase(name) == String.downcase(header_name)
      end)

    [new_header | filtered]
  end

  defp handle_auth_error_with_retry({:error, {:http_error, status, body}}, json, state) do
    # Use the error handler to determine what to do
    case ErrorHandler.handle_auth_error(
           status,
           # We don't have response headers here
           [],
           body,
           %{token_manager: state.token_manager}
         ) do
      {:retry, %{action: :refresh_token}} ->
        # Try to refresh token
        case TokenManager.refresh_now(state.token_manager) do
          {:ok, _new_token} ->
            # Retry the request with new token
            send_with_auth(json, state)

          error ->
            error
        end

      {:error, reason} ->
        {:error, {:auth_error, reason}}

      _ ->
        {:error, {:http_error, status, body}}
    end
  end
end
