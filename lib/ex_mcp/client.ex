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

  alias ExMCP.{Protocol, Transport}

  @reconnect_interval 5_000
  @request_timeout 30_000

  # Client state
  defstruct [
    :transport_mod,
    :transport_opts,
    :transport_state,
    :server_info,
    :server_capabilities,
    :pending_requests,
    :initialized,
    :client_info,
    :handler,
    :handler_state
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

  Transport-specific options are passed through.

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
          {:ok, ExMCP.Types.resource_content()} | {:error, any()}
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
          {:ok, ExMCP.Types.capabilities()} | {:error, :not_initialized}
  def server_capabilities(client) do
    GenServer.call(client, :server_capabilities)
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

  ## Examples

      {:ok, %{}} = ExMCP.Client.set_log_level(client, "debug")
      {:ok, %{}} = ExMCP.Client.set_log_level(client, "error")
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

  > #### Extension Feature {: .warning}
  > This is an ExMCP extension for efficient request batching, not part of the MCP specification.

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

    state = %__MODULE__{
      transport_mod: transport_mod,
      transport_opts: opts,
      transport_state: nil,
      pending_requests: %{},
      initialized: false,
      client_info: client_info,
      handler: handler,
      handler_state: handler_state
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
        case state.transport_mod.send_message(json, state.transport_state) do
          {:ok, new_transport_state} ->
            # Store the batch request
            pending = Map.put(state.pending_requests, batch_id, {from, :batch, request_map})
            {:noreply, %{state | transport_state: new_transport_state, pending_requests: pending}}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end

      {:error, reason} ->
        {:reply, {:error, reason}, state}
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
          initialized: true
      }

      {:ok, new_state}
    end
  end

  defp initialize_connection(transport_state, state) do
    # Build capabilities based on handler
    capabilities = build_client_capabilities(state.handler)
    init_msg = Protocol.encode_initialize(state.client_info, capabilities)

    with {:ok, json} <- Protocol.encode_to_string(init_msg),
         {:ok, _} <- state.transport_mod.send_message(json, transport_state) do
      # Wait for initialize response
      receive do
        {:transport_message, message} ->
          case Protocol.parse_message(message) do
            {:result, result, _id} ->
              # Validate protocol version
              server_version = result["protocolVersion"]

              if validate_protocol_version(server_version) do
                # Send initialized notification
                notif = Protocol.encode_initialized()
                {:ok, json} = Protocol.encode_to_string(notif)
                state.transport_mod.send_message(json, transport_state)
                {:ok, result}
              else
                {:error, {:incompatible_version, server_version}}
              end

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
          case state.transport_mod.send_message(json, state.transport_state) do
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
          case state.transport_mod.send_message(json, state.transport_state) do
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
    # Process batch responses
    parsed_responses = Protocol.parse_batch_response(messages)

    # Group responses by ID
    {results, state} =
      Enum.reduce(parsed_responses, {%{}, state}, fn parsed, {results_acc, state_acc} ->
        case parsed do
          {:result, result, id} ->
            # Atomize keys in result for consistent API
            atomized_result = atomize_keys(result)
            {Map.put(results_acc, id, {:ok, atomized_result}), state_acc}

          {:error, error, id} ->
            {Map.put(results_acc, id, {:error, error}), state_acc}

          {:notification, method, params} ->
            # Handle notifications that come in batch responses
            {:noreply, new_state} = handle_notification(method, params, state_acc)
            {results_acc, new_state}

          _ ->
            {results_acc, state_acc}
        end
      end)

    # Find any batch request waiting for these responses
    {batch_requests, _remaining_pending} =
      Enum.split_with(state.pending_requests, fn {_id, req} ->
        match?({_from, :batch, _map}, req)
      end)

    # Check if we have a complete batch response
    state =
      Enum.reduce(batch_requests, state, fn {batch_id, {from, :batch, request_map}}, acc_state ->
        # Check if all requests in the batch have responses
        all_responses =
          Enum.map(request_map, fn {id, _spec} ->
            Map.get(results, id)
          end)

        if Enum.all?(all_responses, &(&1 != nil)) do
          # All responses received, reply to caller
          GenServer.reply(from, {:ok, all_responses})

          # Remove from pending
          %{acc_state | pending_requests: Map.delete(acc_state.pending_requests, batch_id)}
        else
          # Still waiting for some responses
          acc_state
        end
      end)

    {:noreply, state}
  end

  defp handle_notification(method, params, state) do
    case method do
      "notifications/resources/list_changed" ->
        Logger.info("Resources list changed")
        # Could emit a telemetry event or update a cache here
        {:noreply, state}

      "notifications/resources/updated" ->
        uri = Map.get(params, "uri", "unknown")
        Logger.info("Resource updated: #{uri}")
        {:noreply, state}

      "notifications/tools/list_changed" ->
        Logger.info("Tools list changed")
        {:noreply, state}

      "notifications/prompts/list_changed" ->
        Logger.info("Prompts list changed")
        {:noreply, state}

      "notifications/progress" ->
        token = Map.get(params, "progressToken", "unknown")
        progress = Map.get(params, "progress", 0)
        total = Map.get(params, "total")

        if total do
          Logger.info("Progress [#{token}]: #{progress}/#{total}")
        else
          Logger.info("Progress [#{token}]: #{progress}")
        end

        {:noreply, state}

      "notifications/roots/list_changed" ->
        Logger.info("Roots list changed")
        {:noreply, state}

      "notifications/cancelled" ->
        case Map.get(params, "requestId") do
          nil ->
            # Malformed cancellation notification (missing requestId)
            Logger.warning("Ignoring malformed cancellation notification: #{inspect(params)}")
            {:noreply, state}

          request_id when is_binary(request_id) or is_integer(request_id) ->
            # Convert to string for consistent handling
            request_id_str = to_string(request_id)
            reason = Map.get(params, "reason")
            Logger.info("Request #{request_id_str} cancelled: #{reason || "no reason given"}")

            # Check if this is a valid in-progress request (try both formats)
            cond do
              Map.has_key?(state.pending_requests, request_id) ->
                from = Map.get(state.pending_requests, request_id)
                Logger.debug("Cancelling in-progress request #{request_id}")
                new_pending = Map.delete(state.pending_requests, request_id)
                GenServer.reply(from, {:error, :cancelled})
                {:noreply, %{state | pending_requests: new_pending}}

              Map.has_key?(state.pending_requests, request_id_str) ->
                from = Map.get(state.pending_requests, request_id_str)
                Logger.debug("Cancelling in-progress request #{request_id_str}")
                new_pending = Map.delete(state.pending_requests, request_id_str)
                GenServer.reply(from, {:error, :cancelled})
                {:noreply, %{state | pending_requests: new_pending}}

              true ->
                # Request not found or already completed - ignore as per spec
                Logger.debug("Ignoring cancellation for unknown request #{request_id_str}")
                {:noreply, state}
            end

          _ ->
            # Invalid requestId type
            Logger.warning(
              "Ignoring cancellation with invalid requestId type: #{inspect(params)}"
            )

            {:noreply, state}
        end

      _ ->
        Logger.debug("Received notification: #{method} #{inspect(params)}")
        {:noreply, state}
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

  defp build_client_capabilities(nil), do: %{}

  defp build_client_capabilities(handler) do
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
        Map.put(base, "sampling", %{})
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
      {k, v} when is_binary(k) -> {String.to_existing_atom(k), atomize_keys(v)}
      {k, v} -> {k, atomize_keys(v)}
    end)
  rescue
    ArgumentError ->
      # If atom doesn't exist, keep string keys
      Map.new(map, fn {k, v} -> {k, atomize_keys(v)} end)
  end

  defp atomize_keys(list) when is_list(list) do
    Enum.map(list, &atomize_keys/1)
  end

  defp atomize_keys(value), do: value
end
