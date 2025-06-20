defmodule ExMCP.Server.Legacy do
  @moduledoc """
  MCP server implementation that handles connections from MCP clients.

  This module implements the Model Context Protocol server specification,
  providing a GenServer that manages client connections and delegates
  functionality to a user-provided handler module.

  ## Quick Start

      defmodule MyHandler do
        use ExMCP.Server.Handler

        @impl true
        def handle_list_tools(state) do
          tools = [%{name: "echo", description: "Echo input"}]
          {:ok, tools, state}
        end

        @impl true
        def handle_call_tool("echo", params, state) do
          {:ok, [%{type: "text", text: params["message"]}], state}
        end
      end

      {:ok, server} = ExMCP.Server.start_link(
        handler: MyHandler,
        transport: :stdio
      )

  ## Features

  **MCP Standard Features:**
  - Tools execution and listing
  - Resource management and reading
  - Prompt templates and generation
  - Progress notifications for long operations
  - Resource change subscriptions
  - Roots for URI boundaries
  - LLM sampling support

  **Supported Transports:**
  - `:stdio` - Standard I/O communication (MCP standard)
  - `:http` - HTTP with optional SSE streaming (MCP standard)
  - `:test` - In-memory transport for testing

  ## Handler Implementation

  Your handler module must implement the `ExMCP.Server.Handler` behaviour.
  See `ExMCP.Server.Handler` documentation for required callbacks and examples.

  ## Example

      defmodule MyHandler do
        use ExMCP.Server.Handler

        @impl true
        def handle_initialize(_params, state) do
          {:ok, %{
            name: "my-server",
            version: "1.0.0",
            capabilities: %{
              tools: %{},
              resources: %{},
              prompts: %{},
              sampling: %{}  # Enable LLM features
            }
          }, state}
        end

        @impl true
        def handle_call_tool("process", params, state) do
          # Use progress notifications for long operations
          if progress_token = params["_progressToken"] do
            Task.start(fn ->
              for i <- 1..100 do
                ExMCP.Server.notify_progress(self(), progress_token, i, 100)
                Process.sleep(100)
              end
            end)
          end

          {:ok, [%{type: "text", text: "Processing..."}], state}
        end

        # Implement sampling for LLM integration
        @impl true
        def handle_create_message(params, state) do
          # Your LLM integration here
          {:ok, %{content: %{type: "text", text: "Response"}}, state}
        end
      end

      # Start server
      {:ok, server} = ExMCP.Server.start_link(
        handler: MyHandler,
        transport: :stdio  # or :http
      )

  ## Notifications

  The server can send various notifications to connected clients:

      # Notify about resource changes
      ExMCP.Server.notify_resources_changed(server)
      ExMCP.Server.notify_resource_updated(server, "file:///path")

      # Notify about tool changes
      ExMCP.Server.notify_tools_changed(server)

      # Notify about prompt changes
      ExMCP.Server.notify_prompts_changed(server)

      # Notify about roots changes
      ExMCP.Server.notify_roots_changed(server)

      # Send progress updates
      ExMCP.Server.notify_progress(server, "token", 50, 100)
  """

  use GenServer
  require Logger

  alias ExMCP.Transport
  alias ExMCP.Internal.{Protocol, VersionRegistry}

  defstruct [
    :handler,
    :handler_state,
    :transport_mod,
    :transport_state,
    :initialized,
    :pending_requests,
    :negotiated_version
  ]

  @doc """
  Starts an MCP server.

  ## Options

  - `:handler` - Module implementing `ExMCP.Server.Handler` behaviour (required)
  - `:transport` - Transport type (:stdio, :http, or module)
  - `:handler_args` - Arguments passed to handler's init/1 callback
  - `:name` - GenServer name (optional)

  Transport-specific options are passed through.
  """
  def start_link(opts) do
    {name, opts} = Keyword.pop(opts, :name)
    gen_opts = if name, do: [name: name], else: []
    GenServer.start_link(__MODULE__, opts, gen_opts)
  end

  @doc """
  Notifies connected clients that the resources list has changed.
  """
  def notify_resources_changed(server) do
    GenServer.cast(server, :notify_resources_changed)
  end

  @doc """
  Notifies connected clients that a specific resource has been updated.
  """
  def notify_resource_updated(server, uri) do
    GenServer.cast(server, {:notify_resource_updated, uri})
  end

  @doc """
  Notifies connected clients that the tools list has changed.
  """
  def notify_tools_changed(server) do
    GenServer.cast(server, :notify_tools_changed)
  end

  @doc """
  Notifies connected clients that the prompts list has changed.
  """
  def notify_prompts_changed(server) do
    GenServer.cast(server, :notify_prompts_changed)
  end

  @doc """
  Notifies connected clients that the roots list has changed.
  """
  def notify_roots_changed(server) do
    GenServer.cast(server, :notify_roots_changed)
  end

  @doc """
  Sends a progress notification to connected clients.

  ## Parameters
  - `progress_token` - Token identifying the operation
  - `progress` - Current progress value (0-100 for percentage, or any positive number)
  - `total` - Optional total value for the operation
  - `message` - Optional human-readable progress message
  """
  def notify_progress(server, progress_token, progress, total \\ nil, message \\ nil) do
    GenServer.cast(server, {:notify_progress, progress_token, progress, total, message})
  end

  @doc """
  Sends a ping request to the client.

  The client must respond promptly or may be disconnected.
  """
  @spec ping(GenServer.server(), timeout()) :: {:ok, map()} | {:error, any()}
  def ping(server, timeout \\ 5000) do
    GenServer.call(server, :ping, timeout)
  end

  @doc """
  Requests the list of roots from the client.

  This is used when the server needs to understand what file system
  locations the client has access to.
  """
  @spec list_roots(GenServer.server(), timeout()) :: {:ok, [map()]} | {:error, any()}
  def list_roots(server, timeout \\ 5000) do
    GenServer.call(server, :list_roots, timeout)
  end

  @doc """
  Gets the negotiated protocol version.

  Returns the protocol version that was negotiated during initialization.
  """
  @spec negotiated_version(GenServer.server()) ::
          {:ok, String.t() | nil} | {:error, :not_initialized}
  def negotiated_version(server) do
    GenServer.call(server, :negotiated_version)
  end

  @doc """
  Requests information from the user via elicitation.

  This feature is available in protocol version 2025-06-18 and later.

  ## Parameters
  - message: Human-readable message explaining what information is needed
  - requested_schema: JSON schema defining the expected response structure
  """
  @spec elicit_information(GenServer.server(), String.t(), map(), timeout()) ::
          {:ok, %{action: String.t(), content: map() | nil}} | {:error, any()}
  def elicit_information(server, message, requested_schema, timeout \\ 30_000) do
    GenServer.call(server, {:elicit_information, message, requested_schema}, timeout)
  end

  @doc """
  Requests the client to sample an LLM.

  The client has full discretion over model selection and should
  inform the user before sampling (human in the loop).

  ## Options

  - `:messages` (required) - List of messages to send to the LLM
  - `:model_preferences` - Server's model preferences (may be ignored)
  - `:system_prompt` - System prompt to use (may be modified/omitted)
  - `:include_context` - Whether to include MCP context ("none", "thisServer", "allServers")
  - `:temperature` - Sampling temperature
  - `:max_tokens` - Maximum tokens to sample
  """
  @spec create_message(GenServer.server(), map(), timeout()) :: {:ok, map()} | {:error, any()}
  def create_message(server, params, timeout \\ 30000) do
    GenServer.call(server, {:create_message, params}, timeout)
  end

  @doc """
  Sends a log message to the client.

  ## Parameters
  - server: The server process
  - level: Log level ("debug", "info", "warning", "error")
  - message: The log message content
  - data: Optional structured data (map or any JSON-serializable data)
  """
  @spec send_log_message(GenServer.server(), String.t(), String.t(), any()) :: :ok
  def send_log_message(server, level, message, data \\ nil) do
    GenServer.cast(server, {:send_log_message, level, message, data})
  end

  @doc """
  Stops the server process.

  This is a wrapper around GenServer.stop/1 for API consistency.
  """
  @spec stop(GenServer.server()) :: :ok
  def stop(server) do
    GenServer.stop(server)
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    handler = Keyword.fetch!(opts, :handler)
    handler_args = Keyword.get(opts, :handler_args, [])
    transport_type = Keyword.fetch!(opts, :transport)
    transport_mod = Transport.get_transport(transport_type)

    with {:ok, handler_state} <- handler.init(handler_args),
         {:ok, transport_state} <- init_transport(transport_mod, transport_type, opts) do
      state = %__MODULE__{
        handler: handler,
        handler_state: handler_state,
        transport_mod: transport_mod,
        transport_state: transport_state,
        initialized: false,
        pending_requests: %{}
      }

      # Start receiving messages
      {:ok, state, {:continue, :start_receiver}}
    else
      {:error, reason} ->
        {:stop, reason}
    end
  end

  @impl true
  def handle_continue(:start_receiver, state) do
    start_receiver(state.transport_state, state.transport_mod)
    {:noreply, state}
  end

  @impl true
  def handle_info({:test_transport_connect, client_pid}, state) do
    # Update the test transport state with the connected client
    if state.transport_mod == ExMCP.Transport.Test do
      new_transport_state = %{state.transport_state | peer_pid: client_pid}
      new_state = %{state | transport_state: new_transport_state}
      {:noreply, new_state}
    else
      {:noreply, state}
    end
  end

  @impl true
  def handle_info({:transport_message, message}, state) do
    case Protocol.parse_message(message) do
      {:request, method, params, id} ->
        handle_request(method, params, id, state)

      {:result, result, id} ->
        handle_response(result, id, state)

      {:error, error, id} ->
        handle_error_response(error, id, state)

      {:notification, method, params} ->
        handle_notification(method, params, state)

      {:batch, messages} ->
        # Standard JSON-RPC batch (array of requests)
        # Only supported in 2025-03-26 version
        if state.negotiated_version == "2025-03-26" do
          handle_batch(messages, state)
        else
          Logger.warning(
            "Batch requests require protocol version '2025-03-26', current: #{state.negotiated_version}"
          )

          {:noreply, state}
        end

      _ ->
        Logger.warning("Invalid message received: #{inspect(message)}")
        {:noreply, state}
    end
  end

  def handle_info({:transport_closed, reason}, state) do
    Logger.info("Transport closed: #{inspect(reason)}")

    # For enhanced BEAM transport, we can accept new connections after a client disconnects
    # Reset the transport state to allow new connections
    if state.transport_mod == ExMCP.Transport.Beam.Enhanced do
      # Keep the server running but reset initialization state
      {:noreply, %{state | initialized: false}}
    else
      # For other transports (stdio, SSE), stop the server
      {:stop, :normal, state}
    end
  end

  @impl true
  def handle_cast(:notify_resources_changed, state) do
    send_notification(Protocol.encode_resources_changed(), state)
  end

  def handle_cast({:notify_resource_updated, uri}, state) do
    send_notification(Protocol.encode_resource_updated(uri), state)
  end

  def handle_cast(:notify_tools_changed, state) do
    send_notification(Protocol.encode_tools_changed(), state)
  end

  def handle_cast(:notify_prompts_changed, state) do
    send_notification(Protocol.encode_prompts_changed(), state)
  end

  def handle_cast(:notify_roots_changed, state) do
    send_notification(Protocol.encode_roots_changed(), state)
  end

  def handle_cast({:notify_progress, progress_token, progress, total, message}, state) do
    send_notification(Protocol.encode_progress(progress_token, progress, total, message), state)
  end

  def handle_cast({:send_log_message, level, message, data}, state) do
    params = %{
      "level" => level,
      "logger" => "ExMCP.Server",
      "message" => message
    }

    # Add data field if provided
    params = if data, do: Map.put(params, "data", data), else: params

    notification = Protocol.encode_notification("notifications/message", params)
    send_message(notification, state)
  end

  # Legacy handler for backward compatibility (without data parameter)
  def handle_cast({:send_log_message, level, message}, state) do
    handle_cast({:send_log_message, level, message, nil}, state)
  end

  # Handle manual message injection for testing
  def handle_cast({:handle_message, message}, state) do
    json_message = Jason.encode!(message)

    case Protocol.parse_message(json_message) do
      {:notification, method, params} ->
        handle_notification(method, params, state)

      {:request, method, params, id} ->
        handle_request(method, params, id, state)

      _ ->
        {:noreply, state}
    end
  end

  @impl true
  def handle_call(:negotiated_version, _from, state) do
    if state.initialized do
      {:reply, {:ok, state.negotiated_version}, state}
    else
      {:reply, {:error, :not_initialized}, state}
    end
  end

  def handle_call(:ping, from, state) do
    send_request(Protocol.encode_ping(), from, state)
  end

  def handle_call(:list_roots, from, state) do
    send_request(Protocol.encode_list_roots(), from, state)
  end

  def handle_call({:elicit_information, message, requested_schema}, from, state) do
    # Check if elicitation is supported in negotiated version
    if state.negotiated_version != "2025-06-18" do
      {:reply,
       {:error,
        "Elicitation only supported in protocol version 2025-06-18 or later, current: #{state.negotiated_version}"},
       state}
    else
      request = Protocol.encode_elicitation_create(message, requested_schema)
      send_request(request, from, state)
    end
  end

  def handle_call({:create_message, params}, from, state) do
    send_request(Protocol.encode_create_message(params), from, state)
  end

  # Hot reload support callbacks
  def handle_call(:get_handler_info, _from, state) do
    {:reply, {:ok, %{handler: state.handler}}, state}
  end

  def handle_call(:get_handler_state, _from, state) do
    {:reply, {:ok, state.handler_state}, state}
  end

  def handle_call({:update_handler_state, new_handler_state}, _from, state) do
    {:reply, :ok, %{state | handler_state: new_handler_state}}
  end

  def handle_call({:reload_handler, new_handler_module}, _from, state) do
    # Perform complete handler reload with state migration
    case reload_handler_with_migration(state, new_handler_module) do
      {:ok, new_state} ->
        {:reply, :ok, new_state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def terminate(reason, state) do
    state.handler.terminate(reason, state.handler_state)

    if state.transport_state do
      state.transport_mod.close(state.transport_state)
    end
  end

  # Private functions

  defp start_receiver(transport_state, transport_mod) do
    parent = self()

    spawn_link(fn ->
      receive_loop(transport_state, transport_mod, parent)
    end)
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

  defp handle_request("initialize", params, id, state) do
    case state.handler.handle_initialize(params, state.handler_state) do
      {:ok, result, new_handler_state} ->
        # Extract negotiated version from result
        negotiated_version = result[:protocolVersion] || result["protocolVersion"]

        response = Protocol.encode_response(result, id)

        send_message(response, %{
          state
          | handler_state: new_handler_state,
            initialized: true,
            negotiated_version: negotiated_version
        })

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

  defp handle_request("tools/list", params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      cursor = Map.get(params || %{}, "cursor")

      case state.handler.handle_list_tools(cursor, state.handler_state) do
        {:ok, tools, next_cursor, new_handler_state} ->
          result = %{tools: tools}
          result = if next_cursor, do: Map.put(result, :nextCursor, next_cursor), else: result
          response = Protocol.encode_response(result, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error_code = get_error_code_for_reason(reason)

          error =
            Protocol.encode_error(
              error_code,
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("tools/call", params, id, state)
       when is_map(params) and is_map_key(params, "name") do
    name = params["name"]
    args = params["arguments"] || %{}

    # If _meta is present in params, add it to arguments for the handler
    args_with_meta =
      case params["_meta"] do
        nil -> args
        meta -> Map.put(args, "_meta", meta)
      end

    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_call_tool(name, args_with_meta, state.handler_state) do
        {:ok, result, new_handler_state} ->
          # Check if result is already a map with content key (for isError support)
          response_body =
            if is_map(result) and Map.has_key?(result, :content) do
              result
            else
              # Legacy format - just content array
              %{content: result}
            end

          response = Protocol.encode_response(response_body, id)
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
  end

  defp handle_request("resources/list", params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      cursor = Map.get(params || %{}, "cursor")

      case state.handler.handle_list_resources(cursor, state.handler_state) do
        {:ok, resources, next_cursor, new_handler_state} ->
          result = %{resources: resources}
          result = if next_cursor, do: Map.put(result, :nextCursor, next_cursor), else: result
          response = Protocol.encode_response(result, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error_code = get_error_code_for_reason(reason)

          error =
            Protocol.encode_error(
              error_code,
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("resources/read", %{"uri" => uri}, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_read_resource(uri, state.handler_state) do
        {:ok, content, new_handler_state} ->
          response = Protocol.encode_response(%{contents: [content]}, id)
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
  end

  defp handle_request("prompts/list", params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      cursor = Map.get(params || %{}, "cursor")

      case state.handler.handle_list_prompts(cursor, state.handler_state) do
        {:ok, prompts, next_cursor, new_handler_state} ->
          result = %{prompts: prompts}
          result = if next_cursor, do: Map.put(result, :nextCursor, next_cursor), else: result
          response = Protocol.encode_response(result, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error_code = get_error_code_for_reason(reason)

          error =
            Protocol.encode_error(
              error_code,
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("prompts/get", %{"name" => name} = params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      arguments = Map.get(params, "arguments", %{})

      case state.handler.handle_get_prompt(name, arguments, state.handler_state) do
        {:ok, prompt_data, new_handler_state} ->
          # prompt_data should contain both messages and description
          response = Protocol.encode_response(prompt_data, id)
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
  end

  defp handle_request("sampling/createMessage", params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
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
  end

  defp handle_request("logging/setLevel", %{"level" => level}, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case validate_method_version("logging/setLevel", id, state) do
        :ok ->
          case state.handler.handle_set_log_level(level, state.handler_state) do
            {:ok, new_handler_state} ->
              response = Protocol.encode_response(%{}, id)
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

        :version_error ->
          {:noreply, state}
      end
    end
  end

  defp handle_request("logging/setLevel", _params, id, state) do
    # Missing required "level" parameter
    error =
      Protocol.encode_error(
        Protocol.invalid_params(),
        "Missing required parameter: level",
        nil,
        id
      )

    send_message(error, state)
  end

  defp handle_request("roots/list", _params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_list_roots(state.handler_state) do
        {:ok, roots, new_handler_state} ->
          response = Protocol.encode_response(%{roots: roots}, id)
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
  end

  defp handle_request("resources/subscribe", %{"uri" => uri} = _params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case validate_method_version("resources/subscribe", id, state) do
        :ok ->
          case state.handler.handle_subscribe_resource(uri, state.handler_state) do
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

        :version_error ->
          {:noreply, state}
      end
    end
  end

  defp handle_request("resources/unsubscribe", %{"uri" => uri} = _params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case validate_method_version("resources/unsubscribe", id, state) do
        :ok ->
          case state.handler.handle_unsubscribe_resource(uri, state.handler_state) do
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

        :version_error ->
          {:noreply, state}
      end
    end
  end

  defp handle_request("resources/templates/list", params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      cursor = Map.get(params || %{}, "cursor")

      case state.handler.handle_list_resource_templates(cursor, state.handler_state) do
        {:ok, templates, next_cursor, new_handler_state} ->
          result = %{resourceTemplates: templates}
          result = if next_cursor, do: Map.put(result, :nextCursor, next_cursor), else: result
          response = Protocol.encode_response(result, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error_code = get_error_code_for_reason(reason)

          error =
            Protocol.encode_error(
              error_code,
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("ping", _params, id, state) do
    # Ping always responds with empty result
    response = Protocol.encode_pong(id)
    send_message(response, state)
  end

  defp handle_request(
         "completion/complete",
         %{"ref" => ref, "argument" => argument} = _params,
         id,
         state
       ) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_complete(ref, argument, state.handler_state) do
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
  end

  # Handle missing required parameters for known methods
  defp handle_request("tools/call", _params, id, state) do
    error =
      Protocol.encode_error(
        Protocol.invalid_params(),
        "Missing required parameters: name and arguments",
        nil,
        id
      )

    send_message(error, state)
  end

  defp handle_request("resources/read", _params, id, state) do
    error =
      Protocol.encode_error(
        Protocol.invalid_params(),
        "Missing required parameter: uri",
        nil,
        id
      )

    send_message(error, state)
  end

  defp handle_request("prompts/get", _params, id, state) do
    error =
      Protocol.encode_error(
        Protocol.invalid_params(),
        "Missing required parameter: name",
        nil,
        id
      )

    send_message(error, state)
  end

  defp handle_request("resources/subscribe", _params, id, state) do
    error =
      Protocol.encode_error(
        Protocol.invalid_params(),
        "Missing required parameter: uri",
        nil,
        id
      )

    send_message(error, state)
  end

  defp handle_request("resources/unsubscribe", _params, id, state) do
    error =
      Protocol.encode_error(
        Protocol.invalid_params(),
        "Missing required parameter: uri",
        nil,
        id
      )

    send_message(error, state)
  end

  defp handle_request("completion/complete", _params, id, state) do
    error =
      Protocol.encode_error(
        Protocol.invalid_params(),
        "Missing required parameters: ref and argument",
        nil,
        id
      )

    send_message(error, state)
  end

  defp handle_request(method, _params, id, state) do
    error =
      Protocol.encode_error(
        Protocol.method_not_found(),
        "Method not found: #{method}",
        nil,
        id
      )

    send_message(error, state)
  end

  defp handle_notification("notifications/initialized", _params, state) do
    Logger.debug("Client initialized")
    {:noreply, state}
  end

  defp handle_notification(
         "notifications/cancelled",
         %{"requestId" => request_id} = params,
         state
       )
       when is_binary(request_id) or is_integer(request_id) do
    reason = Map.get(params, "reason")
    Logger.info("Request #{request_id} cancelled: #{reason || "no reason given"}")

    # Normalize request_id to match what's stored in pending_requests
    # Server stores integer IDs internally but may receive string IDs from client
    normalized_id =
      cond do
        is_integer(request_id) ->
          request_id

        is_binary(request_id) ->
          case Integer.parse(request_id) do
            {id, ""} -> id
            # Keep as string if not parseable
            _ -> request_id
          end

        true ->
          request_id
      end

    # Check if this is a valid in-progress request
    case Map.get(state.pending_requests, normalized_id) do
      nil ->
        # Request not found or already completed - ignore as per spec
        Logger.debug("Ignoring cancellation for unknown request #{request_id}")
        {:noreply, state}

      _from ->
        # Request is in progress - cancel it
        Logger.debug("Cancelling in-progress request #{request_id}")

        # Remove from pending requests
        new_pending = Map.delete(state.pending_requests, normalized_id)
        new_state = %{state | pending_requests: new_pending}

        # Per MCP spec: SHOULD not send a response for cancelled request
        # The GenServer caller will get no response (which times out)
        # This is intentional behavior for cancellation

        {:noreply, new_state}
    end
  end

  defp handle_notification("notifications/cancelled", params, state) do
    # Handle malformed cancellation notifications (missing requestId)
    Logger.warning("Ignoring malformed cancellation notification: #{inspect(params)}")
    {:noreply, state}
  end

  defp handle_notification("notifications/message", params, state) do
    level = Map.get(params, "level", "info")
    message = Map.get(params, "message", "")
    data = Map.get(params, "data")

    log_data = if data, do: " - #{inspect(data)}", else: ""

    case level do
      "debug" -> Logger.debug("Client: #{message}#{log_data}")
      "info" -> Logger.info("Client: #{message}#{log_data}")
      "notice" -> Logger.info("Client: #{message}#{log_data}")
      "warning" -> Logger.warning("Client: #{message}#{log_data}")
      "error" -> Logger.error("Client: #{message}#{log_data}")
      "critical" -> Logger.error("Client [CRITICAL]: #{message}#{log_data}")
      "alert" -> Logger.error("Client [ALERT]: #{message}#{log_data}")
      "emergency" -> Logger.error("Client [EMERGENCY]: #{message}#{log_data}")
      _ -> Logger.info("Client [#{level}]: #{message}#{log_data}")
    end

    {:noreply, state}
  end

  defp handle_notification(method, params, state) do
    Logger.warning("Unhandled notification: #{method} with params: #{inspect(params)}")
    {:noreply, state}
  end

  defp handle_batch(messages, state) do
    # Process each message in the batch and collect responses in order
    {responses, final_state} =
      Enum.reduce(messages, {[], state}, fn message, {responses_acc, state_acc} ->
        case message do
          %{"method" => method, "params" => params, "id" => id} ->
            # Process request and collect response
            case handle_request_for_batch(method, params, id, state_acc) do
              {:response, response_msg, newer_state} ->
                {responses_acc ++ [response_msg], newer_state}

              {:error, error_msg, newer_state} ->
                {responses_acc ++ [error_msg], newer_state}
            end

          %{"method" => method, "params" => params} ->
            # Process notification (no response needed)
            {:noreply, new_state} = handle_notification(method, params, state_acc)
            {responses_acc, new_state}

          _ ->
            # Invalid message in batch - add error response
            error =
              Protocol.encode_error(
                Protocol.invalid_request(),
                "Invalid message in batch",
                nil,
                nil
              )

            {responses_acc ++ [error], state_acc}
        end
      end)

    # Send batch response (responses are already in correct order)
    send_message(responses, final_state)
  end

  # Helper function to handle requests for batch processing
  defp handle_request_for_batch(method, params, id, state) do
    # The initialize request MUST NOT be part of a JSON-RPC batch per MCP spec
    if method == "initialize" do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "The initialize request must not be part of a JSON-RPC batch",
          nil,
          id
        )

      {:error, error, state}
    else
      # Call the regular handle_request but capture the response
      # We need to intercept the send_message call

      # Create a temporary state that captures messages instead of sending them
      capture_state = Map.put(state, :capture_mode, true)
      capture_state = Map.put(capture_state, :captured_message, nil)

      # Process the request
      {:noreply, result_state} = handle_request(method, params, id, capture_state)

      # Extract the captured message
      if captured = Map.get(result_state, :captured_message) do
        # Remove capture fields and return the response
        new_state =
          result_state
          |> Map.delete(:capture_mode)
          |> Map.delete(:captured_message)

        {:response, captured, new_state}
      else
        # No message was captured, return an error
        error =
          Protocol.encode_error(Protocol.internal_error(), "Failed to process request", nil, id)

        {:error, error, state}
      end
    end
  end

  defp send_message(message, state) do
    # Check if we're in capture mode (for batch processing)
    if Map.get(state, :capture_mode, false) do
      # Don't actually send, just capture the message
      {:noreply, Map.put(state, :captured_message, message)}
    else
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
    end
  end

  defp send_notification(notification, state) do
    if state.initialized do
      send_message(notification, state)
    else
      {:noreply, state}
    end
  end

  defp format_error(reason) when is_binary(reason), do: reason
  defp format_error(reason), do: inspect(reason)

  # Check if a method is available in the negotiated protocol version
  defp method_available?(method, state) do
    version = state.negotiated_version || VersionRegistry.latest_version()
    Protocol.method_available?(method, version)
  end

  # Validate and potentially reject version-specific methods
  defp validate_method_version(method, id, state) do
    if method_available?(method, state) do
      :ok
    else
      error =
        Protocol.encode_error(
          Protocol.method_not_found(),
          "Method #{method} is not available in protocol version #{state.negotiated_version}",
          nil,
          id
        )

      send_message(error, state)
      :version_error
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
      {:reply, {:error, :not_initialized}, state}
    end
  end

  defp handle_response(result, id, state) do
    case Map.pop(state.pending_requests, id) do
      {nil, _pending} ->
        Logger.warning("Received response for unknown request #{id}")
        {:noreply, state}

      {from, pending} ->
        GenServer.reply(from, {:ok, result})
        {:noreply, %{state | pending_requests: pending}}
    end
  end

  defp handle_error_response(error, id, state) do
    case Map.pop(state.pending_requests, id) do
      {nil, _pending} ->
        Logger.warning("Received error for unknown request #{id}")
        {:noreply, state}

      {from, pending} ->
        GenServer.reply(from, {:error, error})
        {:noreply, %{state | pending_requests: pending}}
    end
  end

  defp init_transport(transport_mod, _transport_type, opts) do
    transport_mod.connect(opts)
  end

  # Helper function to determine the appropriate error code based on the error reason
  defp get_error_code_for_reason(reason) when is_binary(reason) do
    cond do
      String.contains?(reason, "Invalid cursor") -> Protocol.invalid_params()
      String.contains?(reason, "cursor") -> Protocol.invalid_params()
      true -> Protocol.internal_error()
    end
  end

  defp get_error_code_for_reason(_reason), do: Protocol.internal_error()

  # Hot reload helper function
  defp reload_handler_with_migration(state, new_handler_module) do
    # Initialize new handler with existing state structure
    case new_handler_module.init([]) do
      {:ok, new_default_state} ->
        # Smart state migration for hot reloading
        # Preserve data fields but update code-related fields from new handler
        migrated_state =
          if is_map(state.handler_state) and is_map(new_default_state) do
            # Code-related fields that should come from new handler
            code_fields = [:version, :description, :name]

            # Start with new state, then merge old state excluding code fields
            old_data_only = Map.drop(state.handler_state, code_fields)
            Map.merge(new_default_state, old_data_only)
          else
            new_default_state
          end

        new_state = %{state | handler: new_handler_module, handler_state: migrated_state}

        {:ok, new_state}

      {:error, reason} ->
        {:error, {:handler_init_failed, reason}}
    end
  rescue
    error -> {:error, {:handler_init_exception, error}}
  catch
    :exit, reason -> {:error, {:handler_init_exit, reason}}
    :throw, value -> {:error, {:handler_init_throw, value}}
  end
end
