defmodule ExMCP.Client do
  @moduledoc """
  High-level, synchronous MCP client implementation.

  This client provides a reliable, synchronous interface for MCP communication.
  It guarantees that the connection is established and the MCP handshake is
  complete before `start_link/1` returns, eliminating race conditions.

  ## Features

  - Synchronous initialization - client is ready when `start_link` returns
  - Automatic reconnection with exponential backoff
  - Transport abstraction with fallback support
  - Clear lifecycle management
  - Type-safe request/response handling

  ## Usage

      # Start a client - blocks until connected
      {:ok, client} = ExMCP.Client.start_link(
        transport: :http,
        url: "http://localhost:8080"
      )

      # Client is guaranteed to be ready
      {:ok, tools} = ExMCP.Client.list_tools(client)
      {:ok, result} = ExMCP.Client.call_tool(client, "hello", %{name: "world"})
  """

  use GenServer
  require Logger

  alias ExMCP.Error
  alias ExMCP.Internal.Protocol
  alias ExMCP.Response

  # Client state structure
  defmodule State do
    @moduledoc false

    defstruct [
      :transport_mod,
      :transport_state,
      :transport_opts,
      :server_info,
      :server_capabilities,
      :protocol_version,
      :connection_status,
      :pending_requests,
      :last_activity,
      :session_id,
      :receiver_task,
      :reconnect_timer,
      :reconnect_attempts,
      :max_reconnect_attempts,
      :reconnect_interval
    ]

    @type connection_status ::
            :connecting
            | :connected
            | :ready
            | :disconnected
            | :error

    @type t :: %__MODULE__{
            transport_mod: module(),
            transport_state: term(),
            transport_opts: keyword(),
            server_info: map() | nil,
            server_capabilities: map() | nil,
            protocol_version: String.t() | nil,
            connection_status: connection_status(),
            pending_requests: %{String.t() => {from :: GenServer.from(), started_at :: integer()}},
            last_activity: integer() | nil,
            session_id: String.t() | nil,
            receiver_task: Task.t() | nil,
            reconnect_timer: reference() | nil,
            reconnect_attempts: non_neg_integer(),
            max_reconnect_attempts: non_neg_integer(),
            reconnect_interval: non_neg_integer()
          }
  end

  # Public API

  @doc """
  Starts a new MCP client.

  The client will connect to the server and complete the MCP handshake
  before returning. If the connection fails, `{:error, reason}` is returned.

  ## Options

  - `:transport` - Transport module (:stdio, :http, :sse, :native)
  - `:name` - Optional GenServer name
  - `:timeout` - Connection timeout in ms (default: 10_000)
  - `:max_reconnect_attempts` - Max reconnection attempts (default: 5)
  - `:reconnect_interval` - Base reconnection interval in ms (default: 1_000)

  Plus transport-specific options (e.g., `:url` for HTTP, `:command` for stdio)
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    {name_opts, init_opts} = Keyword.split(opts, [:name])
    GenServer.start_link(__MODULE__, init_opts, name_opts)
  end

  @doc """
  Lists available tools from the server.
  Supports pagination with cursor.
  """
  @spec list_tools(GenServer.server(), keyword()) :: {:ok, Response.t()} | {:error, Error.t()}
  def list_tools(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)
    cursor = Keyword.get(opts, :cursor)

    params = if cursor, do: %{cursor: cursor}, else: %{}

    case GenServer.call(client, {:request, "tools/list", params}, timeout) do
      {:ok, raw_response} ->
        response = Response.from_raw_response(raw_response)
        {:ok, response}

      {:error, error_data} when is_map(error_data) ->
        error = Error.from_json_rpc_error(error_data)
        {:error, error}

      {:error, reason} ->
        error = Error.connection_error(inspect(reason))
        {:error, error}
    end
  end

  @doc """
  Calls a tool with the given arguments.

  ## Options

  * `:timeout` - Request timeout in milliseconds (default: 30_000)
  * `:progress_token` - Token for progress notifications
  """
  @spec call_tool(GenServer.server(), String.t(), map(), timeout() | keyword()) ::
          {:ok, Response.t()} | {:error, Error.t()}
  def call_tool(client, tool_name, arguments, timeout_or_opts \\ 30_000)

  def call_tool(client, tool_name, arguments, timeout) when is_integer(timeout) do
    call_tool(client, tool_name, arguments, timeout: timeout)
  end

  def call_tool(client, tool_name, arguments, opts) when is_list(opts) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    progress_token = Keyword.get(opts, :progress_token)

    # Add progress token to arguments meta if provided
    arguments =
      if progress_token do
        Map.put(arguments, "_meta", %{"progressToken" => progress_token})
      else
        arguments
      end

    params = %{"name" => tool_name, "arguments" => arguments}

    case GenServer.call(client, {:request, "tools/call", params}, timeout) do
      {:ok, raw_response} ->
        response = Response.from_raw_response(raw_response, tool_name: tool_name)
        {:ok, response}

      {:error, error_data} when is_map(error_data) ->
        error = Error.from_json_rpc_error(error_data)
        {:error, error}

      {:error, reason} ->
        error = Error.connection_error(inspect(reason))
        {:error, error}
    end
  end

  @doc """
  Lists available resources from the server.
  """
  @spec list_resources(GenServer.server(), timeout()) :: {:ok, Response.t()} | {:error, Error.t()}
  def list_resources(client, timeout \\ 5_000) do
    case GenServer.call(client, {:request, "resources/list", %{}}, timeout) do
      {:ok, raw_response} ->
        response = Response.from_raw_response(raw_response)
        {:ok, response}

      {:error, error_data} when is_map(error_data) ->
        error = Error.from_json_rpc_error(error_data)
        {:error, error}

      {:error, reason} ->
        error = Error.connection_error(inspect(reason))
        {:error, error}
    end
  end

  @doc """
  Reads a resource by URI.
  """
  @spec read_resource(GenServer.server(), String.t(), timeout()) ::
          {:ok, Response.t()} | {:error, Error.t()}
  def read_resource(client, uri, timeout \\ 10_000) do
    params = %{"uri" => uri}

    case GenServer.call(client, {:request, "resources/read", params}, timeout) do
      {:ok, raw_response} ->
        response = Response.from_raw_response(raw_response, resource_uri: uri)
        {:ok, response}

      {:error, error_data} when is_map(error_data) ->
        error = Error.from_json_rpc_error(error_data)
        {:error, error}

      {:error, reason} ->
        error = Error.connection_error(inspect(reason))
        {:error, error}
    end
  end

  @doc """
  Lists available prompts from the server.
  """
  @spec list_prompts(GenServer.server(), timeout()) :: {:ok, Response.t()} | {:error, Error.t()}
  def list_prompts(client, timeout \\ 5_000) do
    case GenServer.call(client, {:request, "prompts/list", %{}}, timeout) do
      {:ok, raw_response} ->
        response = Response.from_raw_response(raw_response)
        {:ok, response}

      {:error, error_data} when is_map(error_data) ->
        error = Error.from_json_rpc_error(error_data)
        {:error, error}

      {:error, reason} ->
        error = Error.connection_error(inspect(reason))
        {:error, error}
    end
  end

  @doc """
  Gets a prompt with the given arguments.
  """
  @spec get_prompt(GenServer.server(), String.t(), map(), timeout()) ::
          {:ok, Response.t()} | {:error, Error.t()}
  def get_prompt(client, prompt_name, arguments \\ %{}, timeout \\ 5_000) do
    params = %{"name" => prompt_name, "arguments" => arguments}

    case GenServer.call(client, {:request, "prompts/get", params}, timeout) do
      {:ok, raw_response} ->
        response = Response.from_raw_response(raw_response, prompt_name: prompt_name)
        {:ok, response}

      {:error, error_data} when is_map(error_data) ->
        error = Error.from_json_rpc_error(error_data)
        {:error, error}

      {:error, reason} ->
        error = Error.connection_error(inspect(reason))
        {:error, error}
    end
  end

  @doc """
  Gets the current connection status.
  """
  @spec get_status(GenServer.server()) :: {:ok, map()}
  def get_status(client) do
    GenServer.call(client, :get_status)
  end

  @doc """
  Gets the negotiated protocol version.
  """
  @spec negotiated_version(GenServer.server()) :: {:ok, String.t()} | {:error, term()}
  def negotiated_version(client) do
    case get_status(client) do
      {:ok, %{protocol_version: version}} when is_binary(version) -> {:ok, version}
      {:ok, _} -> {:error, :not_negotiated}
    end
  end

  @doc """
  Gets the server capabilities.
  """
  @spec server_capabilities(GenServer.server()) :: {:ok, map()} | {:error, term()}
  def server_capabilities(client) do
    case get_status(client) do
      {:ok, %{server_capabilities: caps}} when is_map(caps) -> {:ok, caps}
      {:ok, _} -> {:error, :not_available}
    end
  end

  @doc """
  Performs a completion request.

  ## Breaking Change

  As of version 2025-06-18, this function signature changed from
  `complete(client, ref, timeout)` to `complete(client, ref, params, timeout)`
  to support the new completion parameter structure.
  """
  @spec complete(GenServer.server(), String.t(), map(), timeout()) ::
          {:ok, Response.t()} | {:error, Error.t()}
  def complete(client, ref, params, timeout \\ 5_000) do
    # Extract _meta and build proper request format
    {meta, argument_params} = Map.pop(params, "_meta")

    request_params = %{
      "ref" => ref,
      "argument" => argument_params
    }

    request_params = if meta, do: Map.put(request_params, "_meta", meta), else: request_params

    case GenServer.call(client, {:request, "completion/complete", request_params}, timeout) do
      {:ok, raw_response} ->
        response = Response.from_raw_response(raw_response)
        {:ok, response}

      {:error, error_data} when is_map(error_data) ->
        error = Error.from_json_rpc_error(error_data)
        {:error, error}

      {:error, reason} ->
        error = Error.connection_error(inspect(reason))
        {:error, error}
    end
  end

  @doc """
  Sends a notification to the server.
  """
  @spec send_notification(GenServer.server(), String.t(), map()) :: :ok | {:error, term()}
  def send_notification(client, method, params) do
    GenServer.call(client, {:notification, method, params})
  end

  @doc """
  Sends a ping request to the server.
  """
  @spec ping(GenServer.server(), timeout()) :: {:ok, map()} | {:error, any()}
  def ping(client, timeout \\ 5_000) do
    case GenServer.call(client, {:request, "ping", %{}}, timeout) do
      {:ok, result} ->
        {:ok, result}

      {:error, error_data} when is_map(error_data) ->
        error = Error.from_json_rpc_error(error_data)
        {:error, error}

      {:error, reason} ->
        error = Error.connection_error(inspect(reason))
        {:error, error}
    end
  end

  @doc """
  Sends a cancellation notification to the server.

  ## Examples

      # Cancel with reason
      :ok = ExMCP.Client.send_cancelled(client, "req_123", "Operation cancelled by user")

      # Cancel without reason
      :ok = ExMCP.Client.send_cancelled(client, "req_123")

  """
  @spec send_cancelled(GenServer.server(), String.t(), String.t() | nil) :: :ok
  def send_cancelled(client, request_id, reason \\ nil) do
    params = %{"requestId" => request_id}
    params = if reason, do: Map.put(params, "reason", reason), else: params

    send_notification(client, "notifications/cancelled", params)
  end

  @doc """
  Lists available roots from the server.
  """
  @spec list_roots(GenServer.server(), timeout()) ::
          {:ok, %{roots: [ExMCP.Types.root()]}} | {:error, any()}
  def list_roots(client, timeout \\ 5_000) do
    case GenServer.call(client, {:request, "roots/list", %{}}, timeout) do
      {:ok, %{"roots" => roots}} ->
        {:ok, %{roots: roots}}

      {:ok, response} ->
        {:error, Error.invalid_request("Expected roots list but got: #{inspect(response)}")}

      {:error, error_data} when is_map(error_data) ->
        error = Error.from_json_rpc_error(error_data)
        {:error, error}

      {:error, reason} ->
        error = Error.connection_error(inspect(reason))
        {:error, error}
    end
  end

  @doc """
  Sets the log level for the server.
  """
  @spec set_log_level(GenServer.server(), String.t(), timeout()) :: {:ok, map()} | {:error, any()}
  def set_log_level(client, level, timeout \\ 5_000) do
    params = %{"level" => level}

    case GenServer.call(client, {:request, "logging/setLevel", params}, timeout) do
      {:ok, result} ->
        {:ok, result}

      {:error, error_data} when is_map(error_data) ->
        error = Error.from_json_rpc_error(error_data)
        {:error, error}

      {:error, reason} ->
        error = Error.connection_error(inspect(reason))
        {:error, error}
    end
  end

  @doc """
  Stops the client gracefully.
  """
  @spec stop(GenServer.server()) :: :ok
  @spec stop(GenServer.server(), term()) :: :ok
  def stop(client, reason \\ :normal) do
    GenServer.stop(client, reason)
  end

  # GenServer callbacks

  @impl GenServer
  def init(opts) do
    # Synchronous initialization - complete connection before returning
    case connect_and_initialize(opts) do
      {:ok, state} ->
        {:ok, state}

      {:error, reason} ->
        Logger.error("Failed to initialize MCP client: #{inspect(reason)}")
        {:stop, reason}
    end
  end

  @impl GenServer
  def handle_call({:request, method, params}, from, state) do
    case state.connection_status do
      :ready ->
        # Generate request ID and send
        request_id = generate_request_id()

        request = %{
          "jsonrpc" => "2.0",
          "id" => request_id,
          "method" => method,
          "params" => params
        }

        case send_message(request, state) do
          {:ok, updated_state} ->
            # Track pending request
            pending = Map.put(state.pending_requests, request_id, {from, System.monotonic_time()})

            new_state = %{
              updated_state
              | pending_requests: pending,
                last_activity: System.monotonic_time()
            }

            {:noreply, new_state}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end

      status ->
        {:reply, {:error, {:not_connected, status}}, state}
    end
  end

  def handle_call({:notification, method, params}, _from, state) do
    notification = %{
      "jsonrpc" => "2.0",
      "method" => method,
      "params" => params
    }

    case send_message(notification, state) do
      {:ok, updated_state} ->
        {:reply, :ok, %{updated_state | last_activity: System.monotonic_time()}}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(:get_status, _from, state) do
    status = %{
      connection_status: state.connection_status,
      server_info: state.server_info,
      server_capabilities: state.server_capabilities,
      protocol_version: state.protocol_version,
      pending_requests: map_size(state.pending_requests),
      reconnect_attempts: state.reconnect_attempts
    }

    {:reply, {:ok, status}, state}
  end

  @impl GenServer
  def handle_info({:transport_message, message}, state) do
    # Handle incoming messages from transport
    case Protocol.parse_message(message) do
      {:result, result, id} when is_map_key(state.pending_requests, id) ->
        # Response to a pending request
        {{from, _started_at}, pending} = Map.pop(state.pending_requests, id)

        # Send response to waiting caller
        GenServer.reply(from, {:ok, result})

        {:noreply, %{state | pending_requests: pending, last_activity: System.monotonic_time()}}

      {:error, error, id} when is_map_key(state.pending_requests, id) ->
        # Error response to a pending request
        {{from, _started_at}, pending} = Map.pop(state.pending_requests, id)

        # Send error to waiting caller
        GenServer.reply(from, {:error, error})

        {:noreply, %{state | pending_requests: pending, last_activity: System.monotonic_time()}}

      {:notification, method, params} ->
        # Server notification - handle it
        handle_notification(%{"method" => method, "params" => params}, state)

      {:error, :invalid_message} ->
        Logger.error("Failed to parse transport message: invalid message")
        {:noreply, state}

      _ ->
        # Unknown message type, ignore
        {:noreply, state}
    end
  end

  def handle_info({:transport_closed, reason}, state) do
    Logger.warning("Transport closed: #{inspect(reason)}")
    new_state = %{state | connection_status: :disconnected}

    # Cancel pending requests
    for {_id, {from, _}} <- state.pending_requests do
      GenServer.reply(from, {:error, :disconnected})
    end

    # Schedule reconnection
    {:noreply, schedule_reconnect(new_state)}
  end

  def handle_info(:reconnect, state) do
    case attempt_reconnect(state) do
      {:ok, new_state} ->
        {:noreply, new_state}

      {:error, _reason} ->
        {:noreply, schedule_reconnect(state)}
    end
  end

  def handle_info({:DOWN, _ref, :process, task_pid, reason}, %{receiver_task: task} = state)
      when task != nil and task.pid == task_pid do
    Logger.error("Receiver task crashed: #{inspect(reason)}")
    new_state = %{state | receiver_task: nil, connection_status: :error}
    {:noreply, schedule_reconnect(new_state)}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  # Private functions

  defp connect_and_initialize(opts) do
    transport = determine_transport(opts)
    transport_opts = build_transport_opts(transport, opts)

    with {:ok, transport_state} <- connect_transport(transport, transport_opts) do
      # Check if this is SSE mode
      is_sse = sse_transport?(transport, transport_state)

      if is_sse do
        # For SSE, start receiver BEFORE handshake to capture async responses
        perform_sse_initialization(transport, transport_state, opts)
      else
        # For non-SSE, use the traditional synchronous approach
        with {:ok, state} <- perform_mcp_handshake(transport, transport_state, opts) do
          # Start receiver task
          receiver_task = start_receiver_task(transport, transport_state)

          final_state = %{
            state
            | receiver_task: receiver_task,
              connection_status: :ready,
              reconnect_attempts: 0
          }

          {:ok, final_state}
        end
      end
    end
  end

  defp determine_transport(opts) do
    case Keyword.get(opts, :transport, :stdio) do
      :stdio -> ExMCP.Transport.Stdio
      :http -> ExMCP.Transport.HTTP
      # SSE uses HTTP transport with use_sse flag
      :sse -> ExMCP.Transport.HTTP
      :native -> ExMCP.Transport.Native
      :beam -> ExMCP.Transport.Native
      :test -> ExMCP.Transport.Test
      module when is_atom(module) -> module
    end
  end

  defp build_transport_opts(_transport, opts) do
    # Filter out non-transport options
    Keyword.drop(opts, [
      :transport,
      :timeout,
      :max_reconnect_attempts,
      :reconnect_interval,
      :protocol_version
    ])
  end

  defp connect_transport(transport_mod, opts) do
    case transport_mod.connect(opts) do
      {:ok, transport_state} -> {:ok, transport_state}
      {:error, reason} -> {:error, {:transport_connect_failed, reason}}
    end
  end

  defp perform_mcp_handshake(transport_mod, transport_state, opts) do
    # Build initial state
    initial_state = %State{
      transport_mod: transport_mod,
      transport_state: transport_state,
      transport_opts: build_transport_opts(transport_mod, opts),
      connection_status: :connecting,
      pending_requests: %{},
      session_id: generate_session_id(),
      reconnect_attempts: 0,
      max_reconnect_attempts: Keyword.get(opts, :max_reconnect_attempts, 5),
      reconnect_interval: Keyword.get(opts, :reconnect_interval, 1_000)
    }

    # Send initialize request
    protocol_version = Keyword.get(opts, :protocol_version, "2025-06-18")

    init_request =
      Protocol.encode_initialize(
        %{
          "name" => "ExMCP",
          "version" => "0.1.0"
        },
        %{},
        protocol_version
      )

    # Override the generated ID with our session-specific one
    init_request = Map.put(init_request, "id", "init-#{initial_state.session_id}")

    # Send the initialize request and handle both sync and async response patterns
    case transport_mod.send_message(Jason.encode!(init_request), transport_state) do
      {:ok, state_after_send} ->
        # SSE mode - wait for response asynchronously
        with {:ok, init_response} <-
               wait_for_response(
                 transport_mod,
                 state_after_send,
                 "init-#{initial_state.session_id}",
                 10_000
               ),
             :ok <- validate_initialize_response(init_response),
             {:ok, _} <- send_initialized_notification(transport_mod, state_after_send) do
          handle_successful_initialization(initial_state, init_response, state_after_send)
        end

      {:ok, state_after_send, init_response} ->
        # Non-SSE HTTP mode - immediate response
        with :ok <- validate_initialize_response(init_response),
             {:ok, _} <- send_initialized_notification(transport_mod, state_after_send) do
          handle_successful_initialization(initial_state, init_response, state_after_send)
        end

      error ->
        error
    end
  end

  defp handle_successful_initialization(initial_state, init_response, state_after_send) do
    updated_state = %{
      initial_state
      | server_info: get_in(init_response, ["result", "serverInfo"]),
        server_capabilities: get_in(init_response, ["result", "capabilities"]),
        protocol_version: get_in(init_response, ["result", "protocolVersion"]),
        connection_status: :connected,
        transport_state: state_after_send
    }

    {:ok, updated_state}
  end

  defp wait_for_response(transport_mod, transport_state, expected_id, timeout) do
    # Synchronously wait for a specific response
    deadline = System.monotonic_time(:millisecond) + timeout

    do_wait_for_response(transport_mod, transport_state, expected_id, deadline)
  end

  defp do_wait_for_response(transport_mod, transport_state, expected_id, deadline) do
    now = System.monotonic_time(:millisecond)

    if now >= deadline do
      {:error, :timeout}
    else
      _remaining = deadline - now

      case transport_mod.receive_message(transport_state) do
        {:ok, data, _new_state} ->
          case Protocol.parse_message(data) do
            {:result, result, ^expected_id} ->
              {:ok, %{"result" => result, "id" => expected_id}}

            {:error, error, ^expected_id} ->
              {:ok, %{"error" => error, "id" => expected_id}}

            _ ->
              # Not our response, keep waiting
              do_wait_for_response(transport_mod, transport_state, expected_id, deadline)
          end

        {:error, :timeout} ->
          {:error, :timeout}

        {:error, reason} ->
          {:error, {:transport_error, reason}}
      end
    end
  end

  defp validate_initialize_response(%{"result" => result}) when is_map(result) do
    :ok
  end

  defp validate_initialize_response(%{"error" => error}) do
    {:error, {:initialize_error, error}}
  end

  defp validate_initialize_response(_) do
    {:error, :invalid_initialize_response}
  end

  defp send_initialized_notification(transport_mod, transport_state) do
    notification = Protocol.encode_initialized()

    case transport_mod.send_message(Jason.encode!(notification), transport_state) do
      {:ok, new_state} -> {:ok, new_state}
      # Ignore response for notifications
      {:ok, new_state, _response} -> {:ok, new_state}
      error -> error
    end
  end

  defp start_receiver_task(transport_mod, transport_state) do
    parent = self()

    Task.async(fn ->
      # For SSE, update the parent to this task
      updated_state = update_sse_parent(transport_mod, transport_state)
      receive_loop(transport_mod, updated_state, parent)
    end)
  end

  defp receive_loop(transport_mod, transport_state, parent) do
    case transport_mod.receive_message(transport_state) do
      {:ok, data, new_state} ->
        send(parent, {:transport_message, data})
        receive_loop(transport_mod, new_state, parent)

      {:error, :no_response} ->
        # Non-SSE HTTP mode - no response available, wait briefly and try again
        Process.sleep(10)
        receive_loop(transport_mod, transport_state, parent)

      {:error, :closed} ->
        send(parent, {:transport_closed, :normal})

      {:error, reason} ->
        send(parent, {:transport_closed, reason})
    end
  end

  defp send_message(message, state) do
    encoded = Jason.encode!(message)

    case state.transport_mod.send_message(encoded, state.transport_state) do
      {:ok, new_transport_state} ->
        {:ok, %{state | transport_state: new_transport_state}}

      {:ok, new_transport_state, response_data} ->
        # Non-SSE HTTP mode with immediate response
        # Send the response directly to ourselves as if it came from the receiver task
        send(self(), {:transport_message, response_data})
        {:ok, %{state | transport_state: new_transport_state}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_notification(%{"method" => method, "params" => params}, state) do
    # Log notifications for now
    Logger.debug("Received notification: #{method} - #{inspect(params)}")
    {:noreply, state}
  end

  defp schedule_reconnect(state) do
    if state.reconnect_attempts < state.max_reconnect_attempts do
      # Cancel existing timer
      if state.reconnect_timer do
        Process.cancel_timer(state.reconnect_timer)
      end

      # Exponential backoff
      delay = state.reconnect_interval * 2 ** state.reconnect_attempts
      timer = Process.send_after(self(), :reconnect, delay)

      %{state | reconnect_timer: timer, reconnect_attempts: state.reconnect_attempts + 1}
    else
      Logger.error("Max reconnection attempts reached")
      %{state | connection_status: :error}
    end
  end

  defp attempt_reconnect(state) do
    Logger.info("Attempting to reconnect (attempt #{state.reconnect_attempts})")

    # Clean up old receiver task
    if state.receiver_task do
      Task.shutdown(state.receiver_task, :brutal_kill)
    end

    # Try to reconnect
    opts =
      Keyword.merge(state.transport_opts,
        transport: state.transport_mod,
        max_reconnect_attempts: state.max_reconnect_attempts,
        reconnect_interval: state.reconnect_interval
      )

    case connect_and_initialize(opts) do
      {:ok, new_state} ->
        # Preserve some state from before
        final_state = %{
          new_state
          | pending_requests: state.pending_requests,
            session_id: state.session_id
        }

        {:ok, final_state}

      {:error, reason} ->
        Logger.error("Reconnection failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp generate_request_id do
    "req-#{:erlang.unique_integer([:positive, :monotonic])}"
  end

  defp generate_session_id do
    "session-#{:erlang.unique_integer([:positive, :monotonic])}"
  end

  defp sse_transport?(transport_mod, transport_state) do
    # Check if transport is HTTP with SSE enabled
    transport_mod == ExMCP.Transport.HTTP and
      Map.get(transport_state, :use_sse, false) == true
  end

  defp perform_sse_initialization(transport_mod, transport_state, opts) do
    # Build initial state
    initial_state = %State{
      transport_mod: transport_mod,
      transport_state: transport_state,
      transport_opts: build_transport_opts(transport_mod, opts),
      connection_status: :connecting,
      pending_requests: %{},
      session_id: generate_session_id(),
      reconnect_attempts: 0,
      max_reconnect_attempts: Keyword.get(opts, :max_reconnect_attempts, 5),
      reconnect_interval: Keyword.get(opts, :reconnect_interval, 1_000)
    }

    # Start receiver task FIRST to capture SSE responses
    parent = self()

    receiver_task =
      Task.async(fn ->
        # Update the SSE client to send messages to this task instead
        updated_state = update_sse_parent(transport_mod, transport_state)
        # Set up temporary message collection
        collect_sse_messages(transport_mod, updated_state, parent)
      end)

    # Now perform handshake
    protocol_version = Keyword.get(opts, :protocol_version, "2025-06-18")

    init_request =
      Protocol.encode_initialize(
        %{
          "name" => "ExMCP",
          "version" => "0.1.0"
        },
        %{},
        protocol_version
      )

    # Override the generated ID with our session-specific one
    init_request = Map.put(init_request, "id", "init-#{initial_state.session_id}")

    Logger.debug("Sending initialize request with ID: #{init_request["id"]}")

    # Send the initialize request
    case transport_mod.send_message(Jason.encode!(init_request), transport_state) do
      {:ok, state_after_send} ->
        # Wait for the response through our message collector
        receive do
          {:sse_response, response} ->
            # Process the response
            with :ok <- validate_initialize_response(response),
                 {:ok, _} <- send_initialized_notification(transport_mod, state_after_send) do
              # Stop the temporary collector
              Task.shutdown(receiver_task, :brutal_kill)

              # Start the real receiver task
              real_receiver = start_receiver_task(transport_mod, state_after_send)

              updated_state = %{
                initial_state
                | server_info: get_in(response, ["result", "serverInfo"]),
                  server_capabilities: get_in(response, ["result", "capabilities"]),
                  protocol_version: get_in(response, ["result", "protocolVersion"]),
                  connection_status: :ready,
                  transport_state: state_after_send,
                  receiver_task: real_receiver
              }

              {:ok, updated_state}
            end
        after
          10_000 ->
            Task.shutdown(receiver_task, :brutal_kill)
            {:error, :initialization_timeout}
        end

      error ->
        Task.shutdown(receiver_task, :brutal_kill)
        error
    end
  end

  defp update_sse_parent(_transport_mod, %{sse_pid: sse_pid} = state) when is_pid(sse_pid) do
    # Tell the SSE client to send messages to us instead
    send(sse_pid, {:change_parent, self()})
    # Give it a moment to process
    Process.sleep(50)
    state
  end

  defp update_sse_parent(_transport_mod, state), do: state

  defp collect_sse_messages(transport_mod, transport_state, parent) do
    Logger.debug(
      "Starting SSE message collection, transport_state: #{inspect(Map.keys(transport_state))}"
    )

    case transport_mod.receive_message(transport_state) do
      {:ok, data, new_state} ->
        Logger.debug("SSE message received: #{inspect(data)}")
        # Check if this is the initialization response
        case data do
          %{"id" => "init-" <> _, "result" => _} = response ->
            Logger.debug("Initialization response detected!")
            send(parent, {:sse_response, response})
            # Continue collecting for any other messages
            collect_sse_messages(transport_mod, new_state, parent)

          _ ->
            # Store other messages for later processing
            Logger.debug("Non-init message, continuing...")
            collect_sse_messages(transport_mod, new_state, parent)
        end

      {:error, :no_response} ->
        # Non-SSE HTTP mode - should not happen here
        Process.sleep(10)
        collect_sse_messages(transport_mod, transport_state, parent)

      {:error, :closed} ->
        Logger.debug("SSE connection closed")
        :ok

      {:error, reason} ->
        Logger.debug("SSE error in collector: #{inspect(reason)}")
        :ok
    end
  end

  @doc """
  Sends multiple requests in a batch and waits for all responses.

  Returns a list of results in the same order as the requests.
  Each result is either `{:ok, response}` or `{:error, reason}`.

  **DEPRECATED**: Batch processing was removed in MCP specification 2025-06-18.
  This function now executes requests sequentially for compatibility.
  """
  @deprecated "Batch processing removed in MCP 2025-06-18. Use individual requests instead."
  @spec batch_request(pid(), [{atom(), [any()]}], keyword()) :: {:ok, [any()]} | {:error, term()}
  def batch_request(client, requests, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)

    # Create tasks for all requests
    tasks = Enum.map(requests, &create_batch_task(client, &1))

    # Wait for all tasks to complete
    results = Task.yield_many(tasks, timeout)

    # Convert task results to proper format
    batch_results = Enum.map(results, &process_task_result/1)

    {:ok, batch_results}
  end

  defp create_batch_task(client, {method, args}) do
    Task.async(fn -> execute_batch_method(client, method, args) end)
  end

  defp execute_batch_method(client, method, args) do
    case method do
      :call_tool -> execute_call_tool(client, args)
      :list_tools -> execute_list_tools(client, args)
      :list_resources -> list_resources(client)
      :read_resource -> execute_read_resource(client, args)
      _ -> {:error, {:unsupported_batch_method, method}}
    end
  end

  defp execute_call_tool(client, args) do
    case args do
      [name, arguments] ->
        call_tool(client, name, arguments)

      [name, arguments, timeout] when is_integer(timeout) ->
        call_tool(client, name, arguments, timeout)

      [name, arguments | rest] ->
        call_tool(client, name, arguments, Keyword.new(rest))
    end
  end

  defp execute_list_tools(client, args) do
    case args do
      [] -> list_tools(client)
      [opts] when is_list(opts) -> list_tools(client, opts)
      [timeout] when is_integer(timeout) -> list_tools(client, timeout: timeout)
    end
  end

  defp execute_read_resource(client, args) do
    case args do
      [uri] ->
        read_resource(client, uri)

      [uri, timeout] when is_integer(timeout) ->
        read_resource(client, uri, timeout)

      [uri | rest] ->
        timeout = Keyword.get(Keyword.new(rest), :timeout, 30_000)
        read_resource(client, uri, timeout)
    end
  end

  defp process_task_result({task, result}) do
    case result do
      {:ok, value} ->
        value

      {:exit, reason} ->
        {:error, reason}

      nil ->
        Task.shutdown(task, :brutal_kill)
        {:error, :timeout}
    end
  end
end
