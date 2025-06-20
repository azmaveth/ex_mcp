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

  alias ExMCP.Internal.Protocol
  alias ExMCP.Response
  alias ExMCP.Error

  # Client state structure
  defmodule State do
    @moduledoc false

    defstruct [
      :transport_mod,
      :transport_state,
      :transport_opts,
      :server_info,
      :server_capabilities,
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
  @spec list_tools(GenServer.server(), keyword()) :: {:ok, map()} | {:error, Error.t()}
  def list_tools(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)
    cursor = Keyword.get(opts, :cursor)

    params = if cursor, do: %{cursor: cursor}, else: %{}

    case GenServer.call(client, {:request, "tools/list", params}, timeout) do
      {:ok, %{"tools" => tools} = response} ->
        result = %{tools: tools}

        result =
          if response["nextCursor"],
            do: Map.put(result, :nextCursor, response["nextCursor"]),
            else: result

        {:ok, result}

      {:ok, response} ->
        {:error, Error.invalid_request("Expected tools list but got: #{inspect(response)}")}

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
  @spec list_resources(GenServer.server(), timeout()) :: {:ok, [map()]} | {:error, Error.t()}
  def list_resources(client, timeout \\ 5_000) do
    case GenServer.call(client, {:request, "resources/list", %{}}, timeout) do
      {:ok, %{"resources" => resources}} ->
        {:ok, resources}

      {:ok, response} ->
        {:error, Error.invalid_request("Expected resources list but got: #{inspect(response)}")}

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
  @spec list_prompts(GenServer.server(), timeout()) :: {:ok, [map()]} | {:error, Error.t()}
  def list_prompts(client, timeout \\ 5_000) do
    case GenServer.call(client, {:request, "prompts/list", %{}}, timeout) do
      {:ok, %{"prompts" => prompts}} ->
        {:ok, prompts}

      {:ok, response} ->
        {:error, Error.invalid_request("Expected prompts list but got: #{inspect(response)}")}

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
  Performs a completion request.
  """
  @spec complete(GenServer.server(), map(), timeout()) ::
          {:ok, Response.t()} | {:error, Error.t()}
  def complete(client, ref, timeout \\ 5_000) do
    case GenServer.call(client, {:request, "completion/complete", %{"ref" => ref}}, timeout) do
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

    with {:ok, transport_state} <- connect_transport(transport, transport_opts),
         {:ok, state} <- perform_mcp_handshake(transport, transport_state, opts) do
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

  defp determine_transport(opts) do
    case Keyword.get(opts, :transport, :stdio) do
      :stdio -> ExMCP.Transport.Stdio
      :http -> ExMCP.Transport.HTTP
      :sse -> ExMCP.Transport.SSE
      :native -> ExMCP.Transport.Native
      :beam -> ExMCP.Transport.Native
      :test -> ExMCP.Transport.Test
      module when is_atom(module) -> module
    end
  end

  defp build_transport_opts(_transport, opts) do
    # Filter out non-transport options
    Keyword.drop(opts, [:transport, :timeout, :max_reconnect_attempts, :reconnect_interval])
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
    init_request =
      Protocol.encode_initialize(
        %{
          "name" => "ExMCP",
          "version" => "0.1.0"
        },
        %{},
        "2024-11-05"
      )

    # Override the generated ID with our session-specific one
    init_request = Map.put(init_request, "id", "init-#{initial_state.session_id}")

    with {:ok, _} <- transport_mod.send(transport_state, Jason.encode!(init_request)),
         {:ok, init_response} <-
           wait_for_response(
             transport_mod,
             transport_state,
             "init-#{initial_state.session_id}",
             10_000
           ),
         :ok <- validate_initialize_response(init_response),
         {:ok, _} <- send_initialized_notification(transport_mod, transport_state) do
      updated_state = %{
        initial_state
        | server_info: get_in(init_response, ["result", "serverInfo"]),
          server_capabilities: get_in(init_response, ["result", "capabilities"]),
          connection_status: :connected
      }

      {:ok, updated_state}
    end
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
      remaining = deadline - now

      case transport_mod.recv(transport_state, remaining) do
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
    transport_mod.send(transport_state, Jason.encode!(notification))
  end

  defp start_receiver_task(transport_mod, transport_state) do
    parent = self()

    Task.async(fn ->
      receive_loop(transport_mod, transport_state, parent)
    end)
  end

  defp receive_loop(transport_mod, transport_state, parent) do
    case transport_mod.recv(transport_state, :infinity) do
      {:ok, data, new_state} ->
        send(parent, {:transport_message, data})
        receive_loop(transport_mod, new_state, parent)

      {:error, :closed} ->
        send(parent, {:transport_closed, :normal})

      {:error, reason} ->
        send(parent, {:transport_closed, reason})
    end
  end

  defp send_message(message, state) do
    encoded = Jason.encode!(message)

    case state.transport_mod.send(state.transport_state, encoded) do
      {:ok, new_transport_state} ->
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

  @doc """
  Sends multiple requests in a batch and waits for all responses.

  Returns a list of results in the same order as the requests.
  Each result is either `{:ok, response}` or `{:error, reason}`.
  """
  @spec batch_request(pid(), [{atom(), [any()]}], keyword()) :: {:ok, [any()]} | {:error, term()}
  def batch_request(client, requests, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)

    # Create tasks for all requests
    tasks =
      Enum.map(requests, fn {method, args} ->
        Task.async(fn ->
          case method do
            :call_tool ->
              case args do
                [name, arguments] ->
                  call_tool(client, name, arguments)

                [name, arguments, timeout] when is_integer(timeout) ->
                  call_tool(client, name, arguments, timeout)

                [name, arguments | rest] ->
                  call_tool(client, name, arguments, Keyword.new(rest))
              end

            :list_tools ->
              case args do
                [] -> list_tools(client)
                [opts] when is_list(opts) -> list_tools(client, opts)
                [timeout] when is_integer(timeout) -> list_tools(client, timeout: timeout)
              end

            :list_resources ->
              list_resources(client)

            :read_resource ->
              case args do
                [uri] ->
                  read_resource(client, uri)

                [uri, timeout] when is_integer(timeout) ->
                  read_resource(client, uri, timeout)

                [uri | rest] ->
                  read_resource(client, uri, Keyword.get(Keyword.new(rest), :timeout, 30_000))
              end

            _ ->
              {:error, {:unsupported_batch_method, method}}
          end
        end)
      end)

    # Wait for all tasks to complete
    results = Task.yield_many(tasks, timeout)

    # Convert task results to proper format
    batch_results =
      Enum.map(results, fn {task, result} ->
        case result do
          {:ok, value} ->
            value

          {:exit, reason} ->
            {:error, reason}

          nil ->
            Task.shutdown(task, :brutal_kill)
            {:error, :timeout}
        end
      end)

    {:ok, batch_results}
  end
end
