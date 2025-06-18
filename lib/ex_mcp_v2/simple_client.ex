defmodule ExMCP.SimpleClient do
  @moduledoc """
  Robust synchronous MCP client for v2 architecture.

  This client provides:
  - Synchronous initialization - connection established before start_link returns
  - Automatic reconnection with exponential backoff
  - Comprehensive error handling
  - Transport abstraction

  ## Features

  - **Reliable startup**: Connection guaranteed ready when start_link succeeds
  - **Auto-reconnection**: Handles connection drops with exponential backoff
  - **Error recovery**: Graceful handling of transport and protocol errors
  - **Health monitoring**: Periodic connection health checks
  """

  use GenServer
  require Logger

  alias ExMCP.Internal.Protocol
  alias ExMCP.TransportManager

  defstruct [
    :transport_mod,
    :transport_state,
    :transport_opts,
    :server_info,
    :pending_requests,
    :request_counter,
    :connection_status,
    :reconnect_attempts,
    :max_reconnect_attempts,
    :reconnect_interval,
    :reconnect_timer,
    :health_check_interval,
    :health_check_timer,
    :last_activity
  ]

  @type connection_status :: :connected | :reconnecting | :disconnected | :error

  # Default configuration
  @default_max_reconnect_attempts 5
  # ms
  @default_reconnect_interval 1_000
  # ms
  @default_health_check_interval 30_000

  # Public API

  @doc """
  Starts a new MCP client with robust error handling and transport fallback.

  ## Options

  ### Transport Configuration
  - `:transport` - Single transport type (:stdio, :http, :sse, :native)
  - `:transports` - List of transport specs for fallback: `[{module, opts}, ...]`
  - `:fallback_strategy` - Strategy for transport fallback (`:sequential`, `:parallel`, `:fastest`)

  ### Connection Settings  
  - `:max_reconnect_attempts` - Max reconnection attempts (default: #{@default_max_reconnect_attempts})
  - `:reconnect_interval` - Base reconnection interval in ms (default: #{@default_reconnect_interval})
  - `:health_check_interval` - Health check interval in ms (default: #{@default_health_check_interval})

  ### Transport-specific Options
  All other options are passed to the transport(s).

  ## Examples

      # Single transport
      {:ok, client} = SimpleClient.start_link(transport: :http, url: "http://localhost:8080")
      
      # Multiple transports with fallback
      {:ok, client} = SimpleClient.start_link(
        transports: [
          {ExMCP.Transport.HTTP, [url: "http://localhost:8080"]},
          {ExMCP.Transport.Stdio, [command: "mcp-server"]}
        ],
        fallback_strategy: :sequential
      )
  """
  def start_link(opts) do
    {gen_opts, init_opts} = Keyword.split(opts, [:name, :timeout])
    GenServer.start_link(__MODULE__, init_opts, gen_opts)
  end

  @doc """
  Lists available tools from the server.
  """
  def list_tools(client, timeout \\ 5_000) do
    GenServer.call(client, {:request, "tools/list", %{}}, timeout)
  end

  @doc """
  Calls a tool with the given arguments.
  """
  def call_tool(client, tool_name, args, timeout \\ 30_000) do
    GenServer.call(
      client,
      {:request, "tools/call", %{"name" => tool_name, "arguments" => args}},
      timeout
    )
  end

  @doc """
  Lists available resources from the server.
  """
  def list_resources(client, timeout \\ 5_000) do
    GenServer.call(client, {:request, "resources/list", %{}}, timeout)
  end

  @doc """
  Reads a resource by URI.
  """
  def read_resource(client, uri, timeout \\ 10_000) do
    GenServer.call(client, {:request, "resources/read", %{"uri" => uri}}, timeout)
  end

  @doc """
  Lists available prompts from the server.
  """
  def list_prompts(client, timeout \\ 5_000) do
    GenServer.call(client, {:request, "prompts/list", %{}}, timeout)
  end

  @doc """
  Gets a prompt with the given arguments.
  """
  def get_prompt(client, prompt_name, args \\ %{}, timeout \\ 5_000) do
    GenServer.call(
      client,
      {:request, "prompts/get", %{"name" => prompt_name, "arguments" => args}},
      timeout
    )
  end

  @doc """
  Gets the current connection status and health.
  """
  def get_status(client) do
    GenServer.call(client, :get_status)
  end

  @doc """
  Forces a reconnection attempt.
  """
  def reconnect(client) do
    GenServer.call(client, :force_reconnect)
  end

  @doc """
  Performs a health check by pinging the server.
  """
  def health_check(client, timeout \\ 5_000) do
    GenServer.call(client, :health_check, timeout)
  end

  # GenServer callbacks

  @impl GenServer
  def init(opts) do
    # Synchronous initialization with robust error handling and transport fallback
    config = %{
      max_reconnect_attempts:
        Keyword.get(opts, :max_reconnect_attempts, @default_max_reconnect_attempts),
      reconnect_interval: Keyword.get(opts, :reconnect_interval, @default_reconnect_interval),
      health_check_interval:
        Keyword.get(opts, :health_check_interval, @default_health_check_interval)
    }

    case establish_connection_with_fallback(opts) do
      {:ok, transport_mod, transport_state, server_info} ->
        # Schedule health check
        health_timer =
          if config.health_check_interval > 0 do
            Process.send_after(self(), :health_check, config.health_check_interval)
          end

        # Store original opts for reconnection
        transport_opts = prepare_transport_opts(opts)

        state = %__MODULE__{
          transport_mod: transport_mod,
          transport_state: transport_state,
          transport_opts: transport_opts,
          server_info: server_info,
          pending_requests: %{},
          request_counter: 0,
          connection_status: :connected,
          reconnect_attempts: 0,
          max_reconnect_attempts: config.max_reconnect_attempts,
          reconnect_interval: config.reconnect_interval,
          health_check_interval: config.health_check_interval,
          health_check_timer: health_timer,
          last_activity: System.monotonic_time(:millisecond)
        }

        Logger.info("MCP client connected successfully using #{inspect(transport_mod)}")
        {:ok, state}

      {:error, reason} ->
        Logger.error("Failed to initialize MCP client: #{inspect(reason)}")
        {:stop, reason}
    end
  end

  @impl GenServer
  def handle_call({:request, method, params}, _from, state) do
    case state.connection_status do
      :connected ->
        send_request_with_retry(method, params, state)

      status ->
        Logger.warning("Cannot send request - connection status: #{status}")
        {:reply, {:error, {:not_connected, status}}, state}
    end
  end

  def handle_call(:get_status, _from, state) do
    status = %{
      connection_status: state.connection_status,
      server_info: state.server_info,
      reconnect_attempts: state.reconnect_attempts,
      last_activity: state.last_activity,
      pending_requests: map_size(state.pending_requests)
    }

    {:reply, {:ok, status}, state}
  end

  def handle_call(:force_reconnect, _from, state) do
    Logger.info("Forcing reconnection")

    case attempt_reconnect(state) do
      {:ok, new_state} ->
        {:reply, :ok, new_state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(:health_check, _from, state) do
    case state.connection_status do
      :connected ->
        # Try a simple ping/tools list to verify connectivity
        case send_request_with_retry("tools/list", %{}, state) do
          {:reply, {:ok, _result}, new_state} ->
            {:reply, :ok, new_state}

          {:reply, {:error, reason}, new_state} ->
            Logger.warning("Health check failed: #{inspect(reason)}")
            {:reply, {:error, reason}, new_state}
        end

      status ->
        {:reply, {:error, {:not_connected, status}}, state}
    end
  end

  @impl GenServer
  def handle_info(:health_check, state) do
    # Periodic health check
    case perform_health_check(state) do
      {:ok, new_state} ->
        # Schedule next health check
        timer = Process.send_after(self(), :health_check, state.health_check_interval)
        {:noreply, %{new_state | health_check_timer: timer}}

      {:error, _reason} ->
        # Health check failed, try to reconnect
        Logger.warning("Health check failed, attempting reconnection")
        {:noreply, schedule_reconnect(state)}
    end
  end

  def handle_info(:attempt_reconnect, state) do
    case attempt_reconnect(state) do
      {:ok, new_state} ->
        Logger.info("Reconnection successful")
        {:noreply, new_state}

      {:error, reason} ->
        Logger.warning("Reconnection failed: #{inspect(reason)}")
        {:noreply, schedule_reconnect(state)}
    end
  end

  def handle_info({:transport_error, reason}, state) do
    Logger.error("Transport error: #{inspect(reason)}")
    {:noreply, schedule_reconnect(%{state | connection_status: :disconnected})}
  end

  def handle_info(msg, state) do
    Logger.debug("Unhandled message: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl GenServer
  def terminate(reason, state) do
    Logger.info("MCP client terminating: #{inspect(reason)}")

    # Cancel timers
    if state.reconnect_timer, do: Process.cancel_timer(state.reconnect_timer)
    if state.health_check_timer, do: Process.cancel_timer(state.health_check_timer)

    # Close transport
    if state.transport_state do
      state.transport_mod.close(state.transport_state)
    end

    :ok
  end

  # Private functions

  defp establish_connection_with_fallback(opts) do
    case prepare_transport_config(opts) do
      {:single, transport_mod, transport_opts} ->
        # Single transport mode (legacy compatibility)
        case establish_connection(transport_mod, transport_opts) do
          {:ok, transport_state, server_info} ->
            {:ok, transport_mod, transport_state, server_info}

          {:error, reason} ->
            {:error, reason}
        end

      {:multiple, transport_manager_opts} ->
        # Multiple transport mode with fallback
        case TransportManager.connect(transport_manager_opts) do
          {:ok, {transport_mod, transport_state}} ->
            case do_handshake(transport_mod, transport_state) do
              {:ok, server_info} ->
                {:ok, transport_mod, transport_state, server_info}

              {:error, reason} ->
                {:error, reason}
            end

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp prepare_transport_config(opts) do
    cond do
      Keyword.has_key?(opts, :transports) ->
        # Multiple transports specified
        transport_manager_opts =
          Keyword.take(opts, [
            :transports,
            :fallback_strategy,
            :max_retries,
            :retry_interval
          ])

        {:multiple, transport_manager_opts}

      Keyword.has_key?(opts, :transport) ->
        transport_config = Keyword.get(opts, :transport)

        # Check if transport is a keyword list (mock transport case)
        if is_list(transport_config) and Keyword.get(transport_config, :type) == :mock do
          # Mock transport configuration
          {:single, ExMCP.Testing.MockTransport, transport_config}
        else
          # Regular transport specified
          transport_mod = get_transport(opts)
          transport_opts = get_transport_opts(opts)
          {:single, transport_mod, transport_opts}
        end

      true ->
        # Default to stdio
        {:single, ExMCP.Transport.Stdio, get_transport_opts(opts)}
    end
  end

  defp prepare_transport_opts(opts) do
    # Store full opts for reconnection - the TransportManager will handle parsing
    opts
  end

  defp get_transport(opts) do
    # Check both :transport and :type keys for backward compatibility
    transport_type = Keyword.get(opts, :transport) || Keyword.get(opts, :type, :stdio)

    case transport_type do
      :stdio ->
        ExMCP.Transport.Stdio

      :http ->
        ExMCP.Transport.HTTP

      :sse ->
        ExMCP.Transport.SSE

      :native ->
        ExMCP.Transport.Native

      :beam ->
        ExMCP.Transport.Native

      :mock ->
        ExMCP.Transport.Mock

      mod when is_atom(mod) ->
        mod

      other ->
        # Log for debugging
        require Logger
        Logger.error("Unexpected transport type: #{inspect(other)}, opts: #{inspect(opts)}")
        raise ArgumentError, "Unknown transport type: #{inspect(other)}"
    end
  end

  defp get_transport_opts(opts) do
    Keyword.drop(opts, [
      :transport,
      :transports,
      :fallback_strategy,
      :max_reconnect_attempts,
      :reconnect_interval,
      :health_check_interval,
      :name,
      :timeout,
      :max_retries,
      :retry_interval
    ])
  end

  defp establish_connection(transport_mod, transport_opts) do
    with {:ok, transport_state} <- connect_with_retry(transport_mod, transport_opts),
         {:ok, server_info} <- do_handshake(transport_mod, transport_state) do
      {:ok, transport_state, server_info}
    end
  end

  defp connect_with_retry(transport_mod, transport_opts, attempts \\ 3) do
    case transport_mod.connect(transport_opts) do
      {:ok, transport_state} ->
        {:ok, transport_state}

      {:error, reason} when attempts > 1 ->
        Logger.warning("Connection attempt failed: #{inspect(reason)}, retrying...")
        Process.sleep(1000)
        connect_with_retry(transport_mod, transport_opts, attempts - 1)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_handshake(transport_mod, transport_state) do
    # Send initialize
    init_msg =
      Protocol.encode_initialize(
        %{"name" => "ExMCP", "version" => "0.1.0"},
        %{},
        "2024-11-05"
      )

    with {:ok, _} <- transport_mod.send(transport_state, Jason.encode!(init_msg)),
         {:ok, response, _new_state} <-
           wait_for_response(transport_mod, transport_state, init_msg["id"], 10_000),
         {:ok, _} <- send_initialized(transport_mod, transport_state) do
      # Extract server info from the initialize response
      server_info = case response do
        %{"result" => %{"serverInfo" => info}} -> info
        %{"serverInfo" => info} -> info  # For direct response format
        _ -> nil
      end
      {:ok, server_info}
    end
  end

  defp send_initialized(transport_mod, transport_state) do
    msg = Protocol.encode_initialized()
    transport_mod.send(transport_state, Jason.encode!(msg))
  end

  defp send_request_with_retry(method, params, state) do
    request_id = state.request_counter + 1

    request = %{
      "jsonrpc" => "2.0",
      "id" => request_id,
      "method" => method,
      "params" => params
    }

    case state.transport_mod.send(state.transport_state, Jason.encode!(request)) do
      {:ok, new_transport_state} ->
        # Wait for response synchronously
        case wait_for_response(state.transport_mod, new_transport_state, request_id, 10_000) do
          {:ok, result, final_transport_state} ->
            new_state = %{
              state
              | transport_state: final_transport_state,
                request_counter: request_id,
                last_activity: System.monotonic_time(:millisecond)
            }

            {:reply, {:ok, result}, new_state}

          {:error, reason} ->
            Logger.warning("Request failed: #{inspect(reason)}")
            # Don't immediately disconnect on single request failure
            {:reply, {:error, reason}, state}
        end

      {:error, reason} ->
        Logger.error("Failed to send request: #{inspect(reason)}")
        # Transport error, schedule reconnection
        new_state = schedule_reconnect(%{state | connection_status: :disconnected})
        {:reply, {:error, reason}, new_state}
    end
  end

  defp wait_for_response(transport_mod, transport_state, expected_id, timeout) do
    deadline = System.monotonic_time(:millisecond) + timeout
    do_wait(transport_mod, transport_state, expected_id, deadline)
  end

  defp do_wait(transport_mod, transport_state, expected_id, deadline) do
    now = System.monotonic_time(:millisecond)

    if now > deadline do
      {:error, :timeout}
    else
      remaining = deadline - now

      case transport_mod.recv(transport_state, remaining) do
        {:ok, data, new_state} ->
          case Protocol.parse_message(data) do
            {:result, result, ^expected_id} ->
              {:ok, result, new_state}

            {:error, error, ^expected_id} ->
              {:error, error}

            _ ->
              # Not our message, keep waiting
              do_wait(transport_mod, new_state, expected_id, deadline)
          end

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  defp perform_health_check(state) do
    # Simple health check - try to list tools
    case send_simple_request(state, "tools/list", %{}) do
      {:ok, _result} -> {:ok, state}
      {:error, reason} -> {:error, reason}
    end
  end

  defp send_simple_request(state, method, params) do
    request_id = System.unique_integer([:positive])

    request = %{
      "jsonrpc" => "2.0",
      "id" => request_id,
      "method" => method,
      "params" => params
    }

    with {:ok, _} <- state.transport_mod.send(state.transport_state, Jason.encode!(request)),
         {:ok, result, _} <-
           wait_for_response(state.transport_mod, state.transport_state, request_id, 5_000) do
      {:ok, result}
    end
  end

  defp attempt_reconnect(state) do
    if state.reconnect_attempts >= state.max_reconnect_attempts do
      Logger.error("Max reconnection attempts (#{state.max_reconnect_attempts}) reached")
      {:error, :max_reconnect_attempts_reached}
    else
      Logger.info(
        "Attempting reconnection (#{state.reconnect_attempts + 1}/#{state.max_reconnect_attempts})"
      )

      case establish_connection_with_fallback(state.transport_opts) do
        {:ok, transport_mod, transport_state, server_info} ->
          # Reset reconnection state
          new_state = %{
            state
            | transport_mod: transport_mod,
              transport_state: transport_state,
              server_info: server_info,
              connection_status: :connected,
              reconnect_attempts: 0,
              reconnect_timer: nil,
              last_activity: System.monotonic_time(:millisecond)
          }

          # Restart health check timer
          health_timer =
            if state.health_check_interval > 0 do
              Process.send_after(self(), :health_check, state.health_check_interval)
            end

          {:ok, %{new_state | health_check_timer: health_timer}}

        {:error, reason} ->
          Logger.warning("Reconnection attempt failed: #{inspect(reason)}")
          {:error, reason}
      end
    end
  end

  defp schedule_reconnect(state) do
    # Cancel existing timer
    if state.reconnect_timer do
      Process.cancel_timer(state.reconnect_timer)
    end

    if state.reconnect_attempts < state.max_reconnect_attempts do
      # Exponential backoff
      delay = state.reconnect_interval * :math.pow(2, state.reconnect_attempts)
      # Cap at 30 seconds
      delay = min(delay, 30_000)

      timer = Process.send_after(self(), :attempt_reconnect, round(delay))

      %{
        state
        | connection_status: :reconnecting,
          reconnect_attempts: state.reconnect_attempts + 1,
          reconnect_timer: timer
      }
    else
      Logger.error("Max reconnection attempts reached, giving up")
      %{state | connection_status: :error}
    end
  end
end
