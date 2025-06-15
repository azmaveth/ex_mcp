defmodule ExMCP.Transport.Beam.Connection do
  @moduledoc """
  gen_statem-based connection management for the enhanced BEAM transport.

  This module replaces the mailbox-based architecture with a direct communication
  pattern using gen_statem for connection lifecycle management.

  ## State Machine

  - `connecting` - Initial state, attempting to establish connection
  - `authenticating` - Performing authentication handshake
  - `ready` - Connection established and ready for message exchange
  - `closing` - Connection is being closed gracefully

  ## Performance Benefits

  - Direct socket ownership eliminates extra process hops
  - gen_statem provides efficient state management
  - Registry-based correlation replaces synchronous GenServer.call
  - ETF serialization maximizes BEAM performance
  """

  use GenServer
  require Logger

  alias ExMCP.Transport.Beam.{Correlation, Frame, ZeroCopy}

  # 30 seconds
  @heartbeat_interval 30_000
  # 10 seconds
  @auth_timeout 10_000

  defstruct [
    :socket,
    :peer_info,
    :auth_config,
    :serialization_format,
    :owner_pid,
    :buffer,
    :heartbeat_timer,
    :auth_timer,
    :state,
    stats: %{bytes_sent: 0, bytes_received: 0, messages_sent: 0, messages_received: 0}
  ]

  @type t :: %__MODULE__{}
  @type connection_opts :: [
          socket: :gen_tcp.socket() | nil,
          peer_info: map(),
          auth_config: map() | nil,
          format: :etf | :json,
          owner: pid()
        ]
  @type connection_state :: :connecting | :authenticating | :ready | :closing

  # Client API

  @doc """
  Starts a new connection process.
  """
  @spec start_link(connection_opts()) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Sends a message through the connection with correlation tracking.
  """
  @spec send_message(pid(), map(), keyword()) :: {:ok, reference()} | {:error, term()}
  def send_message(connection, message, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    correlation_id = Correlation.register_request(self(), %{timeout: timeout})

    case GenServer.call(connection, {:send_message, message, correlation_id}) do
      :ok ->
        case Correlation.wait_for_response(correlation_id, timeout) do
          {:ok, response} -> {:ok, response}
          {:error, reason} -> {:error, reason}
        end

      {:error, reason} ->
        Correlation.cancel_request(correlation_id)
        {:error, reason}
    end
  end

  @doc """
  Sends a message without waiting for a response (fire-and-forget).
  """
  @spec send_notification(pid(), map()) :: :ok | {:error, term()}
  def send_notification(connection, message) do
    GenServer.call(connection, {:send_notification, message})
  end

  @doc """
  Gets connection statistics.
  """
  @spec get_stats(pid()) :: map()
  def get_stats(connection) do
    GenServer.call(connection, :get_stats)
  end

  @doc """
  Closes the connection gracefully.
  """
  @spec close(pid()) :: :ok
  def close(connection) do
    GenServer.call(connection, :close)
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    socket = Keyword.get(opts, :socket)
    peer_info = Keyword.get(opts, :peer_info, %{})
    auth_config = Keyword.get(opts, :auth_config)
    format = Keyword.get(opts, :format, :etf)
    owner_pid = Keyword.fetch!(opts, :owner)

    # Monitor owner process
    Process.monitor(owner_pid)

    state = %__MODULE__{
      socket: socket,
      peer_info: peer_info,
      auth_config: auth_config,
      serialization_format: format,
      owner_pid: owner_pid,
      buffer: <<>>,
      state: :connecting
    }

    # Start based on whether we have a socket (server) or need to connect (client)
    if socket do
      # Server-side connection
      if auth_config do
        send(self(), :start_auth)
        {:ok, %{state | state: :authenticating}}
      else
        send(self(), :connection_ready)
        {:ok, %{state | state: :ready}}
      end
    else
      # Client-side connection
      send(self(), :connect)
      {:ok, state}
    end
  end

  @impl true
  def handle_call({:send_message, message, correlation_id}, _from, state) do
    case state.state do
      :ready ->
        case encode_and_send_message(message, :rpc_request, correlation_id, state) do
          {:ok, new_state} ->
            {:reply, :ok, new_state}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end

      other_state ->
        {:reply, {:error, {:invalid_state, other_state}}, state}
    end
  end

  def handle_call({:send_notification, message}, _from, state) do
    case state.state do
      :ready ->
        case encode_and_send_message(message, :notification, nil, state) do
          {:ok, new_state} ->
            {:reply, :ok, new_state}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end

      other_state ->
        {:reply, {:error, {:invalid_state, other_state}}, state}
    end
  end

  def handle_call(:get_stats, _from, state) do
    {:reply, state.stats, state}
  end

  def handle_call({:send_raw_frame, frame_data}, _from, state) do
    case state.state do
      :ready ->
        case :gen_tcp.send(state.socket, frame_data) do
          :ok ->
            new_stats = %{
              state.stats
              | bytes_sent: state.stats.bytes_sent + byte_size(frame_data),
                messages_sent: state.stats.messages_sent + 1
            }

            {:reply, :ok, %{state | stats: new_stats}}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end

      other_state ->
        {:reply, {:error, {:invalid_state, other_state}}, state}
    end
  end

  def handle_call(:close, _from, state) do
    send(self(), :close_connection)
    {:reply, :ok, %{state | state: :closing}}
  end

  @impl true
  def handle_info(:connect, state) when state.state == :connecting do
    case connect_to_peer(state.peer_info) do
      {:ok, socket} ->
        new_state = %{state | socket: socket}

        if state.auth_config do
          send(self(), :start_auth)
          {:noreply, %{new_state | state: :authenticating}}
        else
          send(self(), :connection_ready)
          {:noreply, %{new_state | state: :ready}}
        end

      {:error, reason} ->
        Logger.error("Failed to connect: #{inspect(reason)}")
        {:stop, {:connection_failed, reason}, state}
    end
  end

  def handle_info(:start_auth, state) when state.state == :authenticating do
    # Set authentication timeout
    auth_timer = Process.send_after(self(), :auth_timeout, @auth_timeout)
    new_state = %{state | auth_timer: auth_timer}

    case perform_authentication(state.auth_config, state.socket) do
      :ok ->
        cancel_timer(auth_timer)
        send(self(), :connection_ready)
        {:noreply, %{new_state | auth_timer: nil, state: :ready}}

      {:error, reason} ->
        Logger.error("Authentication failed: #{inspect(reason)}")
        {:stop, {:auth_failed, reason}, state}

      :pending ->
        # Authentication is in progress, wait for response
        {:noreply, new_state}
    end
  end

  def handle_info(:auth_timeout, state) when state.state == :authenticating do
    Logger.error("Authentication timeout")
    {:stop, :auth_timeout, state}
  end

  def handle_info(:connection_ready, state) when state.state == :ready do
    # Set up socket for active message receiving
    :inet.setopts(state.socket, active: :once)

    # Start heartbeat timer
    heartbeat_timer = Process.send_after(self(), :send_heartbeat, @heartbeat_interval)
    new_state = %{state | heartbeat_timer: heartbeat_timer}

    # Notify owner that connection is ready
    send(state.owner_pid, {:connection_ready, self()})

    {:noreply, new_state}
  end

  def handle_info({:tcp, socket, data}, state) when socket == state.socket do
    case state.state do
      :authenticating ->
        # Handle authentication response
        new_state = %{state | buffer: state.buffer <> data}

        case parse_auth_response(new_state.buffer) do
          {:error, :incomplete} ->
            # Need more data
            {:noreply, new_state}

          {:error, reason} ->
            Logger.error("Authentication protocol error: #{inspect(reason)}")
            {:stop, {:auth_error, reason}, state}
        end

      :ready ->
        new_state = %{state | buffer: state.buffer <> data}

        case process_incoming_data(new_state) do
          {:ok, processed_state} ->
            # Re-enable active mode for next message
            :inet.setopts(socket, active: :once)
            {:noreply, processed_state}

          {:error, reason} ->
            Logger.error("Failed to process incoming data: #{inspect(reason)}")
            send(self(), :close_connection)
            {:noreply, %{state | state: :closing}}
        end

      _ ->
        # Ignore data in other states
        {:noreply, state}
    end
  end

  def handle_info(:send_heartbeat, state) when state.state == :ready do
    case send_heartbeat(state) do
      {:ok, new_state} ->
        # Schedule next heartbeat
        heartbeat_timer = Process.send_after(self(), :send_heartbeat, @heartbeat_interval)
        final_state = %{new_state | heartbeat_timer: heartbeat_timer}
        {:noreply, final_state}

      {:error, _reason} ->
        # Heartbeat failed, close connection
        send(self(), :close_connection)
        {:noreply, %{state | state: :closing}}
    end
  end

  def handle_info(:close_connection, state) when state.state == :closing do
    # Clean up resources
    cleanup_connection(state)
    {:stop, :normal, state}
  end

  def handle_info(msg, state) do
    handle_common_info(msg, state)
  end

  @impl true
  def terminate(reason, state) do
    Logger.debug("Connection terminating: #{inspect(reason)}")
    cleanup_connection(state)
    :ok
  end

  # Private helper functions

  defp connect_to_peer(peer_info) do
    host = Map.get(peer_info, :host, ~c"localhost")
    port = Map.get(peer_info, :port, 0)

    case :gen_tcp.connect(host, port, [:binary, active: false, packet: :raw]) do
      {:ok, socket} -> {:ok, socket}
      {:error, reason} -> {:error, reason}
    end
  end

  defp perform_authentication(nil, _socket), do: :ok

  defp perform_authentication(auth_config, socket) do
    case Map.get(auth_config, :type) do
      :bearer ->
        token = Map.fetch!(auth_config, :token)

        auth_frame =
          Frame.encode(%{type: "auth", method: "bearer", token: token}, 1, :rpc_request)

        case auth_frame do
          {:ok, frame_data} ->
            case :gen_tcp.send(socket, frame_data) do
              :ok -> :pending
              {:error, reason} -> {:error, reason}
            end

          {:error, reason} ->
            {:error, reason}
        end

      :none ->
        :ok

      _ ->
        {:error, :unsupported_auth_type}
    end
  end

  defp parse_auth_response(buffer) do
    case Frame.decode(buffer) do
      {:ok, %{type: :rpc_response, payload: response_data}} when is_binary(response_data) ->
        process_auth_json(response_data)

      {:error, :incomplete_frame} ->
        {:error, :incomplete}

      {:error, reason} ->
        {:error, reason}

      _other ->
        {:error, :invalid_auth_response}
    end
  end

  defp process_auth_json(response_data) do
    case Jason.decode(response_data) do
      {:ok, decoded_data} when is_map(decoded_data) ->
        handle_auth_result(decoded_data)

      _ ->
        {:error, :invalid_auth_response}
    end
  end

  defp handle_auth_result(decoded_data) do
    case Map.get(decoded_data, "result") do
      "authenticated" ->
        remaining = <<>>
        {:ok, :authenticated, remaining}

      nil ->
        case Map.get(decoded_data, "error") do
          nil -> {:error, :invalid_auth_response}
          reason -> {:ok, :auth_failed, reason}
        end

      _other ->
        {:error, :invalid_auth_response}
    end
  end

  defp encode_and_send_message(message, msg_type, correlation_id, state) do
    # Add correlation ID to message if provided
    final_message =
      if correlation_id do
        Map.put(message, "id", correlation_id)
      else
        message
      end

    # Apply zero-copy optimization for large payloads
    case ZeroCopy.prepare_message(final_message) do
      {:ok, prepared_message} ->
        case Frame.encode(prepared_message, 1, msg_type) do
          {:ok, frame_data} ->
            case :gen_tcp.send(state.socket, frame_data) do
              :ok ->
                new_stats = %{
                  state.stats
                  | bytes_sent: state.stats.bytes_sent + byte_size(frame_data),
                    messages_sent: state.stats.messages_sent + 1
                }

                {:ok, %{state | stats: new_stats}}

              {:error, reason} ->
                {:error, reason}
            end

          {:error, reason} ->
            {:error, {:frame_encode_error, reason}}
        end

      {:error, reason} ->
        {:error, {:zero_copy_error, reason}}
    end
  end

  defp process_incoming_data(state) do
    case Frame.decode(state.buffer) do
      {:ok, %{type: msg_type, payload: payload}} ->
        # Update stats
        new_stats = %{
          state.stats
          | bytes_received: state.stats.bytes_received + byte_size(state.buffer),
            messages_received: state.stats.messages_received + 1
        }

        # Forward message to owner process (simplified)
        case msg_type do
          :rpc_response ->
            # Extract correlation ID and send response
            if is_map(payload) do
              case Map.get(payload, "id") do
                nil ->
                  Logger.warning("Received response without correlation ID")

                correlation_id ->
                  Correlation.send_response(correlation_id, payload)
              end
            end

          :rpc_request ->
            # Forward request to owner
            send(state.owner_pid, {:incoming_request, payload, self()})

          :notification ->
            # Forward notification to owner
            send(state.owner_pid, {:incoming_notification, payload})

          _ ->
            Logger.debug("Received message type: #{inspect(msg_type)}")
        end

        # Clear buffer (assuming single message per buffer for now)
        {:ok, %{state | buffer: <<>>, stats: new_stats}}

      {:error, :incomplete_frame} ->
        # Need more data, keep current state
        {:ok, state}

      {:error, reason} ->
        {:error, {:decode_error, reason}}
    end
  end

  defp send_heartbeat(state) do
    heartbeat_message = %{"type" => "heartbeat", "timestamp" => System.system_time(:millisecond)}
    encode_and_send_message(heartbeat_message, :heartbeat, nil, state)
  end

  defp handle_common_info({:DOWN, _ref, :process, pid, reason}, state) do
    if pid == state.owner_pid do
      Logger.debug("Owner process died: #{inspect(reason)}")
      {:stop, :owner_down, state}
    else
      {:noreply, state}
    end
  end

  defp handle_common_info({:tcp_closed, socket}, state) when socket == state.socket do
    Logger.debug("Socket closed")
    {:stop, :connection_closed, state}
  end

  defp handle_common_info({:tcp_error, socket, reason}, state) when socket == state.socket do
    Logger.error("Socket error: #{inspect(reason)}")
    {:stop, {:socket_error, reason}, state}
  end

  defp handle_common_info(msg, state) do
    Logger.debug("Unexpected message: #{inspect(msg)}")
    {:noreply, state}
  end

  defp cleanup_connection(state) do
    # Cancel timers
    if state.heartbeat_timer, do: Process.cancel_timer(state.heartbeat_timer)
    if state.auth_timer, do: Process.cancel_timer(state.auth_timer)

    # Close socket
    if state.socket do
      :gen_tcp.close(state.socket)
    end

    # Notify owner
    if state.owner_pid && Process.alive?(state.owner_pid) do
      send(state.owner_pid, {:connection_closed, self()})
    end
  end

  defp cancel_timer(timer_ref) when is_reference(timer_ref), do: Process.cancel_timer(timer_ref)
end
