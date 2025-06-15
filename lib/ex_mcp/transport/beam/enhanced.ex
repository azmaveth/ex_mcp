defmodule ExMCP.Transport.Beam.Enhanced do
  @moduledoc """
  Enhanced BEAM transport implementation that integrates with ExMCP.Protocol.

  This transport bridges the gap between the standard MCP Transport behavior
  (which expects JSON strings) and our enhanced BEAM transport components
  (Frame, Connection, Security, etc.) which work with structured data.

  ## Architecture Integration

  ```
  Client/Server (Protocol-encoded JSON)
         ↓
  Enhanced Transport (JSON ↔ Maps)
         ↓  
  Connection/Frame/Security (Binary frames)
         ↓
  TCP/Network
  ```

  ## Features

  - **Protocol Compatibility**: Fully compatible with ExMCP.Protocol
  - **Enhanced Performance**: Uses optimized Frame, Connection, ZeroCopy, Batch modules
  - **Security**: Integrated security validation at frame and message levels
  - **Observability**: Built-in metrics and monitoring
  - **Fault Tolerance**: Advanced error handling and recovery

  ## Usage

  This transport can be used as a drop-in replacement for the standard BEAM transport:

      # In Client/Server configuration
      transport: ExMCP.Transport.Beam.Enhanced

  All existing Protocol-level APIs continue to work unchanged.
  """

  @behaviour ExMCP.Transport

  require Logger
  alias ExMCP.Transport.Beam.{Batch, Connection, Observability, Security, ZeroCopy}

  @type state :: %{
          connection: pid() | nil,
          batcher: pid() | nil,
          mode: :client | :server,
          config: map(),
          stats: map()
        }

  @doc """
  Establishes an enhanced BEAM transport connection.

  ## Options

  - `:mode` - `:client` or `:server` (required)
  - `:host` - Remote host for client connections (default: "localhost")
  - `:port` - Remote port for client connections (required for client)
  - `:security` - Security configuration map
  - `:batching` - Enable message batching (default: true)
  - `:zero_copy` - Enable zero-copy optimization (default: true)

  ## Examples

      # Client mode
      {:ok, state} = Enhanced.connect([
        mode: :client,
        host: "localhost", 
        port: 8080,
        security: %{max_frame_size: 1_048_576}
      ])

      # Server mode (with existing socket)
      {:ok, state} = Enhanced.connect([
        mode: :server,
        socket: existing_socket,
        security: %{authentication: %{required: true}}
      ])
  """
  @impl true
  @spec connect(keyword()) :: {:ok, state()} | {:error, term()}
  def connect(opts) do
    mode = Keyword.fetch!(opts, :mode)
    security_config = Keyword.get(opts, :security, %{})
    batching_enabled = Keyword.get(opts, :batching, true)

    # Start security service if needed
    case Security.start() do
      {:ok, _pid} -> :ok
      # Already started
      {:error, _} -> :ok
    end

    # Start observability system if needed
    case Observability.start_link() do
      {:ok, _pid} -> :ok
      # Already started
      {:error, _} -> :ok
    end

    # Configure connection options
    connection_opts = build_connection_opts(mode, opts)

    case Connection.start_link(connection_opts) do
      {:ok, connection} ->
        state = %{
          connection: connection,
          batcher: nil,
          mode: mode,
          config: %{
            security: security_config,
            batching: batching_enabled,
            zero_copy: Keyword.get(opts, :zero_copy, true)
          },
          stats: %{
            messages_sent: 0,
            messages_received: 0,
            bytes_sent: 0,
            bytes_received: 0,
            errors: 0
          }
        }

        # Start batcher if enabled
        state =
          if batching_enabled do
            case start_batcher(connection, opts) do
              {:ok, batcher} -> %{state | batcher: batcher}
              {:error, _reason} -> state
            end
          else
            state
          end

        Logger.info("Enhanced BEAM transport connected in #{mode} mode")
        {:ok, state}

      {:error, reason} ->
        Logger.error("Failed to establish enhanced BEAM connection: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Sends a Protocol-encoded JSON message through the enhanced transport.

  The message (JSON string) is parsed, potentially batched, and sent through
  our enhanced connection with security validation and performance optimizations.
  """
  @impl true
  @spec send_message(String.t(), state()) :: {:ok, state()} | {:error, term()}
  def send_message(json_message, state) when is_binary(json_message) do
    with {:ok, message_map} <- Jason.decode(json_message),
         {:ok, new_state} <- do_send_message(message_map, state) do
      # Update stats and record observability metrics
      Observability.record_message_sent(byte_size(json_message))

      updated_stats = %{
        new_state.stats
        | messages_sent: new_state.stats.messages_sent + 1,
          bytes_sent: new_state.stats.bytes_sent + byte_size(json_message)
      }

      {:ok, %{new_state | stats: updated_stats}}
    else
      {:error, %Jason.DecodeError{} = error} ->
        Logger.warning("Failed to decode JSON message: #{inspect(error)}")
        Observability.record_error(:json_decode_error)
        _error_state = update_error_stats(state)
        {:error, {:json_decode_error, error}}

      {:error, reason} = error ->
        Logger.warning("Failed to send message: #{inspect(reason)}")
        Observability.record_error(:send_message_failed)
        _error_state = update_error_stats(state)
        error
    end
  end

  @doc """
  Receives a message from the enhanced transport.

  This is a blocking operation that waits for incoming messages from the connection.
  Messages are automatically validated, decoded, and returned as JSON strings
  for Protocol compatibility.
  """
  @impl true
  @spec receive_message(state()) :: {:ok, String.t(), state()} | {:error, term()}
  def receive_message(state) do
    # In our enhanced design, we use async message handling via owner process
    # This receive_message is mainly for compatibility with the Transport behavior

    receive do
      {:incoming_request, message_map, _connection} ->
        case Jason.encode(message_map) do
          {:ok, json_message} ->
            # Record observability metrics (assume 0 latency for now)
            Observability.record_message_received(byte_size(json_message), 0)

            updated_stats = %{
              state.stats
              | messages_received: state.stats.messages_received + 1,
                bytes_received: state.stats.bytes_received + byte_size(json_message)
            }

            new_state = %{state | stats: updated_stats}
            {:ok, json_message, new_state}

          {:error, error} ->
            Logger.warning("Failed to encode received message: #{inspect(error)}")
            _error_state = update_error_stats(state)
            {:error, {:json_encode_error, error}}
        end

      {:incoming_notification, message_map} ->
        case Jason.encode(message_map) do
          {:ok, json_message} ->
            # Record observability metrics (assume 0 latency for now)
            Observability.record_message_received(byte_size(json_message), 0)

            updated_stats = %{
              state.stats
              | messages_received: state.stats.messages_received + 1,
                bytes_received: state.stats.bytes_received + byte_size(json_message)
            }

            new_state = %{state | stats: updated_stats}
            {:ok, json_message, new_state}

          {:error, error} ->
            Logger.warning("Failed to encode received notification: #{inspect(error)}")
            _error_state = update_error_stats(state)
            {:error, {:json_encode_error, error}}
        end

      {:connection_closed, _connection} ->
        Logger.info("Enhanced BEAM connection closed")
        {:error, :connection_closed}
    after
      # 30 second timeout
      30_000 ->
        {:error, :timeout}
    end
  end

  @doc """
  Closes the enhanced transport connection.

  Gracefully shuts down all components including connection, batcher,
  and cleans up any associated resources.
  """
  @impl true
  @spec close(state()) :: :ok
  def close(state) do
    # Stop batcher if running
    if state.batcher && Process.alive?(state.batcher) do
      Batch.flush(state.batcher)
      GenServer.stop(state.batcher, :normal, 1000)
    end

    # Close connection
    if state.connection && Process.alive?(state.connection) do
      Connection.close(state.connection)
    end

    Logger.info("Enhanced BEAM transport closed")
    :ok
  end

  @doc """
  Checks if the enhanced transport connection is still active.
  """
  @impl true
  @spec connected?(state()) :: boolean()
  def connected?(state) do
    state.connection != nil && Process.alive?(state.connection)
  end

  @doc """
  Gets comprehensive transport statistics.

  Returns detailed metrics about message throughput, errors, and performance.
  """
  @spec get_stats(state()) :: map()
  def get_stats(state) do
    base_stats = state.stats

    connection_stats =
      if state.connection do
        Connection.get_stats(state.connection)
      else
        %{}
      end

    batch_stats =
      if state.batcher do
        Batch.get_stats(state.batcher)
      else
        %{}
      end

    security_stats =
      try do
        Security.get_security_stats()
      catch
        :exit, _ -> %{security_service_not_running: true}
      end

    # Get comprehensive observability stats
    observability_stats =
      try do
        Observability.get_comprehensive_stats()
      catch
        :exit, _ -> %{observability_service_not_running: true}
      end

    Map.merge(base_stats, %{
      connection: connection_stats,
      batching: batch_stats,
      security: security_stats,
      zero_copy:
        try do
          ZeroCopy.get_stats()
        catch
          :exit, _ -> %{zero_copy_service_not_running: true}
        end,
      observability: observability_stats
    })
  end

  # Private helper functions

  defp build_connection_opts(:client, opts) do
    [
      peer_info: %{
        host: Keyword.get(opts, :host, "localhost"),
        port: Keyword.fetch!(opts, :port)
      },
      auth_config: Keyword.get(opts, :auth_config),
      format: :etf,
      owner: self()
    ]
  end

  defp build_connection_opts(:server, opts) do
    [
      socket: Keyword.fetch!(opts, :socket),
      peer_info: Keyword.get(opts, :peer_info, %{}),
      auth_config: Keyword.get(opts, :auth_config),
      format: :etf,
      owner: self()
    ]
  end

  defp start_batcher(connection, opts) do
    batch_opts = [
      connection: connection,
      max_batch_size: Keyword.get(opts, :max_batch_size, 50),
      max_batch_bytes: Keyword.get(opts, :max_batch_bytes, 32_768),
      batch_timeout: Keyword.get(opts, :batch_timeout, 5)
    ]

    Batch.start_link(batch_opts)
  end

  defp do_send_message(message_map, %{batcher: batcher} = state) when not is_nil(batcher) do
    # Use batching for better throughput
    case priority_message?(message_map) do
      true ->
        # Send priority messages immediately
        case Batch.send_priority_message(batcher, message_map) do
          :ok -> {:ok, state}
          {:error, reason} -> {:error, reason}
        end

      false ->
        # Batch regular messages
        case Batch.send_message(batcher, message_map) do
          :ok -> {:ok, state}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  defp do_send_message(message_map, %{connection: connection} = state) do
    # Direct send without batching
    case Connection.send_notification(connection, message_map) do
      :ok -> {:ok, state}
      {:error, reason} -> {:error, reason}
    end
  end

  defp priority_message?(%{"method" => "cancelled"}), do: true
  defp priority_message?(%{"method" => "ping"}), do: true
  defp priority_message?(_), do: false

  defp update_error_stats(state) do
    updated_stats = %{state.stats | errors: state.stats.errors + 1}
    %{state | stats: updated_stats}
  end
end
