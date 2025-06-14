defmodule ExMCP.Transport.Beam.Batch do
  @moduledoc """
  Message batching for high-throughput scenarios in BEAM transport.

  This module provides efficient batching of multiple messages into single
  frames to reduce network overhead and improve throughput when dealing
  with high message volumes.

  ## Architecture

  - **Accumulation**: Messages are accumulated in a buffer until batch criteria are met
  - **Triggering**: Batches are sent when size, count, or time thresholds are reached
  - **Ordering**: Message ordering is preserved within and across batches
  - **Backpressure**: Built-in flow control prevents memory exhaustion

  ## Benefits

  - **Reduced Overhead**: Fewer TCP packets and frame headers
  - **Higher Throughput**: More efficient network utilization
  - **Better Performance**: Reduced syscall overhead
  - **Flow Control**: Automatic backpressure handling

  ## Usage

      # Start a batcher for a connection
      {:ok, batcher} = Batch.start_link([
        connection: connection_pid,
        max_batch_size: 100,
        max_batch_bytes: 64_000,
        batch_timeout: 10
      ])
      
      # Send messages (they'll be batched automatically)
      Batch.send_message(batcher, message1)
      Batch.send_message(batcher, message2)
      
      # Force immediate flush if needed
      Batch.flush(batcher)

  ## Configuration

  - `:max_batch_size` - Maximum messages per batch (default: 50)
  - `:max_batch_bytes` - Maximum bytes per batch (default: 32KB)
  - `:batch_timeout` - Max time to wait before sending partial batch (default: 5ms)
  - `:connection` - Connection process to send batches to
  """

  use GenServer
  require Logger

  alias ExMCP.Transport.Beam.{Frame, ZeroCopy}

  @default_max_batch_size 50
  # 32KB
  @default_max_batch_bytes 32_768
  # 5ms
  @default_batch_timeout 5

  @type batch_opts :: [
          connection: pid(),
          max_batch_size: pos_integer(),
          max_batch_bytes: pos_integer(),
          batch_timeout: pos_integer()
        ]

  defstruct [
    :connection,
    :max_batch_size,
    :max_batch_bytes,
    :batch_timeout,
    :timer_ref,
    messages: [],
    current_bytes: 0,
    stats: %{
      batches_sent: 0,
      messages_batched: 0,
      bytes_batched: 0,
      average_batch_size: 0,
      timeout_flushes: 0,
      size_flushes: 0,
      byte_flushes: 0
    }
  ]

  @doc """
  Starts a message batcher for a connection.
  """
  @spec start_link(batch_opts()) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Sends a message through the batcher.

  The message will be batched with others and sent when batch criteria are met.
  """
  @spec send_message(pid(), map()) :: :ok | {:error, term()}
  def send_message(batcher, message) do
    GenServer.call(batcher, {:send_message, message})
  end

  @doc """
  Sends a message with priority (bypasses batching).

  Priority messages are sent immediately without batching.
  """
  @spec send_priority_message(pid(), map()) :: :ok | {:error, term()}
  def send_priority_message(batcher, message) do
    GenServer.call(batcher, {:send_priority_message, message})
  end

  @doc """
  Forces immediate flush of pending messages.
  """
  @spec flush(pid()) :: :ok
  def flush(batcher) do
    GenServer.call(batcher, :flush)
  end

  @doc """
  Gets batching statistics.
  """
  @spec get_stats(pid()) :: map()
  def get_stats(batcher) do
    GenServer.call(batcher, :get_stats)
  end

  @doc """
  Updates batching configuration.
  """
  @spec configure(pid(), keyword()) :: :ok
  def configure(batcher, opts) do
    GenServer.call(batcher, {:configure, opts})
  end

  @doc """
  Checks if batching would be beneficial for the given scenario.

  Returns recommendations based on message patterns.
  """
  @spec analyze_batching_benefit(list(), keyword()) :: %{
          recommended: boolean(),
          expected_reduction: float(),
          reasoning: String.t()
        }
  def analyze_batching_benefit(messages, opts \\ []) do
    message_count = length(messages)

    total_bytes =
      Enum.reduce(messages, 0, fn msg, acc ->
        acc + estimate_message_size(msg)
      end)

    avg_message_size = if message_count > 0, do: div(total_bytes, message_count), else: 0
    # Frame header size
    frame_overhead_per_message = 8

    # Calculate potential savings
    unbatched_overhead = message_count * frame_overhead_per_message

    max_batch_size = Keyword.get(opts, :max_batch_size, @default_max_batch_size)
    batch_count = ceil(message_count / max_batch_size)
    batched_overhead = batch_count * frame_overhead_per_message

    overhead_reduction = (unbatched_overhead - batched_overhead) / unbatched_overhead

    cond do
      message_count < 5 ->
        %{
          recommended: false,
          expected_reduction: 0.0,
          reasoning: "Too few messages to benefit from batching"
        }

      avg_message_size > 10_000 ->
        %{
          recommended: false,
          expected_reduction: overhead_reduction,
          reasoning: "Large messages have relatively low overhead"
        }

      message_count >= 10 and avg_message_size < 1000 ->
        %{
          recommended: true,
          expected_reduction: overhead_reduction,
          reasoning: "Many small messages will benefit significantly from batching"
        }

      overhead_reduction > 0.2 ->
        %{
          recommended: true,
          expected_reduction: overhead_reduction,
          reasoning:
            "Significant overhead reduction expected (#{Float.round(overhead_reduction * 100, 1)}%)"
        }

      true ->
        %{
          recommended: false,
          expected_reduction: overhead_reduction,
          reasoning: "Marginal benefit expected"
        }
    end
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    connection = Keyword.fetch!(opts, :connection)
    max_batch_size = Keyword.get(opts, :max_batch_size, @default_max_batch_size)
    max_batch_bytes = Keyword.get(opts, :max_batch_bytes, @default_max_batch_bytes)
    batch_timeout = Keyword.get(opts, :batch_timeout, @default_batch_timeout)

    # Monitor the connection
    Process.monitor(connection)

    state = %__MODULE__{
      connection: connection,
      max_batch_size: max_batch_size,
      max_batch_bytes: max_batch_bytes,
      batch_timeout: batch_timeout,
      messages: [],
      current_bytes: 0
    }

    Logger.debug("Batch manager started for connection #{inspect(connection)}")

    {:ok, state}
  end

  @impl true
  def handle_call({:send_message, message}, _from, state) do
    message_size = estimate_message_size(message)
    new_messages = [message | state.messages]
    new_bytes = state.current_bytes + message_size

    # Check if we should flush the batch
    should_flush =
      length(new_messages) >= state.max_batch_size or
        new_bytes >= state.max_batch_bytes

    if should_flush do
      # Send the batch (reverse to maintain original order)
      case send_batch(Enum.reverse(new_messages), state) do
        :ok ->
          # Update stats
          new_stats = update_stats_for_batch(state.stats, new_messages, :size_flush)
          new_state = %{state | messages: [], current_bytes: 0, stats: new_stats}
          cancel_timer(state.timer_ref)
          {:reply, :ok, new_state}

        {:error, reason} ->
          {:reply, {:error, reason}, state}
      end
    else
      # Add to batch and set/reset timer
      new_timer = schedule_flush_timer(state.batch_timeout)
      cancel_timer(state.timer_ref)

      new_state = %{
        state
        | messages: new_messages,
          current_bytes: new_bytes,
          timer_ref: new_timer
      }

      {:reply, :ok, new_state}
    end
  end

  def handle_call({:send_priority_message, message}, _from, state) do
    # Send immediately without batching
    case send_single_message(message, state.connection) do
      :ok -> {:reply, :ok, state}
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call(:flush, _from, state) do
    if length(state.messages) > 0 do
      # Reverse messages to maintain original order
      case send_batch(Enum.reverse(state.messages), state) do
        :ok ->
          new_stats = update_stats_for_batch(state.stats, state.messages, :manual_flush)
          cancel_timer(state.timer_ref)
          new_state = %{state | messages: [], current_bytes: 0, stats: new_stats, timer_ref: nil}
          {:reply, :ok, new_state}

        {:error, reason} ->
          {:reply, {:error, reason}, state}
      end
    else
      {:reply, :ok, state}
    end
  end

  def handle_call(:get_stats, _from, state) do
    current_stats =
      Map.merge(state.stats, %{
        pending_messages: length(state.messages),
        pending_bytes: state.current_bytes
      })

    {:reply, current_stats, state}
  end

  def handle_call({:configure, opts}, _from, state) do
    new_state = %{
      state
      | max_batch_size: Keyword.get(opts, :max_batch_size, state.max_batch_size),
        max_batch_bytes: Keyword.get(opts, :max_batch_bytes, state.max_batch_bytes),
        batch_timeout: Keyword.get(opts, :batch_timeout, state.batch_timeout)
    }

    {:reply, :ok, new_state}
  end

  @impl true
  def handle_info(:flush_timeout, state) do
    if length(state.messages) > 0 do
      # Reverse messages to maintain original order
      case send_batch(Enum.reverse(state.messages), state) do
        :ok ->
          new_stats = update_stats_for_batch(state.stats, state.messages, :timeout_flush)
          new_state = %{state | messages: [], current_bytes: 0, stats: new_stats, timer_ref: nil}
          {:noreply, new_state}

        {:error, reason} ->
          Logger.warning("Failed to send batch on timeout: #{inspect(reason)}")
          {:noreply, %{state | timer_ref: nil}}
      end
    else
      {:noreply, %{state | timer_ref: nil}}
    end
  end

  def handle_info({:DOWN, _ref, :process, pid, reason}, state) do
    if pid == state.connection do
      Logger.info("Connection died: #{inspect(reason)}")
      {:stop, :connection_down, state}
    else
      {:noreply, state}
    end
  end

  def handle_info(msg, state) do
    Logger.debug("Unexpected message in Batch: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, state) do
    # Try to flush remaining messages
    if length(state.messages) > 0 do
      send_batch(state.messages, state)
    end

    cancel_timer(state.timer_ref)
    :ok
  end

  # Private helper functions

  defp send_batch(messages, state) do
    # Messages are already in correct order from caller
    # Create batch frame
    batch_payload = %{
      "type" => "batch",
      "messages" => messages,
      "count" => length(messages)
    }

    # Apply zero-copy optimization to the batch
    case ZeroCopy.prepare_message(batch_payload) do
      {:ok, prepared_payload} ->
        case Frame.encode(prepared_payload, 1, :batch) do
          {:ok, frame_data} ->
            case GenServer.call(state.connection, {:send_raw_frame, frame_data}) do
              :ok ->
                Logger.debug("Sent batch with #{length(messages)} messages")
                :ok

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

  defp send_single_message(message, connection) do
    case ZeroCopy.prepare_message(message) do
      {:ok, prepared_message} ->
        case Frame.encode(prepared_message, 1, :rpc_request) do
          {:ok, frame_data} ->
            GenServer.call(connection, {:send_raw_frame, frame_data})

          {:error, reason} ->
            {:error, {:frame_encode_error, reason}}
        end

      {:error, reason} ->
        {:error, {:zero_copy_error, reason}}
    end
  end

  defp estimate_message_size(message) when is_map(message) do
    case Jason.encode(message) do
      {:ok, json} -> byte_size(json)
      # Fallback estimate
      _ -> 100
    end
  end

  defp estimate_message_size(_), do: 100

  defp schedule_flush_timer(timeout_ms) do
    Process.send_after(self(), :flush_timeout, timeout_ms)
  end

  defp cancel_timer(nil), do: :ok

  defp cancel_timer(timer_ref) when is_reference(timer_ref) do
    Process.cancel_timer(timer_ref)
    :ok
  end

  defp update_stats_for_batch(stats, messages, flush_type) do
    message_count = length(messages)

    batch_bytes =
      Enum.reduce(messages, 0, fn msg, acc ->
        acc + estimate_message_size(msg)
      end)

    new_batches_sent = stats.batches_sent + 1
    new_messages_batched = stats.messages_batched + message_count
    new_bytes_batched = stats.bytes_batched + batch_bytes
    new_avg_batch_size = new_messages_batched / new_batches_sent

    flush_increment =
      case flush_type do
        :timeout_flush -> %{timeout_flushes: stats.timeout_flushes + 1}
        :size_flush -> %{size_flushes: stats.size_flushes + 1}
        :manual_flush -> %{}
      end

    Map.merge(
      %{
        stats
        | batches_sent: new_batches_sent,
          messages_batched: new_messages_batched,
          bytes_batched: new_bytes_batched,
          average_batch_size: new_avg_batch_size
      },
      flush_increment
    )
  end
end
