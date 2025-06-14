defmodule ExMCP.Transport.Beam.ZeroCopy do
  @moduledoc """
  Zero-copy optimization for large payload transfers in BEAM transport.

  This module implements reference-based message passing for payloads larger
  than the specified threshold (default 64KB), avoiding expensive memory copying
  operations that can impact performance and memory usage.

  ## Architecture

  Instead of copying large payloads directly in messages, this system:
  1. Stores large payloads in ETS tables with unique references
  2. Sends only the reference in the actual message
  3. Receiver fetches the payload using the reference
  4. Automatic cleanup when references expire or processes die

  ## Benefits

  - **Memory Efficiency**: Avoids copying large binaries multiple times
  - **Performance**: Reduces serialization/deserialization overhead
  - **Backpressure**: Natural flow control through reference availability
  - **Safety**: Automatic cleanup prevents memory leaks

  ## Usage

      # For senders
      {:ok, ref} = ZeroCopy.store_payload(large_binary)
      message = %{data: {:zero_copy_ref, ref}, other: "fields"}
      
      # For receivers
      case message.data do
        {:zero_copy_ref, ref} ->
          {:ok, payload} = ZeroCopy.fetch_payload(ref)
        normal_data ->
          # Handle normally
      end

  ## Configuration

  - `:threshold` - Minimum size in bytes to use zero-copy (default: 65536)
  - `:max_refs` - Maximum number of stored references (default: 1000)
  - `:ttl_ms` - Time-to-live for stored payloads in ms (default: 30000)
  """

  use GenServer
  require Logger

  # 64KB
  @default_threshold 65_536
  @default_max_refs 1_000
  # 30 seconds
  @default_ttl_ms 30_000
  @table_name :zero_copy_payloads

  @type payload_ref :: reference()
  @type payload :: binary()
  @type zero_copy_data :: {:zero_copy_ref, payload_ref()}

  defstruct [
    :table,
    :threshold,
    :max_refs,
    :ttl_ms,
    stats: %{stored_refs: 0, fetched_refs: 0, expired_refs: 0, bytes_saved: 0}
  ]

  @doc """
  Starts the zero-copy manager.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Stores a large payload and returns a reference.

  Only stores payloads that exceed the threshold size.
  Returns the original payload if below threshold.

  ## Examples

      iex> large_data = :crypto.strong_rand_bytes(100_000)
      iex> {:ok, ref} = ZeroCopy.store_payload(large_data)
      iex> is_reference(ref)
      true

      iex> small_data = "small"
      iex> ZeroCopy.store_payload(small_data)
      {:passthrough, "small"}
  """
  @spec store_payload(payload()) ::
          {:ok, payload_ref()} | {:passthrough, payload()} | {:error, term()}
  def store_payload(payload) when is_binary(payload) do
    threshold = get_threshold()

    if byte_size(payload) >= threshold do
      GenServer.call(__MODULE__, {:store_payload, payload})
    else
      {:passthrough, payload}
    end
  end

  def store_payload(_payload) do
    {:error, :invalid_payload_type}
  end

  @doc """
  Fetches a payload using its reference.

  ## Examples

      iex> {:ok, ref} = ZeroCopy.store_payload(large_data)
      iex> {:ok, fetched_data} = ZeroCopy.fetch_payload(ref)
      iex> fetched_data == large_data
      true

      iex> ZeroCopy.fetch_payload(make_ref())
      {:error, :ref_not_found}
  """
  @spec fetch_payload(payload_ref()) :: {:ok, payload()} | {:error, :ref_not_found | :expired}
  def fetch_payload(ref) when is_reference(ref) do
    case :ets.lookup(@table_name, ref) do
      [{^ref, payload, _stored_at, _owner_pid}] ->
        GenServer.cast(__MODULE__, {:payload_fetched, ref, byte_size(payload)})
        {:ok, payload}

      [] ->
        {:error, :ref_not_found}
    end
  end

  def fetch_payload(_) do
    {:error, :invalid_reference}
  end

  @doc """
  Processes a message that may contain zero-copy references.

  Automatically fetches referenced payloads and replaces them in the message.

  ## Examples

      iex> message = %{data: {:zero_copy_ref, ref}, other: "data"}
      iex> {:ok, processed} = ZeroCopy.process_message(message)
      iex> %{data: actual_payload, other: "data"} = processed
  """
  @spec process_message(map()) :: {:ok, map()} | {:error, term()}
  def process_message(message) when is_map(message) do
    try do
      processed = deep_process_message(message)
      {:ok, processed}
    rescue
      error -> {:error, {:processing_failed, error}}
    end
  end

  def process_message(_message) do
    {:error, :invalid_message_type}
  end

  @doc """
  Prepares a message for transmission by storing large payloads as references.

  Recursively processes the message structure to identify and store large binaries.

  ## Examples

      iex> message = %{large_data: large_binary, small: "text"}
      iex> {:ok, prepared} = ZeroCopy.prepare_message(message)
      iex> %{large_data: {:zero_copy_ref, _ref}, small: "text"} = prepared
  """
  @spec prepare_message(map()) :: {:ok, map()} | {:error, term()}
  def prepare_message(message) when is_map(message) do
    try do
      prepared = deep_prepare_message(message)
      {:ok, prepared}
    rescue
      error -> {:error, {:preparation_failed, error}}
    end
  end

  def prepare_message(_message) do
    {:error, :invalid_message_type}
  end

  @doc """
  Manually releases a stored payload reference.

  Normally references are cleaned up automatically, but this allows
  explicit cleanup for long-running processes.
  """
  @spec release_payload(payload_ref()) :: :ok
  def release_payload(ref) when is_reference(ref) do
    GenServer.cast(__MODULE__, {:release_payload, ref})
  end

  @doc """
  Gets current statistics about zero-copy usage.
  """
  @spec get_stats() :: map()
  def get_stats do
    GenServer.call(__MODULE__, :get_stats)
  end

  @doc """
  Gets the current threshold for zero-copy optimization.
  """
  @spec get_threshold() :: non_neg_integer()
  def get_threshold do
    case GenServer.whereis(__MODULE__) do
      nil -> @default_threshold
      _pid -> GenServer.call(__MODULE__, :get_threshold)
    end
  end

  @doc """
  Updates the threshold for zero-copy optimization.
  """
  @spec set_threshold(non_neg_integer()) :: :ok
  def set_threshold(new_threshold) when is_integer(new_threshold) and new_threshold >= 0 do
    GenServer.call(__MODULE__, {:set_threshold, new_threshold})
  end

  @doc """
  Checks if a value is a zero-copy reference.
  """
  @spec zero_copy_ref?(term()) :: boolean()
  def zero_copy_ref?({:zero_copy_ref, ref}) when is_reference(ref), do: true
  def zero_copy_ref?(_), do: false

  # GenServer callbacks

  @impl true
  def init(opts) do
    threshold = Keyword.get(opts, :threshold, @default_threshold)
    max_refs = Keyword.get(opts, :max_refs, @default_max_refs)
    ttl_ms = Keyword.get(opts, :ttl_ms, @default_ttl_ms)

    # Create ETS table for storing payloads
    # Table schema: {ref, payload, stored_at, owner_pid}
    table =
      :ets.new(@table_name, [
        :set,
        :named_table,
        :public,
        {:read_concurrency, true}
      ])

    # Schedule cleanup
    schedule_cleanup(ttl_ms)

    state = %__MODULE__{
      table: table,
      threshold: threshold,
      max_refs: max_refs,
      ttl_ms: ttl_ms
    }

    Logger.info(
      "ZeroCopy manager started with threshold=#{threshold}B, max_refs=#{max_refs}, ttl=#{ttl_ms}ms"
    )

    {:ok, state}
  end

  @impl true
  def handle_call({:store_payload, payload}, {owner_pid, _}, state) do
    ref = make_ref()
    now = System.monotonic_time(:millisecond)

    # Check if we're at capacity
    if :ets.info(state.table, :size) >= state.max_refs do
      # Clean up oldest entries
      cleanup_oldest_entries(state.table, div(state.max_refs, 10))
    end

    # Store the payload
    :ets.insert(state.table, {ref, payload, now, owner_pid})

    # Monitor the owner process for cleanup
    Process.monitor(owner_pid)

    # Update stats
    bytes_saved = byte_size(payload)

    updated_stats = %{
      state.stats
      | stored_refs: state.stats.stored_refs + 1,
        bytes_saved: state.stats.bytes_saved + bytes_saved
    }

    Logger.debug("Stored zero-copy payload: #{byte_size(payload)} bytes, ref=#{inspect(ref)}")

    {:reply, {:ok, ref}, %{state | stats: updated_stats}}
  end

  def handle_call(:get_stats, _from, state) do
    current_refs = :ets.info(state.table, :size)
    stats = %{state.stats | stored_refs: current_refs}
    {:reply, stats, %{state | stats: stats}}
  end

  def handle_call(:get_threshold, _from, state) do
    {:reply, state.threshold, state}
  end

  def handle_call({:set_threshold, new_threshold}, _from, state) do
    Logger.info("ZeroCopy threshold updated: #{state.threshold} -> #{new_threshold}")
    {:reply, :ok, %{state | threshold: new_threshold}}
  end

  @impl true
  def handle_cast({:payload_fetched, ref, byte_size}, state) do
    # Update fetch stats
    updated_stats = %{
      state.stats
      | fetched_refs: state.stats.fetched_refs + 1
    }

    Logger.debug("Fetched zero-copy payload: #{byte_size} bytes, ref=#{inspect(ref)}")
    {:noreply, %{state | stats: updated_stats}}
  end

  def handle_cast({:release_payload, ref}, state) do
    case :ets.lookup(state.table, ref) do
      [{^ref, _payload, _stored_at, _owner_pid}] ->
        :ets.delete(state.table, ref)
        Logger.debug("Released zero-copy payload ref=#{inspect(ref)}")

      [] ->
        # Already cleaned up
        :ok
    end

    {:noreply, state}
  end

  @impl true
  def handle_info(:cleanup_expired, state) do
    expired_count = cleanup_expired_entries(state.table, state.ttl_ms)

    if expired_count > 0 do
      Logger.debug("Cleaned up #{expired_count} expired zero-copy references")

      updated_stats = %{
        state.stats
        | expired_refs: state.stats.expired_refs + expired_count
      }

      schedule_cleanup(state.ttl_ms)
      {:noreply, %{state | stats: updated_stats}}
    else
      schedule_cleanup(state.ttl_ms)
      {:noreply, state}
    end
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    # Clean up entries owned by the dead process
    cleanup_count = cleanup_entries_by_owner(state.table, pid)

    if cleanup_count > 0 do
      Logger.debug(
        "Cleaned up #{cleanup_count} zero-copy references from dead process #{inspect(pid)}"
      )
    end

    {:noreply, state}
  end

  def handle_info(msg, state) do
    Logger.debug("Unexpected message in ZeroCopy: #{inspect(msg)}")
    {:noreply, state}
  end

  # Private helper functions

  defp deep_process_message({:zero_copy_ref, ref}) do
    case fetch_payload(ref) do
      {:ok, payload} -> payload
      # Keep reference if fetch fails
      {:error, _} -> {:zero_copy_ref, ref}
    end
  end

  defp deep_process_message(map) when is_map(map) do
    Map.new(map, fn {key, value} ->
      {key, deep_process_message(value)}
    end)
  end

  defp deep_process_message(list) when is_list(list) do
    Enum.map(list, &deep_process_message/1)
  end

  defp deep_process_message(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.map(&deep_process_message/1)
    |> List.to_tuple()
  end

  defp deep_process_message(value), do: value

  defp deep_prepare_message(binary) when is_binary(binary) do
    case store_payload(binary) do
      {:ok, ref} -> {:zero_copy_ref, ref}
      {:passthrough, payload} -> payload
      {:error, _} -> binary
    end
  end

  defp deep_prepare_message(map) when is_map(map) do
    Map.new(map, fn {key, value} ->
      {key, deep_prepare_message(value)}
    end)
  end

  defp deep_prepare_message(list) when is_list(list) do
    Enum.map(list, &deep_prepare_message/1)
  end

  defp deep_prepare_message(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.map(&deep_prepare_message/1)
    |> List.to_tuple()
  end

  defp deep_prepare_message(value), do: value

  defp cleanup_expired_entries(table, ttl_ms) do
    now = System.monotonic_time(:millisecond)
    cutoff = now - ttl_ms

    # Use ets:select to find expired entries
    expired_refs =
      :ets.select(table, [
        {{:"$1", :"$2", :"$3", :"$4"}, [{:<, :"$3", cutoff}], [:"$1"]}
      ])

    # Delete expired entries
    Enum.each(expired_refs, fn ref ->
      :ets.delete(table, ref)
    end)

    length(expired_refs)
  end

  defp cleanup_oldest_entries(table, count) do
    # Get all entries sorted by timestamp
    all_entries =
      :ets.select(table, [
        {{:"$1", :"$2", :"$3", :"$4"}, [], [{{:"$1", :"$3"}}]}
      ])

    oldest_refs =
      all_entries
      |> Enum.sort_by(fn {_ref, timestamp} -> timestamp end)
      |> Enum.take(count)
      |> Enum.map(fn {ref, _timestamp} -> ref end)

    # Delete oldest entries
    Enum.each(oldest_refs, fn ref ->
      :ets.delete(table, ref)
    end)

    length(oldest_refs)
  end

  defp cleanup_entries_by_owner(table, owner_pid) do
    # Find all entries owned by the specified process
    owned_refs =
      :ets.select(table, [
        {{:"$1", :"$2", :"$3", :"$4"}, [{:==, :"$4", owner_pid}], [:"$1"]}
      ])

    # Delete owned entries
    Enum.each(owned_refs, fn ref ->
      :ets.delete(table, ref)
    end)

    length(owned_refs)
  end

  defp schedule_cleanup(ttl_ms) do
    # Schedule cleanup to run at half the TTL interval
    # At least every second
    cleanup_interval = max(div(ttl_ms, 2), 1000)
    Process.send_after(self(), :cleanup_expired, cleanup_interval)
  end
end
