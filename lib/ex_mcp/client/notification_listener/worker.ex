defmodule ExMCP.Client.NotificationListener.Worker do
  @moduledoc false

  # Owns the legacy `resources/subscribe` bookkeeping for one client.
  #
  # Every listener that names a resource URI acquires it here, and every wire
  # operation for those URIs runs in this process, one at a time. That gives
  # two guarantees the listener API relies on: a URI is never subscribed and
  # unsubscribed concurrently, and a listener releases exactly the URIs it
  # acquired, so a failed or rolled-back registration can never take away a
  # subscription another listener holds. Callers of
  # `ExMCP.Client.subscribe_notifications/3` wait on this process, never on the
  # client, so the client loop stays free to serve the requests made here.
  #
  # The worker is started by the client on the first registration, monitors
  # the client, and stops when the client stops.

  use GenServer

  alias ExMCP.Internal.RequestParams

  @type uri :: String.t()
  @type listener_id :: reference()

  @spec start(pid()) :: GenServer.on_start()
  def start(client) when is_pid(client), do: GenServer.start(__MODULE__, client)

  @doc "Acquires every URI for a listener; all or nothing."
  @spec acquire(pid(), listener_id(), [uri()], timeout()) :: :ok | {:error, uri(), term()}
  def acquire(_worker, _listener_id, [], _timeout), do: :ok

  def acquire(worker, listener_id, uris, timeout),
    do: GenServer.call(worker, {:acquire, listener_id, uris, timeout}, :infinity)

  @doc "Releases whatever the listener acquired, waiting for any unsubscribe."
  @spec release(pid(), listener_id(), timeout()) :: :ok
  def release(worker, listener_id, timeout),
    do: GenServer.call(worker, {:release, listener_id, timeout}, :infinity)

  @doc "Releases without waiting; used from the client loop."
  @spec release_async(pid(), listener_id(), timeout()) :: :ok
  def release_async(worker, listener_id, timeout),
    do: GenServer.cast(worker, {:release, listener_id, timeout})

  @doc """
  Re-subscribes every held URI after a reconnect and reports the results to
  the client as `{:notification_listeners_resubscribed, results, listener_ids, generation}`.
  """
  @spec resubscribe(pid(), [listener_id()], non_neg_integer(), timeout()) :: :ok
  def resubscribe(worker, listener_ids, generation, timeout),
    do: GenServer.cast(worker, {:resubscribe, listener_ids, generation, timeout})

  @doc "Forgets every acquisition; the server side is gone with the session."
  @spec reset(pid()) :: :ok
  def reset(worker), do: GenServer.cast(worker, :reset)

  @impl true
  def init(client) do
    Process.monitor(client)
    {:ok, %{client: client, counts: %{}, held: %{}}}
  end

  @impl true
  def handle_call({:acquire, listener_id, uris, timeout}, _from, state) do
    uris = Enum.uniq(uris)

    case do_acquire(state, uris, timeout, []) do
      {:ok, state} ->
        {:reply, :ok, %{state | held: Map.put(state.held, listener_id, uris)}}

      {:error, uri, reason, state, acquired} ->
        {:reply, {:error, uri, reason}, release_uris(state, acquired, timeout)}
    end
  end

  def handle_call({:release, listener_id, timeout}, _from, state) do
    {:reply, :ok, release_listener(state, listener_id, timeout)}
  end

  @impl true
  def handle_cast({:release, listener_id, timeout}, state) do
    {:noreply, release_listener(state, listener_id, timeout)}
  end

  def handle_cast({:resubscribe, listener_ids, generation, timeout}, state) do
    results = Map.new(Map.keys(state.counts), &{&1, subscribe(state.client, &1, timeout)})

    GenServer.cast(
      state.client,
      {:notification_listeners_resubscribed, results, listener_ids, generation}
    )

    {:noreply, state}
  end

  def handle_cast(:reset, state), do: {:noreply, %{state | counts: %{}, held: %{}}}

  @impl true
  def handle_info({:DOWN, _ref, :process, client, _reason}, %{client: client} = state),
    do: {:stop, :normal, state}

  def handle_info(_message, state), do: {:noreply, state}

  defp do_acquire(state, [], _timeout, _acquired), do: {:ok, state}

  defp do_acquire(state, [uri | rest], timeout, acquired) do
    case Map.get(state.counts, uri, 0) do
      0 ->
        case subscribe(state.client, uri, timeout) do
          :ok -> do_acquire(put_count(state, uri, 1), rest, timeout, [uri | acquired])
          {:error, reason} -> {:error, uri, reason, state, acquired}
        end

      count ->
        do_acquire(put_count(state, uri, count + 1), rest, timeout, [uri | acquired])
    end
  end

  defp release_listener(state, listener_id, timeout) do
    case Map.pop(state.held, listener_id) do
      {nil, _held} -> state
      {uris, held} -> release_uris(%{state | held: held}, uris, timeout)
    end
  end

  defp release_uris(state, uris, timeout) do
    Enum.reduce(uris, state, fn uri, acc ->
      case Map.get(acc.counts, uri, 0) do
        0 ->
          acc

        1 ->
          _ = request(acc.client, "resources/unsubscribe", uri, timeout)
          %{acc | counts: Map.delete(acc.counts, uri)}

        count ->
          put_count(acc, uri, count - 1)
      end
    end)
  end

  defp put_count(state, uri, count), do: %{state | counts: Map.put(state.counts, uri, count)}

  defp subscribe(client, uri, timeout) do
    case request(client, "resources/subscribe", uri, timeout) do
      {:ok, _result} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  defp request(client, method, uri, timeout) do
    ExMCP.Client.make_request(
      client,
      method,
      RequestParams.uri(uri),
      [timeout: timeout, format: :map],
      timeout
    )
  catch
    :exit, reason -> {:error, {:exit, reason}}
  end
end
