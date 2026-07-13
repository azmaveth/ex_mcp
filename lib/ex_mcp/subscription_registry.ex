defmodule ExMCP.SubscriptionRegistry do
  @moduledoc """
  Tracks MCP resource subscriptions independently for every client session.

  A server process is shared, while subscriptions belong to the HTTP/SSE
  session that requested them. This registry is deliberately transport-neutral;
  transports resolve session IDs to their live delivery processes.
  """

  use GenServer

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: Keyword.get(opts, :name, __MODULE__))
  end

  def subscribe(session_id, uri) when is_binary(session_id) and is_binary(uri),
    do: GenServer.call(__MODULE__, {:subscribe, session_id, uri})

  def subscribe(_, _), do: {:error, :session_and_uri_required}

  def unsubscribe(session_id, uri) when is_binary(session_id) and is_binary(uri),
    do: GenServer.call(__MODULE__, {:unsubscribe, session_id, uri})

  def unsubscribe(_, _), do: {:error, :session_and_uri_required}

  def remove_session(session_id) when is_binary(session_id),
    do: GenServer.call(__MODULE__, {:remove_session, session_id})

  def sessions(uri) when is_binary(uri), do: GenServer.call(__MODULE__, {:sessions, uri})
  def subscriptions(session_id), do: GenServer.call(__MODULE__, {:subscriptions, session_id})

  @impl true
  def init(_opts), do: {:ok, %{by_uri: %{}, by_session: %{}}}

  @impl true
  def handle_call({:subscribe, session_id, uri}, _from, state) do
    by_uri = Map.update(state.by_uri, uri, MapSet.new([session_id]), &MapSet.put(&1, session_id))

    by_session =
      Map.update(state.by_session, session_id, MapSet.new([uri]), &MapSet.put(&1, uri))

    {:reply, :ok, %{state | by_uri: by_uri, by_session: by_session}}
  end

  def handle_call({:unsubscribe, session_id, uri}, _from, state) do
    {:reply, :ok, drop_subscription(state, session_id, uri)}
  end

  def handle_call({:remove_session, session_id}, _from, state) do
    state =
      state.by_session
      |> Map.get(session_id, MapSet.new())
      |> Enum.reduce(state, &drop_subscription(&2, session_id, &1))

    {:reply, :ok, state}
  end

  def handle_call({:sessions, uri}, _from, state) do
    {:reply, state.by_uri |> Map.get(uri, MapSet.new()) |> Enum.sort(), state}
  end

  def handle_call({:subscriptions, session_id}, _from, state) do
    {:reply, state.by_session |> Map.get(session_id, MapSet.new()) |> Enum.sort(), state}
  end

  defp drop_subscription(state, session_id, uri) do
    by_uri = drop_member(state.by_uri, uri, session_id)
    by_session = drop_member(state.by_session, session_id, uri)
    %{state | by_uri: by_uri, by_session: by_session}
  end

  defp drop_member(index, key, member) do
    case Map.get(index, key) do
      nil ->
        index

      members ->
        remaining = MapSet.delete(members, member)

        if MapSet.size(remaining) == 0,
          do: Map.delete(index, key),
          else: Map.put(index, key, remaining)
    end
  end
end
