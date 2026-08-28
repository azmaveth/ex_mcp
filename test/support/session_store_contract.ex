defmodule ExMCP.SessionStoreContract do
  @moduledoc false

  # Test-only facade over the current SessionManager call surface.
  # This is the accepted 1.x event-store contract from docs/STORE_ADAPTER.md.
  # It is not a published Hex behaviour and must not appear in lib/.

  @doc false
  def start_isolated!(opts \\ []) do
    {child_id, opts} = Keyword.pop(opts, :id)

    name =
      Keyword.get_lazy(opts, :name, fn ->
        :"session_store_contract_#{System.unique_integer([:positive])}"
      end)

    opts =
      opts
      |> Keyword.put(:name, name)
      |> Keyword.put_new(:cleanup_interval_ms, 60_000)

    child_id = child_id || {ExMCP.SessionManager, name}

    pid =
      ExUnit.Callbacks.start_supervised!(
        {ExMCP.SessionManager, opts},
        id: child_id
      )

    %{name: name, pid: pid, id: child_id}
  end

  @doc false
  def create_session(store, metadata \\ %{}) do
    GenServer.call(store, {:create_session, metadata})
  end

  @doc false
  def get_session(store, session_id) do
    GenServer.call(store, {:get_session, session_id})
  end

  @doc false
  def terminate_session(store, session_id) do
    GenServer.call(store, {:terminate_session, session_id})
  end

  @doc false
  def append_event(store, session_id, type, data) do
    GenServer.call(store, {:append_event, session_id, type, data})
  end

  @doc false
  def store_event(store, session_id, event_data) do
    GenServer.call(store, {:store_event, session_id, event_data})
  end

  @doc false
  def replay_events_after(store, session_id, cursor \\ nil) do
    GenServer.call(store, {:replay_events_after, session_id, cursor})
  end

  @doc false
  def claim_request_id(store, session_id, request_id) do
    GenServer.call(store, {:claim_request_id, session_id, request_id})
  end

  @doc false
  def get_stats(store) do
    GenServer.call(store, :get_stats)
  end
end
