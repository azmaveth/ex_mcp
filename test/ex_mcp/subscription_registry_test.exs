defmodule ExMCP.SubscriptionRegistryTest do
  use ExUnit.Case, async: false

  setup do
    ensure_started(ExMCP.SessionManager)
    ensure_started(ExMCP.SubscriptionRegistry)
    :ok
  end

  test "many client sessions subscribe independently to one resource" do
    uri = "test://shared"
    sessions = [unique_session("a"), unique_session("b")]

    Enum.each(sessions, fn session_id ->
      assert :ok = ExMCP.SubscriptionRegistry.subscribe(session_id, uri)
    end)

    assert ExMCP.SubscriptionRegistry.sessions(uri) == Enum.sort(sessions)
    assert :ok = ExMCP.SubscriptionRegistry.unsubscribe(hd(sessions), uri)
    assert ExMCP.SubscriptionRegistry.sessions(uri) == [List.last(sessions)]
  end

  test "transport-issued HTTP session identity is retained for SSE" do
    session_id = unique_session("http")

    assert :ok = ExMCP.SessionManager.ensure_session(session_id, %{transport: :http})
    assert {:ok, session} = ExMCP.SessionManager.get_session(session_id)
    assert session.id == session_id
    assert session.transport == :http

    assert :ok = ExMCP.SessionManager.ensure_session(session_id, %{transport: :sse})
    assert {:ok, resumed} = ExMCP.SessionManager.get_session(session_id)
    assert resumed.id == session_id
    assert resumed.transport == :sse
  end

  defp unique_session(prefix),
    do: "#{prefix}-#{System.unique_integer([:positive, :monotonic])}"

  defp ensure_started(module) do
    if Process.whereis(module) == nil do
      start_supervised!(module)
    end
  end
end
