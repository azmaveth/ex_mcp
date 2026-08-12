defmodule ExMCP.SubscriptionRegistryTest do
  use ExUnit.Case, async: false

  import Plug.Conn
  import Plug.Test

  alias ExMCP.{HttpPlug, SessionManager, SubscriptionRegistry}
  alias ExMCP.HttpPlug.SessionRegistry

  defmodule SubscriptionHandler do
    use ExMCP.Server.Handler

    @impl true
    def init(opts), do: {:ok, Map.new(opts)}

    @impl true
    def handle_subscribe_resource(_uri, state), do: {:ok, %{}, state}

    @impl true
    def handle_unsubscribe_resource(_uri, state), do: {:ok, %{}, state}
  end

  defmodule DeliveryHandler do
    use GenServer

    def start_link(owner), do: GenServer.start_link(__MODULE__, owner)

    @impl true
    def init(owner), do: {:ok, owner}

    @impl true
    def handle_call(:request_send, _from, owner), do: {:reply, :ok, owner}

    @impl true
    def handle_cast({:send_event, type, data, opts}, owner) do
      send(owner, {:sse_event, self(), type, data, opts})
      {:noreply, owner}
    end
  end

  test "HTTP sessions subscribe and unsubscribe independently" do
    uri = "test://shared"
    session_a = unique_session("a")
    session_b = unique_session("b")

    on_exit(fn ->
      SessionManager.terminate_session(session_a)
      SessionManager.terminate_session(session_b)
    end)

    assert post_resource_request(session_a, "resources/subscribe", uri).status == 200
    assert post_resource_request(session_b, "resources/subscribe", uri).status == 200
    assert SubscriptionRegistry.sessions(uri) == Enum.sort([session_a, session_b])

    assert post_resource_request(session_a, "resources/unsubscribe", uri).status == 200
    assert SubscriptionRegistry.sessions(uri) == [session_b]

    assert {:ok, %{id: ^session_a}} = SessionManager.get_session(session_a)
    assert {:ok, %{id: ^session_b}} = SessionManager.get_session(session_b)
  end

  test "terminating and expiring sessions remove every subscription" do
    manager_name = {:global, {:subscription_session_manager, make_ref()}}

    manager =
      start_supervised!(
        {SessionManager, name: manager_name, session_ttl_seconds: 0, cleanup_interval_ms: 60_000}
      )

    terminated = unique_session("terminated")
    expired = unique_session("expired")

    :ok = GenServer.call(manager, {:ensure_session, terminated, %{transport: :http}})
    :ok = SubscriptionRegistry.subscribe(terminated, "test://one")
    :ok = SubscriptionRegistry.subscribe(terminated, "test://two")

    :ok = GenServer.call(manager, {:terminate_session, terminated})
    assert SubscriptionRegistry.subscriptions(terminated) == []

    :ok = GenServer.call(manager, {:ensure_session, expired, %{transport: :http}})
    :ok = SubscriptionRegistry.subscribe(expired, "test://one")

    send(manager, :cleanup_expired_sessions)
    assert {:ok, %{status: :terminated}} = GenServer.call(manager, {:get_session, expired})
    assert SubscriptionRegistry.subscriptions(expired) == []
  end

  test "resource broadcasts deliver independently to every connected subscriber" do
    uri = "test://broadcast"
    sessions = Enum.map(["a", "b", "offline"], &unique_session/1)

    handlers =
      for session_id <- Enum.take(sessions, 2) do
        child = Supervisor.child_spec({DeliveryHandler, self()}, id: make_ref())
        handler = start_supervised!(child)
        :ok = SessionRegistry.register(session_id, handler)
        {session_id, handler}
      end

    on_exit(fn ->
      Enum.each(sessions, &SessionManager.terminate_session/1)
      Enum.each(sessions, &SessionRegistry.unregister/1)
    end)

    Enum.each(sessions, fn session_id ->
      assert :ok = SessionManager.ensure_session(session_id, %{transport: :sse})
      assert :ok = SubscriptionRegistry.subscribe(session_id, uri)
    end)

    assert %{subscribers: 3, delivered: 2} = ExMCP.Server.notify_resource_update(uri)

    notification = %{
      "jsonrpc" => "2.0",
      "method" => "notifications/resources/updated",
      "params" => %{"uri" => uri}
    }

    Enum.each(handlers, fn {session_id, handler} ->
      assert_receive {:sse_event, ^handler, "message", ^notification,
                      [event_id: event_id, persist: false]}

      assert [%{id: ^event_id, data: ^notification}] =
               SessionManager.replay_events_after(session_id, nil)
    end)

    offline_session = List.last(sessions)

    assert [%{data: ^notification}] =
             SessionManager.replay_events_after(offline_session, nil)
  end

  defp post_resource_request(session_id, method, uri) do
    request = %{
      "jsonrpc" => "2.0",
      "id" => System.unique_integer([:positive]),
      "method" => method,
      "params" => %{"uri" => uri}
    }

    conn(:post, "/", Jason.encode!(request))
    |> put_req_header("content-type", "application/json")
    |> put_req_header("mcp-session-id", session_id)
    |> HttpPlug.call(HttpPlug.init(handler: SubscriptionHandler, sse_enabled: false))
  end

  defp unique_session(prefix) do
    "#{prefix}-#{System.unique_integer([:positive, :monotonic])}"
  end
end
