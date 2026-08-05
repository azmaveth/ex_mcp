defmodule ExMCP.Client.ModernHTTPSubscriptionTest do
  use ExUnit.Case, async: false

  alias ExMCP.Client
  alias ExMCP.Client.Subscription
  alias ExMCP.HttpPlug
  alias ExMCP.Server.Subscriptions

  defmodule Handler do
    use ExMCP.Server.Handler

    @impl true
    def init(_opts), do: {:ok, %{}}
  end

  setup do
    registry = start_supervised!({Subscriptions, name: nil})
    port = free_port()
    ranch_ref = {:modern_http_subscription_test, System.unique_integer([:positive])}

    {:ok, _pid} =
      Plug.Cowboy.http(
        HttpPlug,
        [
          handler: Handler,
          path: "/mcp",
          protocol_mode: :modern_only,
          subscription_registry: registry,
          subscription_keepalive_interval_ms: 25,
          subscription_max_lifetime_ms: 5_000,
          allowed_origins: ["http://127.0.0.1:#{port}"]
        ],
        ip: {127, 0, 0, 1},
        port: port,
        ref: ranch_ref
      )

    on_exit(fn ->
      try do
        Plug.Cowboy.shutdown(ranch_ref)
      catch
        :exit, _reason -> :ok
      end
    end)

    {:ok, client} =
      Client.start_link(
        transport: :http,
        url: "http://127.0.0.1:#{port}/mcp",
        protocol_mode: :modern_only,
        protocol_version: "2026-07-28",
        use_sse: false,
        health_check_interval: nil,
        stream_idle_timeout: 1_000
      )

    on_exit(fn ->
      if Process.alive?(client), do: Client.disconnect(client)
    end)

    {:ok, registry: registry, client: client}
  end

  test "opens, receives, and cancels a literal modern HTTP subscription", %{
    registry: registry,
    client: client
  } do
    assert {:ok, subscription} =
             Client.listen(client, %{"toolsListChanged" => true}, timeout: 2_000)

    assert %Subscription.Ref{} = subscription
    assert subscription.acknowledged_filter == %{"toolsListChanged" => true}

    assert [%{subscription_id: subscription_id}] = Subscriptions.entries(registry: registry)
    assert subscription_id == subscription.request_id

    assert %{enqueued: 1} =
             Subscriptions.publish(
               "notifications/tools/list_changed",
               %{},
               registry: registry
             )

    assert_receive {:ex_mcp_subscription, ^subscription, "notifications/tools/list_changed",
                    params},
                   1_000

    assert params["_meta"]["io.modelcontextprotocol/subscriptionId"] ==
             subscription.request_id

    assert :ok = Subscription.cancel(subscription, "test complete")
    assert_eventually(fn -> Subscriptions.entries(registry: registry) == [] end)
  end

  test "reopens and resynchronizes after an abrupt HTTP response-stream close", %{
    registry: registry,
    client: client
  } do
    assert {:ok, initial} =
             Client.listen(client, %{"toolsListChanged" => true}, timeout: 2_000)

    assert [entry] = Subscriptions.entries(registry: registry)

    assert :ok =
             Subscriptions.cancel(
               entry.transport_ref,
               entry.subscription_id,
               registry: registry
             )

    assert_receive {:ex_mcp_subscription_resync, subscription_pid, :started}, 1_000
    assert subscription_pid == initial.pid

    assert_eventually(fn ->
      case Subscriptions.entries(registry: registry) do
        [%{subscription_id: new_id}] -> new_id != initial.request_id
        _other -> false
      end
    end)

    assert_receive {:ex_mcp_subscription_resync, current, {:complete, snapshot}}, 2_000
    assert current.pid == initial.pid
    assert {:ok, %{"resultType" => "complete"}} = snapshot["tools"]

    assert %{enqueued: 1} =
             Subscriptions.publish(
               "notifications/tools/list_changed",
               %{},
               registry: registry
             )

    assert_receive {:ex_mcp_subscription, delivered_on, "notifications/tools/list_changed",
                    _params},
                   1_000

    assert delivered_on.request_id == current.request_id
    assert :ok = Subscription.cancel(current)
    assert_eventually(fn -> Subscriptions.entries(registry: registry) == [] end)
  end

  defp free_port do
    {:ok, socket} = :gen_tcp.listen(0, [:binary, ip: {127, 0, 0, 1}])
    {:ok, port} = :inet.port(socket)
    :ok = :gen_tcp.close(socket)
    port
  end

  defp assert_eventually(fun, attempts \\ 50)

  defp assert_eventually(fun, attempts) when attempts > 0 do
    if fun.() do
      assert true
    else
      Process.sleep(10)
      assert_eventually(fun, attempts - 1)
    end
  end

  defp assert_eventually(_fun, 0), do: flunk("condition did not become true")
end
