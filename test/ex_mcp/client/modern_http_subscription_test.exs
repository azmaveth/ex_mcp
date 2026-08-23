defmodule ExMCP.Client.ModernHTTPSubscriptionTest do
  use ExUnit.Case, async: false

  alias ExMCP.Client
  alias ExMCP.Client.Subscription
  alias ExMCP.HttpPlug
  alias ExMCP.Server.Subscriptions
  alias ExMCP.Tasks
  alias ExMCP.Tasks.Extension
  alias ExMCP.Test.HTTPAdapter

  defmodule Handler do
    use ExMCP.Server.Handler, tasks: :store

    @impl true
    def init(_opts), do: {:ok, %{}}
  end

  for adapter <- HTTPAdapter.adapters() do
    describe "with #{adapter}" do
      setup do
        registry = start_supervised!({Subscriptions, name: nil})
        port = HTTPAdapter.free_port()

        {:ok, _server} =
          HTTPAdapter.start_plug(
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
            adapter: unquote(adapter),
            port: port,
            ip: {127, 0, 0, 1}
          )

        {:ok, client} =
          Client.start_link(
            transport: :http,
            url: "http://127.0.0.1:#{port}/mcp",
            protocol_mode: :modern_only,
            protocol_version: "2026-07-28",
            capabilities: Extension.put_capability(%{}),
            use_sse: false,
            health_check_interval: nil,
            stream_idle_timeout: 1_000
          )

        on_exit(fn ->
          try do
            if Process.alive?(client), do: Client.disconnect(client)
          catch
            :exit, _reason -> :ok
          end
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

      test "delivers owner-authorized task transitions and rejects malformed task events", %{
        registry: registry,
        client: client
      } do
        owner = %{principal_id: nil, tenant_id: nil, audience: "/mcp"}

        assert {:ok, created} =
                 Tasks.create("deploy", %{}, owner: owner, notify: false)

        assert {:ok, subscription} =
                 Client.listen(client, %{"taskIds" => [created["taskId"]]}, timeout: 2_000)

        assert subscription.acknowledged_filter == %{"taskIds" => [created["taskId"]]}

        assert {:ok, _completed} =
                 Tasks.complete(
                   created["taskId"],
                   %{"content" => [%{"type" => "text", "text" => "done"}]},
                   owner: owner,
                   subscription_registry: registry
                 )

        assert_receive {:ex_mcp_subscription, ^subscription, "notifications/tasks", params},
                       1_000

        assert params["taskId"] == created["taskId"]
        assert params["status"] == "completed"
        assert params["result"] == %{"content" => [%{"type" => "text", "text" => "done"}]}

        assert %{enqueued: 1} =
                 Subscriptions.publish(
                   "notifications/tasks",
                   %{"taskId" => created["taskId"], "status" => "completed"},
                   registry: registry
                 )

        refute_receive {:ex_mcp_subscription, ^subscription, "notifications/tasks", _params}, 100
        assert :ok = Subscription.cancel(subscription)
      end
    end
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
