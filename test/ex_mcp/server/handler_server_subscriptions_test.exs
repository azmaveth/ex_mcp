defmodule ExMCP.Server.HandlerServerSubscriptionsTest do
  use ExUnit.Case, async: true

  alias ExMCP.Server
  alias ExMCP.Server.{HandlerServer, Subscriptions}

  @subscription_id_key "io.modelcontextprotocol/subscriptionId"

  defmodule Handler do
    use ExMCP.Server.Handler

    @impl true
    def init(_opts), do: {:ok, %{}}
  end

  test "listen remains open while correlated notifications flow over the test transport" do
    registry = start_registry()
    server = start_server(registry)
    connect(server)

    send_request(server, listen_request(71, %{"toolsListChanged" => true}))

    assert_receive {:transport_message, encoded_ack}
    acknowledged = Jason.decode!(encoded_ack)
    assert acknowledged["method"] == "notifications/subscriptions/acknowledged"
    assert acknowledged["params"]["_meta"][@subscription_id_key] == 71
    assert acknowledged["params"]["notifications"] == %{"toolsListChanged" => true}
    refute Map.has_key?(acknowledged, "id")

    assert [%{subscription_id: 71, transport_ref: ^server}] =
             Subscriptions.entries(registry: registry)

    :ok = Server.notify_tools_changed(server)

    assert_receive {:transport_message, encoded_notification}
    notification = Jason.decode!(encoded_notification)
    assert notification["method"] == "notifications/tools/list_changed"
    assert notification["params"]["_meta"][@subscription_id_key] == 71

    :ok = Server.notify_prompts_changed(server)
    refute_receive {:transport_message, _unrequested}

    assert :ok = Subscriptions.close(server, 71, :test_complete, registry: registry)

    assert_receive {:transport_message, encoded_complete}
    completed = Jason.decode!(encoded_complete)
    assert completed["id"] == 71
    assert completed["result"]["resultType"] == "complete"
    assert completed["result"]["_meta"][@subscription_id_key] == 71
  end

  test "stdio-style cancellation removes the long-lived request without a completion response" do
    registry = start_registry()
    server = start_server(registry)
    connect(server)

    send_request(
      server,
      listen_request(72, %{"resourceSubscriptions" => ["test://watched"]})
    )

    assert_receive {:transport_message, _encoded_ack}, 1_000

    cancellation = %{
      "jsonrpc" => "2.0",
      "method" => "notifications/cancelled",
      "params" => %{"requestId" => 72, "reason" => "no longer needed"}
    }

    send_request(server, cancellation)
    assert_eventually(fn -> Subscriptions.entries(registry: registry) == [] end)
    refute_receive {:transport_message, _completion}
  end

  test "invalid filters produce a finite JSON-RPC error instead of opening a stream" do
    registry = start_registry()
    server = start_server(registry)
    connect(server)

    send_request(server, listen_request(73, %{"toolsListChanged" => "yes"}))

    assert_receive {:transport_message, encoded_error}
    error = Jason.decode!(encoded_error)
    assert error["id"] == 73
    assert error["error"]["code"] == -32602
    assert error["error"]["message"] == "Subscription request rejected"
    assert Subscriptions.entries(registry: registry) == []
  end

  test "request-scoped notifications are not stamped as subscription events" do
    registry = start_registry()
    server = start_server(registry)
    connect(server)

    send_request(server, listen_request(74, %{"toolsListChanged" => true}))
    assert_receive {:transport_message, _encoded_ack}

    :ok = Server.notify_progress(server, "request-progress", 1, 2)

    assert_receive {:transport_message, encoded_progress}
    progress = Jason.decode!(encoded_progress)
    assert progress["method"] == "notifications/progress"
    assert progress["params"]["progressToken"] == "request-progress"
    refute get_in(progress, ["params", "_meta", @subscription_id_key])
  end

  defp start_registry do
    child =
      Supervisor.child_spec(
        {Subscriptions, name: nil, max_lifetime_ms: 5_000},
        id: make_ref()
      )

    start_supervised!(child)
  end

  defp start_server(registry) do
    start_supervised!(
      {HandlerServer,
       handler: Handler,
       transport: :test,
       protocol_mode: :modern_only,
       subscription_registry: registry,
       principal_id: "principal-1",
       tenant_id: "tenant-1"}
    )
  end

  defp connect(server), do: send(server, {:test_transport_connect, self()})

  defp send_request(server, request) do
    send(server, {:transport_message, Jason.encode!(request)})
  end

  defp listen_request(id, filter) do
    %{
      "jsonrpc" => "2.0",
      "id" => id,
      "method" => "subscriptions/listen",
      "params" => %{
        "_meta" => %{
          "io.modelcontextprotocol/protocolVersion" => "2026-07-28",
          "io.modelcontextprotocol/clientCapabilities" => %{},
          "io.modelcontextprotocol/clientInfo" => %{
            "name" => "subscription-test",
            "version" => "1"
          }
        },
        "notifications" => filter
      }
    }
  end

  defp assert_eventually(fun, attempts \\ 50)

  defp assert_eventually(fun, attempts) when attempts > 0 do
    if fun.() do
      :ok
    else
      receive do
      after
        10 -> assert_eventually(fun, attempts - 1)
      end
    end
  end

  defp assert_eventually(fun, 0), do: assert(fun.())
end
