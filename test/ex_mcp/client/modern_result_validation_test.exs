defmodule ExMCP.Client.ModernResultValidationTest do
  use ExUnit.Case, async: true

  alias ExMCP.Client.RequestHandler

  defmodule SyncTransport do
    def send_message(message, state) do
      request = Jason.decode!(message)
      send(state.owner, {:outbound_request, request})

      response = %{
        "jsonrpc" => "2.0",
        "id" => request["id"],
        "result" => state.result
      }

      {:ok, state, Jason.encode!(response)}
    end
  end

  test "injects modern metadata and accepts a discriminated result" do
    state = modern_state(%{"resultType" => "complete", "tools" => []})

    assert {:reply, {:ok, %{"resultType" => "complete"}}, _state} =
             RequestHandler.handle_request("tools/list", %{}, {self(), make_ref()}, state)

    assert_receive {:outbound_request, request}
    meta = request["params"]["_meta"]
    assert meta["io.modelcontextprotocol/protocolVersion"] == "2026-07-28"
    assert meta["io.modelcontextprotocol/clientCapabilities"] == %{"roots" => %{}}
    assert meta["io.modelcontextprotocol/clientInfo"]["name"] == "test-client"
  end

  test "rejects a modern result without resultType" do
    state = modern_state(%{"tools" => []})

    assert {:reply, {:error, error}, _state} =
             RequestHandler.handle_request("tools/list", %{}, {self(), make_ref()}, state)

    assert error.type == :protocol_error
    assert error.reason == :missing_result_type
  end

  test "keeps legacy missing-resultType behavior" do
    state = %{modern_state(%{"tools" => []}) | protocol_version: "2025-11-25"}

    assert {:reply, {:ok, %{"tools" => []}}, _state} =
             RequestHandler.handle_request("tools/list", %{}, {self(), make_ref()}, state)
  end

  test "uses server/discover for modern health checks" do
    state = modern_state(%{"resultType" => "complete"})

    assert {:ok, nil, _state} = RequestHandler.send_ping(state)
    assert_receive {:outbound_request, %{"method" => "server/discover"}}
  end

  defp modern_state(result) do
    %ExMCP.Client{
      transport_mod: SyncTransport,
      transport_state: %{owner: self(), result: result},
      transport_opts: [capabilities: %{roots: %{}}],
      protocol_version: "2026-07-28",
      client_info: %{"name" => "test-client", "version" => "1"},
      pending_requests: %{},
      pending_batches: %{},
      cancelled_requests: MapSet.new(),
      default_timeout: 1_000
    }
  end
end
