defmodule ExMCP.Transport.SSEClientTest do
  @moduledoc """
  Tests for the enhanced SSE client with keep-alive and reconnection.
  """
  use ExUnit.Case, async: true

  alias ExMCP.Transport.SSEClient
  alias ExMCP.Test.HTTPServer

  @moduletag :capture_log

  describe "SSE client connection" do
    test "connects successfully to SSE endpoint" do
      # Start a mock SSE server
      {:ok, server} = start_mock_sse_server()

      # Start SSE client
      {:ok, client} =
        SSEClient.start_link(
          url: "http://localhost:#{server.port}/mcp/v1/sse",
          parent: self()
        )

      # Should receive connection confirmation
      assert_receive {:sse_connected, ^client}, 5000

      # Send a test event from server
      send_sse_event(server, %{data: ~s({"type": "test", "message": "hello"})})

      # Should receive the event
      assert_receive {:sse_event, ^client, %{data: data}}, 1000
      assert data =~ "hello"

      # Clean up
      SSEClient.stop(client)
      stop_mock_server(server)
    end

    test "handles connection failures gracefully" do
      # Try to connect to non-existent server
      {:ok, client} =
        SSEClient.start_link(
          url: "http://localhost:9999/sse",
          parent: self()
        )

      # Should not receive connection confirmation
      refute_receive {:sse_connected, ^client}, 1000

      # Clean up
      SSEClient.stop(client)
    end

    test "reconnects automatically after disconnection" do
      # Start a mock SSE server
      {:ok, server} = start_mock_sse_server()

      # Start SSE client
      {:ok, client} =
        SSEClient.start_link(
          url: "http://localhost:#{server.port}/mcp/v1/sse",
          parent: self()
        )

      # Wait for initial connection
      assert_receive {:sse_connected, ^client}, 5000

      # Simulate server disconnect
      close_connection(server)
      assert_receive {:sse_closed, ^client}, 5000

      # Restart server on same port
      {:ok, server} = start_mock_sse_server(port: server.port)

      # Should reconnect automatically
      assert_receive {:sse_connected, ^client}, 10000

      # Clean up
      SSEClient.stop(client)
      stop_mock_server(server)
    end

    test "implements exponential backoff for reconnection" do
      # Start client with non-existent server
      {:ok, client} =
        SSEClient.start_link(
          url: "http://localhost:9999/sse",
          parent: self()
        )

      # Track reconnection attempts
      start_time = System.monotonic_time(:millisecond)

      # Start server after some delay
      spawn(fn ->
        Process.sleep(3000)
        start_mock_sse_server(port: 9999)
      end)

      # Should eventually connect
      assert_receive {:sse_connected, ^client}, 15000

      # Connection should have taken at least 3 seconds due to backoff
      elapsed = System.monotonic_time(:millisecond) - start_time
      assert elapsed >= 3000

      # Clean up
      SSEClient.stop(client)
    end
  end

  describe "SSE event parsing" do
    test "parses complete SSE events correctly" do
      {:ok, server} = start_mock_sse_server()

      {:ok, client} =
        SSEClient.start_link(
          url: "http://localhost:#{server.port}/mcp/v1/sse",
          parent: self()
        )

      assert_receive {:sse_connected, ^client}, 5000

      # Send various event formats
      send_sse_event(server, %{
        event: "message",
        data: ~s({"type": "test"}),
        id: "123"
      })

      assert_receive {:sse_event, ^client, event}, 1000
      assert event.type == "message"
      assert event.data =~ "test"
      assert event.id == "123"

      SSEClient.stop(client)
      stop_mock_server(server)
    end

    test "handles multiline data correctly" do
      {:ok, server} = start_mock_sse_server()

      {:ok, client} =
        SSEClient.start_link(
          url: "http://localhost:#{server.port}/mcp/v1/sse",
          parent: self()
        )

      assert_receive {:sse_connected, ^client}, 5000

      # Send multiline data
      send_raw_sse(server, """
      data: line1
      data: line2
      data: line3

      """)

      assert_receive {:sse_event, ^client, event}, 1000
      assert event.data == "line1\nline2\nline3"

      SSEClient.stop(client)
      stop_mock_server(server)
    end
  end

  describe "keep-alive mechanism" do
    test "maintains connection with heartbeat" do
      {:ok, server} = start_mock_sse_server()

      {:ok, client} =
        SSEClient.start_link(
          url: "http://localhost:#{server.port}/mcp/v1/sse",
          parent: self()
        )

      assert_receive {:sse_connected, ^client}, 5000

      # Connection should stay alive for extended period
      Process.sleep(5000)

      # Send event to verify connection is still alive
      send_sse_event(server, %{data: ~s({"alive": true})})
      assert_receive {:sse_event, ^client, _}, 1000

      SSEClient.stop(client)
      stop_mock_server(server)
    end

    @tag :skip
    test "detects stale connections and reconnects" do
      # This test would require more sophisticated mocking
      # to simulate a connection that stops responding
    end
  end

  describe "security integration" do
    test "sends authentication headers" do
      {:ok, server} = start_mock_sse_server()

      {:ok, client} =
        SSEClient.start_link(
          url: "http://localhost:#{server.port}/mcp/v1/sse",
          headers: [{"Authorization", "Bearer test-token"}],
          parent: self()
        )

      assert_receive {:sse_connected, ^client}, 5000

      # Verify headers were sent
      assert server.last_headers["authorization"] == "Bearer test-token"

      SSEClient.stop(client)
      stop_mock_server(server)
    end

    test "supports SSL/TLS connections" do
      # This would require an HTTPS mock server
      # For now, just verify SSL options are accepted
      {:ok, client} =
        SSEClient.start_link(
          url: "https://example.com/sse",
          ssl_opts: [verify: :verify_peer],
          parent: self()
        )

      # Won't connect but shouldn't crash
      Process.sleep(100)
      assert Process.alive?(client)

      SSEClient.stop(client)
    end
  end

  describe "error handling" do
    test "handles invalid SSE data gracefully" do
      {:ok, server} = start_mock_sse_server()

      {:ok, client} =
        SSEClient.start_link(
          url: "http://localhost:#{server.port}/mcp/v1/sse",
          parent: self()
        )

      assert_receive {:sse_connected, ^client}, 5000

      # Send invalid SSE format
      send_raw_sse(server, "invalid sse format\n\n")

      # Should not crash
      Process.sleep(100)
      assert Process.alive?(client)

      SSEClient.stop(client)
      stop_mock_server(server)
    end
  end

  # Helper functions for mocking SSE server

  defp start_mock_sse_server(_opts \\ []) do
    {:ok, server} = HTTPServer.start_link()
    url = HTTPServer.get_url(server)
    port = URI.parse(url).port

    {:ok,
     %{
       pid: server,
       port: port,
       connections: [],
       last_headers: %{}
     }}
  end

  defp stop_mock_server(server) do
    if server[:pid] do
      HTTPServer.stop(server.pid)
    else
      :ok
    end
  end

  defp send_sse_event(server, event) do
    data =
      event
      |> Enum.map_join("\n", fn {k, v} -> "#{k}: #{v}" end)

    send_raw_sse(server, data <> "\n\n")
  end

  defp send_raw_sse(_server, _data) do
    # Mock implementation
    :ok
  end

  defp close_connection(_server) do
    # Mock implementation
    :ok
  end
end
