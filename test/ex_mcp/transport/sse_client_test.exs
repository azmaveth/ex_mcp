defmodule ExMCP.Transport.SSEClientTest do
  @moduledoc """
  Tests for the enhanced SSE client with keep-alive and reconnection.
  """
  use ExUnit.Case, async: false

  alias ExMCP.Test.HTTPServer
  alias ExMCP.Transport.SSEClient

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

      # Consume the initial "connected" event
      assert_receive {:sse_event, ^client, %{data: _}}, 1000

      # Send a test event from server
      send_sse_event(server, %{data: ~s({"type": "test", "message": "hello"})})

      # Should receive the test event
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

      # Stop server to free the port
      port = server.port
      stop_mock_server(server)

      # Wait a bit for port to be freed
      Process.sleep(100)

      # Restart server on same port
      {:ok, server} = start_mock_sse_server(port: port)

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

      # Collect first few error messages to verify exponential backoff
      assert_receive {:sse_error, ^client, _}, 1000
      t1 = System.monotonic_time(:millisecond)

      assert_receive {:sse_error, ^client, _}, 2000
      t2 = System.monotonic_time(:millisecond)

      assert_receive {:sse_error, ^client, _}, 5000
      t3 = System.monotonic_time(:millisecond)

      # Verify exponential backoff timing
      # First interval should be ~1000ms, second should be ~2000ms
      interval1 = t2 - t1
      interval2 = t3 - t2

      # Allow some tolerance for timing
      assert interval1 >= 900 and interval1 <= 1100,
             "First interval was #{interval1}ms, expected ~1000ms"

      assert interval2 >= 1800 and interval2 <= 2200,
             "Second interval was #{interval2}ms, expected ~2000ms"

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

      # Consume the initial "connected" event
      assert_receive {:sse_event, ^client, %{type: "connected"}}, 1000

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

      # Consume the initial "connected" event
      assert_receive {:sse_event, ^client, %{type: "connected"}}, 1000

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
      last_headers = HTTPServer.get_last_headers(server.pid)
      assert last_headers["authorization"] == "Bearer test-token"

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

  defp start_mock_sse_server(opts \\ []) do
    {:ok, server} = HTTPServer.start_link(opts)
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
    if server[:pid] && Process.alive?(server.pid) do
      # Close all SSE connections first to prevent race conditions
      HTTPServer.close_sse_connections(server.pid)
      # Give connections time to close
      Process.sleep(10)
      # Now stop the server
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

  defp send_raw_sse(server, data) do
    HTTPServer.send_sse_event(server.pid, data)
  end

  defp close_connection(server) do
    HTTPServer.close_sse_connections(server.pid)
  end
end
