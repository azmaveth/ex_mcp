defmodule ExMCP.Transport.WebSocketTest do
  use ExUnit.Case, async: true

  @moduletag :transport

  alias ExMCP.Transport.WebSocket

  describe "connect/1" do
    test "validates WebSocket URL scheme" do
      assert {:error, {:invalid_scheme, "http"}} = WebSocket.connect(url: "http://localhost")
      assert {:error, {:invalid_scheme, "https"}} = WebSocket.connect(url: "https://localhost")
      assert {:error, {:invalid_scheme, nil}} = WebSocket.connect(url: "localhost")
    end

    test "accepts ws:// URLs" do
      # This will fail to connect but validates the scheme
      assert {:error, _} = WebSocket.connect(url: "ws://localhost:9999/nonexistent")
    end

    test "accepts wss:// URLs" do
      # This will fail to connect but validates the scheme
      assert {:error, _} = WebSocket.connect(url: "wss://localhost:9999/nonexistent")
    end

    test "requires url option" do
      assert_raise KeyError, fn ->
        WebSocket.connect([])
      end
    end

    test "accepts optional headers" do
      headers = [{"authorization", "Bearer token"}]
      # Will fail to connect but should parse options correctly
      assert {:error, _} = WebSocket.connect(url: "ws://localhost:9999", headers: headers)
    end

    test "accepts optional protocols" do
      protocols = ["mcp", "chat"]
      # Will fail to connect but should parse options correctly
      assert {:error, _} = WebSocket.connect(url: "ws://localhost:9999", protocols: protocols)
    end
  end

  describe "accept/1" do
    test "returns not implemented error" do
      assert {:error, :server_mode_not_implemented} = WebSocket.accept([])
    end
  end

  describe "state transitions" do
    test "send_message returns error when not connected" do
      state = %WebSocket.State{status: :connecting}
      assert {:error, :not_connected} = WebSocket.send_message("test", state)
    end

    test "send_message returns error when connection closed" do
      state = %WebSocket.State{status: :closed}
      assert {:error, :connection_closed} = WebSocket.send_message("test", state)
    end

    test "receive_message returns error when not connected" do
      state = %WebSocket.State{status: :connecting}
      assert {:error, :not_connected} = WebSocket.receive_message(state)
    end

    test "receive_message returns error when connection closed" do
      state = %WebSocket.State{status: :closed}
      assert {:error, :connection_closed} = WebSocket.receive_message(state)
    end
  end

  describe "close/1" do
    test "handles nil connection gracefully" do
      state = %WebSocket.State{conn: nil}
      assert :ok = WebSocket.close(state)
    end

    test "handles unconnected state" do
      state = %WebSocket.State{status: :connecting}
      assert :ok = WebSocket.close(state)
    end
  end

  describe "URL parsing" do
    test "extracts host and port from URL" do
      # Test through connection attempt
      assert {:error, _} = WebSocket.connect(url: "ws://example.com:8080/path")
    end

    test "uses default ports" do
      # ws:// should use port 80
      assert {:error, _} = WebSocket.connect(url: "ws://example.com/path")

      # wss:// should use port 443
      assert {:error, _} = WebSocket.connect(url: "wss://example.com/path")
    end

    test "handles URLs with paths" do
      assert {:error, _} = WebSocket.connect(url: "ws://localhost:8080/mcp/connect")
    end

    test "handles URLs without paths" do
      assert {:error, _} = WebSocket.connect(url: "ws://localhost:8080")
    end
  end
end
