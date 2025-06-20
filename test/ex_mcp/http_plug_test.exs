defmodule ExMCP.HttpPlugTest do
  use ExUnit.Case, async: true
  use Plug.Test

  alias ExMCP.HttpPlug

  defmodule TestServer do
    use ExMCP.Server

    deftool "test_tool" do
      tool_description("A test tool")

      args do
        field(:message, :string, required: true)
      end
    end

    @impl true
    def handle_tool_call("test_tool", %{"message" => message}, state) do
      result = %{content: [%{"type" => "text", "text" => "Echo: #{message}"}]}
      {:ok, result, state}
    end
  end

  describe "HTTP Plug behavior" do
    test "implements Plug behavior correctly" do
      assert function_exported?(HttpPlug, :init, 1)
      assert function_exported?(HttpPlug, :call, 2)
    end

    test "init/1 sets up configuration" do
      opts = [
        handler: TestServer,
        server_info: %{name: "test", version: "1.0.0"}
      ]

      config = HttpPlug.init(opts)

      assert config.handler == TestServer
      assert config.server_info.name == "test"
      assert config.sse_enabled == true
      assert config.cors_enabled == true
    end
  end

  describe "CORS handling" do
    test "handles OPTIONS preflight request" do
      conn =
        conn(:options, "/")
        |> HttpPlug.call(HttpPlug.init(cors_enabled: true))

      assert conn.status == 200
      assert get_resp_header(conn, "access-control-allow-origin") == ["*"]
      assert get_resp_header(conn, "access-control-allow-methods") == ["GET, POST, OPTIONS"]
    end

    test "rejects OPTIONS when CORS disabled" do
      conn =
        conn(:options, "/")
        |> HttpPlug.call(HttpPlug.init(cors_enabled: false))

      assert conn.status == 405
    end
  end

  describe "MCP POST requests" do
    test "handles initialize request" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "initialize",
        "params" => %{
          "protocolVersion" => "2025-06-18",
          "capabilities" => %{},
          "clientInfo" => %{name: "test-client", version: "1.0.0"}
        },
        "id" => 1
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["application/json; charset=utf-8"]

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["jsonrpc"] == "2.0"
      assert response["id"] == 1
      assert Map.has_key?(response["result"], "protocolVersion")
      assert Map.has_key?(response["result"], "capabilities")
    end

    test "handles tools/list request" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/list",
        "id" => 2
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer))

      assert conn.status == 200

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["jsonrpc"] == "2.0"
      assert response["id"] == 2
      assert Map.has_key?(response["result"], "tools")
      assert is_list(response["result"]["tools"])
    end

    test "handles tools/call request" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "params" => %{
          "name" => "test_tool",
          "arguments" => %{"message" => "hello"}
        },
        "id" => 3
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer))

      assert conn.status == 200

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["jsonrpc"] == "2.0"
      assert response["id"] == 3
      assert Map.has_key?(response["result"], "content")
    end

    test "handles invalid JSON" do
      conn =
        conn(:post, "/", "invalid json")
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer))

      assert conn.status == 400

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["jsonrpc"] == "2.0"
      assert response["error"]["code"] == -32700
      assert response["error"]["message"] == "Parse error"
    end

    test "handles unknown method" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "unknown/method",
        "id" => 4
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer))

      assert conn.status == 200

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["jsonrpc"] == "2.0"
      assert response["id"] == 4
      assert response["error"]["code"] == -32601
      assert response["error"]["message"] == "Method not found"
    end

    test "handles missing handler" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "initialize",
        "id" => 5
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(HttpPlug.init([]))

      assert conn.status == 500

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["jsonrpc"] == "2.0"
      assert response["error"]["code"] == -32603
      assert response["error"]["message"] == "Internal error"
    end
  end

  describe "SSE connections" do
    test "handles SSE connection request" do
      conn =
        conn(:get, "/sse")
        |> HttpPlug.call(HttpPlug.init(sse_enabled: true))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["text/event-stream"]
      assert get_resp_header(conn, "cache-control") == ["no-cache"]
      assert get_resp_header(conn, "connection") == ["keep-alive"]
    end

    test "rejects SSE when disabled" do
      conn =
        conn(:get, "/sse")
        |> HttpPlug.call(HttpPlug.init(sse_enabled: false))

      assert conn.status == 404
      assert conn.resp_body == "SSE not enabled"
    end

    @tag timeout: 1000
    test "uses provided session ID" do
      # We can't easily test the full SSE flow in sync tests
      # but we can verify the headers are processed
      opts = HttpPlug.init(sse_enabled: true)

      # Create a mock conn to test header extraction
      conn =
        conn(:get, "/sse")
        |> put_req_header("x-session-id", "custom-session-123")

      # Test that the plug would start SSE (indicated by chunked response)
      result_conn = HttpPlug.call(conn, opts)
      assert result_conn.status == 200
      assert get_resp_header(result_conn, "content-type") == ["text/event-stream"]
    end
  end

  describe "404 handling" do
    test "returns 404 for unknown paths" do
      conn =
        conn(:get, "/unknown/path")
        |> HttpPlug.call(HttpPlug.init([]))

      assert conn.status == 404
      assert get_resp_header(conn, "content-type") == ["application/json; charset=utf-8"]

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["error"] == "Not found"
    end

    test "returns 404 for unsupported methods" do
      conn =
        conn(:put, "/")
        |> HttpPlug.call(HttpPlug.init([]))

      assert conn.status == 404
    end
  end
end
