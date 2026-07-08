defmodule ExMCP.HttpPlugTest do
  use ExUnit.Case, async: false
  import Plug.Test
  import Plug.Conn

  alias ExMCP.HttpPlug

  defmodule TestServer do
    use ExMCP.Server.Handler
    use ExMCP.Server.DSL, name: "test", version: "1.0.0"

    tool "test_tool", "A test tool" do
      input_schema(%{
        type: "object",
        properties: %{
          message: %{type: "string"}
        },
        required: ["message"]
      })

      run(fn %{"message" => message}, state ->
        {:ok, %{content: [%{"type" => "text", "text" => "Echo: #{message}"}]}, state}
      end)
    end
  end

  defmodule RequestAwareServer do
    use ExMCP.Server.Handler

    @impl true
    def init(opts), do: {:ok, Map.new(opts)}

    @impl true
    def handle_initialize(_params, state) do
      {:ok,
       %{
         name: Map.fetch!(state, :request_path),
         version: Map.fetch!(state, :request_method),
         capabilities: %{}
       }, state}
    end
  end

  defmodule TrackingSessionManager do
    @table :http_plug_test_session_manager

    def start_link(owner) do
      if :ets.whereis(@table) != :undefined do
        :ets.delete(@table)
      end

      :ets.new(@table, [:named_table, :public, :set])
      :ets.insert(@table, {:owner, owner})
      {:ok, self()}
    end

    def get_session(_session_id), do: {:error, :not_found}

    def create_session(_attrs), do: "tracked_session"

    def update_session(session_id, attrs) do
      notify({:session_updated, session_id, attrs})
    end

    def terminate_session(session_id) do
      notify({:session_terminated, session_id})
    end

    defp notify(message) do
      case :ets.lookup(@table, :owner) do
        [{:owner, owner}] -> send(owner, message)
        [] -> :ok
      end

      :ok
    end
  end

  describe "HTTP Plug behavior" do
    test "implements Plug behavior correctly" do
      Code.ensure_loaded!(HttpPlug)

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
      assert config.cors_enabled == false
      assert config.validate_origin == true
      assert config.allowed_origins == []
      assert config.body_limit == 1_000_000
      assert config.handler_opts == []
    end
  end

  describe "session deletion" do
    test "uses the configured session manager" do
      {:ok, _} = TrackingSessionManager.start_link(self())

      conn =
        conn(:delete, "/mcp")
        |> put_req_header("mcp-session-id", "custom-session")
        |> HttpPlug.call(HttpPlug.init(session_manager: TrackingSessionManager))

      assert conn.status == 204
      assert_received {:session_terminated, "custom-session"}
    end
  end

  describe "CORS handling" do
    test "handles OPTIONS preflight request for explicitly allowed wildcard CORS" do
      conn =
        conn(:options, "/")
        |> HttpPlug.call(HttpPlug.init(cors_enabled: true, allowed_origins: :any))

      assert conn.status == 200
      assert get_resp_header(conn, "access-control-allow-origin") == ["*"]

      assert get_resp_header(conn, "access-control-allow-methods") == [
               "GET, POST, DELETE, OPTIONS"
             ]
    end

    test "rejects OPTIONS when CORS disabled" do
      conn =
        conn(:options, "/")
        |> HttpPlug.call(HttpPlug.init(cors_enabled: false))

      assert conn.status == 405
    end

    test "rejects browser origins unless explicitly allowed or same-origin" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "initialize",
        "id" => 1
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_req_header("origin", "https://evil.example")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert conn.status == 403
      assert conn.resp_body == "Origin not allowed"
    end

    test "allows explicitly configured browser origins" do
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
        |> put_req_header("origin", "https://client.example")
        |> HttpPlug.call(
          HttpPlug.init(
            handler: TestServer,
            sse_enabled: false,
            cors_enabled: true,
            allowed_origins: ["https://client.example"]
          )
        )

      assert conn.status == 200
      assert get_resp_header(conn, "access-control-allow-origin") == ["https://client.example"]
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
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

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
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

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
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert conn.status == 200

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["jsonrpc"] == "2.0"
      assert response["id"] == 3
      assert Map.has_key?(response["result"], "content")
    end

    test "resolves handler_opts from the Plug connection and JSON-RPC request" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "initialize",
        "params" => %{
          "protocolVersion" => "2025-06-18",
          "capabilities" => %{},
          "clientInfo" => %{name: "test-client", version: "1.0.0"}
        },
        "id" => 30
      }

      handler_opts = fn conn, request ->
        [
          request_path: conn.request_path,
          request_method: request["method"]
        ]
      end

      conn =
        conn(:post, "/mcp", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(
          HttpPlug.init(
            handler: RequestAwareServer,
            handler_opts: handler_opts,
            sse_enabled: false
          )
        )

      assert conn.status == 200

      {:ok, response} = Jason.decode(conn.resp_body)

      assert response["result"]["serverInfo"] == %{
               "name" => "/mcp",
               "version" => "initialize"
             }
    end

    test "defaults missing handler_opts to empty options" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "params" => %{
          "name" => "test_tool",
          "arguments" => %{"message" => "hello"}
        },
        "id" => 31
      }

      opts = %{
        handler: TestServer,
        server_info: %{name: "test", version: "1.0.0"},
        sse_enabled: false,
        cors_enabled: false,
        oauth_enabled: false,
        auth_config: %{}
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(opts)

      assert conn.status == 200

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["id"] == 31
      assert response["result"]["content"] == [%{"type" => "text", "text" => "Echo: hello"}]
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

    test "rejects non-object JSON envelopes" do
      conn =
        conn(:post, "/", Jason.encode!("not a request"))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer))

      assert conn.status == 400

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["error"]["code"] == -32600
      assert response["error"]["message"] == "Invalid Request"
    end

    test "rejects oversized request bodies" do
      body = Jason.encode!(%{"jsonrpc" => "2.0", "method" => "initialize", "id" => 1})

      conn =
        conn(:post, "/", body)
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, body_limit: 8))

      assert conn.status == 413
      assert conn.resp_body == "Request body too large"
    end

    test "oauth_enabled fails closed when OAuth authorization feature is disabled" do
      Application.put_env(:ex_mcp, :oauth2_enabled, false)

      on_exit(fn ->
        Application.delete_env(:ex_mcp, :oauth2_enabled)
      end)

      request = %{
        "jsonrpc" => "2.0",
        "method" => "initialize",
        "id" => 1
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, oauth_enabled: true))

      assert conn.status == 500

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["error"] == "server_error"
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
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

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
