defmodule ExMCP.HttpPlugTest do
  use ExUnit.Case, async: false
  import Plug.Test
  import Plug.Conn
  import ExUnit.CaptureLog

  alias ExMCP.HttpPlug
  alias ExMCP.HttpPlug.Core
  alias ExMCP.HttpPlug.SessionRegistry
  alias ExMCP.HttpPlug.SSEHandler
  alias ExMCP.Transport.HTTP.RequestHeaders

  defmodule LegacyCaptureConn do
    @behaviour ExMCP.HttpPlug.SSEConnection

    defstruct chunks: []

    @impl true
    def chunk(%__MODULE__{} = conn, data), do: {:ok, %{conn | chunks: conn.chunks ++ [data]}}

    @impl true
    def get_req_header(%__MODULE__{}, _header), do: []
  end

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

  defmodule BlockingRequestServer do
    use ExMCP.Server.Handler

    @impl true
    def init(opts) do
      state = Map.new(opts)
      send(state.test_pid, {:blocking_handler_started, self()})
      {:ok, state}
    end

    @impl true
    def handle_list_tools(_cursor, state) do
      receive do
        :unblock -> {:ok, [], nil, state}
      end
    end
  end

  defmodule CapabilityErrorServer do
    use ExMCP.Server.Handler

    @impl true
    def handle_initialize(_params, state), do: {:ok, %{}, state}

    @impl true
    def handle_list_tools(_cursor, state), do: {:ok, [], nil, state}

    @impl true
    def handle_call_tool(_name, _arguments, state) do
      error = ExMCP.Error.missing_required_client_capability(%{"sampling" => %{}})
      {:error, error, state}
    end
  end

  defmodule HeaderToolServer do
    use ExMCP.Server.Handler

    @tool %{
      "name" => "routed_tool",
      "description" => "Exercises x-mcp-header validation",
      "inputSchema" => %{
        "type" => "object",
        "properties" => %{
          "region" => %{"type" => "string", "x-mcp-header" => "Region"},
          "limit" => %{"type" => "integer", "x-mcp-header" => "Limit"}
        }
      }
    }

    @impl true
    def handle_list_tools(_cursor, state), do: {:ok, [@tool], nil, state}

    @impl true
    def handle_call_tool(_name, arguments, state) do
      {:ok, %{"content" => [%{"type" => "text", "text" => inspect(arguments)}]}, state}
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

    def ensure_session(session_id, attrs) do
      notify({:session_ensured, session_id, attrs})
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
      assert config.sse_enabled == false
      assert config.legacy_http_sse == false
      assert config.cors_enabled == false
      assert config.validate_origin == true
      assert config.allowed_origins == []
      assert config.allowed_hosts == :any
      assert config.body_limit == 1_000_000
      assert config.handler_opts == []
      assert config.handler_call_timeout == 10_000
    end

    test "init/1 accepts a server-side handler call deadline" do
      assert HttpPlug.init(handler_call_timeout: 250).handler_call_timeout == 250
    end

    test "retains sse_enabled as an alias for the deprecated transport" do
      assert HttpPlug.init(sse_enabled: true).legacy_http_sse == true
      assert HttpPlug.init(legacy_http_sse: true).sse_enabled == true
      assert HttpPlug.init(legacy_http_sse: false, sse_enabled: true).legacy_http_sse == false
    end

    test "normalizes legacy HTTP+SSE paths" do
      config =
        HttpPlug.init(legacy_http_sse_path: "events", legacy_http_sse_post_path: "inbox")

      assert config.legacy_http_sse_path == "/events"
      assert config.legacy_http_sse_post_path == "/inbox"
    end

    test "init/1 resolves the SSE mode instead of branching at request time" do
      assert HttpPlug.init(sse_mode: :stream).sse_mode == :stream
      assert HttpPlug.init(sse_mode: :oneshot).sse_mode == :oneshot
      assert HttpPlug.init([]).sse_mode in [:stream, :oneshot]
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

  describe "handler call timeout" do
    @describetag capture_log: true

    test "threads the plug deadline into MessageProcessor" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/list",
        "params" => %{},
        "id" => 41
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(
          HttpPlug.init(
            handler: BlockingRequestServer,
            handler_opts: [test_pid: self()],
            handler_call_timeout: 25,
            sse_enabled: false
          )
        )

      assert conn.status == 200
      assert_received {:blocking_handler_started, _handler_pid}

      assert %{"error" => %{"code" => -32603, "data" => %{"type" => "handler_timeout"}}} =
               Jason.decode!(conn.resp_body)
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

    test "rejects browser origins unless explicitly allowed" do
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

    test "rejects an Origin equal to the request Host when not allow-listed" do
      # DNS rebinding: the Host header is attacker-controlled, so an Origin
      # matching scheme://host:port must not be implicitly trusted.
      # Plug.Test conns use host www.example.com on port 80.
      request = %{
        "jsonrpc" => "2.0",
        "method" => "initialize",
        "id" => 1
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_req_header("origin", "http://www.example.com")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert conn.status == 403
      assert conn.resp_body == "Origin not allowed"
    end

    test "allows requests without an Origin header when validate_origin is enabled" do
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
        |> HttpPlug.call(
          HttpPlug.init(handler: TestServer, sse_enabled: false, validate_origin: true)
        )

      assert conn.status == 200
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

  # Helpers shared by the host validation and session ID validation tests.
  # (Function definitions are not allowed inside describe blocks.)
  defp initialize_conn do
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

    conn(:post, "/", Jason.encode!(request))
    |> put_req_header("content-type", "application/json")
  end

  # Plug.Test forbids put_req_header("host", _); the Host is modelled by
  # conn.host, which is what HttpPlug falls back to when no header is present.
  defp with_host(conn, host), do: %{conn | host: host}

  defp put_modern_headers(conn, request) do
    meta = get_in(request, ["params", "_meta"])
    version = meta["io.modelcontextprotocol/protocolVersion"]
    method = request["method"]

    conn
    |> put_req_header("mcp-protocol-version", version)
    |> put_req_header("mcp-method", method)
    |> maybe_put_modern_name(method, request["params"] || %{})
  end

  defp maybe_put_modern_name(conn, "tools/call", params),
    do: put_req_header(conn, "mcp-name", RequestHeaders.encode_value(params["name"]))

  defp maybe_put_modern_name(conn, "resources/read", params),
    do: put_req_header(conn, "mcp-name", RequestHeaders.encode_value(params["uri"]))

  defp maybe_put_modern_name(conn, "prompts/get", params),
    do: put_req_header(conn, "mcp-name", RequestHeaders.encode_value(params["name"]))

  defp maybe_put_modern_name(conn, _method, _params), do: conn

  defp modern_request(method, params, id) do
    meta = %{
      "io.modelcontextprotocol/protocolVersion" => "2026-07-28",
      "io.modelcontextprotocol/clientCapabilities" => %{}
    }

    %{
      "jsonrpc" => "2.0",
      "id" => id,
      "method" => method,
      "params" => Map.put(params, "_meta", meta)
    }
  end

  defp session_request_conn do
    request = %{
      "jsonrpc" => "2.0",
      "method" => "tools/list",
      "id" => 10
    }

    conn(:post, "/", Jason.encode!(request))
    |> put_req_header("content-type", "application/json")
  end

  describe "host validation" do
    test "default allowed_hosts :any accepts any Host" do
      conn =
        initialize_conn()
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert conn.status == 200
    end

    test "rejects a Host that is not allow-listed with 421" do
      conn =
        initialize_conn()
        |> with_host("evil.example")
        |> HttpPlug.call(
          HttpPlug.init(handler: TestServer, sse_enabled: false, allowed_hosts: ["localhost"])
        )

      assert conn.status == 421

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["error"]["code"] == -32600
      assert response["error"]["message"] =~ "Host"
    end

    test "rejects the Plug.Test default host when only localhost is allowed" do
      # No explicit Host header: falls back to conn.host (www.example.com).
      conn =
        initialize_conn()
        |> HttpPlug.call(
          HttpPlug.init(handler: TestServer, sse_enabled: false, allowed_hosts: ["localhost"])
        )

      assert conn.status == 421
    end

    test "accepts an allow-listed Host" do
      conn =
        initialize_conn()
        |> with_host("localhost")
        |> HttpPlug.call(
          HttpPlug.init(handler: TestServer, sse_enabled: false, allowed_hosts: ["localhost"])
        )

      assert conn.status == 200
    end

    test "accepts an allow-listed IPv6 Host" do
      conn =
        initialize_conn()
        |> with_host("::1")
        |> HttpPlug.call(
          HttpPlug.init(handler: TestServer, sse_enabled: false, allowed_hosts: ["::1"])
        )

      assert conn.status == 200
    end

    # Plug forbids setting the "host" request header directly (it is derived
    # from conn.host), so port and IPv6-bracket normalization is asserted
    # against the function that implements it.
    test "host matching ignores ports and IPv6 brackets" do
      assert Core.host_allowed?("localhost:4000", ["localhost"])
      assert Core.host_allowed?("LOCALHOST:4000", ["localhost"])
      assert Core.host_allowed?("[::1]:8080", ["::1"])
      assert Core.host_allowed?("[::1]:8080", ["[::1]"])
      assert Core.host_allowed?("::1", ["::1"])
      refute Core.host_allowed?("evil.example:8080", ["localhost"])
      refute Core.host_allowed?("evil.example", ["localhost"])
      refute Core.host_allowed?(nil, ["localhost"])
      assert Core.host_allowed?("anything.example", :any)
    end
  end

  describe "session ID validation" do
    test "modern requests are stateless and ignore legacy session headers" do
      {:ok, _} = TrackingSessionManager.start_link(self())

      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/list",
        "id" => 9,
        "params" => %{
          "_meta" => %{
            "io.modelcontextprotocol/protocolVersion" => "2026-07-28",
            "io.modelcontextprotocol/clientCapabilities" => %{}
          }
        }
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> put_req_header("mcp-session-id", "ignored legacy session")
        |> put_req_header("last-event-id", "ignored-event")
        |> HttpPlug.call(
          HttpPlug.init(
            handler: TestServer,
            protocol_mode: :modern_only,
            session_manager: TrackingSessionManager,
            sse_enabled: false
          )
        )

      assert conn.status == 200
      assert get_resp_header(conn, "mcp-session-id") == []
      assert %{"result" => %{"resultType" => "complete"}} = Jason.decode!(conn.resp_body)
      refute_received {:session_ensured, _session_id, _attrs}
    end

    test "legacy requests retain session-scoped behavior" do
      {:ok, _} = TrackingSessionManager.start_link(self())

      conn =
        session_request_conn()
        |> HttpPlug.call(
          HttpPlug.init(
            handler: TestServer,
            protocol_mode: :prefer_modern,
            session_manager: TrackingSessionManager,
            sse_enabled: false
          )
        )

      assert conn.status == 200
      assert [session_id] = get_resp_header(conn, "mcp-session-id")
      assert_received {:session_ensured, ^session_id, %{transport: :http}}
    end

    test "accepts and echoes a UUID session id" do
      uuid = "123e4567-e89b-12d3-a456-426614174000"

      conn =
        session_request_conn()
        |> put_req_header("mcp-session-id", uuid)
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert conn.status == 200
      assert get_resp_header(conn, "mcp-session-id") == [uuid]
    end

    test "rejects session ids longer than 128 bytes without echoing them" do
      long_id = String.duplicate("a", 129)

      conn =
        session_request_conn()
        |> put_req_header("mcp-session-id", long_id)
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert conn.status == 400
      assert get_resp_header(conn, "mcp-session-id") == []
      refute conn.resp_body =~ long_id

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["error"]["code"] == -32600
    end

    test "rejects session ids with control characters" do
      # Injected directly to bypass any header-value validation in Plug.Test.
      base = session_request_conn()
      conn = %{base | req_headers: [{"mcp-session-id", "bad\nid"} | base.req_headers]}

      conn = HttpPlug.call(conn, HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert conn.status == 400
      assert get_resp_header(conn, "mcp-session-id") == []
      refute conn.resp_body =~ "bad\nid"
    end

    test "rejects session ids with characters outside the token charset" do
      conn =
        session_request_conn()
        |> put_req_header("mcp-session-id", "not a valid id!")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert conn.status == 400
      refute conn.resp_body =~ "not a valid id!"
    end

    test "validates the legacy x-session-id header on POST" do
      conn =
        session_request_conn()
        |> put_req_header("x-session-id", "bad session id")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert conn.status == 400
    end

    test "rejects invalid session ids on SSE connections" do
      conn =
        conn(:get, "/sse")
        |> put_req_header("mcp-session-id", "bad session id")
        |> HttpPlug.call(HttpPlug.init(sse_enabled: true))

      assert conn.status == 400
    end

    test "rejects invalid session ids on DELETE" do
      conn =
        conn(:delete, "/mcp")
        |> put_req_header("mcp-session-id", "bad session id")
        |> HttpPlug.call(HttpPlug.init([]))

      assert conn.status == 400
    end
  end

  describe "modern-only HTTP methods" do
    test "rejects GET and DELETE on the MCP endpoint with 405" do
      opts =
        HttpPlug.init(
          handler: TestServer,
          path: "/mcp",
          protocol_mode: :modern_only,
          sse_enabled: true
        )

      get_conn =
        conn(:get, "/mcp")
        |> put_req_header("accept", "text/event-stream")
        |> HttpPlug.call(opts)

      delete_conn =
        conn(:delete, "/mcp")
        |> put_req_header("mcp-session-id", "legacy-session")
        |> HttpPlug.call(opts)

      assert get_conn.status == 405
      assert delete_conn.status == 405
      assert get_resp_header(get_conn, "allow") == ["POST"]
    end
  end

  describe "MCP POST requests" do
    test "returns HTTP 400 for invalid modern request metadata" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/list",
        "params" => %{
          "_meta" => %{
            "io.modelcontextprotocol/protocolVersion" => "2026-07-28"
          }
        },
        "id" => 101
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert conn.status == 400
      assert Jason.decode!(conn.resp_body)["error"]["code"] == -32602
    end

    test "stamps valid modern handler results" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/list",
        "params" => %{
          "_meta" => %{
            "io.modelcontextprotocol/protocolVersion" => "2026-07-28",
            "io.modelcontextprotocol/clientCapabilities" => %{}
          }
        },
        "id" => 102
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> HttpPlug.call(
          HttpPlug.init(
            handler: TestServer,
            server_info: %{name: "configured-server", version: "1"},
            sse_enabled: false
          )
        )

      assert conn.status == 200
      result = Jason.decode!(conn.resp_body)["result"]
      assert result["resultType"] == "complete"

      assert result["_meta"]["io.modelcontextprotocol/serverInfo"] == %{
               "name" => "configured-server",
               "version" => "1"
             }
    end

    test "serves modern discovery without initialize" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "server/discover",
        "params" => %{
          "_meta" => %{
            "io.modelcontextprotocol/protocolVersion" => "2026-07-28",
            "io.modelcontextprotocol/clientCapabilities" => %{}
          }
        },
        "id" => 103
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> HttpPlug.call(
          HttpPlug.init(
            handler: TestServer,
            server_info: %{name: "discoverable", version: "1"},
            protocol_mode: :modern_only,
            sse_enabled: false
          )
        )

      assert conn.status == 200
      result = Jason.decode!(conn.resp_body)["result"]
      assert result["resultType"] == "complete"
      assert result["supportedVersions"] == ["2026-07-28"]
      assert result["capabilities"]["tools"] == %{}
      assert result["ttlMs"] >= 0
      assert result["cacheScope"] in ["public", "private"]
    end

    test "preserves missing client capability errors from handlers" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "params" => %{
          "name" => "needs_sampling",
          "arguments" => %{},
          "_meta" => %{
            "io.modelcontextprotocol/protocolVersion" => "2026-07-28",
            "io.modelcontextprotocol/clientCapabilities" => %{}
          }
        },
        "id" => 104
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> HttpPlug.call(HttpPlug.init(handler: CapabilityErrorServer, sse_enabled: false))

      assert conn.status == 200
      error = Jason.decode!(conn.resp_body)["error"]
      assert error["code"] == -32021
      assert error["data"]["requiredCapabilities"] == %{"sampling" => %{}}
    end

    test "rejects missing or mismatched modern request headers before dispatch" do
      request = modern_request("tools/list", %{}, 105)

      missing =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert missing.status == 400
      assert Jason.decode!(missing.resp_body)["error"]["code"] == -32020

      mismatched =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> put_req_header("mcp-method", "resources/list")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert mismatched.status == 400
      assert Jason.decode!(mismatched.resp_body)["error"]["code"] == -32020
    end

    test "accepts an encoded Mcp-Name and rejects a name/body mismatch" do
      request =
        modern_request(
          "tools/call",
          %{"name" => "test_tool", "arguments" => %{"message" => "hi"}},
          106
        )

      accepted =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> put_req_header("mcp-name", RequestHeaders.encode_value("test_tool"))
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert accepted.status == 200, accepted.resp_body
      assert get_resp_header(accepted, "mcp-protocol-version") == ["2026-07-28"]

      rejected =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> put_req_header("mcp-name", "another_tool")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert rejected.status == 400
      assert Jason.decode!(rejected.resp_body)["error"]["code"] == -32020
    end

    test "returns HTTP 404 and JSON-RPC method-not-found for unknown modern methods" do
      request = modern_request("unknown/modern", %{}, 107)

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, sse_enabled: false))

      assert conn.status == 404
      assert Jason.decode!(conn.resp_body)["error"]["code"] == -32601
      assert get_resp_header(conn, "mcp-session-id") == []
    end

    test "streams modern subscriptions/listen over the POST response with keepalives" do
      request =
        modern_request(
          "subscriptions/listen",
          %{"notifications" => %{"toolsListChanged" => true}},
          117
        )

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> HttpPlug.call(
          HttpPlug.init(
            handler: TestServer,
            protocol_mode: :modern_only,
            subscription_keepalive_interval_ms: 5,
            subscription_max_lifetime_ms: 25
          )
        )

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["text/event-stream"]
      assert get_resp_header(conn, "x-accel-buffering") == ["no"]
      assert get_resp_header(conn, "mcp-session-id") == []
      assert conn.resp_body =~ ":\r\n"

      messages =
        conn.resp_body
        |> String.split("\r\n\r\n", trim: true)
        |> Enum.flat_map(fn
          "data: " <> json -> [Jason.decode!(json)]
          _comment -> []
        end)

      assert [acknowledgment, completion] = messages

      assert acknowledgment["method"] == "notifications/subscriptions/acknowledged"
      assert get_in(acknowledgment, ["params", "notifications"]) == %{"toolsListChanged" => true}

      assert completion == %{
               "jsonrpc" => "2.0",
               "id" => 117,
               "result" => %{
                 "resultType" => "complete",
                 "_meta" => %{"io.modelcontextprotocol/subscriptionId" => 117}
               }
             }
    end

    test "rejects an invalid modern subscription filter without opening an SSE stream" do
      request =
        modern_request(
          "subscriptions/listen",
          %{"notifications" => %{"unknownEvents" => true}},
          118
        )

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, protocol_mode: :modern_only))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["application/json; charset=utf-8"]

      error = Jason.decode!(conn.resp_body)["error"]
      assert error["code"] == -32602
      assert error["data"]["reason"] == "unknown_subscription_filter"
    end

    test "preserves the missing-capability error for task subscriptions" do
      request =
        modern_request(
          "subscriptions/listen",
          %{"notifications" => %{"taskIds" => ["task-1"]}},
          119
        )

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, protocol_mode: :modern_only))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["application/json; charset=utf-8"]

      error = Jason.decode!(conn.resp_body)["error"]
      assert error["code"] == ExMCP.Protocol.ErrorCodes.missing_required_client_capability()

      assert error["data"] == %{
               "requiredCapabilities" => ExMCP.Tasks.Extension.required_capabilities()
             }
    end

    test "validates x-mcp-header values against tool arguments before dispatch" do
      request =
        modern_request(
          "tools/call",
          %{"name" => "routed_tool", "arguments" => %{"region" => "us-west1", "limit" => 42}},
          108
        )

      accepted =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> put_req_header("mcp-param-region", "us-west1")
        |> put_req_header("mcp-param-limit", "42.0")
        |> HttpPlug.call(HttpPlug.init(handler: HeaderToolServer, sse_enabled: false))

      assert accepted.status == 200, accepted.resp_body

      missing =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> put_req_header("mcp-param-region", "us-west1")
        |> HttpPlug.call(HttpPlug.init(handler: HeaderToolServer, sse_enabled: false))

      assert missing.status == 400
      assert Jason.decode!(missing.resp_body)["error"]["code"] == -32020

      mismatched =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_modern_headers(request)
        |> put_req_header("mcp-param-region", "eu-central1")
        |> put_req_header("mcp-param-limit", "42")
        |> HttpPlug.call(HttpPlug.init(handler: HeaderToolServer, sse_enabled: false))

      assert mismatched.status == 400
      assert Jason.decode!(mismatched.resp_body)["error"]["code"] == -32020
    end

    test "does not expose Mcp-Param values through server logs or telemetry" do
      secret = "header-only-routing-secret"
      handler_id = "mcp-param-confidentiality-#{System.unique_integer([:positive])}"
      test_pid = self()

      :ok =
        :telemetry.attach(
          handler_id,
          [:ex_mcp, :server, :http, :request],
          fn event, measurements, metadata, owner ->
            send(owner, {:http_telemetry, event, measurements, metadata})
          end,
          test_pid
        )

      on_exit(fn -> :telemetry.detach(handler_id) end)

      request =
        modern_request(
          "tools/call",
          %{"name" => "routed_tool", "arguments" => %{"region" => "body-region", "limit" => 42}},
          109
        )

      log =
        capture_log([level: :debug], fn ->
          conn(:post, "/", Jason.encode!(request))
          |> put_req_header("content-type", "application/json")
          |> put_modern_headers(request)
          |> put_req_header("mcp-param-region", secret)
          |> put_req_header("mcp-param-limit", "42")
          |> HttpPlug.call(HttpPlug.init(handler: HeaderToolServer, sse_enabled: false))
        end)

      refute log =~ secret

      assert_receive {:http_telemetry, _event, measurements, metadata}
      refute inspect({measurements, metadata}) =~ secret
    end

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
    test "does not enable the deprecated HTTP+SSE route by default" do
      conn =
        conn(:get, "/sse")
        |> HttpPlug.call(HttpPlug.init([]))

      assert conn.status == 404
      assert conn.resp_body == "SSE not enabled"
    end

    test "handles SSE connection request" do
      conn =
        conn(:get, "/sse")
        |> HttpPlug.call(HttpPlug.init(legacy_http_sse: true))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["text/event-stream"]
      assert get_resp_header(conn, "cache-control") == ["no-cache"]
      assert get_resp_header(conn, "connection") == ["keep-alive"]
      assert conn.resp_body =~ "event: endpoint"
      assert conn.resp_body =~ "data: http://www.example.com/message?sessionId="
      refute conn.resp_body =~ "event: connected"
    end

    test "supports configured legacy GET and POST paths" do
      conn =
        conn(:get, "/events")
        |> HttpPlug.call(
          HttpPlug.init(
            legacy_http_sse: true,
            legacy_http_sse_path: "/events",
            legacy_http_sse_post_path: "/inbox"
          )
        )

      assert conn.status == 200
      assert conn.resp_body =~ "event: endpoint"
      assert conn.resp_body =~ "data: http://www.example.com/inbox?sessionId="
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

    test "does not expose the deprecated route in modern-only mode" do
      conn =
        conn(:get, "/sse")
        |> HttpPlug.call(HttpPlug.init(protocol_mode: :modern_only, legacy_http_sse: true))

      assert conn.status == 404
      assert conn.resp_body == "SSE not enabled"
    end

    test "legacy POST sends its JSON-RPC response on the open SSE stream" do
      session_id = "legacy-session"

      {:ok, handler} =
        SSEHandler.start_link(%LegacyCaptureConn{}, session_id, %{
          conn_module: LegacyCaptureConn,
          initial_sse_event: {"endpoint", {:raw, "/message?sessionId=#{session_id}"}}
        })

      :ok = SessionRegistry.register(session_id, handler)

      request = %{
        "jsonrpc" => "2.0",
        "method" => "initialize",
        "params" => %{
          "protocolVersion" => "2024-11-05",
          "capabilities" => %{},
          "clientInfo" => %{"name" => "legacy-client", "version" => "1.0.0"}
        },
        "id" => 77
      }

      conn =
        conn(:post, "/message?sessionId=#{session_id}", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, legacy_http_sse: true))

      assert conn.status == 202
      assert conn.resp_body == ""

      chunks = :sys.get_state(handler).conn.chunks
      message = List.last(chunks)
      assert message =~ "event: message"
      assert message =~ ~s("id":77)
      assert message =~ ~s("protocolVersion":"2024-11-05")

      SSEHandler.close(handler)
    end

    test "legacy POST rejects an unknown SSE session" do
      conn =
        conn(:post, "/message?sessionId=missing", Jason.encode!(%{"jsonrpc" => "2.0"}))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(HttpPlug.init(handler: TestServer, legacy_http_sse: true))

      assert conn.status == 404
      assert conn.resp_body == "Legacy SSE session not found"
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
