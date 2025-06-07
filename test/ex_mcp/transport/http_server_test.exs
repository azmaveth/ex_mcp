defmodule ExMCP.Transport.HTTPServerTest do
  @moduledoc """
  Tests for HTTP server transport with security and CORS support.
  """
  use ExUnit.Case, async: true
  import Plug.Test
  import Plug.Conn

  alias ExMCP.Transport.HTTPServer

  defmodule TestHandler do
    @moduledoc false
    use ExMCP.Server.Handler

    @impl true
    def init(_args), do: {:ok, %{}}

    @impl true
    def handle_initialize(_params, state) do
      result = %{
        protocolVersion: "2025-03-26",
        serverInfo: %{name: "test-server", version: "1.0.0"},
        capabilities: %{tools: %{}}
      }

      {:ok, result, state}
    end

    @impl true
    def handle_list_tools(_cursor, state) do
      tools = [
        %{name: "test_tool", description: "A test tool"}
      ]

      {:ok, tools, nil, state}
    end

    # Implement required callbacks with minimal implementations
    @impl true
    def handle_call_tool(_name, _args, state), do: {:ok, [], state}
    @impl true
    def handle_list_resources(_cursor, state), do: {:ok, [], nil, state}
    @impl true
    def handle_read_resource(_uri, state), do: {:error, "Not found", state}
    @impl true
    def handle_list_prompts(_cursor, state), do: {:ok, [], nil, state}
    @impl true
    def handle_get_prompt(_name, _args, state), do: {:error, "Not found", state}
    @impl true
    def handle_complete(_ref, _arg, state), do: {:ok, %{completion: []}, state}
    @impl true
    def handle_list_resource_templates(_cursor, state), do: {:ok, [], nil, state}
    @impl true
    def handle_subscribe_resource(_uri, state), do: {:ok, %{}, state}
    @impl true
    def handle_unsubscribe_resource(_uri, state), do: {:ok, %{}, state}
    @impl true
    def handle_create_message(_params, state), do: {:error, "Not supported", state}
    @impl true
    def handle_list_roots(state), do: {:ok, [], state}
    @impl true
    def handle_set_log_level(_level, state), do: {:ok, state}
  end

  defp create_plug(security_opts \\ %{}) do
    # Add test-friendly defaults
    default_security = %{
      validate_origin: false,
      enforce_https: false,
      include_security_headers: true
    }

    HTTPServer.init(
      handler: TestHandler,
      security: Map.merge(default_security, security_opts)
    )
  end

  describe "CORS support" do
    test "includes CORS headers in response" do
      security_config = %{
        validate_origin: true,
        allowed_origins: ["https://example.com"],
        cors: %{
          allowed_origins: ["https://example.com"],
          allowed_methods: ["GET", "POST", "OPTIONS"],
          allowed_headers: ["Content-Type", "Authorization"],
          allow_credentials: true
        }
      }

      plug_opts = create_plug(security_config)

      conn =
        conn(:options, "/")
        |> put_req_header("origin", "https://example.com")
        |> Map.put(:host, "api.example.com")
        |> HTTPServer.call(plug_opts)

      assert conn.status == 200
      assert get_resp_header(conn, "access-control-allow-origin") == ["https://example.com"]
      assert get_resp_header(conn, "access-control-allow-methods") == ["GET, POST, OPTIONS"]

      assert get_resp_header(conn, "access-control-allow-headers") == [
               "Content-Type, Authorization"
             ]

      assert get_resp_header(conn, "access-control-allow-credentials") == ["true"]
    end

    test "handles CORS preflight requests" do
      security_config = %{
        validate_origin: true,
        allowed_origins: ["https://example.com"],
        cors: %{
          allowed_origins: ["https://example.com"],
          allowed_methods: ["POST"]
        }
      }

      plug_opts = create_plug(security_config)

      conn =
        conn(:options, "/")
        |> put_req_header("origin", "https://example.com")
        |> Map.put(:host, "api.example.com")
        |> put_req_header("access-control-request-method", "POST")
        |> HTTPServer.call(plug_opts)

      assert conn.status == 200
      assert get_resp_header(conn, "access-control-allow-origin") == ["https://example.com"]
      assert get_resp_header(conn, "access-control-allow-methods") == ["POST"]
    end

    test "rejects origin not in allowed list" do
      security_config = %{
        validate_origin: true,
        allowed_origins: ["https://example.com"]
      }

      plug_opts = create_plug(security_config)

      conn =
        conn(:post, "/", "{}")
        |> put_req_header("origin", "https://evil.com")
        |> Map.put(:host, "api.example.com")
        |> put_req_header("content-type", "application/json")
        |> HTTPServer.call(plug_opts)

      assert conn.status == 403
      assert String.contains?(conn.resp_body, "Origin not allowed")
    end
  end

  describe "security headers" do
    test "includes standard security headers" do
      plug_opts = create_plug(%{include_security_headers: true})

      conn =
        conn(:options, "/")
        |> put_req_header("origin", "http://localhost")
        |> Map.put(:host, "localhost")
        |> HTTPServer.call(plug_opts)

      # Check for security headers
      assert get_resp_header(conn, "x-content-type-options") == ["nosniff"]
      assert get_resp_header(conn, "x-frame-options") == ["DENY"]
      assert get_resp_header(conn, "x-xss-protection") == ["1; mode=block"]
      assert get_resp_header(conn, "strict-transport-security") != []
      assert get_resp_header(conn, "referrer-policy") == ["strict-origin-when-cross-origin"]
    end

    test "skips security headers when disabled" do
      plug_opts = create_plug(%{include_security_headers: false})

      conn =
        conn(:options, "/")
        |> put_req_header("origin", "http://localhost")
        |> Map.put(:host, "localhost")
        |> HTTPServer.call(plug_opts)

      assert get_resp_header(conn, "x-content-type-options") == []
      assert get_resp_header(conn, "x-frame-options") == []
    end
  end

  describe "origin validation" do
    test "validates origin header when required" do
      security_config = %{
        validate_origin: true,
        allowed_origins: ["https://example.com"]
      }

      plug_opts = create_plug(security_config)

      # Valid origin
      conn =
        conn(:post, "/", "{}")
        |> put_req_header("origin", "https://example.com")
        |> Map.put(:host, "api.example.com")
        |> put_req_header("content-type", "application/json")
        |> HTTPServer.call(plug_opts)

      # JSON parse error, but security passed
      assert conn.status == 400
    end

    test "requires origin header when validation enabled" do
      security_config = %{
        validate_origin: true,
        allowed_origins: ["https://example.com"]
      }

      plug_opts = create_plug(security_config)

      conn =
        conn(:post, "/", "{}")
        |> Map.put(:host, "api.example.com")
        |> put_req_header("content-type", "application/json")
        |> HTTPServer.call(plug_opts)

      assert conn.status == 400
      assert String.contains?(conn.resp_body, "Origin header required")
    end

    test "allows localhost without origin validation" do
      security_config = %{validate_origin: false}

      plug_opts = create_plug(security_config)

      conn =
        conn(:post, "/", "{}")
        |> Map.put(:host, "localhost")
        |> put_req_header("content-type", "application/json")
        |> HTTPServer.call(plug_opts)

      # Should pass security (fail on JSON parsing)
      assert conn.status == 400
      assert String.contains?(conn.resp_body, "Invalid")
    end
  end

  describe "HTTPS enforcement" do
    test "requires HTTPS for non-localhost when enforced" do
      security_config = %{
        enforce_https: true,
        # Focus on HTTPS test
        validate_origin: false
      }

      plug_opts = create_plug(security_config)

      conn =
        conn(:post, "/", "{}")
        |> Map.put(:host, "example.com")
        |> put_req_header("x-forwarded-proto", "http")
        |> put_req_header("content-type", "application/json")
        |> HTTPServer.call(plug_opts)

      assert conn.status == 400
      assert String.contains?(conn.resp_body, "HTTPS required")
    end

    test "allows HTTPS when enforced" do
      security_config = %{
        enforce_https: true,
        validate_origin: false
      }

      plug_opts = create_plug(security_config)

      conn =
        conn(:post, "/", "{}")
        |> Map.put(:host, "example.com")
        |> put_req_header("x-forwarded-proto", "https")
        |> put_req_header("content-type", "application/json")
        |> HTTPServer.call(plug_opts)

      # Should pass HTTPS check (fail on JSON parsing)
      assert conn.status == 400
      assert String.contains?(conn.resp_body, "Invalid")
    end

    test "allows HTTP for localhost" do
      security_config = %{
        enforce_https: true,
        validate_origin: false
      }

      plug_opts = create_plug(security_config)

      conn =
        conn(:post, "/", "{}")
        |> Map.put(:host, "localhost")
        |> put_req_header("content-type", "application/json")
        |> HTTPServer.call(plug_opts)

      # Should pass HTTPS check for localhost
      assert conn.status == 400
      assert String.contains?(conn.resp_body, "Invalid")
    end
  end

  describe "method handling" do
    test "handles OPTIONS method for CORS preflight" do
      plug_opts = create_plug(%{validate_origin: false})

      conn =
        conn(:options, "/")
        |> Map.put(:host, "localhost")
        |> HTTPServer.call(plug_opts)

      assert conn.status == 200
      assert conn.resp_body == ""
    end

    test "rejects unsupported methods" do
      plug_opts = create_plug(%{validate_origin: false})

      conn =
        conn(:put, "/")
        |> Map.put(:host, "localhost")
        |> HTTPServer.call(plug_opts)

      assert conn.status == 405
      assert String.contains?(conn.resp_body, "Method not allowed")
    end

    test "handles POST for MCP requests" do
      plug_opts = create_plug(%{validate_origin: false})

      valid_mcp = %{
        jsonrpc: "2.0",
        method: "initialize",
        params: %{
          protocolVersion: "2025-03-26",
          clientInfo: %{name: "test", version: "1.0"},
          capabilities: %{}
        },
        id: 1
      }

      conn =
        conn(:post, "/", Jason.encode!(valid_mcp))
        |> Map.put(:host, "localhost")
        |> put_req_header("content-type", "application/json")
        |> HTTPServer.call(plug_opts)

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["application/json"]

      # Response should be valid JSON
      assert {:ok, _response} = Jason.decode(conn.resp_body)
    end
  end

  describe "MCP message handling" do
    test "processes valid MCP initialize request" do
      plug_opts = create_plug(%{validate_origin: false})

      message = %{
        jsonrpc: "2.0",
        method: "initialize",
        params: %{
          protocolVersion: "2025-03-26",
          clientInfo: %{name: "test", version: "1.0"},
          capabilities: %{}
        },
        id: 1
      }

      conn =
        conn(:post, "/", Jason.encode!(message))
        |> Map.put(:host, "localhost")
        |> put_req_header("content-type", "application/json")
        |> HTTPServer.call(plug_opts)

      assert conn.status == 200

      {:ok, response} = Jason.decode(conn.resp_body)
      assert response["jsonrpc"] == "2.0"
      assert response["id"] == 1
      assert is_map(response["result"])
    end

    test "handles invalid JSON gracefully" do
      plug_opts = create_plug(%{validate_origin: false})

      conn =
        conn(:post, "/", "invalid json{")
        |> Map.put(:host, "localhost")
        |> put_req_header("content-type", "application/json")
        |> HTTPServer.call(plug_opts)

      assert conn.status == 400
      assert String.contains?(conn.resp_body, "Invalid")
    end
  end

  describe "DNS rebinding protection" do
    test "prevents classic DNS rebinding attack" do
      # Attacker controls evil.com to resolve to 127.0.0.1
      security_config = %{
        validate_origin: true,
        allowed_origins: ["https://trusted.com"]
      }

      plug_opts = create_plug(security_config)

      conn =
        conn(:post, "/", "{}")
        |> put_req_header("origin", "https://evil.com")
        |> Map.put(:host, "127.0.0.1:8080")
        |> put_req_header("content-type", "application/json")
        |> HTTPServer.call(plug_opts)

      assert conn.status == 403
      assert String.contains?(conn.resp_body, "Origin not allowed")
    end

    test "prevents subdomain DNS rebinding attack" do
      security_config = %{
        validate_origin: true,
        allowed_origins: ["https://trusted.com"]
      }

      plug_opts = create_plug(security_config)

      conn =
        conn(:post, "/", "{}")
        |> put_req_header("origin", "https://127.0.0.1.evil.com")
        |> Map.put(:host, "localhost:8080")
        |> put_req_header("content-type", "application/json")
        |> HTTPServer.call(plug_opts)

      assert conn.status == 403
      assert String.contains?(conn.resp_body, "Origin not allowed")
    end
  end
end
