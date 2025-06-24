defmodule ExMCP.HttpPlugOAuthTest do
  use ExUnit.Case, async: false
  use Plug.Test

  import Mox

  alias ExMCP.HttpPlug

  # Mock for ServerGuard dependency
  defmock(ServerGuardMock, for: ExMCP.Authorization.ServerGuard)

  # A dummy handler for the plug
  defmodule DummyHandler do
    def process(_request, _context) do
      %{
        "jsonrpc" => "2.0",
        "result" => %{"status" => "ok"},
        "id" => "1"
      }
    end
  end

  @server_info %{name: "test-server", version: "1.0.0"}
  @auth_config %{introspection_endpoint: "https://auth.example.com/introspect"}

  @opts_oauth_disabled [
    handler: DummyHandler,
    server_info: @server_info,
    sse_enabled: false
  ]

  @opts_oauth_enabled [
    handler: DummyHandler,
    server_info: @server_info,
    sse_enabled: false,
    oauth_enabled: true,
    auth_config: @auth_config
  ]

  setup do
    # By default, stub ServerGuard to succeed
    stub(ServerGuardMock, :authorize, fn _headers, _scopes, _config ->
      {:ok, %{scope: "mcp:tools:execute"}}
    end)

    # By default, disable protocol version header requirement
    Application.put_env(:ex_mcp, :protocol_version_required, false)

    on_exit(fn ->
      Application.delete_env(:ex_mcp, :protocol_version_required)
    end)
  end

  describe "OAuth enabled vs disabled" do
    test "allows POST request without token when oauth is disabled" do
      conn =
        conn(:post, "/", ~s({"jsonrpc": "2.0", "method": "test", "id": "1"}))
        |> put_req_header("content-type", "application/json")

      conn = HttpPlug.call(conn, HttpPlug.init(@opts_oauth_disabled))

      assert conn.status == 200
      assert Jason.decode!(conn.resp_body)["result"] == %{"status" => "ok"}
    end

    test "rejects POST request without token when oauth is enabled" do
      # Mock ServerGuard to return a specific error for missing token
      error_response =
        {401, ~s(Bearer realm="test-server", error="invalid_request"),
         ~s({"error": "invalid_request"})}

      expect(ServerGuardMock, :authorize, fn _, _, _ -> {:error, error_response} end)

      conn =
        conn(:post, "/", ~s({"jsonrpc": "2.0", "method": "test", "id": "1"}))
        |> put_req_header("content-type", "application/json")

      conn = HttpPlug.call(conn, HttpPlug.init(@opts_oauth_enabled))

      assert conn.status == 401
      assert get_resp_header(conn, "www-authenticate") == [error_response |> elem(1)]
      assert conn.resp_body == error_response |> elem(2)
    end
  end

  describe "Protocol Version Header" do
    test "rejects request with missing version header when required" do
      Application.put_env(:ex_mcp, :protocol_version_required, true)

      conn =
        conn(:post, "/", ~s({"jsonrpc": "2.0", "method": "test", "id": "1"}))
        |> put_req_header("content-type", "application/json")

      conn = HttpPlug.call(conn, HttpPlug.init(@opts_oauth_disabled))

      assert conn.status == 400
      body = Jason.decode!(conn.resp_body)
      assert body["error"]["code"] == -32600
      assert String.contains?(body["error"]["message"], "Missing MCP-Protocol-Version header")
    end

    test "accepts request with correct version header when required" do
      Application.put_env(:ex_mcp, :protocol_version_required, true)

      conn =
        conn(:post, "/", ~s({"jsonrpc": "2.0", "method": "test", "id": "1"}))
        |> put_req_header("content-type", "application/json")
        |> put_req_header("mcp-protocol-version", "2025-06-18")

      conn = HttpPlug.call(conn, HttpPlug.init(@opts_oauth_disabled))
      assert conn.status == 200
    end
  end

  describe "Well-known resource endpoint" do
    test "GET /.well-known/oauth-protected-resource returns metadata when enabled" do
      conn = conn(:get, "/.well-known/oauth-protected-resource")
      conn = HttpPlug.call(conn, HttpPlug.init(@opts_oauth_enabled))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["application/json"]
      body = Jason.decode!(conn.resp_body)
      assert body["resource"] == "test-server"
      assert "mcp:tools:list" in body["scopes_supported"]
    end

    test "GET /.well-known/oauth-protected-resource returns 404 when disabled" do
      conn = conn(:get, "/.well-known/oauth-protected-resource")
      conn = HttpPlug.call(conn, HttpPlug.init(@opts_oauth_disabled))
      assert conn.status == 404
    end
  end

  describe "Authorization for different request types" do
    test "authorizes POST request with valid token" do
      expect(ServerGuardMock, :authorize, fn headers, scopes, config ->
        assert {"authorization", "Bearer valid-token"} in headers
        assert "mcp:tools:execute:my_tool" in scopes
        assert config.realm == "test-server"
        {:ok, %{}}
      end)

      request_body =
        ~s({"jsonrpc": "2.0", "method": "tools/execute", "params": {"tool_name": "my_tool"}, "id": "1"})

      conn =
        conn(:post, "/", request_body)
        |> put_req_header("content-type", "application/json")
        |> put_req_header("authorization", "Bearer valid-token")

      conn = HttpPlug.call(conn, HttpPlug.init(@opts_oauth_enabled))
      assert conn.status == 200
    end

    test "authorizes SSE connection with valid token" do
      # Enable SSE and set test mode
      opts =
        @opts_oauth_enabled
        |> Keyword.put(:sse_enabled, true)

      Application.put_env(:ex_mcp, :test_mode, true)

      expect(ServerGuardMock, :authorize, fn headers, _scopes, _config ->
        assert {"authorization", "Bearer valid-token"} in headers
        {:ok, %{}}
      end)

      conn =
        conn(:get, "/sse")
        |> put_req_header("authorization", "Bearer valid-token")

      conn = HttpPlug.call(conn, HttpPlug.init(opts))

      # In test mode, the plug sends a chunk and returns.
      assert conn.status == 200
      assert conn.state == :sent
      assert conn.resp_body =~ "event: connected"

      Application.delete_env(:ex_mcp, :test_mode)
    end

    test "rejects SSE connection with invalid token" do
      opts =
        @opts_oauth_enabled
        |> Keyword.put(:sse_enabled, true)

      error_response =
        {401, ~s(Bearer realm="test-server", error="invalid_token"),
         ~s({"error": "invalid_token"})}

      expect(ServerGuardMock, :authorize, fn _, _, _ -> {:error, error_response} end)

      conn =
        conn(:get, "/sse")
        |> put_req_header("authorization", "Bearer invalid-token")

      conn = HttpPlug.call(conn, HttpPlug.init(opts))

      assert conn.status == 401
      assert get_resp_header(conn, "www-authenticate") == [error_response |> elem(1)]
      assert conn.resp_body == error_response |> elem(2)
    end
  end
end
