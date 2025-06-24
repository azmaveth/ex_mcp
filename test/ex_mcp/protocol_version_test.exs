defmodule ExMCP.ProtocolVersionTest do
  @moduledoc """
  Tests for MCP Protocol Version header validation.

  Tests the implementation of the 2025-06-18 specification requirement
  that HTTP transport must include MCP-Protocol-Version headers.
  """
  use ExUnit.Case, async: true

  import Plug.Test
  import Plug.Conn

  alias ExMCP.HttpPlug

  setup do
    # Enable protocol version header validation for tests
    original_value = Application.get_env(:ex_mcp, :protocol_version_required)
    Application.put_env(:ex_mcp, :protocol_version_required, true)

    on_exit(fn ->
      if original_value do
        Application.put_env(:ex_mcp, :protocol_version_required, original_value)
      else
        Application.delete_env(:ex_mcp, :protocol_version_required)
      end
    end)

    # Basic test handler
    handler = fn request ->
      case request do
        %{"method" => "test/echo", "params" => params} ->
          {:ok, %{"jsonrpc" => "2.0", "result" => params, "id" => request["id"]}}

        _ ->
          {:error, :method_not_found}
      end
    end

    opts = %{
      handler: handler,
      server_info: %{name: "test-server", version: "1.0.0"},
      cors_enabled: false,
      sse_enabled: false,
      oauth_enabled: false,
      auth_config: %{}
    }

    {:ok, opts: opts}
  end

  describe "request validation" do
    test "accepts requests with correct protocol version header", %{opts: opts} do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "test/echo",
        "params" => %{"message" => "hello"},
        "id" => 1
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_req_header("mcp-protocol-version", "2025-06-18")
        |> HttpPlug.call(opts)

      assert conn.status == 200

      response = Jason.decode!(conn.resp_body)
      assert response["result"] == %{"message" => "hello"}
    end

    test "rejects requests with incorrect protocol version", %{opts: opts} do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "test/echo",
        "params" => %{"message" => "hello"},
        "id" => 1
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_req_header("mcp-protocol-version", "2024-01-01")
        |> HttpPlug.call(opts)

      assert conn.status == 400

      response = Jason.decode!(conn.resp_body)
      assert response["error"]["code"] == -32600
      assert response["error"]["message"] =~ "Unsupported MCP-Protocol-Version: 2024-01-01"
      assert response["error"]["data"]["expectedVersion"] == "2025-06-18"
    end

    test "rejects requests missing protocol version header", %{opts: opts} do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "test/echo",
        "params" => %{"message" => "hello"},
        "id" => 1
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(opts)

      assert conn.status == 400

      response = Jason.decode!(conn.resp_body)
      assert response["error"]["code"] == -32600
      assert response["error"]["message"] =~ "Missing MCP-Protocol-Version header"
      assert response["error"]["data"]["expectedVersion"] == "2025-06-18"
    end
  end

  describe "response headers" do
    test "includes protocol version header in successful responses", %{opts: opts} do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "test/echo",
        "params" => %{"message" => "hello"},
        "id" => 1
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_req_header("mcp-protocol-version", "2025-06-18")
        |> HttpPlug.call(opts)

      assert conn.status == 200
      assert get_resp_header(conn, "mcp-protocol-version") == ["2025-06-18"]
    end

    test "includes protocol version header in error responses", %{opts: opts} do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "test/echo",
        "params" => %{"message" => "hello"},
        "id" => 1
      }

      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> put_req_header("mcp-protocol-version", "wrong-version")
        |> HttpPlug.call(opts)

      assert conn.status == 400
      assert get_resp_header(conn, "mcp-protocol-version") == ["2025-06-18"]
    end
  end

  describe "feature flag" do
    test "bypasses validation when feature flag is disabled", %{opts: opts} do
      # Disable the feature flag
      Application.put_env(:ex_mcp, :protocol_version_required, false)

      request = %{
        "jsonrpc" => "2.0",
        "method" => "test/echo",
        "params" => %{"message" => "hello"},
        "id" => 1
      }

      # Request without protocol version header should still work
      conn =
        conn(:post, "/", Jason.encode!(request))
        |> put_req_header("content-type", "application/json")
        |> HttpPlug.call(opts)

      assert conn.status == 200
      response = Jason.decode!(conn.resp_body)
      assert response["result"] == %{"message" => "hello"}

      # Response should not include protocol version header
      assert get_resp_header(conn, "mcp-protocol-version") == []
    end
  end

  describe "CORS integration" do
    test "includes mcp-protocol-version in allowed headers", %{opts: opts} do
      cors_opts = Map.put(opts, :cors_enabled, true)

      conn =
        conn(:options, "/")
        |> HttpPlug.call(cors_opts)

      assert conn.status == 200
      allowed_headers = get_resp_header(conn, "access-control-allow-headers")
      assert length(allowed_headers) == 1
      assert allowed_headers |> hd() |> String.contains?("mcp-protocol-version")
    end
  end
end
