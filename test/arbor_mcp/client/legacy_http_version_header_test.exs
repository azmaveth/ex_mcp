defmodule Arbor.MCP.Client.LegacyHTTPVersionHeaderTest do
  @moduledoc """
  The HTTP transport's `MCP-Protocol-Version` header must name the version
  the server selected in `initialize` from the first message after the
  handshake, `notifications/initialized` included. A strict server rejects a
  header that disagrees with the negotiated version.
  """

  use ExUnit.Case, async: false

  alias Arbor.MCP.Client

  @negotiated "2025-03-26"

  setup do
    bypass = Bypass.open()
    test_pid = self()

    Bypass.expect(bypass, "POST", "/mcp", fn conn ->
      {:ok, body, conn} = Plug.Conn.read_body(conn)
      message = Jason.decode!(body)
      header = Plug.Conn.get_req_header(conn, "mcp-protocol-version")
      send(test_pid, {:strict_server, message["method"], header})
      respond(conn, message, header)
    end)

    %{bypass: bypass}
  end

  for mode <- [:legacy_only, :prefer_legacy, :prefer_modern] do
    test "notifications/initialized carries the negotiated version in #{mode}", %{bypass: bypass} do
      assert {:ok, client} =
               Client.start_link(
                 transport: :http,
                 url: "http://127.0.0.1:#{bypass.port}/mcp",
                 use_sse: false,
                 protocol_mode: unquote(mode),
                 health_check_interval: nil,
                 reconnect: false
               )

      assert_receive {:strict_server, "initialize", _requested}, 2_000
      assert_receive {:strict_server, "notifications/initialized", [@negotiated]}, 2_000
      assert {:ok, @negotiated} = Client.negotiated_version(client)

      assert {:ok, _pong} = Client.ping(client)
      assert_receive {:strict_server, "ping", [@negotiated]}, 2_000

      Client.stop(client)
    end
  end

  test "modern_only never falls back to initialize against the strict older server", %{
    bypass: bypass
  } do
    Process.flag(:trap_exit, true)

    assert {:error, _reason} =
             Client.start_link(
               transport: :http,
               url: "http://127.0.0.1:#{bypass.port}/mcp",
               use_sse: false,
               protocol_mode: :modern_only,
               health_check_interval: nil,
               reconnect: false
             )

    assert_receive {:strict_server, "server/discover", _header}, 2_000
    refute_received {:strict_server, "initialize", _header}
  end

  defp respond(conn, %{"method" => "initialize", "id" => id}, _header) do
    json(conn, 200, %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{
        "protocolVersion" => @negotiated,
        "capabilities" => %{},
        "serverInfo" => %{"name" => "strict-older-server", "version" => "1"}
      }
    })
  end

  defp respond(conn, %{"method" => _method} = message, header) when header != [@negotiated] do
    json(conn, 400, %{
      "jsonrpc" => "2.0",
      "id" => message["id"],
      "error" => %{"code" => -32600, "message" => "Unsupported MCP-Protocol-Version"}
    })
  end

  defp respond(conn, %{"method" => "notifications/initialized"}, _header) do
    Plug.Conn.resp(conn, 202, "")
  end

  defp respond(conn, %{"method" => "ping", "id" => id}, _header) do
    json(conn, 200, %{"jsonrpc" => "2.0", "id" => id, "result" => %{}})
  end

  defp json(conn, status, body) do
    conn
    |> Plug.Conn.put_resp_content_type("application/json")
    |> Plug.Conn.resp(status, Jason.encode!(body))
  end
end
