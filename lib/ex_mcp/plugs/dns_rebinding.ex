defmodule ExMCP.Plugs.DnsRebinding do
  @moduledoc """
  Plug for DNS rebinding protection.

  Validates that the Host header is a localhost address, rejecting
  requests from non-localhost origins. This prevents DNS rebinding
  attacks where a malicious website redirects to localhost to access
  local MCP servers.

  ## Usage

      plug ExMCP.Plugs.DnsRebinding

  Or with custom allowed hosts:

      plug ExMCP.Plugs.DnsRebinding, allowed_hosts: ["localhost", "myhost.local"]

  """

  @behaviour Plug
  import Plug.Conn

  @default_allowed_hosts ["localhost", "127.0.0.1", "::1", "[::1]", "0.0.0.0"]

  @impl true
  def init(opts) do
    %{
      allowed_hosts:
        Keyword.get(opts, :allowed_hosts, @default_allowed_hosts)
        |> Enum.map(&String.downcase/1)
    }
  end

  @impl true
  def call(conn, opts) do
    host =
      conn
      |> get_req_header("host")
      |> List.first("")
      |> String.split(":")
      |> List.first()
      |> String.downcase()

    if host in opts.allowed_hosts do
      conn
    else
      conn
      |> put_resp_content_type("text/plain")
      |> send_resp(403, "Forbidden: Invalid Host header")
      |> halt()
    end
  end
end
