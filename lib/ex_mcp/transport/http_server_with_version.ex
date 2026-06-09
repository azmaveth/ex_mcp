defmodule ExMCP.Transport.HTTPServerWithVersion do
  @moduledoc """
  Example HTTP server configuration that includes protocol version validation.

  This module demonstrates how to integrate the protocol version plug
  with the existing HTTP server transport.

  ## Usage Example

      # In your Phoenix router
      scope "/mcp" do
        forward "/", ExMCP.Transport.HTTPServerWithVersion,
          handler: MyMCPHandler,
          security: %{
            validate_origin: true,
            allowed_origins: ["https://app.example.com"]
          }
      end

      # Or with Plug.Router
      defmodule MyRouter do
        use Plug.Router

        plug ExMCP.Plugs.ProtocolVersion
        plug :match
        plug :dispatch

        forward "/mcp", to: ExMCP.Transport.HTTPServer,
          init_opts: [handler: MyMCPHandler]
      end
  """

  alias ExMCP.Plugs.ProtocolVersion
  alias ExMCP.Transport.HTTPServer

  @behaviour Plug

  @doc """
  Initialize with HTTPServer options.
  """
  @impl Plug
  def init(opts) do
    opts
  end

  @doc """
  Call implementation that forwards to HTTPServer after protocol validation.
  """
  @impl Plug
  def call(conn, opts) do
    conn = ProtocolVersion.call(conn, ProtocolVersion.init([]))

    if conn.halted do
      conn
    else
      HTTPServer.call(conn, HTTPServer.init(opts))
    end
  end
end
