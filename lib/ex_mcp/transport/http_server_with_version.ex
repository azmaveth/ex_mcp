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

  use Plug.Builder

  alias ExMCP.Transport.HTTPServer

  # Add the protocol version plug to the pipeline
  plug(ExMCP.Plugs.ProtocolVersion)

  @doc """
  Initialize with HTTPServer options.
  """
  def init(opts) do
    # This wrapper passes through all options to HTTPServer
    opts
  end

  @doc """
  Call implementation that forwards to HTTPServer after protocol validation.
  """
  def call(conn, opts) do
    # The protocol version plug has already run via the plug macro
    # Now forward to the HTTPServer
    HTTPServer.call(conn, HTTPServer.init(opts))
  end
end
