defmodule Arbor.MCP.Transport.HTTPServerWithVersion do
  @moduledoc deprecated:
               "Use Arbor.MCP.HttpPlug instead. Arbor.MCP.Transport.HTTPServerWithVersion will be removed in 2.0.0."
  @moduledoc """
  Example HTTP server configuration that includes protocol version validation.

  > #### Deprecated {: .error}
  >
  > This module wraps the deprecated `Arbor.MCP.Transport.HTTPServer` and will be
  > removed in 2.0.0. `Arbor.MCP.HttpPlug` validates the protocol version itself;
  > use it directly.

  This module demonstrates how to integrate the protocol version plug
  with the existing HTTP server transport.

  ## Usage Example

      # In your Phoenix router
      scope "/mcp" do
        forward "/", Arbor.MCP.Transport.HTTPServerWithVersion,
          handler: MyMCPHandler,
          security: %{
            validate_origin: true,
            allowed_origins: ["https://app.example.com"]
          }
      end

      # Or with Plug.Router
      defmodule MyRouter do
        use Plug.Router

        plug Arbor.MCP.Plugs.ProtocolVersion
        plug :match
        plug :dispatch

        forward "/mcp", to: Arbor.MCP.Transport.HTTPServer,
          init_opts: [handler: MyMCPHandler]
      end
  """

  alias Arbor.MCP.Plugs.ProtocolVersion
  alias Arbor.MCP.Transport.HTTPServer

  @behaviour Plug

  @deprecated "Use Arbor.MCP.HttpPlug instead. Arbor.MCP.Transport.HTTPServerWithVersion will be removed in 2.0.0."
  @doc """
  Initialize with HTTPServer options.
  """
  @impl Plug
  def init(opts) do
    opts
  end

  @deprecated "Use Arbor.MCP.HttpPlug instead. Arbor.MCP.Transport.HTTPServerWithVersion will be removed in 2.0.0."
  @doc """
  Call implementation that forwards to HTTPServer after protocol validation.
  """
  @impl Plug
  def call(conn, opts) do
    conn = ProtocolVersion.call(conn, ProtocolVersion.init([]))

    if conn.halted do
      conn
    else
      # Both modules are deprecated together; go through apply/3 so this
      # wrapper does not itself trip the deprecation warning under
      # --warnings-as-errors.
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      apply(HTTPServer, :call, [conn, apply(HTTPServer, :init, [opts])])
    end
  end
end
