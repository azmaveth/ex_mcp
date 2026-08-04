defmodule ExMCP.Plugs.ProtocolVersion do
  @moduledoc """
  Plug for validating and extracting the MCP-Protocol-Version header.

  This plug ensures that incoming HTTP requests carry a protocol version the
  server actually supports.

  ## Behavior

  - If no header is present, the configured default
    (`ExMCP.Internal.VersionRegistry.preferred_version/0`) is assumed
  - If an unsupported version is provided, returns 400 Bad Request
  - Adds the validated version to conn.assigns[:mcp_version]

  The supported list and the default both come from
  `ExMCP.Internal.VersionRegistry`, which is the single source of truth for
  protocol versions across the client, the server transports, and this plug.

  ## Usage

      plug ExMCP.Plugs.ProtocolVersion
  """

  import Plug.Conn
  require Logger

  alias ExMCP.Internal.VersionRegistry

  @behaviour Plug

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    if ExMCP.FeatureFlags.enabled?(:protocol_version_header) do
      validate_protocol_version(conn)
    else
      # When feature is disabled, just set default version
      assign(conn, :mcp_version, default_version())
    end
  end

  defp validate_protocol_version(conn) do
    default = default_version()

    case get_req_header(conn, "mcp-protocol-version") do
      [] ->
        Logger.debug("No MCP-Protocol-Version header found, using default: #{default}")
        assign(conn, :mcp_version, default)

      [version | _] ->
        if VersionRegistry.supported?(version) do
          Logger.debug("Valid MCP-Protocol-Version: #{version}")
          assign(conn, :mcp_version, version)
        else
          reject_version(conn, version)
        end
    end
  end

  defp reject_version(conn, invalid_version) do
    Logger.warning("Invalid MCP-Protocol-Version: #{invalid_version}")

    error_response = %{
      jsonrpc: "2.0",
      error: %{
        code: -32600,
        message: "Invalid Request",
        data: %{
          reason: "Unsupported protocol version: #{invalid_version}",
          supported_versions: supported_versions()
        }
      }
    }

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(400, Jason.encode!(error_response))
    |> halt()
  end

  @doc """
  Get the list of supported protocol versions.
  """
  @spec supported_versions() :: [String.t()]
  defdelegate supported_versions, to: VersionRegistry

  @doc """
  Get the default protocol version.
  """
  @spec default_version() :: String.t()
  defdelegate default_version, to: VersionRegistry, as: :preferred_version
end
