defmodule ExMCP.Plugs.ProtocolVersion do
  @moduledoc """
  Plug for validating and extracting the MCP-Protocol-Version header.

  This plug ensures that incoming HTTP requests include a valid protocol
  version header as required by the MCP 2025-06-18 specification.

  ## Behavior

  - If no header is present, defaults to "2025-06-18" (per spec)
  - If an unsupported version is provided, returns 400 Bad Request
  - Adds the validated version to conn.assigns[:mcp_version]

  ## Usage

      plug ExMCP.Plugs.ProtocolVersion
  """

  import Plug.Conn
  require Logger

  @behaviour Plug

  @supported_versions ["2025-06-18", "2025-03-26", "2024-11-05"]
  @default_version "2025-06-18"

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    if ExMCP.FeatureFlags.enabled?(:protocol_version_header) do
      validate_protocol_version(conn)
    else
      # When feature is disabled, just set default version
      assign(conn, :mcp_version, @default_version)
    end
  end

  defp validate_protocol_version(conn) do
    case get_req_header(conn, "mcp-protocol-version") do
      [] ->
        Logger.debug("No MCP-Protocol-Version header found, using default: #{@default_version}")
        assign(conn, :mcp_version, @default_version)

      [version] when version in @supported_versions ->
        Logger.debug("Valid MCP-Protocol-Version: #{version}")
        assign(conn, :mcp_version, version)

      [invalid_version] ->
        Logger.warning("Invalid MCP-Protocol-Version: #{invalid_version}")

        error_response = %{
          jsonrpc: "2.0",
          error: %{
            code: -32600,
            message: "Invalid Request",
            data: %{
              reason: "Unsupported protocol version: #{invalid_version}",
              supported_versions: @supported_versions
            }
          }
        }

        conn
        |> put_resp_content_type("application/json")
        |> send_resp(400, Jason.encode!(error_response))
        |> halt()
    end
  end

  @doc """
  Get the list of supported protocol versions.
  """
  @spec supported_versions() :: [String.t()]
  def supported_versions, do: @supported_versions

  @doc """
  Get the default protocol version.
  """
  @spec default_version() :: String.t()
  def default_version, do: @default_version
end
