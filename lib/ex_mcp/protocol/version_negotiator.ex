defmodule ExMCP.Protocol.VersionNegotiator do
  @moduledoc """
  Handles protocol version negotiation during the MCP initialization phase.

  This module is responsible for negotiating the protocol version between
  client and server during the initialization handshake, as specified in
  the MCP 2025-06-18 specification.
  """

  require Logger
  alias ExMCP.Authorization.ScopeValidator

  @supported_versions ["2025-06-18", "2025-03-26", "2024-11-05"]
  @latest_version "2025-06-18"

  @doc """
  Negotiate the protocol version based on client capabilities.

  Takes the client's supported versions and returns the best matching version
  that both client and server support.

  ## Parameters

  - `client_versions` - List of protocol versions supported by the client

  ## Returns

  - `{:ok, version}` - Successfully negotiated version
  - `{:error, :no_compatible_version}` - No compatible version found

  ## Examples

      iex> ExMCP.Protocol.VersionNegotiator.negotiate(["2025-06-18", "2025-03-26"])
      {:ok, "2025-06-18"}
      
      iex> ExMCP.Protocol.VersionNegotiator.negotiate(["2024-01-01"])
      {:error, :no_compatible_version}
  """
  @spec negotiate(list(String.t())) :: {:ok, String.t()} | {:error, :no_compatible_version}
  def negotiate(client_versions) when is_list(client_versions) do
    # Find the highest version that both client and server support
    compatible_versions =
      client_versions
      |> Enum.filter(&(&1 in @supported_versions))
      |> Enum.sort(&version_compare/2)

    case compatible_versions do
      [best_version | _] ->
        Logger.info("Protocol version negotiated: #{best_version}")
        {:ok, best_version}

      [] ->
        Logger.warning(
          "No compatible protocol version found. Client versions: #{inspect(client_versions)}"
        )

        {:error, :no_compatible_version}
    end
  end

  def negotiate(_), do: {:error, :no_compatible_version}

  @doc """
  Get the list of supported protocol versions.
  """
  @spec supported_versions() :: [String.t()]
  def supported_versions, do: @supported_versions

  @doc """
  Get the latest supported protocol version.
  """
  @spec latest_version() :: String.t()
  def latest_version, do: @latest_version

  @doc """
  Check if a specific version is supported.
  """
  @spec supported?(String.t()) :: boolean()
  def supported?(version) when is_binary(version) do
    version in @supported_versions
  end

  def supported?(_), do: false

  @doc """
  Build the server capabilities response including protocol version info.

  This is used during the initialization response to inform the client
  about server capabilities and the negotiated protocol version.
  """
  @spec build_capabilities(String.t()) :: map()
  def build_capabilities(negotiated_version) do
    base_capabilities = %{
      protocolVersion: negotiated_version,
      serverInfo: %{
        name: "ExMCP",
        version: Application.spec(:ex_mcp, :vsn) |> to_string()
      }
    }

    # Add version-specific capabilities
    capabilities =
      case negotiated_version do
        "2025-06-18" ->
          oauth2_capability =
            if ExMCP.FeatureFlags.enabled?(:oauth2_auth) do
              %{
                scopes_supported: ScopeValidator.get_all_static_scopes(),
                bearer_token_types_supported: ["bearer"],
                resource_server: "ExMCP"
              }
            else
              false
            end

          %{
            # Features added in 2025-06-18
            experimental: %{
              protocolVersionHeader: true,
              structuredOutput: ExMCP.FeatureFlags.enabled?(:structured_output),
              oauth2: oauth2_capability
            }
          }

        "2025-03-26" ->
          %{
            # Features available in 2025-03-26
            experimental: %{
              # Batch support was removed in 2025-06-18
              batchRequests: true
            }
          }

        "2024-11-05" ->
          %{
            # Basic features for 2024-11-05
            experimental: %{
              batchRequests: true
            }
          }

        _ ->
          %{}
      end

    Map.put(base_capabilities, :capabilities, capabilities)
  end

  # Private function to compare version strings
  # Later versions should sort first (descending order)
  defp version_compare(v1, v2) do
    v1 >= v2
  end
end
