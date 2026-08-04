defmodule ExMCP.Protocol.VersionNegotiator do
  @moduledoc """
  Handles protocol version negotiation during the MCP initialization phase.

  This module is responsible for negotiating the protocol version between
  client and server during the initialization handshake, as specified in
  the MCP 2025-06-18 specification.
  """

  require Logger
  alias ExMCP.Internal.VersionRegistry

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
      |> Enum.filter(&(&1 in VersionRegistry.supported_versions()))
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
  def supported_versions, do: VersionRegistry.supported_versions()

  @doc """
  Get the latest supported protocol version.
  """
  @spec latest_version() :: String.t()
  def latest_version, do: VersionRegistry.latest_version()

  @doc """
  Check if a specific version is supported.
  """
  @spec supported?(String.t()) :: boolean()
  def supported?(version) when is_binary(version) do
    VersionRegistry.supported?(version)
  end

  def supported?(_), do: false

  @doc """
  Build a legacy initialize-result wrapper using the canonical capability registry.

  This function is retained as a 1.x compatibility shim. New code should use
  `ExMCP.Protocol.Initialize.build_initialize_result/2`; code that only needs
  the capability map should use `ExMCP.Server.Capabilities.build_capabilities/2`.
  """
  @deprecated "Use ExMCP.Protocol.Initialize or ExMCP.Server.Capabilities"
  @spec build_capabilities(String.t()) :: map()
  def build_capabilities(negotiated_version) do
    protocol_version =
      if VersionRegistry.supported?(negotiated_version),
        do: negotiated_version,
        else: VersionRegistry.preferred_version()

    %{
      protocolVersion: protocol_version,
      serverInfo: %{
        name: "ExMCP",
        version: Application.spec(:ex_mcp, :vsn) |> to_string()
      },
      capabilities: VersionRegistry.capabilities_for_version(protocol_version)
    }
  end

  # Private function to compare version strings
  # Later versions should sort first (descending order)
  defp version_compare(v1, v2) do
    v1 >= v2
  end
end
