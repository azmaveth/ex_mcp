defmodule ExMCP.VersionRegistry do
  @moduledoc """
  Registry for MCP protocol versions and their capabilities.

  This module manages protocol version differences and provides
  version-specific behavior for the MCP implementation.
  """

  @type version :: String.t()
  @type capability_key :: atom()
  @type feature :: atom()

  # Protocol versions in order of preference (newest first)
  @versions [
    {"draft", "Draft specification with experimental features"},
    {"2025-03-26", "Current stable specification"},
    {"2024-11-05", "Previous stable specification"}
  ]

  @doc """
  Get all supported protocol versions.
  """
  @spec supported_versions() :: [version()]
  def supported_versions do
    Enum.map(@versions, fn {version, _desc} -> version end)
  end

  @doc """
  Get the latest stable protocol version.
  """
  @spec latest_version() :: version()
  def latest_version, do: "2025-03-26"

  @doc """
  Get the preferred protocol version from configuration or default.
  """
  @spec preferred_version() :: version()
  def preferred_version do
    Application.get_env(:ex_mcp, :protocol_version, latest_version())
  end

  @doc """
  Check if a version is supported.
  """
  @spec supported?(version()) :: boolean()
  def supported?(version) do
    version in supported_versions()
  end

  @doc """
  Get capabilities available in a specific protocol version.
  """
  @spec capabilities_for_version(version()) :: %{capability_key() => any()}
  def capabilities_for_version("2024-11-05") do
    %{
      # Base capabilities available in 2024-11-05
      prompts: %{},
      resources: %{subscribe: false, listChanged: false},
      tools: %{},
      logging: %{},
      # No experimental features
      experimental: %{}
    }
  end

  def capabilities_for_version("2025-03-26") do
    %{
      # Enhanced capabilities in 2025-03-26
      prompts: %{listChanged: true},
      resources: %{subscribe: true, listChanged: true},
      tools: %{},
      logging: %{setLevel: true},
      completion: %{},
      # Batch processing available in 2025-03-26
      experimental: %{batchProcessing: true}
    }
  end

  def capabilities_for_version("draft") do
    %{
      # All capabilities from 2025-03-26
      prompts: %{listChanged: true},
      resources: %{subscribe: true, listChanged: true},
      tools: %{outputSchema: true},
      logging: %{setLevel: true},
      completion: %{},
      # Draft-specific experimental features
      experimental: %{
        elicitation: true,
        structuredContent: true,
        toolOutputSchema: true
      }
    }
  end

  def capabilities_for_version(_unknown), do: capabilities_for_version(latest_version())

  @doc """
  Check if a feature is available in a specific version.
  """
  @spec feature_available?(version(), feature()) :: boolean()
  def feature_available?(version, feature) do
    case feature do
      # Version-specific features
      :resource_subscription -> version in ["2025-03-26", "draft"]
      :prompts_list_changed -> version in ["2025-03-26", "draft"]
      :resources_list_changed -> version in ["2025-03-26", "draft"]
      :logging_set_level -> version in ["2025-03-26", "draft"]
      :completion -> version in ["2025-03-26", "draft"]
      # Version-specific features
      :batch_processing -> version == "2025-03-26"
      
      # Draft-only features
      :elicitation -> version == "draft"
      :structured_content -> version == "draft"
      :tool_output_schema -> version == "draft"
      # Base features available in all versions
      :prompts -> true
      :resources -> true
      :tools -> true
      :logging -> true
      _ -> false
    end
  end

  @doc """
  Get the message format differences for a version.
  """
  @spec message_format(version()) :: map()
  def message_format("2024-11-05") do
    %{
      # Basic message format
      supports_batch: false,
      supports_progress: true,
      supports_cancellation: true,
      notification_methods: [
        "notifications/initialized",
        "notifications/tools/list_changed",
        "notifications/resources/list_changed",
        "notifications/prompts/list_changed",
        "notifications/progress",
        "notifications/message",
        "notifications/cancelled"
      ]
    }
  end

  def message_format("2025-03-26") do
    %{
      # Enhanced message format
      supports_batch: false,
      supports_progress: true,
      supports_cancellation: true,
      notification_methods: [
        "notifications/initialized",
        "notifications/tools/list_changed",
        "notifications/resources/list_changed",
        "notifications/prompts/list_changed",
        "notifications/progress",
        "notifications/message",
        "notifications/cancelled",
        "notifications/resources/updated",
        "logging/setLevel"
      ]
    }
  end

  def message_format("draft") do
    %{
      # Full draft format with batch support
      supports_batch: true,
      supports_progress: true,
      supports_cancellation: true,
      notification_methods: [
        "notifications/initialized",
        "notifications/tools/list_changed",
        "notifications/resources/list_changed",
        "notifications/prompts/list_changed",
        "notifications/progress",
        "notifications/message",
        "notifications/cancelled",
        "notifications/resources/updated",
        "logging/setLevel"
      ]
    }
  end

  def message_format(_unknown), do: message_format(latest_version())

  @doc """
  Negotiate protocol version between client and server.

  Returns the best mutually supported version or an error.
  """
  @spec negotiate_version(version(), [version()]) ::
          {:ok, version()} | {:error, :version_mismatch}
  def negotiate_version(client_version, server_versions) do
    cond do
      # Exact match
      client_version in server_versions ->
        {:ok, client_version}

      # Client version is supported by us
      supported?(client_version) ->
        # Find best common version
        common_versions = Enum.filter(supported_versions(), &(&1 in server_versions))

        if common_versions != [] do
          {:ok, hd(common_versions)}
        else
          {:error, :version_mismatch}
        end

      # Unknown client version, propose our best that server supports
      true ->
        our_versions = supported_versions()
        common_versions = Enum.filter(our_versions, &(&1 in server_versions))

        if common_versions != [] do
          {:ok, hd(common_versions)}
        else
          {:error, :version_mismatch}
        end
    end
  end

  @doc """
  Get version-specific type module.

  This allows loading different type definitions based on protocol version.
  """
  @spec types_module(version()) :: module()
  def types_module("2024-11-05"), do: ExMCP.Types.V20241105
  def types_module("2025-03-26"), do: ExMCP.Types.V20250326
  def types_module("draft"), do: ExMCP.Types.Draft
  def types_module(_), do: ExMCP.Types
end

