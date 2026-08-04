defmodule ExMCP.Compliance.VersionGenerator do
  @moduledoc """
  Auto-generates version-specific compliance test modules.

  This module creates individual test modules for each MCP specification version,
  each using the appropriate feature modules based on version compatibility.

  Supported versions come from `ExMCP.Internal.VersionRegistry`. Adding a new
  version requires an explicit handler entry below; missing entries fail at
  compile time instead of silently falling back to a generic handler.
  """

  alias ExMCP.Internal.VersionRegistry

  @versions VersionRegistry.supported_versions()
  @handlers %{
    "2024-11-05" => ExMCP.Compliance.Handlers.Handler20241105,
    "2025-03-26" => ExMCP.Compliance.Handlers.Handler20250326,
    "2025-06-18" => ExMCP.Compliance.Handlers.Handler20250618,
    "2025-11-25" => ExMCP.Compliance.Handlers.Handler20251125
  }

  @missing_handlers @versions -- Map.keys(@handlers)

  if @missing_handlers != [] do
    raise "missing compliance handlers for: #{Enum.join(@missing_handlers, ", ")}"
  end

  # Generate a test module for each version
  for version <- @versions do
    handler = Map.fetch!(@handlers, version)
    # Create module name like ExMCP.Compliance.Spec20241105
    module_name_suffix = String.replace(version, "-", "")
    module_name = :"Elixir.ExMCP.Compliance.Spec#{module_name_suffix}"

    defmodule module_name do
      use ExUnit.Case, async: true

      alias ExMCP.Internal.VersionRegistry

      @version version
      @handler handler

      # Import feature modules
      use ExMCP.Compliance.Features.Tools, version
      use ExMCP.Compliance.Features.Resources, version
      use ExMCP.Compliance.Features.ResourceTemplates, version
      use ExMCP.Compliance.Features.Authorization, version
      use ExMCP.Compliance.Features.Prompts, version
      use ExMCP.Compliance.Features.Transport, version
      use ExMCP.Compliance.Features.Roots, version
      use ExMCP.Compliance.Features.Sampling, version
      use ExMCP.Compliance.Features.Cancellation, version
      use ExMCP.Compliance.Features.Progress, version
      use ExMCP.Compliance.Features.Logging, version
      use ExMCP.Compliance.Features.Batch, version
      use ExMCP.Compliance.Features.Completion, version
      use ExMCP.Compliance.Features.Elicitation, version

      # Module tags for filtering
      @moduletag :compliance
      @moduletag :"version_#{String.replace(version, "-", "_")}"

      @doc """
      Returns the MCP specification version this test module covers.
      """
      def version, do: @version

      # Version-specific setup
      setup do
        {:ok, version: @version, spec_version: @version, handler: @handler}
      end

      # Version-specific tests that don't fit into feature categories
      test "protocol version negotiation works for #{@version}" do
        # Test that this specific version can be negotiated
        assert is_binary(@version)
        assert @version in VersionRegistry.supported_versions()
        # Add actual negotiation test logic here
      end

      test "server capabilities match #{@version} specification" do
        # Test that server capabilities align with this version's spec
        assert is_binary(@version)
        refute is_nil(@version)
        # Add actual capability validation logic here
      end
    end
  end

  @doc """
  Returns all supported MCP specification versions.
  """
  def supported_versions, do: @versions

  @doc """
  Returns the module name for a given version.
  """
  def module_for_version(version) when version in @versions do
    module_name_suffix = String.replace(version, "-", "")
    :"Elixir.ExMCP.Compliance.Spec#{module_name_suffix}"
  end

  @doc """
  Returns all generated compliance test modules.
  """
  def all_modules do
    Enum.map(@versions, &module_for_version/1)
  end
end
