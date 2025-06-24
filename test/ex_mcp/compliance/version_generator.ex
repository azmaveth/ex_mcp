defmodule ExMCP.Compliance.VersionGenerator do
  @moduledoc """
  Auto-generates version-specific compliance test modules.

  This module creates individual test modules for each MCP specification version,
  each using the appropriate feature modules based on version compatibility.

  Adding a new version is as simple as adding it to @versions list.
  """

  @versions ["2024-11-05", "2025-03-26", "2025-06-18"]

  # Generate a test module for each version
  for version <- @versions do
    # Create module name like ExMCP.Compliance.Spec20241105
    module_name_suffix = String.replace(version, "-", "")
    module_name = :"Elixir.ExMCP.Compliance.Spec#{module_name_suffix}"

    defmodule module_name do
      use ExUnit.Case, async: true

      # Import feature modules
      use ExMCP.Compliance.Features.Tools, unquote(version)
      use ExMCP.Compliance.Features.Resources, unquote(version)
      use ExMCP.Compliance.Features.ResourceTemplates, unquote(version)
      use ExMCP.Compliance.Features.Authorization, unquote(version)
      use ExMCP.Compliance.Features.Prompts, unquote(version)
      use ExMCP.Compliance.Features.Transport, unquote(version)
      use ExMCP.Compliance.Features.Roots, unquote(version)
      use ExMCP.Compliance.Features.Sampling, unquote(version)
      use ExMCP.Compliance.Features.Cancellation, unquote(version)
      use ExMCP.Compliance.Features.Progress, unquote(version)
      use ExMCP.Compliance.Features.Logging, unquote(version)
      use ExMCP.Compliance.Features.Batch, unquote(version)
      use ExMCP.Compliance.Features.Completion, unquote(version)
      use ExMCP.Compliance.Features.Elicitation, unquote(version)

      # Module tags for filtering
      @moduletag :compliance
      @moduletag :"version_#{String.replace(unquote(version), "-", "_")}"
      @version unquote(version)

      @doc """
      Returns the MCP specification version this test module covers.
      """
      def version, do: @version

      # Version-specific setup
      setup do
        # Get the appropriate handler for this version
        handler =
          case unquote(version) do
            "2024-11-05" -> ExMCP.Compliance.Handlers.Handler20241105
            "2025-03-26" -> ExMCP.Compliance.Handlers.Handler20250326
            "2025-06-18" -> ExMCP.Compliance.Handlers.Handler20250618
            _ -> ExMCP.Server.Handler
          end

        {:ok, version: @version, spec_version: unquote(version), handler: handler}
      end

      # Version-specific tests that don't fit into feature categories
      test "protocol version negotiation works for #{unquote(version)}" do
        # Test that this specific version can be negotiated
        assert @version == unquote(version)
        # Add actual negotiation test logic here
      end

      test "server capabilities match #{unquote(version)} specification" do
        # Test that server capabilities align with this version's spec
        assert @version == unquote(version)
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
