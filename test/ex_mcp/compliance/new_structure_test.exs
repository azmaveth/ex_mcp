defmodule ExMCP.Compliance.NewStructureTest do
  use ExUnit.Case, async: true

  alias ExMCP.Compliance.{
    Spec20241105,
    Spec20250326,
    Spec20250618,
    VersionGenerator
  }

  alias ExMCP.Compliance.Features.{
    Authorization,
    Prompts,
    Resources,
    Tools,
    Transport
  }

  @moduletag :compliance
  @moduletag :new_structure
  @moduletag :skip

  describe "new compliance test structure" do
    test "version generator module loads correctly" do
      # Test that we can load the version generator
      assert Code.ensure_loaded?(ExMCP.Compliance.VersionGenerator)

      # Test that it has the expected functions
      versions = VersionGenerator.supported_versions()
      assert is_list(versions)
      assert length(versions) == 3
    end

    test "feature modules load correctly" do
      # Test that all feature modules can be loaded
      feature_modules = [
        ExMCP.Compliance.Features.Tools,
        ExMCP.Compliance.Features.Resources,
        ExMCP.Compliance.Features.Authorization,
        ExMCP.Compliance.Features.Prompts,
        ExMCP.Compliance.Features.Transport
      ]

      for module <- feature_modules do
        assert Code.ensure_loaded?(module), "Failed to load #{module}"
      end
    end

    test "generated version modules exist" do
      # Test that the generated modules exist
      version_modules = [
        ExMCP.Compliance.Spec20241105,
        ExMCP.Compliance.Spec20250326,
        ExMCP.Compliance.Spec20250618
      ]

      for module <- version_modules do
        assert Code.ensure_loaded?(module), "Failed to load #{module}"
        assert function_exported?(module, :version, 0), "#{module} missing version/0 function"
      end
    end

    test "version modules return correct versions" do
      assert Spec20241105.version() == "2024-11-05"
      assert Spec20250326.version() == "2025-03-26"
      assert Spec20250618.version() == "2025-06-18"
    end
  end
end
