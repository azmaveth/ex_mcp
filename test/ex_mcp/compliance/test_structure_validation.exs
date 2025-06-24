defmodule ExMCP.Compliance.StructureValidationTest do
  use ExUnit.Case, async: true

  @moduletag :compliance
  @moduletag :structure_validation

  test "version generator creates all expected modules" do
    versions = ExMCP.Compliance.VersionGenerator.supported_versions()
    assert length(versions) == 3
    assert "2024-11-05" in versions
    assert "2025-03-26" in versions
    assert "2025-06-18" in versions
  end

  test "generated modules exist and are accessible" do
    modules = ExMCP.Compliance.VersionGenerator.all_modules()

    for module <- modules do
      # Check that module exists
      assert Code.ensure_loaded?(module)

      # Check that module has version function
      assert function_exported?(module, :version, 0)
    end
  end

  test "version-specific modules have correct versions" do
    assert ExMCP.Compliance.Spec20241105.version() == "2024-11-05"
    assert ExMCP.Compliance.Spec20250326.version() == "2025-03-26"
    assert ExMCP.Compliance.Spec20250618.version() == "2025-06-18"
  end

  test "feature modules are properly imported" do
    # Test that feature modules exist
    assert Code.ensure_loaded?(ExMCP.Compliance.Features.Tools)
    assert Code.ensure_loaded?(ExMCP.Compliance.Features.Resources)
    assert Code.ensure_loaded?(ExMCP.Compliance.Features.Authorization)
    assert Code.ensure_loaded?(ExMCP.Compliance.Features.Prompts)
    assert Code.ensure_loaded?(ExMCP.Compliance.Features.Transport)
  end
end
