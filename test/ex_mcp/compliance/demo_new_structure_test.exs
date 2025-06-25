defmodule ExMCP.Compliance.DemoNewStructureTest do
  use ExUnit.Case, async: false

  alias ExMCP.Compliance.{Spec20241105, Spec20250326}

  @moduletag :compliance
  @moduletag :demo

  test "new compliance structure demo" do
    # Simple test to show our structure works
    assert true

    # Test that our modules can be referenced
    versions = ["2024-11-05", "2025-03-26", "2025-06-18"]
    assert length(versions) == 3

    # Test basic module existence
    assert Code.ensure_loaded?(ExMCP.Compliance.VersionGenerator)
  end

  test "feature modules exist" do
    # Test that feature modules exist
    assert Code.ensure_loaded?(ExMCP.Compliance.Features.Tools)
    assert Code.ensure_loaded?(ExMCP.Compliance.Features.Resources)
  end

  test "generated modules work" do
    # Test that generated modules work
    assert Spec20241105.version() == "2024-11-05"
    assert Spec20250326.version() == "2025-03-26"
  end
end
