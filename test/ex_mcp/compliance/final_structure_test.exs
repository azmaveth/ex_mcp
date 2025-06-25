defmodule ExMCP.Compliance.FinalStructureTest do
  use ExUnit.Case, async: false

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
  @moduletag :final_validation

  describe "new compliance test structure validation" do
    test "all version modules are generated and functional" do
      # Test that all version modules exist
      assert Code.ensure_loaded?(ExMCP.Compliance.Spec20241105)
      assert Code.ensure_loaded?(ExMCP.Compliance.Spec20250326)
      assert Code.ensure_loaded?(ExMCP.Compliance.Spec20250618)

      # Test that they return correct versions
      assert Spec20241105.version() == "2024-11-05"
      assert Spec20250326.version() == "2025-03-26"
      assert Spec20250618.version() == "2025-06-18"
    end

    test "all feature modules are loaded and functional" do
      # Test that all feature modules exist
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

    test "version generator provides correct information" do
      versions = VersionGenerator.supported_versions()
      assert length(versions) == 3
      assert "2024-11-05" in versions
      assert "2025-03-26" in versions
      assert "2025-06-18" in versions

      # Test module mapping
      assert VersionGenerator.module_for_version("2024-11-05") ==
               Spec20241105

      assert VersionGenerator.module_for_version("2025-03-26") ==
               Spec20250326

      assert VersionGenerator.module_for_version("2025-06-18") ==
               Spec20250618
    end

    test "feature test functions are accessible" do
      # Test that feature test functions exist and are callable
      # Tools
      assert function_exported?(Tools, :test_basic_tools_list, 1)
      assert function_exported?(Tools, :test_tool_annotations, 1)

      # Resources
      assert function_exported?(
               Resources,
               :test_basic_resources_list,
               1
             )

      assert function_exported?(
               Resources,
               :test_resource_subscriptions,
               1
             )

      # Authorization (2025-03-26+ only)
      assert function_exported?(
               Authorization,
               :test_oauth_authorization,
               1
             )

      assert function_exported?(
               Authorization,
               :test_authorization_metadata,
               1
             )

      # Transport
      assert function_exported?(Transport, :test_jsonrpc_format, 1)
      assert function_exported?(Transport, :test_batch_processing, 1)

      # Prompts
      assert function_exported?(Prompts, :test_basic_prompts_list, 1)
    end

    test "version-specific features are properly gated" do
      # Test that version-specific features only run for appropriate versions

      # Authorization should only work for 2025-03-26+
      assert_raise FunctionClauseError, fn ->
        Authorization.test_oauth_authorization("2024-11-05")
      end

      # But should work for 2025-03-26+
      Authorization.test_oauth_authorization("2025-03-26")
      Authorization.test_oauth_authorization("2025-06-18")

      # Batch processing should only work for 2025-03-26+
      assert_raise FunctionClauseError, fn ->
        Transport.test_batch_processing("2024-11-05")
      end

      # But should work for 2025-03-26+
      Transport.test_batch_processing("2025-03-26")
      Transport.test_batch_processing("2025-06-18")
    end

    test "basic feature tests work for all versions" do
      versions = ["2024-11-05", "2025-03-26", "2025-06-18"]

      for version <- versions do
        # These should work for all versions
        Tools.test_basic_tools_list(version)
        Resources.test_basic_resources_list(version)
        Transport.test_jsonrpc_format(version)
        Prompts.test_basic_prompts_list(version)
      end
    end
  end

  describe "compliance test organization" do
    test "tests are properly tagged for filtering" do
      # This test itself should have compliance tags
      assert @moduletag[:compliance] == :compliance
      assert @moduletag[:final_validation] == :final_validation
    end
  end
end
