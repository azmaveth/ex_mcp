defmodule ExMCP.FeatureFlagsTest do
  use ExUnit.Case, async: true

  # Clean up application environment after each test to ensure isolation.
  setup do
    on_exit(fn ->
      Application.delete_env(:ex_mcp, :protocol_version_required)
      Application.delete_env(:ex_mcp, :structured_output_enabled)
      Application.delete_env(:ex_mcp, :oauth2_enabled)
    end)
  end

  describe "enabled?/1" do
    test "returns false for all features by default" do
      refute ExMCP.FeatureFlags.enabled?(:protocol_version_header)
      refute ExMCP.FeatureFlags.enabled?(:structured_output)
      refute ExMCP.FeatureFlags.enabled?(:oauth2_auth)
    end

    test "returns true for :protocol_version_header when enabled" do
      Application.put_env(:ex_mcp, :protocol_version_required, true)
      assert ExMCP.FeatureFlags.enabled?(:protocol_version_header)
    end

    test "returns true for :structured_output when enabled" do
      Application.put_env(:ex_mcp, :structured_output_enabled, true)
      assert ExMCP.FeatureFlags.enabled?(:structured_output)
    end

    test "returns true for :oauth2_auth when enabled" do
      Application.put_env(:ex_mcp, :oauth2_enabled, true)
      assert ExMCP.FeatureFlags.enabled?(:oauth2_auth)
    end

    test "returns false for an unknown feature flag" do
      refute ExMCP.FeatureFlags.enabled?(:some_unknown_feature)
    end
  end

  describe "all/0" do
    test "returns a map of all features with default values (false)" do
      expected_map = %{
        protocol_version_header: false,
        structured_output: false,
        oauth2_auth: false
      }

      assert ExMCP.FeatureFlags.all() == expected_map
    end

    test "returns a map reflecting enabled features" do
      Application.put_env(:ex_mcp, :protocol_version_required, true)
      Application.put_env(:ex_mcp, :oauth2_enabled, true)

      expected_map = %{
        protocol_version_header: true,
        structured_output: false,
        oauth2_auth: true
      }

      assert ExMCP.FeatureFlags.all() == expected_map
    end
  end
end
