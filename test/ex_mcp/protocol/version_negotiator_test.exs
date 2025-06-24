defmodule ExMCP.Protocol.VersionNegotiatorTest do
  use ExUnit.Case, async: true

  alias ExMCP.Protocol.VersionNegotiator

  describe "negotiate/1" do
    test "returns the highest mutually supported version" do
      assert {:ok, "2025-06-18"} = VersionNegotiator.negotiate(["2025-06-18", "2025-03-26"])
      assert {:ok, "2025-06-18"} = VersionNegotiator.negotiate(["2025-03-26", "2025-06-18"])
    end

    test "returns 2025-03-26 when it's the only supported version" do
      assert {:ok, "2025-03-26"} = VersionNegotiator.negotiate(["2025-03-26", "2024-01-01"])
      assert {:ok, "2025-03-26"} = VersionNegotiator.negotiate(["2025-03-26"])
    end

    test "returns 2024-11-05 when it's the only supported version" do
      assert {:ok, "2024-11-05"} = VersionNegotiator.negotiate(["2024-11-05", "2024-01-01"])
      assert {:ok, "2024-11-05"} = VersionNegotiator.negotiate(["2024-11-05"])
    end

    test "prefers newer versions over older ones" do
      assert {:ok, "2025-06-18"} =
               VersionNegotiator.negotiate(["2024-11-05", "2025-06-18", "2025-03-26"])

      assert {:ok, "2025-03-26"} = VersionNegotiator.negotiate(["2024-11-05", "2025-03-26"])
    end

    test "returns error when no compatible version found" do
      assert {:error, :no_compatible_version} = VersionNegotiator.negotiate(["2024-01-01"])
      assert {:error, :no_compatible_version} = VersionNegotiator.negotiate(["invalid"])
      assert {:error, :no_compatible_version} = VersionNegotiator.negotiate([])
    end

    test "handles invalid input" do
      assert {:error, :no_compatible_version} = VersionNegotiator.negotiate(nil)
      assert {:error, :no_compatible_version} = VersionNegotiator.negotiate("not a list")
      assert {:error, :no_compatible_version} = VersionNegotiator.negotiate(%{})
    end

    test "filters out unsupported versions" do
      client_versions = ["2025-06-18", "2024-12-31", "2025-03-26", "2023-01-01"]
      assert {:ok, "2025-06-18"} = VersionNegotiator.negotiate(client_versions)
    end

    test "handles duplicate versions in client list" do
      assert {:ok, "2025-06-18"} =
               VersionNegotiator.negotiate(["2025-06-18", "2025-06-18", "2025-03-26"])
    end
  end

  describe "supported_versions/0" do
    test "returns the correct list of supported versions" do
      versions = VersionNegotiator.supported_versions()
      assert is_list(versions)
      assert "2025-06-18" in versions
      assert "2025-03-26" in versions
      assert "2024-11-05" in versions
      assert length(versions) == 3
    end
  end

  describe "latest_version/0" do
    test "returns the latest version" do
      assert VersionNegotiator.latest_version() == "2025-06-18"
    end
  end

  describe "supported?/1" do
    test "returns true for supported versions" do
      assert VersionNegotiator.supported?("2025-06-18")
      assert VersionNegotiator.supported?("2025-03-26")
      assert VersionNegotiator.supported?("2024-11-05")
    end

    test "returns false for unsupported versions" do
      refute VersionNegotiator.supported?("2024-01-01")
      refute VersionNegotiator.supported?("invalid")
      refute VersionNegotiator.supported?("")
    end

    test "returns false for non-string input" do
      refute VersionNegotiator.supported?(nil)
      refute VersionNegotiator.supported?(123)
      refute VersionNegotiator.supported?(:atom)
      refute VersionNegotiator.supported?([])
    end
  end

  describe "build_capabilities/1" do
    test "builds capabilities for 2025-06-18" do
      capabilities = VersionNegotiator.build_capabilities("2025-06-18")

      assert capabilities.protocolVersion == "2025-06-18"
      assert capabilities.serverInfo.name == "ExMCP"
      assert is_binary(capabilities.serverInfo.version)
      assert capabilities.capabilities.experimental.protocolVersionHeader == true
      # These depend on feature flags
      assert is_boolean(capabilities.capabilities.experimental.structuredOutput)
      assert is_boolean(capabilities.capabilities.experimental.oauth2)
    end

    test "builds capabilities for 2025-03-26" do
      capabilities = VersionNegotiator.build_capabilities("2025-03-26")

      assert capabilities.protocolVersion == "2025-03-26"
      assert capabilities.serverInfo.name == "ExMCP"
      assert capabilities.capabilities.experimental.batchRequests == true
    end

    test "builds capabilities for 2024-11-05" do
      capabilities = VersionNegotiator.build_capabilities("2024-11-05")

      assert capabilities.protocolVersion == "2024-11-05"
      assert capabilities.serverInfo.name == "ExMCP"
      assert capabilities.capabilities.experimental.batchRequests == true
    end

    test "includes application version" do
      capabilities = VersionNegotiator.build_capabilities("2025-06-18")

      # The version should be a string (from mix.exs)
      assert is_binary(capabilities.serverInfo.version)
      # Should not be nil or empty
      assert capabilities.serverInfo.version != ""
      assert capabilities.serverInfo.version != nil
    end
  end
end
