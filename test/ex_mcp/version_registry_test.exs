defmodule ExMCP.VersionRegistryTest do
  use ExUnit.Case

  alias ExMCP.{Protocol}
  alias ExMCP.Internal.VersionRegistry

  describe "version registry" do
    test "lists supported versions in order" do
      versions = VersionRegistry.supported_versions()
      assert "2025-06-18" in versions
      assert "2025-03-26" in versions
      assert "2024-11-05" in versions
    end

    test "identifies latest stable version" do
      assert VersionRegistry.latest_version() == "2025-06-18"
    end

    test "gets preferred version from config" do
      # Default should be latest
      assert VersionRegistry.preferred_version() == "2025-03-26"

      # Can be overridden by config
      Application.put_env(:ex_mcp, :protocol_version, "2025-06-18")
      assert VersionRegistry.preferred_version() == "2025-06-18"
      Application.delete_env(:ex_mcp, :protocol_version)
    end

    test "checks if versions are supported" do
      assert VersionRegistry.supported?("2025-03-26")
      assert VersionRegistry.supported?("2025-06-18")
      assert VersionRegistry.supported?("2025-06-18")
      refute VersionRegistry.supported?("1.0.0")
      refute VersionRegistry.supported?("unknown")
    end

    test "returns version-specific capabilities" do
      # 2024-11-05 capabilities
      caps_2024 = VersionRegistry.capabilities_for_version("2024-11-05")
      refute caps_2024.resources.subscribe
      refute caps_2024.resources.listChanged
      assert caps_2024.experimental == %{}

      # 2025-03-26 capabilities
      caps_2025 = VersionRegistry.capabilities_for_version("2025-03-26")
      assert caps_2025.resources.subscribe
      assert caps_2025.resources.listChanged
      assert caps_2025.prompts.listChanged
      assert caps_2025.logging.setLevel
      assert caps_2025.experimental.batchProcessing

      # 2025-06-18 capabilities
      caps_2025_06 = VersionRegistry.capabilities_for_version("2025-06-18")
      assert caps_2025_06.tools.outputSchema
      assert caps_2025_06.experimental.elicitation
      assert caps_2025_06.experimental.structuredContent
      assert caps_2025_06.experimental.toolOutputSchema
    end

    test "checks feature availability by version" do
      # Base features available in all versions
      assert VersionRegistry.feature_available?("2025-06-18", :prompts)
      assert VersionRegistry.feature_available?("2025-06-18", :resources)
      assert VersionRegistry.feature_available?("2025-06-18", :tools)

      # 2025-03-26 features
      assert VersionRegistry.feature_available?("2025-03-26", :resource_subscription)
      assert VersionRegistry.feature_available?("2025-03-26", :prompts_list_changed)
      assert VersionRegistry.feature_available?("2025-03-26", :logging_set_level)
      assert VersionRegistry.feature_available?("2025-03-26", :batch_processing)
      refute VersionRegistry.feature_available?("2024-11-05", :resource_subscription)

      # 2025-06-18 features
      assert VersionRegistry.feature_available?("2025-06-18", :elicitation)
      assert VersionRegistry.feature_available?("2025-06-18", :structured_content)
      refute VersionRegistry.feature_available?("2025-03-26", :elicitation)
    end

    test "negotiates protocol versions" do
      # Exact match
      assert {:ok, "2025-03-26"} =
               VersionRegistry.negotiate_version("2025-03-26", ["2025-03-26", "2025-06-18"])

      # Client version supported, exact match
      assert {:ok, "2025-06-18"} =
               VersionRegistry.negotiate_version("2025-06-18", ["2025-03-26", "2025-06-18"])

      # Unknown client version, propose best server supports
      assert {:ok, "2025-06-18"} =
               VersionRegistry.negotiate_version("unknown", ["2025-06-18"])

      # No common version
      assert {:error, :version_mismatch} =
               VersionRegistry.negotiate_version("2025-06-18", ["1.0.0"])
    end

    test "returns appropriate type module for version" do
      assert VersionRegistry.types_module("2024-11-05") == ExMCP.Types.V20241105
      assert VersionRegistry.types_module("2025-03-26") == ExMCP.Types.V20250326
      assert VersionRegistry.types_module("2025-06-18") == ExMCP.Types.V20250618
      assert VersionRegistry.types_module("unknown") == ExMCP.Types
    end
  end

  describe "protocol version-specific methods" do
    test "identifies version-specific methods" do
      # 2025-06-18 only
      assert Protocol.method_available?("elicitation/create", "2025-06-18")
      refute Protocol.method_available?("elicitation/create", "2025-03-26")
      refute Protocol.method_available?("elicitation/create", "2024-11-05")

      # 2025-03-26 and 2025-06-18
      assert Protocol.method_available?("resources/subscribe", "2025-03-26")
      assert Protocol.method_available?("resources/subscribe", "2025-06-18")
      refute Protocol.method_available?("resources/subscribe", "2024-11-05")

      assert Protocol.method_available?("logging/setLevel", "2025-03-26")
      assert Protocol.method_available?("logging/setLevel", "2025-06-18")
      refute Protocol.method_available?("logging/setLevel", "2024-11-05")

      # Available in all versions
      assert Protocol.method_available?("tools/list", "2024-11-05")
      assert Protocol.method_available?("resources/list", "2024-11-05")
      assert Protocol.method_available?("prompts/list", "2024-11-05")
    end

    test "validates message version compatibility" do
      # Valid methods
      assert :ok = Protocol.validate_message_version(%{"method" => "tools/list"}, "2025-06-18")

      assert :ok =
               Protocol.validate_message_version(
                 %{"method" => "resources/subscribe"},
                 "2025-03-26"
               )

      assert :ok =
               Protocol.validate_message_version(
                 %{"method" => "elicitation/create"},
                 "2025-06-18"
               )

      # Invalid methods for version
      assert {:error, msg} =
               Protocol.validate_message_version(
                 %{"method" => "elicitation/create"},
                 "2025-03-26"
               )

      assert msg =~ "not available"
      assert msg =~ "2025-03-26"

      assert :ok =
               Protocol.validate_message_version(
                 %{"method" => "resources/subscribe"},
                 "2025-06-18"
               )

      # Should fail for older version  
      assert {:error, msg2} =
               Protocol.validate_message_version(
                 %{"method" => "resources/subscribe"},
                 "2024-11-05"
               )

      assert msg2 =~ "not available"
    end

    test "encodes initialize with preferred version" do
      # Uses default version
      request = Protocol.encode_initialize(%{name: "test", version: "1.0"}, %{})
      assert request["params"]["protocolVersion"] == VersionRegistry.preferred_version()

      # Can override version
      request = Protocol.encode_initialize(%{name: "test", version: "1.0"}, %{}, "2025-06-18")
      assert request["params"]["protocolVersion"] == "2025-06-18"

      # Falls back to preferred if nil
      request = Protocol.encode_initialize(%{name: "test", version: "1.0"}, %{}, nil)
      assert request["params"]["protocolVersion"] == VersionRegistry.preferred_version()
    end
  end
end
