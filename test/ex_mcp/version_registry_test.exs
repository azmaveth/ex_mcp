defmodule ExMCP.VersionRegistryTest do
  use ExUnit.Case

  alias ExMCP.Internal.Protocol
  alias ExMCP.Internal.VersionRegistry

  describe "version registry" do
    test "lists supported versions in order" do
      versions = VersionRegistry.supported_versions()
      assert "2025-11-25" in versions
      assert "2025-06-18" in versions
      assert "2025-03-26" in versions
      assert "2024-11-05" in versions
    end

    test "identifies latest stable version" do
      assert VersionRegistry.latest_version() == "2025-11-25"
    end

    test "gets preferred version from config" do
      # Default should be latest
      assert VersionRegistry.preferred_version() == "2025-11-25"

      # Can be overridden by config
      Application.put_env(:ex_mcp, :protocol_version, "2025-06-18")
      assert VersionRegistry.preferred_version() == "2025-06-18"
      Application.delete_env(:ex_mcp, :protocol_version)
    end

    test "checks if versions are supported" do
      assert VersionRegistry.supported?("2025-11-25")
      assert VersionRegistry.supported?("2025-06-18")
      assert VersionRegistry.supported?("2025-03-26")
      refute VersionRegistry.supported?("1.0.0")
      refute VersionRegistry.supported?("unknown")
    end

    test "returns version-specific capabilities" do
      # 2024-11-05 capabilities - subscribe and listChanged are defined in the spec
      caps_2024 = VersionRegistry.capabilities_for_version("2024-11-05")
      assert caps_2024.resources.subscribe
      assert caps_2024.resources.listChanged
      assert caps_2024.experimental == %{}

      # 2025-03-26 capabilities
      caps_2025 = VersionRegistry.capabilities_for_version("2025-03-26")
      assert caps_2025.resources.subscribe
      assert caps_2025.resources.listChanged
      assert caps_2025.prompts.listChanged
      # logging is a presence indicator (empty object) per the spec
      assert caps_2025.logging == %{}
      assert caps_2025.experimental.batchProcessing

      # 2025-06-18 capabilities
      caps_2025_06 = VersionRegistry.capabilities_for_version("2025-06-18")
      assert caps_2025_06.tools.listChanged
      assert caps_2025_06.experimental.elicitation
      assert caps_2025_06.experimental.structuredContent
      assert caps_2025_06.experimental.toolOutputSchema

      # 2025-11-25 capabilities
      caps_2025_11 = VersionRegistry.capabilities_for_version("2025-11-25")
      assert caps_2025_11.tools.listChanged
      assert Map.has_key?(caps_2025_11, :tasks)
    end

    test "checks feature availability by version" do
      # Base features available in all versions
      assert VersionRegistry.feature_available?("2025-06-18", :prompts)
      assert VersionRegistry.feature_available?("2025-06-18", :resources)
      assert VersionRegistry.feature_available?("2025-06-18", :tools)

      # resource_subscription, prompts_list_changed, logging_set_level are base features
      # available in all versions (defined in 2024-11-05 schema)
      assert VersionRegistry.feature_available?("2025-03-26", :resource_subscription)
      assert VersionRegistry.feature_available?("2025-03-26", :prompts_list_changed)
      assert VersionRegistry.feature_available?("2025-03-26", :logging_set_level)
      assert VersionRegistry.feature_available?("2025-03-26", :batch_processing)
      assert VersionRegistry.feature_available?("2024-11-05", :resource_subscription)

      # 2025-06-18 features
      assert VersionRegistry.feature_available?("2025-06-18", :elicitation)
      assert VersionRegistry.feature_available?("2025-06-18", :structured_content)
      refute VersionRegistry.feature_available?("2025-03-26", :elicitation)

      # 2025-11-25 features
      assert VersionRegistry.feature_available?("2025-11-25", :tasks)
      assert VersionRegistry.feature_available?("2025-11-25", :icons)
      assert VersionRegistry.feature_available?("2025-11-25", :url_elicitation)
      assert VersionRegistry.feature_available?("2025-11-25", :tool_calling_in_sampling)
      # 2025-11-25 also supports all older features
      assert VersionRegistry.feature_available?("2025-11-25", :elicitation)
      assert VersionRegistry.feature_available?("2025-11-25", :structured_content)
      assert VersionRegistry.feature_available?("2025-11-25", :resource_subscription)
      refute VersionRegistry.feature_available?("2025-06-18", :tasks)
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
      assert VersionRegistry.types_module("2025-11-25") == ExMCP.Types.V20251125
      assert VersionRegistry.types_module("unknown") == ExMCP.Types
    end
  end

  describe "protocol version-specific methods" do
    test "identifies version-specific methods" do
      # 2025-06-18 and 2025-11-25
      assert Protocol.method_available?("elicitation/create", "2025-06-18")
      assert Protocol.method_available?("elicitation/create", "2025-11-25")
      refute Protocol.method_available?("elicitation/create", "2025-03-26")
      refute Protocol.method_available?("elicitation/create", "2024-11-05")

      # resources/subscribe and logging/setLevel are defined in the 2024-11-05 schema
      # and are available in all versions
      assert Protocol.method_available?("resources/subscribe", "2025-03-26")
      assert Protocol.method_available?("resources/subscribe", "2025-06-18")
      assert Protocol.method_available?("resources/subscribe", "2025-11-25")
      assert Protocol.method_available?("resources/subscribe", "2024-11-05")

      assert Protocol.method_available?("logging/setLevel", "2025-03-26")
      assert Protocol.method_available?("logging/setLevel", "2025-06-18")
      assert Protocol.method_available?("logging/setLevel", "2025-11-25")
      assert Protocol.method_available?("logging/setLevel", "2024-11-05")

      # 2025-11-25 only methods
      assert Protocol.method_available?("tasks/get", "2025-11-25")
      assert Protocol.method_available?("tasks/list", "2025-11-25")
      assert Protocol.method_available?("tasks/result", "2025-11-25")
      assert Protocol.method_available?("tasks/cancel", "2025-11-25")
      refute Protocol.method_available?("tasks/get", "2025-06-18")

      # Available in all versions
      assert Protocol.method_available?("tools/list", "2024-11-05")
      assert Protocol.method_available?("resources/list", "2024-11-05")
      assert Protocol.method_available?("prompts/list", "2024-11-05")
    end

    test "validates message version compatibility" do
      # Valid methods
      assert :ok = Protocol.validate_message_version(%{"method" => "tools/list"}, "2025-11-25")
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

      assert :ok =
               Protocol.validate_message_version(
                 %{"method" => "elicitation/create"},
                 "2025-11-25"
               )

      # 2025-11-25 only methods
      assert :ok =
               Protocol.validate_message_version(
                 %{"method" => "tasks/get"},
                 "2025-11-25"
               )

      assert {:error, _} =
               Protocol.validate_message_version(
                 %{"method" => "tasks/get"},
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

      # resources/subscribe is available in 2024-11-05 (defined in the spec)
      assert :ok =
               Protocol.validate_message_version(
                 %{"method" => "resources/subscribe"},
                 "2024-11-05"
               )
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
