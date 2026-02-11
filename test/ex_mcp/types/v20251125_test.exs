defmodule ExMCP.Types.V20251125Test do
  @moduledoc """
  Tests for ExMCP.Types.V20251125 type definition module.

  Verifies the module exists, exports the correct protocol version,
  and defines the expected types for the 2025-11-25 MCP specification.
  """
  use ExUnit.Case, async: true

  alias ExMCP.Types.V20251125

  describe "module existence" do
    test "module is defined and loadable" do
      assert Code.ensure_loaded?(V20251125)
    end
  end

  describe "protocol_version/0" do
    test "returns the correct protocol version string" do
      assert V20251125.protocol_version() == "2025-11-25"
    end

    test "protocol version is a valid date string" do
      version = V20251125.protocol_version()
      assert is_binary(version)
      assert String.match?(version, ~r/^\d{4}-\d{2}-\d{2}$/)
    end
  end

  describe "type definitions" do
    test "module exports expected type metadata" do
      # Verify the module has type information by checking beam attributes
      {:ok, types} = Code.Typespec.fetch_types(V20251125)
      type_names = Enum.map(types, fn {_kind, {name, _def, _args}} -> name end)

      # Core types re-exported from ExMCP.Types
      assert :json_value in type_names
      assert :json_schema in type_names
      assert :request_id in type_names
      assert :cursor in type_names
      assert :progress_token in type_names
      assert :log_level in type_names
      assert :meta in type_names

      # New in 2025-11-25: Icons
      assert :icon in type_names

      # Implementation with title/description
      assert :implementation in type_names

      # Capabilities
      assert :client_capabilities in type_names
      assert :server_capabilities in type_names

      # Tool types
      assert :tool_annotations in type_names
      assert :task_support in type_names
      assert :tool in type_names

      # Resource types
      assert :resource in type_names
      assert :resource_annotations in type_names

      # Prompt types
      assert :prompt in type_names

      # Tool result
      assert :call_tool_result in type_names

      # Task types (new in 2025-11-25)
      assert :task_state in type_names
      assert :task in type_names
      assert :create_task_result in type_names

      # URL elicitation (new in 2025-11-25)
      assert :url_elicit_request in type_names

      # Tool calling in sampling
      assert :tool_choice in type_names
      assert :tool_use_content in type_names
      assert :tool_result_content in type_names

      # Elicitation result
      assert :elicit_result in type_names
    end
  end
end
