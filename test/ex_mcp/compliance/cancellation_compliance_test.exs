defmodule ExMCP.Compliance.CancellationComplianceTest do
  @moduledoc """
  Tests for MCP cancellation protocol compliance.

  These tests validate that cancellation follows the MCP specification requirements.
  """
  use ExUnit.Case, async: true

  @moduletag :compliance

  alias ExMCP.Internal.Protocol, as: Protocol

  describe "Cancellation Protocol Validation" do
    test "encode_cancelled validates initialize request cannot be cancelled" do
      # MCP spec: initialize request MUST NOT be cancelled
      assert {:error, :cannot_cancel_initialize} =
               Protocol.encode_cancelled("initialize", "test reason")
    end

    test "encode_cancelled allows other requests to be cancelled" do
      assert {:ok, notification} =
               Protocol.encode_cancelled("req_123", "User cancelled")

      assert notification["method"] == "notifications/cancelled"
      assert notification["params"]["requestId"] == "req_123"
      assert notification["params"]["reason"] == "User cancelled"

      # Verify it's a notification (no id field)
      refute Map.has_key?(notification, "id")
    end

    test "encode_cancelled works without reason" do
      # Reason is optional in the MCP spec
      assert {:ok, notification} =
               Protocol.encode_cancelled("req_456")

      assert notification["method"] == "notifications/cancelled"
      assert notification["params"]["requestId"] == "req_456"
      refute Map.has_key?(notification["params"], "reason")
    end

    test "encode_cancelled! bypasses validation for backward compatibility" do
      # This function exists for cases where validation needs to be bypassed
      notification = Protocol.encode_cancelled!("initialize", "test")

      assert notification["method"] == "notifications/cancelled"
      assert notification["params"]["requestId"] == "initialize"
      assert notification["params"]["reason"] == "test"

      # Still a notification
      refute Map.has_key?(notification, "id")
    end

    test "cancellation notification follows JSON-RPC format" do
      {:ok, notification} = Protocol.encode_cancelled("req_789", "Timeout")

      # Must have jsonrpc version
      assert notification["jsonrpc"] == "2.0"

      # Must have method
      assert notification["method"] == "notifications/cancelled"

      # Must have params
      assert is_map(notification["params"])
      assert notification["params"]["requestId"] == "req_789"
      assert notification["params"]["reason"] == "Timeout"

      # Must NOT have id (it's a notification)
      refute Map.has_key?(notification, "id")
    end
  end

  describe "Cancellation Message Format" do
    test "cancelled notification has correct structure" do
      {:ok, msg} = Protocol.encode_cancelled("test_id", "User action")

      expected_structure = %{
        "jsonrpc" => "2.0",
        "method" => "notifications/cancelled",
        "params" => %{
          "requestId" => "test_id",
          "reason" => "User action"
        }
      }

      assert msg == expected_structure
    end

    test "cancelled notification without reason has minimal structure" do
      {:ok, msg} = Protocol.encode_cancelled("test_id")

      expected_structure = %{
        "jsonrpc" => "2.0",
        "method" => "notifications/cancelled",
        "params" => %{
          "requestId" => "test_id"
        }
      }

      assert msg == expected_structure
    end
  end
end
