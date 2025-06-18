defmodule ExMCP.V1SimpleClientTest do
  use ExUnit.Case

  alias ExMCP.Protocol

  describe "new protocol features" do
    test "protocol version is correct" do
      msg = Protocol.encode_initialize(%{name: "test", version: "1.0"})
      assert msg["params"]["protocolVersion"] == "2025-03-26"
    end

    test "roots operations encoding" do
      # Test list roots
      msg = Protocol.encode_list_roots()
      assert msg["method"] == "roots/list"
      assert is_integer(msg["id"])

      # Test roots changed notification
      notif = Protocol.encode_roots_changed()
      assert notif["method"] == "notifications/roots/list_changed"
      refute Map.has_key?(notif, "id")
    end

    test "resource subscription encoding" do
      # Test subscribe
      msg = Protocol.encode_subscribe_resource("file:///test")
      assert msg["method"] == "resources/subscribe"
      assert msg["params"]["uri"] == "file:///test"
      assert is_integer(msg["id"])

      # Test unsubscribe
      msg = Protocol.encode_unsubscribe_resource("file:///test")
      assert msg["method"] == "resources/unsubscribe"
      assert msg["params"]["uri"] == "file:///test"
      assert is_integer(msg["id"])
    end

    test "sampling encoding" do
      params = %{
        messages: [%{role: "user", content: %{type: "text", text: "Hello"}}],
        max_tokens: 100
      }

      msg = Protocol.encode_create_message(params)
      assert msg["method"] == "sampling/createMessage"
      assert msg["params"] == params
      assert is_integer(msg["id"])
    end

    test "client has new methods" do
      # Just verify the functions exist and have correct arity
      # Functions with default args are exported with both arities
      assert function_exported?(ExMCP.Client, :list_roots, 1) ||
               function_exported?(ExMCP.Client, :list_roots, 2)

      assert function_exported?(ExMCP.Client, :subscribe_resource, 2) ||
               function_exported?(ExMCP.Client, :subscribe_resource, 3)

      assert function_exported?(ExMCP.Client, :unsubscribe_resource, 2) ||
               function_exported?(ExMCP.Client, :unsubscribe_resource, 3)

      assert function_exported?(ExMCP.Client, :create_message, 2) ||
               function_exported?(ExMCP.Client, :create_message, 3)
    end
  end
end
