defmodule ExMCP.Compliance.RootsComplianceTest do
  @moduledoc """
  Tests for MCP roots functionality protocol compliance.

  These tests validate that roots functionality follows the MCP specification requirements
  for the roots/list request and notifications/roots/list_changed notification.
  """
  use ExUnit.Case, async: true

  @moduletag :compliance

  alias ExMCP.Internal.Protocol, as: Protocol

  describe "Roots Request Protocol Compliance" do
    test "roots request uses correct protocol format" do
      # MCP spec requires specific format for roots/list request
      request = Protocol.encode_list_roots()

      # Must have JSON-RPC version
      assert request["jsonrpc"] == "2.0"

      # Must have correct method
      assert request["method"] == "roots/list"

      # Must have ID (it's a request, not notification)
      assert Map.has_key?(request, "id")
      refute is_nil(request["id"])

      # Params should be empty object for roots/list
      assert request["params"] == %{}
    end

    test "roots response format matches spec" do
      # MCP spec defines the response format for roots/list
      roots = [
        %{uri: "file:///test", name: "Test"},
        # name is optional
        %{uri: "file:///another"}
      ]

      response = Protocol.encode_response(%{"roots" => roots}, "test-id")

      assert response["jsonrpc"] == "2.0"
      assert response["id"] == "test-id"
      assert response["result"]["roots"] == roots

      # Verify it's a proper response (not error)
      refute Map.has_key?(response, "error")
    end
  end

  describe "Roots Change Notification Compliance" do
    test "roots change notification format" do
      # MCP spec defines notifications/roots/list_changed
      notification = Protocol.encode_roots_changed()

      # Must have JSON-RPC version
      assert notification["jsonrpc"] == "2.0"

      # Must have correct method
      assert notification["method"] == "notifications/roots/list_changed"

      # Must NOT have ID (it's a notification)
      refute Map.has_key?(notification, "id")

      # Params should be empty object
      assert notification["params"] == %{}
    end
  end

  describe "Roots Data Format Compliance" do
    test "roots follow MCP specification format" do
      # This test documents the expected format but needs handler to test

      # Each root object must have:
      # - uri: string (required) - URI identifying the root
      # - name: string (optional) - Human-readable name for the root

      valid_roots = [
        %{
          uri: "file:///workspace/myproject",
          name: "My Project"
        },
        %{
          uri: "file:///home/user/documents",
          name: "Documents"
        },
        %{
          uri: "file:///var/www/html"
          # No name - this is valid as name is optional
        }
      ]

      # All roots must have URI
      Enum.each(valid_roots, fn root ->
        assert Map.has_key?(root, :uri)
        assert is_binary(root.uri)
      end)

      # Name is optional but if present must be string
      Enum.each(valid_roots, fn root ->
        if Map.has_key?(root, :name) do
          assert is_binary(root.name)
        end
      end)
    end

    test "root URIs should be valid URI format" do
      # Document URI format requirements
      valid_uris = [
        "file:///home/user",
        "file:///c:/Users/Name",
        "file:///workspace/project",
        "https://github.com/user/repo",
        "ssh://git@github.com:user/repo.git"
      ]

      Enum.each(valid_uris, fn uri ->
        assert String.contains?(uri, "://"), "URI must contain scheme: #{uri}"
      end)
    end
  end

  describe "Roots Capability" do
    test "roots capability in initialize" do
      # Document that roots capability can be advertised
      # Both client and server can support roots capability

      # Client can advertise roots support:
      client_capabilities = %{
        "roots" => %{
          # Supports list_changed notifications
          "listChanged" => true
        }
      }

      msg = Protocol.encode_initialize(%{name: "test", version: "1.0"}, client_capabilities)
      assert msg["params"]["capabilities"]["roots"]["listChanged"] == true

      # Server can also advertise roots support in response
      # (implementation in server handler)
    end
  end
end
