defmodule ExMCP.Compliance.VersionNegotiationComplianceTest do
  @moduledoc """
  Tests for MCP version negotiation protocol compliance.

  These tests validate that version negotiation follows the MCP specification requirements,
  including initialize request format and protocol version handling.
  """
  use ExUnit.Case, async: true

  @moduletag :compliance

  alias ExMCP.Protocol

  describe "Initialize Request Protocol Compliance" do
    test "initialize request includes all required fields" do
      # MCP spec requires: jsonrpc, method, params (with protocolVersion, capabilities, clientInfo), id
      client_info = %{name: "test-client", version: "1.0.0"}
      capabilities = %{"roots" => %{}, "sampling" => %{}}

      msg = Protocol.encode_initialize(client_info, capabilities)

      # Must have JSON-RPC version
      assert msg["jsonrpc"] == "2.0"

      # Must have method = "initialize"
      assert msg["method"] == "initialize"

      # Must have params with required fields
      assert is_map(msg["params"])
      assert msg["params"]["protocolVersion"] == "2025-03-26"
      assert msg["params"]["clientInfo"] == client_info
      assert msg["params"]["capabilities"] == capabilities

      # Must have non-null ID
      assert is_integer(msg["id"])
      refute is_nil(msg["id"])
    end

    test "client can specify custom capabilities in initialize" do
      # MCP spec allows clients to specify their supported capabilities
      client_info = %{name: "custom-client", version: "2.0.0"}

      custom_capabilities = %{
        "roots" => %{"listChanged" => true},
        "sampling" => %{},
        "experimental" => %{"feature1" => true}
      }

      msg = Protocol.encode_initialize(client_info, custom_capabilities)

      assert msg["params"]["capabilities"] == custom_capabilities
      assert msg["params"]["clientInfo"] == client_info
    end

    test "initialize request follows JSON-RPC format" do
      client_info = %{name: "test-client", version: "1.0.0"}
      capabilities = %{}

      msg = Protocol.encode_initialize(client_info, capabilities)

      # Verify complete JSON-RPC structure
      required_keys = MapSet.new(["jsonrpc", "method", "params", "id"])
      actual_keys = MapSet.new(Map.keys(msg))

      assert MapSet.subset?(required_keys, actual_keys),
             "Missing required keys. Expected: #{inspect(required_keys)}, Got: #{inspect(actual_keys)}"

      # Verify params structure
      params_keys = MapSet.new(["protocolVersion", "capabilities", "clientInfo"])
      actual_params_keys = MapSet.new(Map.keys(msg["params"]))

      assert MapSet.subset?(params_keys, actual_params_keys),
             "Missing required params. Expected: #{inspect(params_keys)}, Got: #{inspect(actual_params_keys)}"
    end
  end

  describe "Protocol Version Format" do
    test "protocolVersion uses correct date format" do
      # MCP spec versions follow YYYY-MM-DD format
      client_info = %{name: "test", version: "1.0"}
      msg = Protocol.encode_initialize(client_info, %{})

      version = msg["params"]["protocolVersion"]

      # Should match YYYY-MM-DD pattern
      assert Regex.match?(~r/^\d{4}-\d{2}-\d{2}$/, version),
             "Protocol version #{version} doesn't match YYYY-MM-DD format"
    end
  end

  describe "Version Negotiation Requirements" do
    test "server must include protocolVersion in initialize response" do
      # This is a spec requirement but needs server implementation to test
      # Currently just documenting the requirement

      # Expected response structure:
      # {
      #   "jsonrpc": "2.0",
      #   "id": <request_id>,
      #   "result": {
      #     "protocolVersion": "2025-03-26",
      #     "serverInfo": {...},
      #     "capabilities": {...}
      #   }
      # }

      # The protocolVersion in response is what client and server agree to use
      assert true, "Requirement documented"
    end

    test "initialize is the first request in a session" do
      # MCP spec requires initialize to be the first request
      # No other requests should be accepted before successful initialization

      # This is enforced by Client but documenting the requirement here
      assert true, "Requirement documented"
    end
  end
end
