defmodule ExMCP.ACP.ProtocolTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Protocol

  describe "encode_initialize/3" do
    test "produces valid JSON-RPC with correct method" do
      msg = Protocol.encode_initialize(%{"name" => "test", "version" => "1.0"})

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "initialize"
      assert is_integer(msg["id"])
      assert msg["params"]["clientInfo"] == %{"name" => "test", "version" => "1.0"}
      assert msg["params"]["protocolVersion"] == 1
    end

    test "includes capabilities when provided" do
      caps = %{"streaming" => true}
      msg = Protocol.encode_initialize(%{"name" => "t", "version" => "1"}, caps)

      assert msg["params"]["capabilities"] == %{"streaming" => true}
    end

    test "uses custom protocol version" do
      msg = Protocol.encode_initialize(%{"name" => "t", "version" => "1"}, nil, 2)
      assert msg["params"]["protocolVersion"] == 2
    end
  end

  describe "encode_session_new/2" do
    test "produces valid JSON-RPC" do
      msg = Protocol.encode_session_new("/home/user")

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "session/new"
      assert msg["params"]["cwd"] == "/home/user"
      assert is_integer(msg["id"])
    end

    test "omits nil fields but always includes mcpServers" do
      msg = Protocol.encode_session_new()

      refute Map.has_key?(msg["params"], "cwd")
      # mcpServers is always present (some agents like Gemini require it)
      assert msg["params"]["mcpServers"] == []
    end

    test "includes mcp_servers" do
      servers = [%{"uri" => "http://localhost:3000"}]
      msg = Protocol.encode_session_new("/tmp", servers)

      assert msg["params"]["mcpServers"] == servers
    end
  end

  describe "encode_session_load/3" do
    test "includes session ID" do
      msg = Protocol.encode_session_load("sess_abc")

      assert msg["method"] == "session/load"
      assert msg["params"]["sessionId"] == "sess_abc"
    end
  end

  describe "encode_session_prompt/2" do
    test "produces valid request" do
      blocks = [%{"type" => "text", "text" => "Hello"}]
      msg = Protocol.encode_session_prompt("sess_1", blocks)

      assert msg["method"] == "session/prompt"
      assert msg["params"]["sessionId"] == "sess_1"
      assert msg["params"]["prompt"] == blocks
      assert is_integer(msg["id"])
    end
  end

  describe "encode_session_cancel/1" do
    test "produces notification without id" do
      msg = Protocol.encode_session_cancel("sess_1")

      assert msg["method"] == "session/cancel"
      assert msg["params"]["sessionId"] == "sess_1"
      refute Map.has_key?(msg, "id")
    end
  end

  describe "encode_session_set_mode/2" do
    test "produces valid request" do
      msg = Protocol.encode_session_set_mode("sess_1", "code")

      assert msg["method"] == "session/setMode"
      assert msg["params"]["sessionId"] == "sess_1"
      assert msg["params"]["modeId"] == "code"
    end
  end

  describe "encode_session_set_config_option/3" do
    test "produces valid request" do
      msg = Protocol.encode_session_set_config_option("sess_1", "theme", "dark")

      assert msg["method"] == "session/setConfigOption"
      assert msg["params"]["sessionId"] == "sess_1"
      assert msg["params"]["configId"] == "theme"
      assert msg["params"]["value"] == "dark"
    end
  end

  describe "encode_permission_response/2" do
    test "wraps outcome in result" do
      msg = Protocol.encode_permission_response(42, %{"optionId" => "allow"})

      assert msg["jsonrpc"] == "2.0"
      assert msg["id"] == 42
      assert msg["result"]["outcome"] == %{"optionId" => "allow"}
    end
  end

  describe "encode_file_read_response/2" do
    test "wraps content in result" do
      msg = Protocol.encode_file_read_response(7, "file contents here")

      assert msg["result"]["content"] == "file contents here"
      assert msg["id"] == 7
    end
  end

  describe "encode_file_write_response/1" do
    test "returns empty result" do
      msg = Protocol.encode_file_write_response(9)

      assert msg["result"] == %{}
      assert msg["id"] == 9
    end
  end

  describe "round-trip encoding" do
    test "encode → JSON → decode → parse" do
      msg = Protocol.encode_session_new("/tmp")
      json = Jason.encode!(msg)
      {:ok, decoded} = Jason.decode(json)

      assert {:request, "session/new", params, _id} = Protocol.parse_message(decoded)
      assert params["cwd"] == "/tmp"
    end

    test "notification round-trip has no id" do
      msg = Protocol.encode_session_cancel("s1")
      json = Jason.encode!(msg)
      {:ok, decoded} = Jason.decode(json)

      assert {:notification, "session/cancel", params} = Protocol.parse_message(decoded)
      assert params["sessionId"] == "s1"
    end

    test "response round-trip" do
      msg = Protocol.encode_response(%{"sessionId" => "abc"}, 42)
      json = Jason.encode!(msg)

      assert {:result, result, 42} = Protocol.parse_message(json)
      assert result["sessionId"] == "abc"
    end
  end
end
