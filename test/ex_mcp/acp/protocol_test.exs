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
      caps = %{"fs" => %{"readTextFile" => true}}
      msg = Protocol.encode_initialize(%{"name" => "t", "version" => "1"}, caps)

      assert msg["params"]["clientCapabilities"] == %{"fs" => %{"readTextFile" => true}}
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
      assert msg["params"]["mcpServers"] == []
    end
  end

  describe "encode_session_resume/3" do
    test "includes session ID, cwd, and mcp servers" do
      servers = [%{"type" => "stdio", "name" => "local", "command" => "mcp", "args" => []}]
      msg = Protocol.encode_session_resume("sess_abc", "/tmp/project", servers)

      assert msg["method"] == "session/resume"
      assert msg["params"]["sessionId"] == "sess_abc"
      assert msg["params"]["cwd"] == "/tmp/project"
      assert msg["params"]["mcpServers"] == servers
    end

    test "defaults mcp servers to an empty list" do
      msg = Protocol.encode_session_resume("sess_abc")
      assert msg["params"]["mcpServers"] == []
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

  describe "encode_session_close/1" do
    test "produces a request with an id" do
      msg = Protocol.encode_session_close("sess_1")

      assert msg["method"] == "session/close"
      assert msg["params"]["sessionId"] == "sess_1"
      assert is_integer(msg["id"])
    end
  end

  describe "encode_session_set_mode/2" do
    test "produces valid request" do
      msg = Protocol.encode_session_set_mode("sess_1", "code")

      assert msg["method"] == "session/set_mode"
      assert msg["params"]["sessionId"] == "sess_1"
      assert msg["params"]["modeId"] == "code"
    end
  end

  describe "encode_session_set_config_option/3" do
    test "produces valid request" do
      msg = Protocol.encode_session_set_config_option("sess_1", "theme", "dark")

      assert msg["method"] == "session/set_config_option"
      assert msg["params"]["sessionId"] == "sess_1"
      assert msg["params"]["configId"] == "theme"
      assert msg["params"]["value"] == "dark"
    end
  end

  describe "encode_permission_response/2" do
    test "wraps the selected outcome in the stable response shape" do
      msg =
        Protocol.encode_permission_response(42, %{"outcome" => "selected", "optionId" => "allow"})

      assert msg["jsonrpc"] == "2.0"
      assert msg["id"] == 42
      assert msg["result"]["outcome"]["outcome"] == "selected"
      assert msg["result"]["outcome"]["optionId"] == "allow"
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
    test "returns empty object result" do
      msg = Protocol.encode_file_write_response(9)

      assert msg["result"] == %{}
      assert msg["id"] == 9
    end
  end

  describe "encode_authenticate/1" do
    test "wraps method ID strings using the stable ACP shape" do
      msg = Protocol.encode_authenticate("api-key")

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "authenticate"
      assert msg["params"]["methodId"] == "api-key"
      assert is_integer(msg["id"])
    end

    test "passes map params through for adapter compatibility" do
      msg = Protocol.encode_authenticate(%{"provider" => "api_key", "key" => "sk-123"})

      assert msg["params"]["provider"] == "api_key"
      assert msg["params"]["key"] == "sk-123"
    end

    test "works with empty params" do
      msg = Protocol.encode_authenticate()
      assert msg["method"] == "authenticate"
      assert msg["params"] == %{}
    end
  end

  describe "encode_logout/0" do
    test "produces valid request" do
      msg = Protocol.encode_logout()

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "logout"
      assert msg["params"] == %{}
      assert is_integer(msg["id"])
    end
  end

  describe "agent-side encoders" do
    test "encodes initialize response" do
      msg =
        Protocol.encode_initialize_response(
          1,
          %{"name" => "agent", "version" => "1.0.0"},
          %{"sessionCapabilities" => %{"list" => %{}}},
          [%{"id" => "api-key", "name" => "API Key"}],
          1
        )

      assert msg["jsonrpc"] == "2.0"
      assert msg["id"] == 1
      assert msg["result"]["agentInfo"]["name"] == "agent"
      assert msg["result"]["agentCapabilities"]["sessionCapabilities"]["list"] == %{}
      assert msg["result"]["authMethods"] == [%{"id" => "api-key", "name" => "API Key"}]
      assert msg["result"]["protocolVersion"] == 1
    end

    test "encodes session and prompt responses" do
      assert Protocol.encode_session_response(2, "sess_1")["result"] == %{
               "sessionId" => "sess_1"
             }

      assert Protocol.encode_session_list_response(3, [%{"sessionId" => "s", "cwd" => "/tmp"}])[
               "result"
             ] == %{"sessions" => [%{"sessionId" => "s", "cwd" => "/tmp"}]}

      assert Protocol.encode_prompt_response(4, "end_turn")["result"] == %{
               "stopReason" => "end_turn"
             }
    end

    test "encodes stable session updates" do
      msg = Protocol.encode_agent_message_chunk("sess_1", "hello")

      assert msg["method"] == "session/update"
      assert msg["params"]["sessionId"] == "sess_1"

      assert msg["params"]["update"] == %{
               "sessionUpdate" => "agent_message_chunk",
               "content" => %{"type" => "text", "text" => "hello"}
             }

      plan = Protocol.encode_plan("sess_1", [%{"content" => "Do it", "status" => "pending"}])
      assert plan["params"]["update"]["sessionUpdate"] == "plan"

      usage = Protocol.encode_usage_update("sess_1", 42, 100)
      assert usage["params"]["update"]["sessionUpdate"] == "usage_update"
      assert usage["params"]["update"]["used"] == 42
      assert usage["params"]["update"]["size"] == 100
    end

    test "encodes agent-to-client requests" do
      permission =
        Protocol.encode_permission_request(
          "sess_1",
          %{"toolName" => "edit"},
          [%{"optionId" => "allow", "name" => "Allow", "kind" => "allow_once"}]
        )

      assert permission["method"] == "session/request_permission"
      assert permission["params"]["toolCall"]["toolName"] == "edit"
      assert is_integer(permission["id"])

      read = Protocol.encode_file_read_request("sess_1", "/tmp/a.txt", line: 2, limit: 10)
      assert read["method"] == "fs/read_text_file"
      assert read["params"]["line"] == 2
      assert read["params"]["limit"] == 10

      terminal =
        Protocol.encode_terminal_request("terminal/output", "sess_1", %{
          "terminalId" => "term_1"
        })

      assert terminal["method"] == "terminal/output"
      assert terminal["params"]["sessionId"] == "sess_1"
      assert terminal["params"]["terminalId"] == "term_1"
    end
  end

  describe "encode_session_list/1" do
    test "produces valid request" do
      msg = Protocol.encode_session_list()

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "session/list"
      assert is_integer(msg["id"])
    end

    test "includes cursor when provided" do
      msg = Protocol.encode_session_list(cursor: "page2")
      assert msg["params"]["cursor"] == "page2"
    end

    test "includes cwd filter when provided" do
      msg = Protocol.encode_session_list(cwd: "/tmp/project")
      assert msg["params"]["cwd"] == "/tmp/project"
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
