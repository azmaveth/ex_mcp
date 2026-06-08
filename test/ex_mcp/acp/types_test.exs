defmodule ExMCP.ACP.TypesTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Types

  describe "text_block/1" do
    test "creates a text content block" do
      block = Types.text_block("Hello")
      assert block == %{"type" => "text", "text" => "Hello"}
    end
  end

  describe "image_block/2" do
    test "creates an image content block" do
      block = Types.image_block("image/png", "base64data")

      assert block == %{
               "type" => "image",
               "mimeType" => "image/png",
               "data" => "base64data"
             }
    end
  end

  describe "client_info/2" do
    test "creates client info map" do
      info = Types.client_info("my_client", "1.0.0")
      assert info == %{"name" => "my_client", "version" => "1.0.0"}
    end
  end

  describe "auth_method/3" do
    test "creates auth method metadata" do
      method = Types.auth_method("api-key", "API Key", description: "Use a local API key")

      assert method == %{
               "id" => "api-key",
               "name" => "API Key",
               "description" => "Use a local API key"
             }
    end
  end

  describe "agent_capabilities/1" do
    test "creates stable ACP capability groups" do
      caps =
        Types.agent_capabilities(
          load_session: true,
          acp_mcp: true,
          image: true,
          http_mcp: true,
          session_list: true,
          session_resume: true,
          session_close: true,
          session_delete: true,
          session_fork: true,
          session_additional_directories: true,
          beam_mcp: true,
          logout: true
        )

      assert caps["loadSession"] == true
      assert caps["promptCapabilities"]["image"] == true
      assert caps["mcpCapabilities"]["acp"] == true
      assert caps["mcpCapabilities"]["http"] == true

      assert get_in(caps, ["mcpCapabilities", "_meta", "ex_mcp.mcpCapabilities", "beam"]) == true
      assert caps["sessionCapabilities"]["list"] == %{}
      assert caps["sessionCapabilities"]["resume"] == %{}
      assert caps["sessionCapabilities"]["close"] == %{}
      assert caps["sessionCapabilities"]["delete"] == %{}
      assert caps["sessionCapabilities"]["fork"] == %{}
      assert caps["sessionCapabilities"]["additionalDirectories"] == %{}
      assert caps["auth"]["logout"] == %{}
    end
  end

  describe "new_session_params/2" do
    test "creates params with cwd" do
      params = Types.new_session_params("/home/user/project")
      assert params == %{"cwd" => "/home/user/project", "mcpServers" => []}
    end

    test "includes mcp_servers when provided" do
      servers = [%{"uri" => "http://localhost:3000", "name" => "local"}]
      params = Types.new_session_params("/tmp", mcp_servers: servers)

      assert params == %{
               "cwd" => "/tmp",
               "mcpServers" => servers
             }
    end

    test "includes additional_directories when provided" do
      params = Types.new_session_params("/tmp", additional_directories: ["/tmp/shared"])

      assert params == %{
               "cwd" => "/tmp",
               "mcpServers" => [],
               "additionalDirectories" => ["/tmp/shared"]
             }
    end
  end

  describe "prompt_params/2" do
    test "wraps string content as text block" do
      params = Types.prompt_params("sess_1", "Fix the bug")

      assert params == %{
               "sessionId" => "sess_1",
               "prompt" => [%{"type" => "text", "text" => "Fix the bug"}]
             }
    end

    test "passes block list through" do
      blocks = [
        Types.text_block("Look at this:"),
        Types.image_block("image/png", "abc123")
      ]

      params = Types.prompt_params("sess_1", blocks)

      assert params["sessionId"] == "sess_1"
      assert length(params["prompt"]) == 2
      assert hd(params["prompt"])["type"] == "text"
    end
  end

  describe "error codes" do
    test "auth_required_code returns -32000" do
      assert Types.auth_required_code() == -32_000
    end

    test "resource_not_found_code returns -32002" do
      assert Types.resource_not_found_code() == -32_002
    end
  end

  describe "audio_block/2" do
    test "creates an audio content block" do
      block = Types.audio_block("audio/wav", "audiodata")
      assert block == %{"type" => "audio", "mimeType" => "audio/wav", "data" => "audiodata"}
    end
  end

  describe "resource_link_block/2" do
    test "creates a resource link block" do
      block = Types.resource_link_block("file:///src/main.ex", name: "main.ex")
      assert block["type"] == "resource_link"
      assert block["uri"] == "file:///src/main.ex"
      assert block["name"] == "main.ex"
    end
  end

  describe "resource_block/2" do
    test "creates an embedded text resource block" do
      block = Types.resource_block("file:///src/main.ex", text: "defmodule Main do end")

      assert block["type"] == "resource"
      assert block["resource"]["uri"] == "file:///src/main.ex"
      assert block["resource"]["text"] == "defmodule Main do end"
    end
  end

  describe "plan_entry/3" do
    test "creates a plan entry with defaults" do
      entry = Types.plan_entry("Fix the auth bug")
      assert entry["content"] == "Fix the auth bug"
      assert entry["priority"] == "medium"
      assert entry["status"] == "pending"
    end

    test "creates a plan entry with custom priority and status" do
      entry = Types.plan_entry("Deploy to prod", "high", "in_progress")
      assert entry["priority"] == "high"
      assert entry["status"] == "in_progress"
    end
  end

  describe "plan_update/2" do
    test "creates a stable plan session update notification" do
      entries = [
        Types.plan_entry("Step 1: Read the code", "high", "completed"),
        Types.plan_entry("Step 2: Write the fix", "high", "in_progress"),
        Types.plan_entry("Step 3: Run tests", "medium", "pending")
      ]

      msg = Types.plan_update("sess_1", entries)
      assert msg["method"] == "session/update"
      assert msg["params"]["sessionId"] == "sess_1"

      update = msg["params"]["update"]
      assert update["sessionUpdate"] == "plan"
      assert length(update["entries"]) == 3
      assert hd(update["entries"])["status"] == "completed"
    end
  end

  describe "session update builders" do
    test "creates current mode update with currentModeId" do
      msg = Types.current_mode_update("sess_1", "code")
      update = msg["params"]["update"]

      assert update["sessionUpdate"] == "current_mode_update"
      assert update["currentModeId"] == "code"
      refute Map.has_key?(update, "modeId")
    end

    test "creates available commands update with availableCommands" do
      msg = Types.available_commands_update("sess_1", [%{"name" => "test"}])
      update = msg["params"]["update"]

      assert update["sessionUpdate"] == "available_commands_update"
      assert update["availableCommands"] == [%{"name" => "test"}]
    end

    test "creates config option update with complete configOptions" do
      option =
        Types.select_config_option("model", "Model", "fast", [
          Types.config_option_value("fast", "Fast")
        ])

      msg = Types.config_option_update("sess_1", [option])
      update = msg["params"]["update"]

      assert update["sessionUpdate"] == "config_option_update"
      assert update["configOptions"] == [option]
    end

    test "creates session info update with title and updatedAt" do
      msg =
        Types.session_info_update("sess_1", title: "Fix auth", updatedAt: "2026-05-28T00:00:00Z")

      update = msg["params"]["update"]

      assert update["sessionUpdate"] == "session_info_update"
      assert update["title"] == "Fix auth"
      assert update["updatedAt"] == "2026-05-28T00:00:00Z"
    end

    test "creates usage update with used and size" do
      msg = Types.usage_update("sess_1", 42, 100)
      update = msg["params"]["update"]

      assert update["sessionUpdate"] == "usage_update"
      assert update["used"] == 42
      assert update["size"] == 100
    end
  end

  describe "MCP server config builders" do
    test "creates stdio server config" do
      server =
        Types.stdio_mcp_server("local", "mcp-server",
          args: ["--stdio"],
          env: %{TOKEN: "secret"}
        )

      assert server["type"] == "stdio"
      assert server["name"] == "local"
      assert server["command"] == "mcp-server"
      assert server["args"] == ["--stdio"]
      assert server["env"] == [%{"name" => "TOKEN", "value" => "secret"}]
    end

    test "creates HTTP and SSE server configs" do
      headers = [{"authorization", "Bearer token"}]

      assert Types.http_mcp_server("remote", "https://example.com/mcp", headers: headers) == %{
               "type" => "http",
               "name" => "remote",
               "url" => "https://example.com/mcp",
               "headers" => [%{"name" => "authorization", "value" => "Bearer token"}]
             }

      assert Types.sse_mcp_server("events", "https://example.com/sse") == %{
               "type" => "sse",
               "name" => "events",
               "url" => "https://example.com/sse",
               "headers" => []
             }
    end
  end
end
