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

  describe "new_session_params/2" do
    test "creates params with cwd" do
      params = Types.new_session_params("/home/user/project")
      assert params == %{"cwd" => "/home/user/project"}
    end

    test "creates empty params when no cwd" do
      params = Types.new_session_params()
      assert params == %{}
    end

    test "includes mcp_servers when provided" do
      servers = [%{"uri" => "http://localhost:3000", "name" => "local"}]
      params = Types.new_session_params("/tmp", mcp_servers: servers)

      assert params == %{
               "cwd" => "/tmp",
               "mcpServers" => servers
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
    test "creates a plan_update session update notification" do
      entries = [
        Types.plan_entry("Step 1: Read the code", "high", "completed"),
        Types.plan_entry("Step 2: Write the fix", "high", "in_progress"),
        Types.plan_entry("Step 3: Run tests", "medium", "pending")
      ]

      msg = Types.plan_update("sess_1", entries)
      assert msg["method"] == "session/update"
      assert msg["params"]["sessionId"] == "sess_1"

      update = msg["params"]["update"]
      assert update["sessionUpdate"] == "plan_update"
      assert length(update["entries"]) == 3
      assert hd(update["entries"])["status"] == "completed"
    end
  end
end
