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
               "mediaType" => "image/png",
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
               "content" => [%{"type" => "text", "text" => "Fix the bug"}]
             }
    end

    test "passes block list through" do
      blocks = [
        Types.text_block("Look at this:"),
        Types.image_block("image/png", "abc123")
      ]

      params = Types.prompt_params("sess_1", blocks)

      assert params["sessionId"] == "sess_1"
      assert length(params["content"]) == 2
      assert hd(params["content"])["type"] == "text"
    end
  end
end
