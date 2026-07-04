defmodule ExMCP.ACP.Adapters.Codex.EventsTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Codex.Events

  test "normalizes tool kinds and statuses" do
    assert Events.tool_kind("Read File") == "read"
    assert Events.tool_kind("apply_patch") == "edit"
    assert Events.tool_kind("shell") == "execute"
    assert Events.tool_kind(nil) == "other"

    assert Events.normalize_tool_status("queued", "completed") == "pending"
    assert Events.normalize_tool_status("canceled", "completed") == "cancelled"
    assert Events.normalize_tool_status("unknown", "completed") == "completed"
  end

  test "builds tool content blocks" do
    assert Events.tool_text_content("hello") == %{
             "type" => "content",
             "content" => %{"type" => "text", "text" => "hello"}
           }

    assert Events.tool_diff_content("lib/file.ex", "+ok") == %{
             "type" => "diff",
             "path" => "lib/file.ex",
             "oldText" => nil,
             "newText" => "+ok"
           }
  end

  test "maps common titles and ids" do
    assert Events.command_title("") == "Run Command"
    assert Events.command_title("mix test") == "mix test"
    assert Events.mcp_tool_title(%{"server" => "repo", "tool" => "search"}) == "mcp.repo.search"
    assert Events.dynamic_tool_title(%{"namespace" => "fs", "tool" => "read"}) == "fs:read"

    assert Events.item_id(%{"itemId" => "item-1"}, %{"id" => "item-2"}) == "item-1"
    assert Events.item_type(%{"type" => "mcpToolCall"}) == "mcpToolCall"
  end

  test "marks replay metadata without dropping existing metadata" do
    message = %{
      "params" => %{
        "update" => %{
          "sessionUpdate" => "agent_message_chunk",
          "_meta" => %{"ex_mcp" => %{"adapter" => "codex"}}
        }
      }
    }

    assert get_in(Events.mark_replay(message), ["params", "update", "_meta", "ex_mcp"]) == %{
             "adapter" => "codex",
             "replay" => true
           }
  end
end
