defmodule ExMCP.ACP.AdapterEventsTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.AdapterEvents

  test "builds text content session updates" do
    assert AdapterEvents.agent_message_chunk("session-1", "hello") == %{
             "jsonrpc" => "2.0",
             "method" => "session/update",
             "params" => %{
               "sessionId" => "session-1",
               "update" => %{
                 "sessionUpdate" => "agent_message_chunk",
                 "content" => %{"type" => "text", "text" => "hello"}
               }
             }
           }

    assert get_in(AdapterEvents.agent_thought_chunk("session-1", "thinking"), [
             "params",
             "update",
             "sessionUpdate"
           ]) == "agent_thought_chunk"

    assert get_in(AdapterEvents.user_message_chunk("session-1", "user"), [
             "params",
             "update",
             "sessionUpdate"
           ]) == "user_message_chunk"
  end

  test "builds text updates with metadata" do
    assert get_in(
             AdapterEvents.agent_message_chunk("session-1", "done",
               meta: %{"ex_mcp" => %{"final" => true}}
             ),
             ["params", "update", "_meta"]
           ) == %{"ex_mcp" => %{"final" => true}}
  end

  test "builds text updates with message IDs" do
    assert get_in(
             AdapterEvents.agent_message_chunk("session-1", "done", message_id: "msg-1"),
             ["params", "update", "messageId"]
           ) == "msg-1"

    assert get_in(
             AdapterEvents.user_message_chunk("session-1", "replay", messageId: "msg-2"),
             ["params", "update", "messageId"]
           ) == "msg-2"
  end

  test "builds arbitrary content chunks" do
    content = %{"type" => "image", "uri" => "file:///tmp/image.png"}

    assert get_in(
             AdapterEvents.content_chunk("session-1", "user_message_chunk", content,
               meta: %{"ex_mcp" => %{"replay" => true}}
             ),
             ["params", "update"]
           ) == %{
             "sessionUpdate" => "user_message_chunk",
             "content" => content,
             "_meta" => %{"ex_mcp" => %{"replay" => true}}
           }
  end

  test "builds resource link chunks" do
    update =
      get_in(
        AdapterEvents.resource_link_chunk("session-1", "file:///tmp/out.md",
          name: "out.md",
          message_id: "msg-3"
        ),
        ["params", "update"]
      )

    assert update["content"] == %{
             "type" => "resource_link",
             "uri" => "file:///tmp/out.md",
             "name" => "out.md"
           }

    assert update["messageId"] == "msg-3"
  end

  test "builds selector and command updates" do
    assert get_in(AdapterEvents.current_mode_update("session-1", "auto"), [
             "params",
             "update"
           ]) == %{"sessionUpdate" => "current_mode_update", "currentModeId" => "auto"}

    commands = [%{"name" => "compact", "description" => "Compact context"}]

    assert get_in(AdapterEvents.available_commands_update("session-1", commands), [
             "params",
             "update"
           ]) == %{
             "sessionUpdate" => "available_commands_update",
             "availableCommands" => commands
           }
  end

  test "builds plan and config option updates" do
    entries = [%{"content" => "Inspect", "status" => "pending"}]
    options = [%{"id" => "mode", "type" => "select"}]

    assert get_in(AdapterEvents.plan("session-1", entries), ["params", "update"]) == %{
             "sessionUpdate" => "plan",
             "entries" => entries
           }

    assert get_in(AdapterEvents.config_option_update("session-1", options), [
             "params",
             "update"
           ]) == %{
             "sessionUpdate" => "config_option_update",
             "configOptions" => options
           }
  end

  test "builds tool call updates" do
    attrs = %{"toolCallId" => "call-1", "status" => "completed"}

    assert get_in(AdapterEvents.tool_call("session-1", attrs), ["params", "update"]) ==
             Map.put(attrs, "sessionUpdate", "tool_call")

    assert get_in(AdapterEvents.tool_call_update("session-1", attrs), ["params", "update"]) ==
             Map.put(attrs, "sessionUpdate", "tool_call_update")
  end
end
