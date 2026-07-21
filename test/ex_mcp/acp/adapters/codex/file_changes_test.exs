defmodule ExMCP.ACP.Adapters.Codex.FileChangesTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.Codex.FileChanges

  test "started maps the existing file change tool call shape" do
    params = %{"itemId" => "edit-1"}

    item = %{
      "type" => "fileChange",
      "id" => "provider-edit-1",
      "status" => "running",
      "changes" => [%{"path" => "lib/a.ex", "diff" => "@@ diff"}]
    }

    update =
      "thread-1"
      |> FileChanges.started(params, item)
      |> get_in(["params", "update"])

    assert update == %{
             "sessionUpdate" => "tool_call",
             "toolCallId" => "edit-1",
             "title" => "Edit File",
             "kind" => "edit",
             "status" => "in_progress",
             "rawInput" => %{"changes" => item["changes"]}
           }
  end

  test "completed maps the existing file change terminal update shape" do
    item = %{
      "type" => "fileChange",
      "id" => "edit-1",
      "status" => "completed",
      "changes" => [%{"path" => "lib/a.ex", "newText" => "updated"}]
    }

    update =
      "thread-1"
      |> FileChanges.completed(item)
      |> get_in(["params", "update"])

    assert update["sessionUpdate"] == "tool_call_update"
    assert update["toolCallId"] == "edit-1"
    assert update["kind"] == "edit"
    assert update["status"] == "completed"
    assert update["rawOutput"] == %{"changes" => item["changes"], "status" => "completed"}

    assert update["content"] == [
             %{
               "type" => "diff",
               "path" => "lib/a.ex",
               "oldText" => nil,
               "newText" => "updated"
             }
           ]
  end
end
