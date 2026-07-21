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

  test "patch updated emits every current change as an ordered diff snapshot" do
    params = %{
      "itemId" => "edit-1",
      "changes" => [
        %{"path" => "lib/a.ex", "newText" => "first"},
        %{"path" => "lib/b.ex", "diff" => "second"}
      ]
    }

    update =
      "thread-1"
      |> FileChanges.patch_updated(params)
      |> get_in(["params", "update"])

    assert update["sessionUpdate"] == "tool_call_update"
    assert update["toolCallId"] == "edit-1"
    assert update["kind"] == "edit"
    assert update["rawOutput"] == params

    assert update["content"] == [
             %{
               "type" => "diff",
               "path" => "lib/a.ex",
               "oldText" => nil,
               "newText" => "first"
             },
             %{
               "type" => "diff",
               "path" => "lib/b.ex",
               "oldText" => nil,
               "newText" => "second"
             }
           ]
  end

  test "started patch updated and completed preserve one opaque item identity" do
    params = %{"itemId" => "opaque-edit", "changes" => []}

    item = %{
      "type" => "fileChange",
      "id" => "opaque-edit",
      "status" => "completed",
      "changes" => []
    }

    ids = [
      FileChanges.started("thread-1", params, item),
      FileChanges.patch_updated("thread-1", params),
      FileChanges.completed("thread-1", item)
    ]

    assert Enum.map(ids, &get_in(&1, ["params", "update", "toolCallId"])) ==
             List.duplicate("opaque-edit", 3)
  end

  test "completed and patch updated share ordered content conversion" do
    changes = [
      %{"path" => "lib/a.ex", "newText" => "first"},
      %{"path" => "lib/b.ex", "diff" => "second"}
    ]

    patch = FileChanges.patch_updated("thread-1", %{"itemId" => "edit-1", "changes" => changes})

    completed =
      FileChanges.completed("thread-1", %{
        "type" => "fileChange",
        "id" => "edit-1",
        "status" => "completed",
        "changes" => changes
      })

    assert get_in(patch, ["params", "update", "content"]) ==
             get_in(completed, ["params", "update", "content"])
  end

  test "patch updated treats non-list changes as an empty snapshot" do
    update =
      "thread-1"
      |> FileChanges.patch_updated(%{"itemId" => "edit-1", "changes" => %{"path" => "a"}})
      |> get_in(["params", "update"])

    assert update["content"] == []
  end

  test "malformed individual changes use the existing Events fallback" do
    update =
      "thread-1"
      |> FileChanges.patch_updated(%{
        "itemId" => "edit-1",
        "changes" => [%{"unexpected" => true}]
      })
      |> get_in(["params", "update"])

    assert update["content"] == [
             %{
               "type" => "content",
               "content" => %{"type" => "text", "text" => ~s({"unexpected":true})}
             }
           ]
  end
end
