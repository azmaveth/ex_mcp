defmodule ExMCP.ACP.Adapters.Codex.FileChanges do
  @moduledoc """
  Pure mappings for Codex file change lifecycle notifications.
  """

  alias ExMCP.ACP.AdapterEvents
  alias ExMCP.ACP.Adapters.Codex.Events

  @spec started(String.t(), map(), map()) :: map()
  def started(session_id, params, item) do
    AdapterEvents.tool_call(session_id, %{
      "toolCallId" => Events.item_id(params, item),
      "title" => "Edit File",
      "kind" => "edit",
      "status" => Events.normalize_tool_status(item["status"], "in_progress"),
      "rawInput" => %{"changes" => item["changes"]}
    })
  end

  @spec patch_updated(String.t(), map()) :: map()
  def patch_updated(session_id, params) do
    changes = normalize_changes(params["changes"])

    AdapterEvents.tool_call_update(session_id, %{
      "toolCallId" => Events.item_id(params, %{}),
      "kind" => "edit",
      "content" => Enum.map(changes, &Events.file_change_content/1),
      "rawOutput" => params
    })
  end

  @spec completed(String.t(), map()) :: map()
  def completed(session_id, item) do
    changes = normalize_changes(item["changes"])

    AdapterEvents.tool_call_update(session_id, %{
      "toolCallId" => Events.item_id(%{}, item),
      "kind" => "edit",
      "status" => Events.normalize_tool_status(item["status"], "completed"),
      "content" => Enum.map(changes, &Events.file_change_content/1),
      "rawOutput" => %{"changes" => changes, "status" => item["status"]}
    })
  end

  defp normalize_changes(changes) when is_list(changes), do: changes
  defp normalize_changes(_changes), do: []
end
