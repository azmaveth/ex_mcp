defmodule ExMCP.ACP.Adapters.Codex.Events do
  @moduledoc """
  Pure event-mapping helpers for the Codex ACP adapter.
  """

  alias ExMCP.ACP.AdapterEvents

  @tool_status_aliases %{
    "pending" => "pending",
    "queued" => "pending",
    "running" => "in_progress",
    "in_progress" => "in_progress",
    "completed" => "completed",
    "success" => "completed",
    "succeeded" => "completed",
    "failed" => "failed",
    "errored" => "failed",
    "cancelled" => "cancelled",
    "canceled" => "cancelled"
  }

  @spec tool_call_started(String.t() | nil, map()) :: map()
  def tool_call_started(session_id, item) do
    AdapterEvents.tool_call(session_id, %{
      "toolCallId" => item["callId"] || item["id"],
      "title" => item["name"],
      "kind" => tool_kind(item["name"]),
      "rawInput" => item["arguments"],
      "status" => "pending"
    })
  end

  @spec item_type(map()) :: String.t() | nil
  def item_type(%{"type" => type}) when is_binary(type), do: type
  def item_type(_item), do: nil

  @spec item_id(map(), map()) :: any()
  def item_id(params, item) do
    params["callId"] || params["itemId"] || params["item_id"] || item["callId"] || item["id"]
  end

  @spec normalize_tool_status(any(), String.t()) :: String.t()
  def normalize_tool_status(status, fallback) when is_binary(status) do
    Map.get(@tool_status_aliases, status, fallback)
  end

  def normalize_tool_status(_status, fallback), do: fallback

  @spec mcp_tool_title(map()) :: String.t()
  def mcp_tool_title(%{"server" => server, "tool" => tool})
      when is_binary(server) and is_binary(tool),
      do: "#{server}:#{tool}"

  def mcp_tool_title(%{"tool" => tool}) when is_binary(tool), do: tool
  def mcp_tool_title(_item), do: "MCP Tool"

  @spec dynamic_tool_title(map()) :: String.t()
  def dynamic_tool_title(%{"namespace" => namespace, "tool" => tool})
      when is_binary(namespace) and is_binary(tool),
      do: "#{namespace}:#{tool}"

  def dynamic_tool_title(%{"tool" => tool}) when is_binary(tool), do: tool
  def dynamic_tool_title(_item), do: "Tool Call"

  @spec file_change_content(map()) :: map()
  def file_change_content(%{"path" => path, "newText" => new_text}) do
    tool_diff_content(path, new_text || "")
  end

  def file_change_content(%{"path" => path, "diff" => diff}) do
    tool_diff_content(path, diff || "")
  end

  def file_change_content(change), do: tool_text_content(format_raw(change))

  @spec dynamic_tool_content(any()) :: [map()]
  def dynamic_tool_content(items) when is_list(items) do
    Enum.map(items, fn
      %{"text" => text} -> tool_text_content(text)
      %{"content" => %{"text" => text}} -> tool_text_content(text)
      item -> tool_text_content(format_raw(item))
    end)
  end

  def dynamic_tool_content(value), do: [tool_text_content(format_raw(value))]

  @spec mark_replay(map()) :: map()
  def mark_replay(%{"params" => %{"update" => update}} = message) do
    meta =
      update
      |> Map.get("_meta", %{})
      |> Map.update("ex_mcp", %{"replay" => true}, &Map.put(&1, "replay", true))

    put_in(message, ["params", "update"], Map.put(update, "_meta", meta))
  end

  def mark_replay(message), do: message

  @spec tool_text_content(any()) :: map()
  def tool_text_content(text) do
    %{
      "type" => "content",
      "content" => %{"type" => "text", "text" => to_string(text || "")}
    }
  end

  @spec tool_diff_content(any(), any()) :: map()
  def tool_diff_content(path, new_text) do
    %{
      "type" => "diff",
      "path" => path || "",
      "oldText" => nil,
      "newText" => to_string(new_text || "")
    }
  end

  @spec command_title(any()) :: String.t()
  def command_title(command) when is_binary(command) and command != "", do: command
  def command_title(_command), do: "Run Command"

  @spec tool_kind(any()) :: String.t()
  def tool_kind(name) when is_binary(name) do
    name = String.downcase(name)

    cond do
      String.contains?(name, ["read", "view", "open"]) -> "read"
      String.contains?(name, ["write", "edit", "patch", "update"]) -> "edit"
      String.contains?(name, ["delete", "remove"]) -> "delete"
      String.contains?(name, ["move", "rename"]) -> "move"
      String.contains?(name, ["search", "grep", "find"]) -> "search"
      String.contains?(name, ["exec", "command", "bash", "shell"]) -> "execute"
      String.contains?(name, ["think", "reason"]) -> "think"
      String.contains?(name, ["fetch", "web"]) -> "fetch"
      true -> "other"
    end
  end

  def tool_kind(_name), do: "other"

  @spec format_web_search_results(any()) :: String.t()
  def format_web_search_results(results) when is_binary(results), do: results
  def format_web_search_results(nil), do: ""
  def format_web_search_results(results), do: Jason.encode!(results)

  @spec format_raw(any()) :: String.t()
  def format_raw(value) when is_binary(value), do: value
  def format_raw(nil), do: ""

  def format_raw(value) do
    Jason.encode!(value)
  rescue
    _ -> inspect(value)
  end
end
