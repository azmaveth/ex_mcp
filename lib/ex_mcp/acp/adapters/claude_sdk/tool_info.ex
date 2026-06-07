defmodule ExMCP.ACP.Adapters.ClaudeSDK.ToolInfo do
  @moduledoc """
  Pure Claude tool-use to ACP tool-call metadata conversion.
  """

  @tool_kinds %{
    "Read" => "read",
    "Write" => "edit",
    "Edit" => "edit",
    "MultiEdit" => "edit",
    "NotebookEdit" => "edit",
    "Bash" => "execute",
    "Grep" => "search",
    "Glob" => "search",
    "WebFetch" => "fetch",
    "WebSearch" => "search",
    "Agent" => "think",
    "Task" => "think",
    "TodoRead" => "read",
    "TodoWrite" => "think"
  }

  @doc "Converts a Claude tool use into ACP tool-call fields."
  @spec from_use(String.t(), map(), String.t() | nil, String.t() | nil) :: map()
  def from_use(name, input, id, cwd) do
    name = name || "tool"
    input = input || %{}

    name
    |> do_from_use(input, id, cwd)
    |> Map.put_new("kind", Map.get(@tool_kinds, name, "other"))
    |> Map.put_new("rawInput", input)
    |> compact()
  end

  @doc "Converts Claude TodoWrite input into ACP plan entries."
  @spec plan_entries(map()) :: [map()]
  def plan_entries(%{"todos" => todos}) when is_list(todos) do
    Enum.map(todos, fn todo ->
      %{
        "content" => todo["content"] || todo["text"] || todo["title"] || "",
        "priority" => todo["priority"] || "medium",
        "status" => plan_status(todo["status"])
      }
    end)
  end

  def plan_entries(_), do: []

  defp do_from_use("Read", input, _id, cwd) do
    path = input["file_path"] || input["path"]

    %{
      "title" => "Read #{display_path(path, cwd)}#{format_line_suffix(input)}",
      "kind" => "read",
      "locations" => if(path, do: [%{"path" => path, "line" => input["offset"] || 1}], else: [])
    }
  end

  defp do_from_use("Write", input, _id, cwd) do
    path = input["file_path"] || input["path"]

    %{
      "title" => "Write #{display_path(path, cwd)}",
      "kind" => "edit",
      "content" =>
        if(path && input["content"],
          do: [
            %{"type" => "diff", "path" => path, "oldText" => nil, "newText" => input["content"]}
          ],
          else: []
        ),
      "locations" => if(path, do: [%{"path" => path, "line" => 1}], else: [])
    }
  end

  defp do_from_use("Edit", input, _id, cwd) do
    path = input["file_path"] || input["path"]

    %{
      "title" => "Edit #{display_path(path, cwd)}",
      "kind" => "edit",
      "content" =>
        if(path && input["old_string"] && input["new_string"],
          do: [
            %{
              "type" => "diff",
              "path" => path,
              "oldText" => input["old_string"],
              "newText" => input["new_string"]
            }
          ],
          else: []
        ),
      "locations" => if(path, do: [%{"path" => path, "line" => 1}], else: [])
    }
  end

  defp do_from_use("MultiEdit", input, _id, cwd) do
    path = input["file_path"] || input["path"]
    edits = input["edits"] || []

    %{
      "title" => "Edit #{display_path(path, cwd)}",
      "kind" => "edit",
      "content" =>
        Enum.map(edits, fn edit ->
          %{
            "type" => "diff",
            "path" => path,
            "oldText" => edit["old_string"],
            "newText" => edit["new_string"]
          }
        end),
      "locations" => if(path, do: [%{"path" => path, "line" => 1}], else: [])
    }
  end

  defp do_from_use("Bash", input, id, _cwd) do
    command = input["command"] || ""

    %{
      "title" => if(command != "", do: truncate(command, 80), else: "Terminal"),
      "kind" => "execute",
      "content" => [%{"type" => "terminal", "terminalId" => id}],
      "_meta" => %{"ex_mcp.claude_sdk" => %{"terminal" => true, "command" => command}}
    }
  end

  defp do_from_use("Grep", input, _id, _cwd) do
    pattern = input["pattern"] || ""

    %{
      "title" => "Search: #{truncate(pattern, 60)}",
      "kind" => "search",
      "content" => text_content(pattern)
    }
  end

  defp do_from_use("Glob", input, _id, _cwd) do
    %{"title" => "Find: #{truncate(input["pattern"] || "", 60)}", "kind" => "search"}
  end

  defp do_from_use("WebFetch", input, _id, _cwd) do
    url = input["url"] || ""
    %{"title" => "Fetch: #{truncate(url, 80)}", "kind" => "fetch", "content" => text_content(url)}
  end

  defp do_from_use("WebSearch", input, _id, _cwd) do
    %{"title" => "Search: #{truncate(input["query"] || "", 60)}", "kind" => "search"}
  end

  defp do_from_use("TodoWrite", input, _id, _cwd) do
    count = input |> plan_entries() |> length()
    %{"title" => "Update plan#{if count > 0, do: " (#{count})", else: ""}", "kind" => "think"}
  end

  defp do_from_use(name, input, _id, _cwd) when name in ["Agent", "Task"] do
    desc = input["description"] || input["prompt"] || "Task"

    %{
      "title" => truncate(desc, 80),
      "kind" => "think",
      "content" => text_content(input["prompt"])
    }
  end

  defp do_from_use(name, _input, _id, _cwd) do
    %{"title" => name, "kind" => Map.get(@tool_kinds, name, "other")}
  end

  defp text_content(nil), do: []
  defp text_content(""), do: []

  defp text_content(text) do
    [%{"type" => "content", "content" => %{"type" => "text", "text" => text}}]
  end

  defp display_path(nil, _cwd), do: "File"

  defp display_path(path, cwd) when is_binary(path) and is_binary(cwd) do
    resolved_cwd = Path.expand(cwd)

    if String.starts_with?(path, resolved_cwd <> "/") do
      Path.relative_to(path, resolved_cwd)
    else
      Path.basename(path)
    end
  end

  defp display_path(path, _cwd) when is_binary(path), do: Path.basename(path)

  defp format_line_suffix(%{"limit" => limit, "offset" => offset})
       when is_integer(limit) and limit > 0 and is_integer(offset) do
    " (#{offset}-#{offset + limit - 1})"
  end

  defp format_line_suffix(%{"offset" => offset}) when is_integer(offset) and offset > 1 do
    " (from line #{offset})"
  end

  defp format_line_suffix(_), do: ""

  defp plan_status("completed"), do: "completed"
  defp plan_status("done"), do: "completed"
  defp plan_status("in_progress"), do: "in_progress"
  defp plan_status("active"), do: "in_progress"
  defp plan_status(_), do: "pending"

  defp truncate(str, max) when is_binary(str) and byte_size(str) > max do
    String.slice(str, 0, max) <> "..."
  end

  defp truncate(str, _max) when is_binary(str), do: str
  defp truncate(_, _), do: ""

  defp compact(map) do
    map
    |> Enum.reject(fn {_key, value} -> value in [nil, [], %{}] end)
    |> Map.new()
  end
end
