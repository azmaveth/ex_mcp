defmodule ExMCP.ACP.Adapters.Codex.Content do
  @moduledoc false
  # Pure content mapping for the Codex ACP adapter: ACP prompt blocks to
  # app-server input items, native item started/completed events to ACP
  # session updates, history replay, and streamed-text reconciliation. Each
  # function returns plain data; the root adapter owns session state and
  # decides how the resulting messages are emitted.

  alias ExMCP.ACP.AdapterEvents
  alias ExMCP.ACP.Adapters.Codex.{Events, FileChanges}
  alias ExMCP.Internal.Maps

  @type session_id :: String.t() | nil
  @type item :: map()
  @type message :: map()

  # Prompt mapping

  @spec extract_input_items(term()) :: [map()]
  def extract_input_items(nil), do: [%{"type" => "text", "text" => ""}]

  def extract_input_items(prompt) when is_binary(prompt),
    do: [text_input(prompt)]

  def extract_input_items(blocks) when is_list(blocks) do
    items =
      Enum.flat_map(blocks, fn
        %{"type" => "text", "text" => text} ->
          [text_input(text)]

        %{"type" => "image"} = img ->
          [image_input(img)]

        %{"type" => "resource_link"} = resource ->
          [text_input(format_uri_as_link(resource["name"], resource["uri"]))]

        %{"type" => "resource", "resource" => %{"text" => text, "uri" => uri}} ->
          [
            text_input(
              "#{format_uri_as_link(nil, uri)}\n<context ref=\"#{uri}\">\n#{text}\n</context>"
            )
          ]

        %{
          "type" => "resource",
          "resource" => %{"blob" => blob, "mimeType" => mime_type, "uri" => uri}
        } ->
          if image_mime_type?(mime_type) do
            [%{"type" => "image", "url" => "data:#{mime_type};base64,#{blob}"}]
          else
            mime_type = mime_type || "application/octet-stream"

            context =
              [
                format_uri_as_link(nil, uri),
                ~s(<context ref="#{uri}" mimeType="#{mime_type}" encoding="base64">),
                blob,
                "</context>"
              ]
              |> Enum.join("\n")

            [
              text_input(context)
            ]
          end

        _ ->
          []
      end)

    if items == [], do: [text_input("")], else: items
  end

  def extract_input_items(_), do: [text_input("")]

  defp text_input(text),
    do: %{"type" => "text", "text" => to_string(text || ""), "text_elements" => []}

  defp image_input(%{"uri" => uri}) when is_binary(uri) and uri != "" do
    %{"type" => "image", "url" => uri}
  end

  defp image_input(%{"data" => data} = img) do
    mime_type = img["mimeType"] || "image/png"
    %{"type" => "image", "url" => "data:#{mime_type};base64,#{data}"}
  end

  defp image_input(_img), do: %{"type" => "image", "url" => ""}

  defp image_mime_type?(mime_type) when is_binary(mime_type),
    do: String.starts_with?(mime_type, "image/")

  defp image_mime_type?(_mime_type), do: false

  defp format_uri_as_link(name, uri) when is_binary(name) and name != "", do: "[@#{name}](#{uri})"

  defp format_uri_as_link(_name, "file://" <> path = uri) do
    name = path |> String.split("/") |> List.last()
    "[@#{name}](#{uri})"
  end

  defp format_uri_as_link(_name, uri) when is_binary(uri), do: uri
  defp format_uri_as_link(_name, nil), do: ""

  # Native item mapping

  @spec item_started(session_id(), item(), map()) :: [message()]
  def item_started(session_id, item, params) do
    case Events.item_type(item) do
      type when type in ["function_call", "functionCall"] ->
        [Events.tool_call_started(session_id, item)]

      "commandExecution" ->
        tool_call_id = Events.item_id(params, item)

        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => tool_call_id,
            "title" => Events.command_title(item["command"]),
            "kind" => "execute",
            "status" => Events.normalize_tool_status(item["status"], "in_progress"),
            "rawInput" => %{"command" => item["command"], "cwd" => item["cwd"]},
            "content" => [%{"type" => "terminal", "terminalId" => tool_call_id}],
            "_meta" => Events.terminal_info(tool_call_id, item["cwd"])
          })

        [notification]

      "fileChange" ->
        [FileChanges.started(session_id, params, item)]

      "mcpToolCall" ->
        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => Events.mcp_tool_title(item),
            "kind" => "execute",
            "status" => Events.normalize_tool_status(item["status"], "in_progress"),
            "rawInput" => Events.mcp_raw_input(item),
            "_meta" => %{"is_mcp_tool_call" => true}
          })

        [notification]

      "dynamicToolCall" ->
        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => Events.dynamic_tool_title(item),
            "kind" => Events.tool_kind(item["tool"]),
            "status" => Events.normalize_tool_status(item["status"], "in_progress"),
            "rawInput" => item["arguments"]
          })

        [notification]

      "webSearch" ->
        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => Events.web_search_title(item),
            "kind" => "search",
            "status" => "in_progress",
            "rawInput" => item
          })

        [notification]

      "imageView" ->
        path = item["path"] || ""

        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => "View Image #{path}",
            "kind" => "read",
            "status" => "completed",
            "content" => [
              %{
                "type" => "content",
                "content" => %{"type" => "resource_link", "name" => path, "uri" => path}
              }
            ],
            "locations" => [%{"path" => path}],
            "rawInput" => %{"path" => path}
          })

        [notification]

      "imageGeneration" ->
        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => "Image generation",
            "kind" => "other",
            "status" => Events.normalize_tool_status(item["status"], "in_progress"),
            "rawInput" => %{"revisedPrompt" => item["revisedPrompt"]}
          })

        [notification]

      _ ->
        []
    end
  end

  @spec item_completed(session_id(), item()) :: [message()]
  def item_completed(session_id, %{"type" => "reasoning"} = item) do
    text =
      (item["content"] || item["summary"] || [])
      |> List.wrap()
      |> Enum.join("\n")

    notification =
      AdapterEvents.agent_thought_chunk(session_id, text, meta: %{"ex_mcp" => %{"final" => true}})

    [notification]
  end

  def item_completed(session_id, %{"type" => "function_call"} = item) do
    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["callId"] || item["id"],
        "status" => "completed",
        "kind" => Events.tool_kind(item["name"]),
        "rawInput" => item["arguments"]
      })

    [notification]
  end

  def item_completed(session_id, %{"type" => "functionCall"} = item) do
    item_completed(session_id, Map.put(item, "type", "function_call"))
  end

  def item_completed(session_id, %{"type" => "function_call_output"} = item) do
    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["callId"] || item["id"],
        "status" => if(item["isError"], do: "failed", else: "completed"),
        "content" => [Events.tool_text_content(item["output"] || item["text"] || "")],
        "rawOutput" => item["output"] || item["text"] || ""
      })

    [notification]
  end

  def item_completed(session_id, %{"type" => "commandExecution"} = item) do
    tool_call_id = item["id"]

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => tool_call_id,
        "status" => Events.normalize_tool_status(item["status"], "completed"),
        "rawOutput" => %{
          "exit_code" => item["exitCode"],
          "formatted_output" => item["aggregatedOutput"] || ""
        },
        "_meta" => Events.terminal_exit(tool_call_id, item["exitCode"])
      })

    [notification]
  end

  def item_completed(session_id, %{"type" => "patch"} = item) do
    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["callId"] || item["id"],
        "kind" => "edit",
        "status" => "completed",
        "content" => [Events.tool_diff_content(item["path"], item["diff"] || item["text"] || "")]
      })

    [notification]
  end

  def item_completed(session_id, %{"type" => "fileChange"} = item) do
    [FileChanges.completed(session_id, item)]
  end

  def item_completed(session_id, %{"type" => "mcpToolCall"} = item) do
    output = item["result"] || item["error"] || %{}

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["id"],
        "status" =>
          Events.normalize_tool_status(
            item["status"],
            if(item["error"], do: "failed", else: "completed")
          ),
        "rawInput" => Events.mcp_raw_input(item),
        "rawOutput" => Events.mcp_raw_output(item) || output
      })

    [notification]
  end

  def item_completed(session_id, %{"type" => "dynamicToolCall"} = item) do
    output = item["contentItems"] || []

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["id"],
        "status" =>
          Events.normalize_tool_status(
            item["status"],
            if(item["success"] == false, do: "failed", else: "completed")
          ),
        "content" => Events.dynamic_tool_content(output),
        "rawOutput" => output
      })

    [notification]
  end

  def item_completed(session_id, %{"type" => "webSearch"} = item) do
    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["id"],
        "title" => Events.web_search_title(item),
        "status" => "completed",
        "rawInput" => item
      })

    [notification]
  end

  def item_completed(session_id, %{"type" => "imageView"} = item) do
    item_started(session_id, item, %{})
  end

  def item_completed(session_id, %{"type" => "imageGeneration"} = item) do
    content =
      []
      |> maybe_add_image_revised_prompt(item["revisedPrompt"])
      |> maybe_add_generated_image(item)

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["id"],
        "status" => Events.normalize_tool_status(item["status"], "completed"),
        "content" => content,
        "rawOutput" => item
      })

    [notification]
  end

  def item_completed(session_id, %{"type" => "contextCompaction"} = _item) do
    [AdapterEvents.agent_message_chunk(session_id, "Context compacted\n")]
  end

  def item_completed(_session_id, _item), do: []

  @spec unstreamed_text(String.t(), String.t()) :: String.t()
  def unstreamed_text(text, ""), do: text
  def unstreamed_text(text, streamed) when text == streamed, do: ""

  # When deltas were streamed but the completed text is not an extension of
  # them, trust the stream: repeating the whole message is the duplication this
  # guards against, and the accumulator already holds what the client saw.
  def unstreamed_text(text, streamed) do
    if String.starts_with?(text, streamed) do
      binary_part(text, byte_size(streamed), byte_size(text) - byte_size(streamed))
    else
      ""
    end
  end

  defp maybe_add_image_revised_prompt(content, prompt) when is_binary(prompt) and prompt != "" do
    content ++ [Events.tool_text_content("Revised prompt: #{prompt}")]
  end

  defp maybe_add_image_revised_prompt(content, _prompt), do: content

  defp maybe_add_generated_image(content, %{"result" => result} = item)
       when is_binary(result) and result != "" do
    image =
      %{"type" => "image", "data" => result, "mimeType" => "image/png"}
      |> Maps.put_non_empty("uri", item["savedPath"])

    content ++ [%{"type" => "content", "content" => image}]
  end

  defp maybe_add_generated_image(content, _item), do: content

  # History replay

  @spec replay_turns(session_id(), [map()]) :: [message()]
  def replay_turns(session_id, turns) do
    Enum.flat_map(turns, fn turn ->
      turn
      |> Map.get("items", [])
      |> Enum.flat_map(&replay_item(session_id, &1))
    end)
  end

  defp replay_item(session_id, %{"type" => "agent_message"} = item) do
    [
      AdapterEvents.agent_message_chunk(session_id, item["text"] || item["message"] || "",
        meta: %{"ex_mcp" => %{"replay" => true}}
      )
    ]
  end

  defp replay_item(session_id, %{"type" => "reasoning"} = item) do
    [
      AdapterEvents.agent_thought_chunk(session_id, item["text"] || item["summary"] || "",
        meta: %{"ex_mcp" => %{"replay" => true}}
      )
    ]
  end

  defp replay_item(session_id, item) do
    Enum.map(item_completed(session_id, item), &Events.mark_replay/1)
  end
end
