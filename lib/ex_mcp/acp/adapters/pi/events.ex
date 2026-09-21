defmodule ExMCP.ACP.Adapters.Pi.Events do
  @moduledoc false

  # Pure folding of Pi's inbound stream events into ACP notifications for the
  # Pi ACP adapter. Nothing here owns a Port or reads the filesystem: the root
  # adapter still dispatches `process_event/2`, takes the edit snapshots that
  # `tool_execution_end/5` needs, and writes to the subprocess.

  alias ExMCP.ACP.AdapterEvents
  alias ExMCP.ACP.Adapters.Pi.{RPC, Tools}

  @typedoc """
  The subset of the adapter struct the stream-event helpers read and update.
  """
  @type state :: %{
          required(:session_id) => String.t() | nil,
          required(:cwd) => String.t() | nil,
          required(:current_tool_calls) => map(),
          required(:active_tool_executions) => map(),
          optional(any()) => any()
        }

  @doc """
  Folds a streamed `toolcall_*` assistant message event into a tool-call
  notification, minting the ACP tool call the first time the id is seen.

  Returns `{nil, state}` when the event carries no usable tool-call id.
  """
  @spec tool_call_stream_update(map(), state()) :: {map() | nil, state()}
  def tool_call_stream_update(tool_event, state) do
    tool_call =
      tool_event["toolCall"] ||
        partial_content_at(tool_event) ||
        tool_event

    tool_call_id = tool_call["id"] || tool_event["id"]
    tool_name = tool_call["name"] || tool_event["name"] || "tool"

    if is_binary(tool_call_id) and tool_call_id != "" do
      raw_input = tool_raw_input(tool_call)
      locations = Tools.locations(raw_input, state.cwd)
      existing_status = state.current_tool_calls[tool_call_id]
      status = existing_status || "pending"

      update =
        %{
          "toolCallId" => tool_call_id,
          "title" => tool_name,
          "kind" => Tools.kind(tool_name),
          "status" => status,
          "locations" => locations,
          "rawInput" => raw_input
        }
        |> RPC.compact()

      if existing_status do
        {AdapterEvents.tool_call_update(state.session_id, update), state}
      else
        state = %{
          state
          | current_tool_calls: Map.put(state.current_tool_calls, tool_call_id, "pending")
        }

        {AdapterEvents.tool_call(state.session_id, update), state}
      end
    else
      {nil, state}
    end
  end

  @doc """
  Builds the `tool_execution_start` notification and records the execution.

  `line` is the line number resolved from the pre-edit snapshot, which the
  root adapter takes because it touches the filesystem.
  """
  @spec tool_execution_start(String.t(), String.t(), map(), pos_integer() | nil, state()) ::
          {map(), state()}
  def tool_execution_start(tool_call_id, tool_name, args, line, state) do
    locations = Tools.locations(args, state.cwd, line)

    update = %{
      "toolCallId" => tool_call_id,
      "title" => tool_name,
      "kind" => Tools.kind(tool_name),
      "status" => "in_progress",
      "locations" => locations,
      "rawInput" => args
    }

    {notification, current_tool_calls} =
      if Map.has_key?(state.current_tool_calls, tool_call_id) do
        {AdapterEvents.tool_call_update(state.session_id, RPC.compact(update)),
         Map.put(state.current_tool_calls, tool_call_id, "in_progress")}
      else
        {AdapterEvents.tool_call(state.session_id, RPC.compact(update)),
         Map.put(state.current_tool_calls, tool_call_id, "in_progress")}
      end

    state = %{
      state
      | active_tool_executions:
          Map.put(state.active_tool_executions, tool_call_id, %{name: tool_name, args: args}),
        current_tool_calls: current_tool_calls
    }

    {notification, state}
  end

  @spec tool_execution_update(String.t() | nil, String.t(), term()) :: map()
  def tool_execution_update(session_id, tool_call_id, partial_result) do
    text = Tools.result_text(partial_result)

    session_id
    |> AdapterEvents.tool_call_update(%{
      "toolCallId" => tool_call_id,
      "status" => "in_progress",
      "content" => Tools.text_content(text),
      "rawOutput" => partial_result
    })
    |> RPC.compact()
  end

  @doc """
  Builds the terminal `tool_execution_end` notification and forgets the call.

  `content` already carries the optional edit diff, which the root adapter
  resolves from its snapshot because it reads the file back.
  """
  @spec tool_execution_end(String.t(), term(), boolean(), term(), state()) :: {map(), state()}
  def tool_execution_end(tool_call_id, result, is_error, content, state) do
    notification =
      state.session_id
      |> AdapterEvents.tool_call_update(%{
        "toolCallId" => tool_call_id,
        "status" => if(is_error, do: "failed", else: "completed"),
        "content" => content,
        "rawOutput" => result
      })
      |> RPC.compact()

    state = %{
      state
      | active_tool_executions: Map.delete(state.active_tool_executions, tool_call_id),
        current_tool_calls: Map.delete(state.current_tool_calls, tool_call_id)
    }

    {notification, state}
  end

  @doc """
  Builds the notifications for Pi's auto-compaction and auto-retry status
  events: the raw event as session info, preceded by a human-readable chunk
  when the type has one.
  """
  @spec auto_status_messages(String.t() | nil, String.t(), map()) :: [map()]
  def auto_status_messages(session_id, type, event) do
    info =
      AdapterEvents.session_info_update(session_id, %{
        "_meta" => %{"ex_mcp" => %{"pi" => event}}
      })

    case auto_status_text(type) do
      nil ->
        [info]

      text ->
        [
          AdapterEvents.agent_message_chunk(session_id, text),
          info
        ]
    end
  end

  @spec usage_from_agent_end(map()) :: map()
  def usage_from_agent_end(event) do
    usage =
      event
      |> Map.get("messages", [])
      |> Enum.filter(&(&1["role"] == "assistant"))
      |> List.last()
      |> case do
        %{"usage" => usage} -> usage
        _ -> %{}
      end

    %{
      "inputTokens" => usage["input"] || 0,
      "outputTokens" => usage["output"] || 0,
      "cacheReadTokens" => usage["cacheRead"] || 0,
      "cacheWriteTokens" => usage["cacheWrite"] || 0,
      "cost" => get_in(usage, ["cost", "total"])
    }
  end

  @doc """
  Replays a `get_messages` payload as ACP session updates for session/load.
  """
  @spec replay_messages(term(), String.t() | nil) :: [map()]
  def replay_messages(data, session_id) do
    messages = if is_map(data) and is_list(data["messages"]), do: data["messages"], else: []

    Enum.flat_map(messages, fn message ->
      case message["role"] do
        "user" ->
          replay_text_update(
            session_id,
            "user_message_chunk",
            normalize_message_text(message["content"])
          )

        "assistant" ->
          replay_text_update(
            session_id,
            "agent_message_chunk",
            normalize_message_text(message["content"])
          )

        "toolResult" ->
          tool_name = message["toolName"] || "tool"
          tool_call_id = message["toolCallId"] || "tool-#{System.unique_integer([:positive])}"
          text = Tools.result_text(message)

          [
            AdapterEvents.tool_call(session_id, %{
              "toolCallId" => tool_call_id,
              "title" => tool_name,
              "kind" => Tools.kind(tool_name),
              "status" => "completed",
              "rawOutput" => message
            }),
            AdapterEvents.tool_call_update(session_id, %{
              "toolCallId" => tool_call_id,
              "status" => if(message["isError"], do: "failed", else: "completed"),
              "content" => Tools.text_content(text),
              "rawOutput" => message
            })
            |> RPC.compact()
          ]

        _ ->
          []
      end
    end)
  end

  defp auto_status_text("auto_compaction_start"), do: "Context nearing limit, compacting."
  defp auto_status_text("auto_compaction_end"), do: "Compaction finished, resuming."
  defp auto_status_text("auto_retry_start"), do: "Retrying after transient failure."
  defp auto_status_text("auto_retry_end"), do: "Retry finished, resuming."
  defp auto_status_text(_type), do: nil

  defp tool_raw_input(%{"arguments" => args}) when is_map(args), do: args
  defp tool_raw_input(%{"args" => args}) when is_map(args), do: args

  defp tool_raw_input(%{"partialArgs" => partial}) when is_binary(partial) do
    case Jason.decode(partial) do
      {:ok, args} when is_map(args) -> args
      _ -> %{"partialArgs" => partial}
    end
  end

  defp tool_raw_input(_tool_call), do: %{}

  # Pi streams a tool call either under `toolCall` or as one entry of the
  # partial content list addressed by `contentIndex`. `Access` cannot index a
  # list, so the previous `get_in/2` raised on exactly the shape this clause
  # exists to support.
  defp partial_content_at(%{"partial" => %{"content" => content}} = tool_event)
       when is_list(content) do
    case tool_event["contentIndex"] || 0 do
      index when is_integer(index) and index >= 0 ->
        case Enum.at(content, index) do
          entry when is_map(entry) -> entry
          _other -> nil
        end

      _index ->
        nil
    end
  end

  defp partial_content_at(_tool_event), do: nil

  defp replay_text_update(_session_id, _type, ""), do: []

  defp replay_text_update(session_id, type, text) do
    [
      AdapterEvents.session_update_type(session_id, type, %{
        "content" => %{"type" => "text", "text" => text}
      })
    ]
  end

  defp normalize_message_text(content) when is_binary(content), do: content

  defp normalize_message_text(content) when is_list(content) do
    content
    |> Enum.flat_map(fn
      %{"type" => "text", "text" => text} when is_binary(text) -> [text]
      _ -> []
    end)
    |> Enum.join("")
  end

  defp normalize_message_text(_content), do: ""
end
