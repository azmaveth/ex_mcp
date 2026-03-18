defmodule ExMCP.ACP.Adapters.Pi do
  @moduledoc """
  ACP adapter for the Pi coding agent (badlogic/pi-mono).

  Translates between ACP JSON-RPC and Pi's RPC NDJSON protocol.
  Pi runs as a subprocess in `--mode rpc` and communicates via
  JSONL on stdin/stdout.

  ## Pi RPC Protocol

  - **Input:** JSONL on stdin: `{"type":"prompt","id":"msg-1","message":"..."}`
  - **Output:** JSONL on stdout with event types: message_update, agent_end,
    tool_execution_start/update/end, auto_compaction_start/end, etc.

  ## ACP Mapping

  | Pi Event | ACP Message |
  |---|---|
  | `message_update` (text_delta) | `session/update` notification (text) |
  | `message_update` (thinking_delta) | `session/update` notification (thinking) |
  | `message_update` (tool_call) | `session/update` notification (tool_call) |
  | `tool_execution_start/end` | `session/update` notification (tool_result) |
  | `agent_end` | prompt response result |
  | `auto_compaction_*` | `session/update` notification (status) |

  ## Features

  - Session persistence via `--session <path>` flag
  - Thinking level control (off/minimal/low/medium/high/xhigh)
  - Steering and follow-up message queuing
  - Image support with data-url prefix stripping
  - Tool execution streaming with progress updates
  - Context compaction (manual and auto)
  - Session forking and switching
  - Model switching mid-session

  ## Configuration

      config :arbor_ai, :acp_providers, %{
        pi: %{
          transport_mod: ExMCP.ACP.AdapterTransport,
          adapter: ExMCP.ACP.Adapters.Pi,
          adapter_opts: [
            cli_path: "pi",
            model: "anthropic/claude-sonnet-4-20250514",
            thinking_level: "medium",
            session_path: "/path/to/session.jsonl"
          ]
        }
      }
  """

  @behaviour ExMCP.ACP.Adapter

  require Logger

  @thinking_levels ~w(off minimal low medium high xhigh)

  defstruct [
    :session_id,
    :model,
    :session_file,
    :thinking_level,
    text_acc: [],
    pending_prompt_id: nil,
    tool_calls: [],
    active_tool_executions: %{},
    msg_counter: 0,
    is_streaming: false,
    opts: []
  ]

  # ── Adapter Callbacks ──────────────────────────────────────────

  @impl true
  def init(opts) do
    {:ok,
     %__MODULE__{
       opts: opts,
       model: Keyword.get(opts, :model),
       thinking_level: Keyword.get(opts, :thinking_level, "medium")
     }}
  end

  @impl true
  def command(opts) do
    cli_path = Keyword.get(opts, :cli_path, "pi")

    args = ["--mode", "rpc", "--no-themes"]

    args =
      args
      |> append_opt(opts, :model, "--model")
      |> append_opt(opts, :cwd, "--cwd")
      |> append_opt(opts, :system_prompt, "--system-prompt")
      |> append_opt(opts, :api_key, "--api-key")
      |> append_opt(opts, :session_path, "--session")
      |> append_opt(opts, :session_dir, "--session-dir")

    # Disable session persistence if requested
    args =
      if Keyword.get(opts, :no_session, false) do
        args ++ ["--no-session"]
      else
        args
      end

    {cli_path, args}
  end

  @impl true
  def capabilities do
    %{
      "streaming" => true,
      "thinkingLevels" => @thinking_levels,
      "supportedModes" => [
        %{"id" => "code", "label" => "Code Mode"}
      ],
      "features" => %{
        "steering" => true,
        "followUp" => true,
        "compaction" => true,
        "sessionForking" => true,
        "modelSwitching" => true,
        "bash" => true
      }
    }
  end

  # ── Outbound: ACP → Pi RPC ────────────────────────────────────

  @impl true
  def translate_outbound(%{"method" => "initialize"}, state) do
    {:ok, :skip, state}
  end

  def translate_outbound(%{"method" => "session/new", "id" => _id}, state) do
    # Send new_session to Pi to start fresh
    rpc_msg = %{"type" => "new_session"}
    data = encode_rpc(rpc_msg)
    {:ok, data, state}
  end

  def translate_outbound(%{"method" => "session/load"}, state) do
    # Session loading is handled via --session flag at startup
    {:ok, :skip, state}
  end

  def translate_outbound(
        %{"method" => "session/prompt", "id" => id, "params" => params},
        state
      ) do
    content = extract_prompt_text(params["prompt"])
    msg_id = "msg-#{state.msg_counter + 1}"

    rpc_msg = %{
      "type" => "prompt",
      "id" => msg_id,
      "message" => content
    }

    # Add images if present (with data-url stripping)
    images = extract_images(params["prompt"])
    rpc_msg = if images != [], do: Map.put(rpc_msg, "images", images), else: rpc_msg

    # Support streaming behavior (steer vs follow-up)
    rpc_msg =
      case params["streamingBehavior"] do
        nil -> rpc_msg
        behavior -> Map.put(rpc_msg, "streamingBehavior", behavior)
      end

    data = encode_rpc(rpc_msg)

    state = %{
      state
      | pending_prompt_id: id,
        msg_counter: state.msg_counter + 1,
        text_acc: [],
        tool_calls: [],
        is_streaming: true
    }

    {:ok, data, state}
  end

  def translate_outbound(%{"method" => "session/cancel"}, state) do
    # Send abort to Pi
    rpc_msg = %{"type" => "abort"}
    data = encode_rpc(rpc_msg)
    {:ok, data, state}
  end

  # ── Extended Pi Commands via ACP Extensions ───────────────────
  # These map ACP extension methods to Pi RPC commands.

  def translate_outbound(%{"method" => "pi/steer", "params" => params}, state) do
    rpc_msg = %{"type" => "steer", "message" => params["message"]}

    rpc_msg =
      case params["images"] do
        nil -> rpc_msg
        images -> Map.put(rpc_msg, "images", normalize_images(images))
      end

    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(%{"method" => "pi/follow_up", "params" => params}, state) do
    rpc_msg = %{"type" => "follow_up", "message" => params["message"]}

    rpc_msg =
      case params["images"] do
        nil -> rpc_msg
        images -> Map.put(rpc_msg, "images", normalize_images(images))
      end

    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(%{"method" => "pi/compact", "params" => params}, state) do
    rpc_msg = %{"type" => "compact"}

    rpc_msg =
      case params["customInstructions"] do
        nil -> rpc_msg
        instr -> Map.put(rpc_msg, "customInstructions", instr)
      end

    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(%{"method" => "pi/compact"}, state) do
    {:ok, encode_rpc(%{"type" => "compact"}), state}
  end

  def translate_outbound(%{"method" => "pi/set_thinking_level", "params" => params}, state) do
    level = params["level"]

    if level in @thinking_levels do
      rpc_msg = %{"type" => "set_thinking_level", "level" => level}
      state = %{state | thinking_level: level}
      {:ok, encode_rpc(rpc_msg), state}
    else
      {:ok, :skip, state}
    end
  end

  def translate_outbound(%{"method" => "pi/set_model", "params" => params}, state) do
    rpc_msg = %{
      "type" => "set_model",
      "provider" => params["provider"],
      "modelId" => params["modelId"]
    }

    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(%{"method" => "pi/get_state"}, state) do
    {:ok, encode_rpc(%{"type" => "get_state"}), state}
  end

  def translate_outbound(%{"method" => "pi/get_session_stats"}, state) do
    {:ok, encode_rpc(%{"type" => "get_session_stats"}), state}
  end

  def translate_outbound(%{"method" => "pi/switch_session", "params" => params}, state) do
    rpc_msg = %{"type" => "switch_session", "sessionPath" => params["sessionPath"]}
    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(%{"method" => "pi/fork", "params" => params}, state) do
    rpc_msg = %{"type" => "fork", "entryId" => params["entryId"]}
    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(%{"method" => "pi/get_fork_messages"}, state) do
    {:ok, encode_rpc(%{"type" => "get_fork_messages"}), state}
  end

  def translate_outbound(%{"method" => "pi/bash", "params" => params}, state) do
    rpc_msg = %{"type" => "bash", "command" => params["command"]}
    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(%{"method" => "pi/export_html", "params" => params}, state) do
    rpc_msg = %{"type" => "export_html"}

    rpc_msg =
      case params["outputPath"] do
        nil -> rpc_msg
        path -> Map.put(rpc_msg, "outputPath", path)
      end

    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(%{"method" => "pi/export_html"}, state) do
    {:ok, encode_rpc(%{"type" => "export_html"}), state}
  end

  def translate_outbound(%{"method" => "pi/set_session_name", "params" => params}, state) do
    rpc_msg = %{"type" => "set_session_name", "name" => params["name"]}
    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(%{"method" => "pi/get_commands"}, state) do
    {:ok, encode_rpc(%{"type" => "get_commands"}), state}
  end

  def translate_outbound(%{"method" => "pi/get_available_models"}, state) do
    {:ok, encode_rpc(%{"type" => "get_available_models"}), state}
  end

  def translate_outbound(%{"method" => "pi/set_auto_compaction", "params" => params}, state) do
    rpc_msg = %{"type" => "set_auto_compaction", "enabled" => params["enabled"]}
    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(%{"method" => "pi/get_messages"}, state) do
    {:ok, encode_rpc(%{"type" => "get_messages"}), state}
  end

  def translate_outbound(%{"method" => "pi/cycle_model"}, state) do
    {:ok, encode_rpc(%{"type" => "cycle_model"}), state}
  end

  def translate_outbound(%{"method" => "pi/cycle_thinking_level"}, state) do
    {:ok, encode_rpc(%{"type" => "cycle_thinking_level"}), state}
  end

  def translate_outbound(%{"method" => "pi/set_steering_mode", "params" => params}, state) do
    rpc_msg = %{"type" => "set_steering_mode", "mode" => params["mode"]}
    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(%{"method" => "pi/set_follow_up_mode", "params" => params}, state) do
    rpc_msg = %{"type" => "set_follow_up_mode", "mode" => params["mode"]}
    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(%{"method" => "pi/set_auto_retry", "params" => params}, state) do
    rpc_msg = %{"type" => "set_auto_retry", "enabled" => params["enabled"]}
    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(%{"method" => "pi/abort_retry"}, state) do
    {:ok, encode_rpc(%{"type" => "abort_retry"}), state}
  end

  def translate_outbound(%{"method" => "pi/abort_bash"}, state) do
    {:ok, encode_rpc(%{"type" => "abort_bash"}), state}
  end

  def translate_outbound(%{"method" => "pi/get_last_assistant_text"}, state) do
    {:ok, encode_rpc(%{"type" => "get_last_assistant_text"}), state}
  end

  # Extension UI response — CRITICAL: forwards dialog responses back to Pi
  # When Pi sends extension_ui_request (select/confirm/input/editor),
  # the host must send back extension_ui_response via this method.
  def translate_outbound(
        %{"method" => "pi/extension_ui_response", "params" => params},
        state
      ) do
    rpc_msg = %{"type" => "extension_ui_response", "id" => params["id"]}

    rpc_msg =
      cond do
        Map.has_key?(params, "value") ->
          Map.put(rpc_msg, "value", params["value"])

        Map.has_key?(params, "confirmed") ->
          Map.put(rpc_msg, "confirmed", params["confirmed"])

        Map.has_key?(params, "cancelled") ->
          Map.put(rpc_msg, "cancelled", true)

        true ->
          Map.put(rpc_msg, "cancelled", true)
      end

    {:ok, encode_rpc(rpc_msg), state}
  end

  def translate_outbound(_msg, state) do
    {:ok, :skip, state}
  end

  # ── Inbound: Pi RPC → ACP ─────────────────────────────────────

  @impl true
  def translate_inbound(line, state) do
    trimmed = String.trim(line)

    if trimmed == "" do
      {:skip, state}
    else
      case Jason.decode(trimmed) do
        {:ok, event} ->
          process_event(event, state)

        {:error, _reason} ->
          Logger.debug("[Pi Adapter] Non-JSON line: #{String.slice(trimmed, 0..100)}")
          {:skip, state}
      end
    end
  end

  # ── Event Processing ───────────────────────────────────────────

  # Text streaming — message_update with text_delta
  defp process_event(
         %{
           "type" => "message_update",
           "assistantMessageEvent" => %{"type" => "text_delta", "delta" => delta}
         },
         state
       ) do
    state = %{state | text_acc: [delta | state.text_acc]}

    notification =
      build_session_update(state, %{
        "type" => "agent_message_chunk",
        "content" => %{"type" => "text", "text" => delta}
      })

    {:messages, [notification], state}
  end

  # Thinking streaming — message_update with thinking_delta
  defp process_event(
         %{
           "type" => "message_update",
           "assistantMessageEvent" => %{"type" => "thinking_delta", "delta" => delta}
         },
         state
       ) do
    notification =
      build_session_update(state, %{
        "type" => "agent_message_chunk",
        "content" => %{"type" => "thinking", "text" => delta}
      })

    {:messages, [notification], state}
  end

  # Tool call start — message_update with toolcall_end (contains full tool call)
  defp process_event(
         %{
           "type" => "message_update",
           "assistantMessageEvent" => %{"type" => "toolcall_end"} = tool_event
         },
         state
       ) do
    tool_call = tool_event["toolCall"] || %{}

    notification =
      build_session_update(state, %{
        "type" => "agent_message_chunk",
        "content" => %{
          "type" => "tool_call",
          "name" => tool_call["name"] || tool_event["name"],
          "arguments" => tool_call["arguments"] || tool_call["args"] || %{},
          "id" => tool_call["id"] || "tc-#{state.msg_counter}"
        }
      })

    state = %{state | tool_calls: [tool_call | state.tool_calls]}
    {:messages, [notification], state}
  end

  # Tool call — message_update with tool_call type (legacy format)
  defp process_event(
         %{
           "type" => "message_update",
           "assistantMessageEvent" => %{"type" => "tool_call"} = tool_event
         },
         state
       ) do
    notification =
      build_session_update(state, %{
        "type" => "agent_message_chunk",
        "content" => %{
          "type" => "tool_call",
          "name" => tool_event["name"],
          "arguments" => tool_event["arguments"] || tool_event["args"],
          "id" => tool_event["id"] || "tc-#{state.msg_counter}"
        }
      })

    state = %{state | tool_calls: [tool_event | state.tool_calls]}
    {:messages, [notification], state}
  end

  # Tool execution start — emit tool execution notification
  defp process_event(
         %{
           "type" => "tool_execution_start",
           "toolCallId" => tool_call_id,
           "toolName" => tool_name
         } = event,
         state
       ) do
    state = %{
      state
      | active_tool_executions:
          Map.put(state.active_tool_executions, tool_call_id, %{
            name: tool_name,
            args: event["args"]
          })
    }

    notification =
      build_session_update(state, %{
        "type" => "tool_execution",
        "status" => "started",
        "toolCallId" => tool_call_id,
        "toolName" => tool_name,
        "arguments" => event["args"]
      })

    {:messages, [notification], state}
  end

  # Tool execution progress update
  defp process_event(
         %{
           "type" => "tool_execution_update",
           "toolCallId" => tool_call_id,
           "toolName" => tool_name
         } = event,
         state
       ) do
    partial = event["partialResult"]
    content = extract_tool_content(partial)

    notification =
      build_session_update(state, %{
        "type" => "tool_execution",
        "status" => "progress",
        "toolCallId" => tool_call_id,
        "toolName" => tool_name,
        "content" => content
      })

    {:messages, [notification], state}
  end

  # Tool execution end — emit tool result
  defp process_event(
         %{
           "type" => "tool_execution_end",
           "toolCallId" => tool_call_id,
           "toolName" => tool_name
         } = event,
         state
       ) do
    state = %{
      state
      | active_tool_executions: Map.delete(state.active_tool_executions, tool_call_id)
    }

    result = event["result"]
    content = extract_tool_content(result)

    notification =
      build_session_update(state, %{
        "type" => "tool_result",
        "toolCallId" => tool_call_id,
        "toolName" => tool_name,
        "content" => content,
        "isError" => event["isError"] || false
      })

    {:messages, [notification], state}
  end

  # Other message_update events (text_start, text_end, toolcall_start, toolcall_delta, etc.)
  defp process_event(%{"type" => "message_update"}, state) do
    {:skip, state}
  end

  # Agent end — conversation complete, send final response
  defp process_event(%{"type" => "agent_end"} = event, state) do
    text = state.text_acc |> Enum.reverse() |> Enum.join("")

    # Extract usage from the last assistant message
    messages = Map.get(event, "messages", [])

    usage =
      messages
      |> Enum.filter(fn m -> m["role"] == "assistant" end)
      |> List.last()
      |> case do
        %{"usage" => u} -> u
        _ -> %{}
      end

    response = %{
      "jsonrpc" => "2.0",
      "id" => state.pending_prompt_id,
      "result" => %{
        "result" => text,
        "sessionId" => state.session_id || "default",
        "usage" => %{
          "inputTokens" => usage["input"] || 0,
          "outputTokens" => usage["output"] || 0,
          "cacheReadTokens" => usage["cacheRead"] || 0,
          "cacheWriteTokens" => usage["cacheWrite"] || 0,
          "cost" => get_in(usage, ["cost", "total"])
        }
      }
    }

    state = %{
      state
      | text_acc: [],
        pending_prompt_id: nil,
        tool_calls: [],
        is_streaming: false
    }

    {:messages, [response], state}
  end

  # Auto-compaction events — notify via session/update
  defp process_event(%{"type" => "auto_compaction_start"} = event, state) do
    notification =
      build_session_update(state, %{
        "type" => "status",
        "status" => "compacting",
        "reason" => event["reason"]
      })

    {:messages, [notification], state}
  end

  defp process_event(%{"type" => "auto_compaction_end"} = event, state) do
    notification =
      build_session_update(state, %{
        "type" => "status",
        "status" => "compaction_complete",
        "result" => event["result"],
        "aborted" => event["aborted"]
      })

    {:messages, [notification], state}
  end

  # Auto-retry events
  defp process_event(%{"type" => "auto_retry_start"} = event, state) do
    notification =
      build_session_update(state, %{
        "type" => "status",
        "status" => "retrying",
        "attempt" => event["attempt"],
        "maxAttempts" => event["maxAttempts"],
        "errorMessage" => event["errorMessage"]
      })

    {:messages, [notification], state}
  end

  defp process_event(%{"type" => "auto_retry_end"} = event, state) do
    notification =
      build_session_update(state, %{
        "type" => "status",
        "status" => if(event["success"], do: "retry_succeeded", else: "retry_failed"),
        "attempt" => event["attempt"]
      })

    {:messages, [notification], state}
  end

  # RPC response — pass through as-is for pi/* method responses
  defp process_event(%{"type" => "response", "success" => true} = event, state) do
    # Extract session info from get_state responses
    state = maybe_update_session_info(event, state)

    # If there's data in the response, wrap it as a notification
    case event["data"] do
      nil ->
        {:skip, state}

      data ->
        notification =
          build_session_update(state, %{
            "type" => "rpc_response",
            "command" => event["command"],
            "data" => data
          })

        {:messages, [notification], state}
    end
  end

  defp process_event(%{"type" => "response", "success" => false} = event, state) do
    Logger.warning("[Pi Adapter] RPC command failed: #{inspect(event["error"])}")

    notification =
      build_session_update(state, %{
        "type" => "rpc_error",
        "command" => event["command"],
        "error" => event["error"]
      })

    {:messages, [notification], state}
  end

  # Extension UI requests — pass through for the host to handle
  defp process_event(%{"type" => "extension_ui_request"} = event, state) do
    notification =
      build_session_update(state, %{
        "type" => "extension_ui_request",
        "request" => event
      })

    {:messages, [notification], state}
  end

  # Extension errors
  defp process_event(%{"type" => "extension_error"} = event, state) do
    Logger.warning("[Pi Adapter] Extension error: #{event["extensionPath"]} — #{event["error"]}")

    {:skip, state}
  end

  # Lifecycle events — skip
  defp process_event(%{"type" => type}, state)
       when type in [
              "agent_start",
              "turn_start",
              "turn_end",
              "message_start",
              "message_end"
            ] do
    {:skip, state}
  end

  # Catch-all
  defp process_event(event, state) do
    Logger.debug("[Pi Adapter] Unhandled: #{inspect(Map.get(event, "type"))}")
    {:skip, state}
  end

  # ── Helpers ────────────────────────────────────────────────────

  defp encode_rpc(msg) do
    Jason.encode!(msg) <> "\n"
  end

  defp build_session_update(state, update) do
    %{
      "jsonrpc" => "2.0",
      "method" => "session/update",
      "params" => %{
        "sessionId" => state.session_id || "default",
        "update" => update
      }
    }
  end

  defp append_opt(args, opts, key, flag) do
    case Keyword.get(opts, key) do
      nil -> args
      value -> args ++ [flag, to_string(value)]
    end
  end

  defp extract_prompt_text(prompt) when is_binary(prompt), do: prompt

  defp extract_prompt_text(prompt) when is_list(prompt) do
    prompt
    |> Enum.filter(fn
      %{"type" => "text"} -> true
      _ -> false
    end)
    |> Enum.map_join("\n", fn %{"text" => text} -> text end)
  end

  defp extract_prompt_text(%{"content" => content}), do: extract_prompt_text(content)
  defp extract_prompt_text(_), do: ""

  defp extract_images(prompt) when is_list(prompt) do
    Enum.flat_map(prompt, fn
      %{"type" => "image"} = img ->
        [normalize_image(img)]

      _ ->
        []
    end)
  end

  defp extract_images(_), do: []

  defp normalize_images(images) when is_list(images), do: Enum.map(images, &normalize_image/1)
  defp normalize_images(_), do: []

  # Normalize image content: strip data-url prefix if present, ensure Pi format
  defp normalize_image(%{"data" => data, "mimeType" => mime_type}) do
    %{
      "type" => "image",
      "data" => strip_data_url(data),
      "mimeType" => mime_type
    }
  end

  defp normalize_image(%{"data" => data} = img) do
    %{
      "type" => "image",
      "data" => strip_data_url(data),
      "mimeType" => img["mimeType"] || detect_mime_type(data)
    }
  end

  defp normalize_image(img), do: img

  # Strip data:image/...;base64, prefix from base64 data
  defp strip_data_url(data) when is_binary(data) do
    case Regex.run(~r/^data:[^;]+;base64,(.+)$/s, data) do
      [_, base64] -> base64
      _ -> data
    end
  end

  defp strip_data_url(data), do: data

  defp detect_mime_type(data) when is_binary(data) do
    # Try to detect from data-url prefix
    case Regex.run(~r/^data:([^;]+);base64,/, data) do
      [_, mime] -> mime
      _ -> "image/png"
    end
  end

  defp detect_mime_type(_), do: "image/png"

  # Extract text content from Pi tool result format
  defp extract_tool_content(nil), do: ""

  defp extract_tool_content(%{"content" => content}) when is_list(content) do
    Enum.map_join(content, "\n", fn
      %{"type" => "text", "text" => text} -> text
      _ -> ""
    end)
  end

  defp extract_tool_content(%{"content" => content}) when is_binary(content), do: content
  defp extract_tool_content(_), do: ""

  # Update session info from get_state responses
  defp maybe_update_session_info(
         %{"command" => "get_state", "data" => data},
         state
       )
       when is_map(data) do
    state
    |> maybe_set(:session_file, data["sessionFile"])
    |> maybe_set(:session_id, data["sessionId"])
    |> maybe_set(:thinking_level, data["thinkingLevel"])
  end

  defp maybe_update_session_info(_, state), do: state

  defp maybe_set(state, _key, nil), do: state
  defp maybe_set(state, key, value), do: Map.put(state, key, value)
end
