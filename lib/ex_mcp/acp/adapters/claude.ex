defmodule ExMCP.ACP.Adapters.Claude do
  @moduledoc """
  Adapter for Claude Code CLI.

  Translates between ACP JSON-RPC and Claude's stream-json NDJSON protocol.
  Ported from Arbor's `CliTransport` + `StreamParser`.

  ## Claude CLI Protocol

  - **Input:** NDJSON on stdin with `{"type":"user","message":{...},"session_id":"..."}`
  - **Output:** NDJSON on stdout with event types: `stream_event`, `assistant`, `user`, `result`
  - **Args:** `--output-format stream-json --input-format stream-json --verbose`

  ## ACP Mapping

  | Claude Event | ACP Message |
  |---|---|
  | `stream_event` (text_delta) | `session/update` notification (text) |
  | `stream_event` (thinking_delta) | `session/update` notification (thinking) |
  | `assistant` | accumulate content blocks |
  | `assistant` (tool_use) | `session/update` notification (tool_call) |
  | `user` (tool_result) | `session/update` notification (tool_result) |
  | `result` | prompt response result |

  ## Features

  - Session resume via `--resume <session_id>` flag
  - Thinking block streaming with deduplication
  - Multi-turn tool use cycle tracking
  - Usage tracking with cache token support
  - Configurable thinking budget

  ## Limitations

  - No session persistence/listing (sessions managed by Claude CLI)
  - No mode switching (Claude CLI uses `--dangerously-skip-permissions`)
  - No session cancel (would need SIGINT to Port subprocess)
  - No runtime config changes (static at launch)
  """

  @behaviour ExMCP.ACP.Adapter

  require Logger

  # Stop reason classification matching Zed's error semantics
  @stop_reasons %{
    "end_turn" => "end_turn",
    "stop" => "end_turn",
    "max_tokens" => "max_tokens",
    "tool_use" => "tool_use",
    "error" => "error"
  }

  defstruct [
    :session_id,
    :model,
    text_acc: [],
    thinking_acc: [],
    thinking_blocks: [],
    current_block_type: nil,
    usage: nil,
    pending_prompt_id: nil,
    in_tool_use: false,
    opts: []
  ]

  # Adapter callbacks

  @impl true
  def init(opts) do
    {:ok, %__MODULE__{opts: opts}}
  end

  @impl true
  def command(opts) do
    thinking_budget = Keyword.get(opts, :max_thinking_tokens, 10_000)

    args = [
      "--output-format",
      "stream-json",
      "--input-format",
      "stream-json",
      "--verbose",
      "--max-thinking-tokens",
      to_string(thinking_budget),
      "--dangerously-skip-permissions"
    ]

    args = append_optional(args, opts, :model, "--model")
    args = append_optional(args, opts, :system_prompt, "--system-prompt")

    # Session resume
    args =
      case Keyword.get(opts, :session_id) do
        nil -> args
        id -> args ++ ["--resume", id]
      end

    cli_path = Keyword.get(opts, :cli_path, "claude")
    {cli_path, args}
  end

  @impl true
  def capabilities do
    %{
      "streaming" => true
      # Note: Claude CLI supports plan mode via --allowedTools but we don't
      # expose mode switching through the adapter. Session modes would require
      # the bridge to restart the subprocess with different flags.
    }
  end

  @impl true
  def config_options do
    [
      %{
        "id" => "model",
        "name" => "Model",
        "category" => "model",
        "description" => "Claude model to use",
        "type" => "string"
      },
      %{
        "id" => "thinking_budget",
        "name" => "Thinking Budget",
        "category" => "thought_level",
        "description" => "Max thinking tokens",
        "type" => "integer",
        "default" => 10_000
      }
    ]
  end

  # ── Outbound: ACP → Claude CLI ───────────────────────────────

  @impl true
  def translate_outbound(%{"method" => "initialize"}, state) do
    # Initialize is synthesized by the bridge
    {:ok, :skip, state}
  end

  def translate_outbound(%{"method" => "session/new"}, state) do
    # Claude doesn't have explicit session creation — session starts on first prompt
    {:ok, :skip, state}
  end

  def translate_outbound(%{"method" => "session/load"}, state) do
    # Session resume is handled via --resume flag at startup.
    # To resume a session, pass session_id in adapter_opts when creating the bridge.
    {:ok, :skip, state}
  end

  def translate_outbound(
        %{"method" => "session/prompt", "id" => id, "params" => params},
        state
      ) do
    content = extract_prompt_text(params["prompt"])
    session_id = params["sessionId"] || state.session_id || "default"

    stdin_msg = %{
      "type" => "user",
      "message" => %{"role" => "user", "content" => content},
      "session_id" => session_id
    }

    data = Jason.encode!(stdin_msg) <> "\n"

    state = reset_accumulators(%{state | pending_prompt_id: id, session_id: session_id})
    {:ok, data, state}
  end

  def translate_outbound(%{"method" => "session/cancel"}, state) do
    # Cancel would need SIGINT — not directly supported via Port.command.
    # The bridge would need to send OS signal to the Port subprocess.
    # For now, log and skip.
    Logger.debug("[Claude Adapter] session/cancel not supported (requires SIGINT)")
    {:ok, :skip, state}
  end

  # Explicit handlers for ACP methods we don't support —
  # better than silently dropping via catch-all
  def translate_outbound(%{"method" => "session/setMode"}, state) do
    Logger.debug("[Claude Adapter] session/setMode not supported (static permissions)")
    {:ok, :skip, state}
  end

  # ACP spec: session/set_config_option — store model for reference
  def translate_outbound(
        %{"method" => "session/set_config_option", "params" => %{"configId" => "model"} = params},
        state
      ) do
    state = %{state | model: params["value"]}

    Logger.debug(
      "[Claude Adapter] Model preference stored: #{params["value"]} (static at startup)"
    )

    {:ok, :skip, state}
  end

  def translate_outbound(%{"method" => "session/set_config_option"}, state) do
    {:ok, :skip, state}
  end

  def translate_outbound(_msg, state) do
    {:ok, :skip, state}
  end

  # ── Inbound: Claude CLI → ACP ─────────────────────────────────

  @impl true
  def translate_inbound(line, state) do
    trimmed = String.trim(line)

    case Jason.decode(trimmed) do
      {:ok, event} ->
        process_event(event, state)

      {:error, _} ->
        {:skip, state}
    end
  end

  # Event processing — ported from Arbor.AI.StreamParser

  defp process_event(%{"type" => "stream_event", "event" => event}, state) do
    process_stream_event(event, state)
  end

  defp process_event(%{"type" => "assistant", "message" => message}, state) do
    process_assistant_message(message, state)
  end

  defp process_event(%{"type" => "user", "message" => message}, state) do
    # Tool results from Claude CLI's internal tool use.
    # Emit a session update for observability, but don't affect text accumulation.
    notifications = extract_tool_results(message, state)

    case notifications do
      [] -> {:skip, state}
      notifs -> {:messages, notifs, state}
    end
  end

  defp process_event(%{"type" => "result"} = result, state) do
    process_result(result, state)
  end

  # System/status events from Claude CLI
  defp process_event(%{"type" => "system"} = event, state) do
    # Claude CLI sends system events for status updates (compaction, etc.)
    case event["message"] do
      nil ->
        {:skip, state}

      message ->
        notification =
          session_update(state.session_id, %{
            "sessionUpdate" => "status",
            "status" => "info",
            "message" => message
          })

        {:messages, [notification], state}
    end
  end

  # Rate limit events
  defp process_event(%{"type" => "rate_limit_event"} = event, state) do
    notification =
      session_update(state.session_id, %{
        "sessionUpdate" => "status",
        "status" => "rate_limited",
        "retryAfter" => event["retry_after"]
      })

    {:messages, [notification], state}
  end

  defp process_event(_event, state) do
    {:skip, state}
  end

  # Stream events produce ACP session/update notifications

  defp process_stream_event(
         %{"type" => "content_block_start", "content_block" => block},
         state
       ) do
    block_type = block_type_from(block)
    {:skip, %{state | current_block_type: block_type}}
  end

  defp process_stream_event(%{"type" => "content_block_delta", "delta" => delta}, state) do
    process_delta(delta, state)
  end

  defp process_stream_event(%{"type" => "content_block_stop"}, state) do
    state = finalize_current_block(state)
    {:skip, state}
  end

  defp process_stream_event(_event, state) do
    {:skip, state}
  end

  defp process_delta(%{"type" => "text_delta", "text" => text}, state) do
    state = %{state | text_acc: [text | state.text_acc]}

    notification =
      session_update(state.session_id, %{
        "sessionUpdate" => "agent_message_chunk",
        "content" => %{"type" => "text", "text" => text}
      })

    {:messages, [notification], state}
  end

  defp process_delta(%{"type" => "thinking_delta", "thinking" => thinking}, state) do
    state = %{
      state
      | thinking_acc: [thinking | state.thinking_acc],
        current_block_type: :thinking
    }

    notification =
      session_update(state.session_id, %{
        "sessionUpdate" => "thinking",
        "content" => thinking
      })

    {:messages, [notification], state}
  end

  defp process_delta(_delta, state) do
    {:skip, state}
  end

  # Assistant message — accumulate thinking/text blocks, emit tool_call notifications

  defp process_assistant_message(%{"content" => content} = message, state)
       when is_list(content) do
    session_id = message["id"]
    model = message["model"]

    has_tool_use = Enum.any?(content, &(&1["type"] == "tool_use"))
    has_text = Enum.any?(content, &(&1["type"] == "text"))

    # When a new assistant message arrives after tool use with text content,
    # clear the previous text accumulator so we capture the final answer
    state =
      if state.in_tool_use and has_text do
        %{state | text_acc: [], in_tool_use: false}
      else
        state
      end

    {state, notifications} = process_content_blocks(content, state)

    state =
      state
      |> maybe_set(:session_id, session_id)
      |> maybe_set(:model, model)

    # Mark that we're in a tool use cycle
    state = if has_tool_use, do: %{state | in_tool_use: true}, else: state

    case notifications do
      [] -> {:skip, state}
      notifs -> {:messages, notifs, state}
    end
  end

  defp process_assistant_message(_message, state), do: {:skip, state}

  defp process_content_blocks(content, state) when is_list(content) do
    Enum.reduce(content, {state, []}, fn block, {st, notifs} ->
      {new_st, new_notifs} = process_content_block(block, st)
      {new_st, notifs ++ new_notifs}
    end)
  end

  defp process_content_block(%{"type" => "thinking"} = block, state) do
    thinking_block = %{
      type: :thinking,
      text: block["thinking"] || "",
      signature: block["signature"]
    }

    # Dedup: only add if not already from streaming
    state =
      if Enum.any?(state.thinking_blocks, &(&1.text == thinking_block.text)) do
        state
      else
        %{state | thinking_blocks: [thinking_block | state.thinking_blocks]}
      end

    {state, []}
  end

  defp process_content_block(%{"type" => "text", "text" => text}, state)
       when is_binary(text) do
    # Accumulate text from assistant message when streaming deltas were absent
    state =
      if state.text_acc == [] do
        %{state | text_acc: [text]}
      else
        state
      end

    # Emit as agent_message_chunk for streaming visibility
    notification =
      session_update(state.session_id, %{
        "sessionUpdate" => "agent_message_chunk",
        "content" => %{"type" => "text", "text" => text}
      })

    {state, [notification]}
  end

  defp process_content_block(%{"type" => "tool_use"} = block, state) do
    # Claude CLI is calling one of its own tools (Grep, Read, Write, etc.)
    # Extract tool name for better display
    tool_name = block["name"] || "tool"

    notification =
      session_update(state.session_id, %{
        "sessionUpdate" => "tool_call",
        "title" => tool_display_name(tool_name),
        "toolCallId" => block["id"],
        "toolName" => tool_name,
        "input" => block["input"]
      })

    {state, [notification]}
  end

  defp process_content_block(_block, state), do: {state, []}

  # Result event — finalize and produce ACP prompt response

  defp process_result(result, state) do
    usage = extract_usage(result)
    session_id = result["session_id"] || state.session_id

    text =
      case state.text_acc do
        [] ->
          # No streaming deltas received — fall back to the result event's text field.
          # Claude CLI in stream-json stdin mode may skip content_block_delta events.
          result["result"] || ""

        acc ->
          IO.iodata_to_binary(Enum.reverse(acc))
      end

    state = finalize_thinking_block(state)

    thinking =
      case state.thinking_blocks do
        [] -> nil
        blocks -> Enum.reverse(blocks)
      end

    state = %{state | usage: usage, session_id: session_id}

    # Build ACP response messages
    messages = []

    # Usage update notification (emit before final result so clients can display it)
    messages = [
      session_update(session_id, %{
        "sessionUpdate" => "usage",
        "inputTokens" => usage.input_tokens,
        "outputTokens" => usage.output_tokens,
        "cacheReadTokens" => usage.cache_read_tokens,
        "cacheCreationTokens" => usage.cache_creation_tokens
      })
      | messages
    ]

    # Status update
    messages = [
      session_update(session_id, %{"sessionUpdate" => "status", "status" => "completed"})
      | messages
    ]

    # If we have a pending prompt ID, send the result
    messages =
      if state.pending_prompt_id do
        stop_reason = classify_stop_reason(result)

        response_result = %{
          "stopReason" => stop_reason,
          "text" => text,
          "usage" => format_usage(usage)
        }

        response_result =
          if thinking do
            thinking_data =
              Enum.map(thinking, fn block ->
                %{"text" => block.text, "signature" => block[:signature]}
              end)

            Map.put(response_result, "thinking", thinking_data)
          else
            response_result
          end

        response = %{
          "jsonrpc" => "2.0",
          "result" => response_result,
          "id" => state.pending_prompt_id
        }

        [response | messages]
      else
        messages
      end

    state = %{state | pending_prompt_id: nil}
    {:messages, Enum.reverse(messages), state}
  end

  # ── Helpers ────────────────────────────────────────────────────

  defp block_type_from(%{"type" => "thinking"}), do: :thinking
  defp block_type_from(%{"type" => "text"}), do: :text
  defp block_type_from(_), do: :text

  defp finalize_current_block(%{current_block_type: :thinking} = state) do
    finalize_thinking_block(state)
  end

  defp finalize_current_block(state) do
    %{state | current_block_type: nil}
  end

  defp finalize_thinking_block(%{thinking_acc: []} = state) do
    %{state | current_block_type: nil}
  end

  defp finalize_thinking_block(state) do
    text = IO.iodata_to_binary(Enum.reverse(state.thinking_acc))

    block = %{type: :thinking, text: text, signature: nil}

    %{
      state
      | thinking_blocks: [block | state.thinking_blocks],
        thinking_acc: [],
        current_block_type: nil
    }
  end

  defp extract_tool_results(%{"content" => content}, state) when is_list(content) do
    content
    |> Enum.filter(&(&1["type"] == "tool_result"))
    |> Enum.map(fn result ->
      session_update(state.session_id, %{
        "sessionUpdate" => "tool_result",
        "toolCallId" => result["tool_use_id"],
        "toolName" => result["tool_name"],
        "content" => result["content"],
        "isError" => result["is_error"] || false
      })
    end)
  end

  defp extract_tool_results(_, _state), do: []

  defp reset_accumulators(state) do
    %{
      state
      | text_acc: [],
        thinking_acc: [],
        thinking_blocks: [],
        current_block_type: nil,
        usage: nil,
        in_tool_use: false
    }
  end

  defp extract_usage(result) do
    raw = result["usage"] || %{}

    %{
      input_tokens: raw["input_tokens"] || 0,
      output_tokens: raw["output_tokens"] || 0,
      cache_read_tokens: raw["cache_read_input_tokens"] || 0,
      cache_creation_tokens: raw["cache_creation_input_tokens"] || 0
    }
  end

  defp format_usage(usage) do
    %{
      "inputTokens" => usage.input_tokens,
      "outputTokens" => usage.output_tokens,
      "cacheReadTokens" => usage.cache_read_tokens,
      "cacheCreationTokens" => usage.cache_creation_tokens
    }
  end

  # Classify stop reason with more granularity than binary error/success
  defp classify_stop_reason(result) do
    cond do
      result["is_error"] ->
        "error"

      result["stop_reason"] ->
        Map.get(@stop_reasons, result["stop_reason"], result["stop_reason"])

      # Check for max tokens by examining the result text
      result["usage"] && result["usage"]["output_tokens"] &&
          result["usage"]["output_tokens"] >= (result["usage"]["max_output_tokens"] || 999_999) ->
        "max_tokens"

      true ->
        "end_turn"
    end
  end

  # Human-readable tool display names for common Claude CLI tools
  defp tool_display_name("Read"), do: "Read File"
  defp tool_display_name("Write"), do: "Write File"
  defp tool_display_name("Edit"), do: "Edit File"
  defp tool_display_name("Bash"), do: "Run Command"
  defp tool_display_name("Grep"), do: "Search"
  defp tool_display_name("Glob"), do: "Find Files"
  defp tool_display_name("WebFetch"), do: "Fetch URL"
  defp tool_display_name("WebSearch"), do: "Web Search"
  defp tool_display_name(name), do: name

  defp session_update(session_id, update) do
    %{
      "jsonrpc" => "2.0",
      "method" => "session/update",
      "params" => %{
        "sessionId" => session_id || "default",
        "update" => update
      }
    }
  end

  defp extract_prompt_text(nil), do: ""

  defp extract_prompt_text(blocks) when is_list(blocks) do
    blocks
    |> Enum.filter(&(&1["type"] == "text"))
    |> Enum.map_join("\n", &(&1["text"] || ""))
  end

  defp extract_prompt_text(text) when is_binary(text), do: text

  defp maybe_set(state, _key, nil), do: state
  defp maybe_set(state, key, value), do: Map.put(state, key, value)

  defp append_optional(args, opts, key, flag) do
    case Keyword.get(opts, key) do
      nil -> args
      value -> args ++ [flag, to_string(value)]
    end
  end
end
