defmodule ExMCP.ACP.Adapters.Claude do
  @moduledoc """
  Adapter for Claude Code CLI.

  Translates between ACP JSON-RPC and Claude's stream-json NDJSON protocol.
  Ported from Arbor's `CliTransport` + `StreamParser`.

  ## Claude CLI Protocol

  - **Input:** NDJSON on stdin with `{"type":"user","message":{...},"session_id":"..."}`
  - **Output:** NDJSON on stdout with event types: `stream_event`, `assistant`, `result`
  - **Args:** `--output-format stream-json --input-format stream-json --verbose`

  ## ACP Mapping

  | Claude Event | ACP Message |
  |---|---|
  | `stream_event` (text_delta) | `session/update` notification (kind: text) |
  | `stream_event` (thinking_delta) | `session/update` notification (kind: thinking) |
  | `assistant` | accumulate content blocks |
  | `result` | prompt response result |
  """

  @behaviour ExMCP.ACP.Adapter

  require Logger

  defstruct [
    :session_id,
    :model,
    text_acc: [],
    thinking_acc: [],
    thinking_blocks: [],
    current_block_type: nil,
    usage: nil,
    pending_prompt_id: nil,
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
      "streaming" => true,
      "supportedModes" => [
        %{"id" => "plan", "label" => "Plan Mode"},
        %{"id" => "code", "label" => "Code Mode"}
      ]
    }
  end

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
    # Session resume is handled via --resume flag at startup
    {:ok, :skip, state}
  end

  def translate_outbound(%{"method" => "session/prompt", "id" => id, "params" => params}, state) do
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
    # Cancel would need SIGINT — not directly supported via Port.command
    # The bridge would need to send OS signal to the Port subprocess
    {:ok, :skip, state}
  end

  def translate_outbound(_msg, state) do
    {:ok, :skip, state}
  end

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
    state = process_assistant_message(message, state)
    {:skip, state}
  end

  defp process_event(%{"type" => "result"} = result, state) do
    process_result(result, state)
  end

  defp process_event(_event, state) do
    {:skip, state}
  end

  # Stream events produce ACP session/update notifications

  defp process_stream_event(%{"type" => "content_block_start", "content_block" => block}, state) do
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

  # Assistant message — accumulate thinking blocks for dedup

  defp process_assistant_message(%{"content" => content} = message, state)
       when is_list(content) do
    session_id = message["id"]
    model = message["model"]

    state = process_content_blocks(content, state)

    state =
      state
      |> maybe_set(:session_id, session_id)
      |> maybe_set(:model, model)

    state
  end

  defp process_assistant_message(_message, state), do: state

  defp process_content_blocks(content, state) when is_list(content) do
    Enum.reduce(content, state, &process_content_block/2)
  end

  defp process_content_block(%{"type" => "thinking"} = block, state) do
    thinking_block = %{
      type: :thinking,
      text: block["thinking"] || "",
      signature: block["signature"]
    }

    # Dedup: only add if not already from streaming
    if Enum.any?(state.thinking_blocks, &(&1.text == thinking_block.text)) do
      state
    else
      %{state | thinking_blocks: [thinking_block | state.thinking_blocks]}
    end
  end

  defp process_content_block(%{"type" => "text", "text" => text}, state)
       when is_binary(text) do
    # Accumulate text from assistant message when streaming deltas were absent
    if state.text_acc == [] do
      %{state | text_acc: [text]}
    else
      state
    end
  end

  defp process_content_block(_block, state), do: state

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

    # Build ACP response
    messages = []

    # Status update
    messages = [
      session_update(session_id, %{"sessionUpdate" => "status", "status" => "completed"})
      | messages
    ]

    # If we have a pending prompt ID, send the result
    messages =
      if state.pending_prompt_id do
        response_result = %{
          "stopReason" => if(result["is_error"], do: "error", else: "end_turn"),
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

  # Helpers

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

  defp reset_accumulators(state) do
    %{
      state
      | text_acc: [],
        thinking_acc: [],
        thinking_blocks: [],
        current_block_type: nil,
        usage: nil
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
