defmodule ExMCP.ACP.Adapters.Pi do
  @moduledoc """
  ACP adapter for the Pi coding agent (badlogic/pi-mono).

  Translates between ACP JSON-RPC and Pi's RPC NDJSON protocol.
  Pi runs as a subprocess in `--mode rpc` and communicates via
  JSONL on stdin/stdout.

  ## Pi RPC Protocol

  - **Input:** JSONL on stdin: `{"type":"prompt","id":"msg-1","content":"..."}`
  - **Output:** JSONL on stdout with event types: text_delta, tool_call,
    tool_result, done, agent_start, turn_start, message_update

  ## ACP Mapping

  | Pi Event | ACP Message |
  |---|---|
  | `text_delta` | `session/update` notification (kind: text) |
  | `tool_call` | `session/update` notification (tool_call) |
  | `tool_result` | `session/update` notification (tool_result) |
  | `done` | prompt response result |
  | `agent_start` | (internal) |
  | `turn_start` | (internal) |

  ## Configuration

      config :arbor_ai, :acp_providers, %{
        pi: %{
          transport_mod: ExMCP.ACP.AdapterTransport,
          adapter: Arbor.AI.AcpAdapters.Pi,
          adapter_opts: [
            cli_path: "pi",
            model: "claude-sonnet"
          ]
        }
      }
  """

  @behaviour ExMCP.ACP.Adapter

  require Logger

  defstruct [
    :session_id,
    :model,
    text_acc: [],
    pending_prompt_id: nil,
    tool_calls: [],
    msg_counter: 0,
    opts: []
  ]

  # ── Adapter Callbacks ──────────────────────────────────────────

  @impl true
  def init(opts) do
    {:ok,
     %__MODULE__{
       opts: opts,
       model: Keyword.get(opts, :model)
     }}
  end

  @impl true
  def command(opts) do
    cli_path = Keyword.get(opts, :cli_path, "pi")

    args = ["--mode", "rpc"]

    # Add model if specified
    args =
      case Keyword.get(opts, :model) do
        nil -> args
        model -> args ++ ["--model", model]
      end

    # Add working directory
    args =
      case Keyword.get(opts, :cwd) do
        nil -> args
        cwd -> args ++ ["--cwd", cwd]
      end

    # Add system prompt
    args =
      case Keyword.get(opts, :system_prompt) do
        nil -> args
        prompt -> args ++ ["--system-prompt", prompt]
      end

    # Add API key if provided
    args =
      case Keyword.get(opts, :api_key) do
        nil -> args
        key -> args ++ ["--api-key", key]
      end

    {cli_path, args}
  end

  @impl true
  def capabilities do
    %{
      "streaming" => true,
      "supportedModes" => [
        %{"id" => "code", "label" => "Code Mode"}
      ]
    }
  end

  # ── Outbound: ACP → Pi RPC ────────────────────────────────────

  @impl true
  def translate_outbound(%{"method" => "initialize"}, state) do
    {:ok, :skip, state}
  end

  def translate_outbound(%{"method" => "session/new"}, state) do
    # Pi starts a session implicitly on first prompt
    {:ok, :skip, state}
  end

  def translate_outbound(%{"method" => "session/load"}, state) do
    {:ok, :skip, state}
  end

  def translate_outbound(%{"method" => "session/prompt", "id" => id, "params" => params}, state) do
    content = extract_prompt_text(params["prompt"])
    msg_id = "msg-#{state.msg_counter + 1}"

    rpc_msg = %{
      "type" => "prompt",
      "id" => msg_id,
      "content" => content
    }

    # Add images if present
    rpc_msg =
      case extract_images(params["prompt"]) do
        [] -> rpc_msg
        images -> Map.put(rpc_msg, "images", images)
      end

    data = Jason.encode!(rpc_msg) <> "\n"

    state = %{
      state
      | pending_prompt_id: id,
        msg_counter: state.msg_counter + 1,
        text_acc: [],
        tool_calls: []
    }

    {:ok, data, state}
  end

  def translate_outbound(%{"method" => "session/cancel"}, state) do
    # Pi supports steering — send interrupt
    rpc_msg = %{"type" => "interrupt"}
    data = Jason.encode!(rpc_msg) <> "\n"
    {:ok, data, state}
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

  # Text streaming
  defp process_event(%{"type" => "event", "name" => "text_delta", "delta" => delta}, state) do
    state = %{state | text_acc: [delta | state.text_acc]}

    notification =
      build_session_update(state, %{
        "type" => "agent_message_chunk",
        "content" => %{"type" => "text", "text" => delta}
      })

    {:messages, [notification], state}
  end

  # Thinking/reasoning
  defp process_event(%{"type" => "event", "name" => "thinking_delta", "delta" => delta}, state) do
    notification =
      build_session_update(state, %{
        "type" => "agent_message_chunk",
        "content" => %{"type" => "thinking", "text" => delta}
      })

    {:messages, [notification], state}
  end

  # Tool call
  defp process_event(%{"type" => "event", "name" => "tool_call"} = event, state) do
    tool_data = Map.get(event, "data", %{})

    notification =
      build_session_update(state, %{
        "type" => "agent_message_chunk",
        "content" => %{
          "type" => "tool_call",
          "name" => tool_data["name"],
          "arguments" => tool_data["arguments"] || tool_data["args"],
          "id" => tool_data["id"] || "tc-#{state.msg_counter}"
        }
      })

    state = %{state | tool_calls: [tool_data | state.tool_calls]}
    {:messages, [notification], state}
  end

  # Tool result
  defp process_event(%{"type" => "event", "name" => "tool_result"} = event, state) do
    result_data = Map.get(event, "data", %{})

    notification =
      build_session_update(state, %{
        "type" => "agent_message_chunk",
        "content" => %{
          "type" => "tool_result",
          "name" => result_data["name"],
          "result" => result_data["result"] || result_data["content"],
          "id" => result_data["id"]
        }
      })

    {:messages, [notification], state}
  end

  # Turn/agent lifecycle events (internal, no ACP mapping needed)
  defp process_event(%{"type" => "event", "name" => name}, state)
       when name in ["agent_start", "turn_start", "message_update"] do
    {:skip, state}
  end

  # Done — send final response
  defp process_event(%{"type" => "event", "name" => "done"} = event, state) do
    text = state.text_acc |> Enum.reverse() |> Enum.join("")

    usage = Map.get(event, "usage", %{})

    response = %{
      "jsonrpc" => "2.0",
      "id" => state.pending_prompt_id,
      "result" => %{
        "result" => text,
        "sessionId" => state.session_id || "default",
        "usage" => %{
          "inputTokens" => usage["input_tokens"] || 0,
          "outputTokens" => usage["output_tokens"] || 0
        }
      }
    }

    state = %{state | text_acc: [], pending_prompt_id: nil, tool_calls: []}
    {:messages, [response], state}
  end

  # RPC response (success/error for commands)
  defp process_event(%{"type" => "response", "name" => _name, "success" => true}, state) do
    {:skip, state}
  end

  defp process_event(%{"type" => "response", "name" => name, "success" => false} = event, state) do
    Logger.warning("[Pi Adapter] RPC command failed: #{name} — #{inspect(event["error"])}")
    {:skip, state}
  end

  # Catch-all
  defp process_event(event, state) do
    Logger.debug(
      "[Pi Adapter] Unhandled event: #{inspect(Map.get(event, "type"))}:#{inspect(Map.get(event, "name"))}"
    )

    {:skip, state}
  end

  # ── Helpers ────────────────────────────────────────────────────

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
      %{"type" => "image", "data" => data} -> [data]
      _ -> []
    end)
  end

  defp extract_images(_), do: []
end
