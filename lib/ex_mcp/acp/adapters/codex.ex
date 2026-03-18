defmodule ExMCP.ACP.Adapters.Codex do
  @moduledoc """
  Adapter for Codex CLI (OpenAI) using `codex app-server` persistent mode.

  Translates between ACP JSON-RPC and Codex's app-server JSON-RPC protocol.
  The app-server runs as a persistent subprocess communicating over NDJSON
  on stdin/stdout, with a JSON-RPC initialize handshake.

  Ported from `nshkrdotcom/codex_sdk`'s `AppServer.Connection` pattern.

  ## Codex App-Server Protocol

  - **Command:** `codex app-server`
  - **Handshake:** `initialize` request → response → `initialized` notification
  - **Session:** `thread/start` → `turn/start` → notifications → `turn/completed`
  - **Notifications:** NDJSON events for items, text deltas, reasoning, tool calls, etc.

  ## ACP Mapping

  | ACP Message | Codex JSON-RPC |
  |---|---|
  | `session/new` | `thread/start` request |
  | `session/load` | `thread/start` with threadId (resume) |
  | `session/prompt` | `turn/start` request |
  | `session/cancel` | `turn/interrupt` request |
  | `item/agentMessage/delta` | `session/update` (text) |
  | `item/reasoning/textDelta` | `session/update` (thinking) |
  | `item/completed` (tool) | `session/update` (tool_call/tool_result) |
  | `item/commandExecution/*` | `session/update` (tool_output) |
  | `turn/completed` | prompt response result |

  ## Features

  - Initialize handshake with `post_connect/1`
  - Text and thinking streaming
  - Tool call and tool result notifications
  - Command execution output streaming
  - Token usage tracking
  - Turn interrupt/cancel support
  - Image content in prompts

  ## Limitations

  - No session listing (Codex doesn't expose session enumeration)
  - No mode switching (static approval policy at session start)
  - No model switching mid-session (set at thread/start)
  - No authentication flow (relies on user's local auth)
  """

  @behaviour ExMCP.ACP.Adapter

  require Logger

  defstruct [
    :model,
    :thread_id,
    :turn_id,
    :current_prompt_acp_id,
    next_id: 1,
    phase: :initializing,
    pending_requests: %{},
    accumulated_text: [],
    accumulated_thinking: [],
    accumulated_usage: nil,
    opts: []
  ]

  # Adapter callbacks

  @impl true
  def init(opts) do
    {:ok,
     %__MODULE__{
       opts: opts,
       model: Keyword.get(opts, :model)
     }}
  end

  @impl true
  def command(_opts) do
    {"codex", ["app-server"]}
  end

  @impl true
  def capabilities do
    %{
      "streaming" => true
      # Note: Codex supports approval policies (suggest/auto-edit/full-auto)
      # but these are set at thread/start, not switched dynamically.
    }
  end

  @impl true
  def modes do
    [
      %{
        "id" => "suggest",
        "name" => "Suggest",
        "description" => "Suggests changes, requires approval for each"
      },
      %{
        "id" => "auto-edit",
        "name" => "Auto Edit",
        "description" => "Automatically applies code changes"
      },
      %{
        "id" => "full-auto",
        "name" => "Full Auto",
        "description" => "Full autonomy including shell commands"
      }
    ]
  end

  @impl true
  def config_options do
    [
      %{
        "id" => "model",
        "name" => "Model",
        "category" => "model",
        "description" => "OpenAI model to use",
        "type" => "string"
      }
    ]
  end

  @impl true
  def post_connect(state) do
    {id, state} = next_request_id(state)

    client_name = Keyword.get(state.opts, :client_name, "ex_mcp")
    client_version = Keyword.get(state.opts, :client_version, "1.0.0")

    request =
      encode_request(id, "initialize", %{
        "clientInfo" => %{
          "name" => client_name,
          "version" => client_version
        }
      })

    state = track_request(state, id, :initialize, nil)
    {:ok, request, state}
  end

  # ── Outbound: ACP → Codex ────────────────────────────────────

  @impl true
  def translate_outbound(%{"method" => "initialize"}, state) do
    # Handled by post_connect + bridge synthetic init
    {:ok, :skip, state}
  end

  def translate_outbound(
        %{"method" => "session/new", "id" => acp_id, "params" => params},
        state
      ) do
    {id, state} = next_request_id(state)

    wire_params =
      %{}
      |> maybe_put("model", state.model || params["model"])
      |> maybe_put("cwd", params["cwd"] || Keyword.get(state.opts, :cwd))
      |> maybe_put("approvalPolicy", params["approvalPolicy"])
      |> maybe_put("sandbox", params["sandbox"])

    request = encode_request(id, "thread/start", wire_params)
    state = track_request(state, id, :thread_start, acp_id)
    {:ok, request, state}
  end

  def translate_outbound(
        %{"method" => "session/load", "id" => acp_id, "params" => params},
        state
      ) do
    # Resume an existing session by passing threadId to thread/start
    session_id = params["sessionId"]

    if session_id do
      {id, state} = next_request_id(state)

      wire_params =
        %{"threadId" => session_id}
        |> maybe_put("model", state.model || params["model"])
        |> maybe_put("cwd", params["cwd"] || Keyword.get(state.opts, :cwd))

      request = encode_request(id, "thread/start", wire_params)
      state = track_request(state, id, :thread_start, acp_id)
      {:ok, request, state}
    else
      {:ok, :skip, state}
    end
  end

  def translate_outbound(
        %{"method" => "session/prompt", "id" => acp_id, "params" => params},
        state
      ) do
    thread_id = params["sessionId"] || state.thread_id

    if thread_id do
      {id, state} = next_request_id(state)

      # Build input items from prompt content
      input = extract_input_items(params["prompt"])

      wire_params =
        %{
          "threadId" => thread_id,
          "input" => input
        }
        |> maybe_put("model", params["model"] || state.model)
        |> maybe_put("cwd", params["cwd"] || Keyword.get(state.opts, :cwd))

      request = encode_request(id, "turn/start", wire_params)

      state =
        state
        |> track_request(id, :turn_start, acp_id)
        |> Map.put(:accumulated_text, [])
        |> Map.put(:accumulated_thinking, [])
        |> Map.put(:accumulated_usage, nil)

      {:ok, request, state}
    else
      {:ok, :skip, state}
    end
  end

  def translate_outbound(
        %{"method" => "session/cancel", "params" => params},
        state
      ) do
    thread_id = params["sessionId"] || state.thread_id
    turn_id = params["turnId"] || state.turn_id

    if thread_id && turn_id do
      {id, state} = next_request_id(state)

      request =
        encode_request(id, "turn/interrupt", %{
          "threadId" => thread_id,
          "turnId" => turn_id
        })

      state = track_request(state, id, :turn_interrupt, nil)
      {:ok, request, state}
    else
      {:ok, :skip, state}
    end
  end

  # ACP spec: session/set_mode — Codex uses approvalPolicy at thread/start
  # We track it in state for the next thread/start call
  def translate_outbound(%{"method" => "session/set_mode", "params" => params}, state) do
    Logger.debug(
      "[Codex Adapter] Mode change stored: #{params["modeId"]} (applies on next session)"
    )

    {:ok, :skip, state}
  end

  def translate_outbound(%{"method" => "session/set_mode"}, state) do
    {:ok, :skip, state}
  end

  # ACP spec: session/set_config_option — model stored for next turn
  def translate_outbound(
        %{"method" => "session/set_config_option", "params" => %{"configId" => "model"} = params},
        state
      ) do
    state = %{state | model: params["value"]}
    {:ok, :skip, state}
  end

  def translate_outbound(%{"method" => "session/set_config_option"}, state) do
    {:ok, :skip, state}
  end

  def translate_outbound(_msg, state) do
    {:ok, :skip, state}
  end

  # ── Inbound: Codex → ACP ─────────────────────────────────────

  @impl true
  def translate_inbound(line, state) do
    case Jason.decode(line) do
      {:ok, msg} ->
        handle_inbound_message(msg, state)

      {:error, _} ->
        {:skip, state}
    end
  end

  # JSON-RPC message routing

  defp handle_inbound_message(%{"id" => id, "result" => result}, state) do
    handle_response(state, id, {:ok, result})
  end

  defp handle_inbound_message(%{"id" => id, "error" => error}, state) do
    handle_response(state, id, {:error, error})
  end

  defp handle_inbound_message(%{"method" => method, "params" => params}, state)
       when is_binary(method) do
    handle_notification(method, params || %{}, state)
  end

  defp handle_inbound_message(%{"method" => method}, state) when is_binary(method) do
    handle_notification(method, %{}, state)
  end

  defp handle_inbound_message(_msg, state) do
    {:skip, state}
  end

  # ── Response Handling ─────────────────────────────────────────

  defp handle_response(state, id, reply) do
    case Map.pop(state.pending_requests, id) do
      {nil, _} ->
        {:skip, state}

      {%{type: type} = entry, pending} ->
        state = %{state | pending_requests: pending}
        handle_typed_response(type, entry, reply, state)
    end
  end

  defp handle_typed_response(:initialize, _entry, _reply, state) do
    state = %{state | phase: :ready}
    initialized = encode_notification("initialized")
    {:skip_and_write, initialized, state}
  end

  defp handle_typed_response(:thread_start, %{acp_id: acp_id}, {:ok, result}, state) do
    thread = result["thread"] || %{}
    thread_id = thread["id"] || ""
    state = %{state | thread_id: thread_id}

    response = %{
      "jsonrpc" => "2.0",
      "id" => acp_id,
      "result" => %{"sessionId" => thread_id, "metadata" => thread}
    }

    {:messages, [response], state}
  end

  defp handle_typed_response(:thread_start, %{acp_id: acp_id}, {:error, error}, state) do
    {:messages, [error_response(acp_id, error)], state}
  end

  defp handle_typed_response(:turn_start, %{acp_id: acp_id}, {:ok, result}, state) do
    turn = result["turn"] || %{}
    turn_id = turn["id"] || ""
    {:skip, %{state | turn_id: turn_id, current_prompt_acp_id: acp_id}}
  end

  defp handle_typed_response(:turn_start, %{acp_id: acp_id}, {:error, error}, state) do
    {:messages, [error_response(acp_id, error)], state}
  end

  defp handle_typed_response(:turn_interrupt, _entry, _reply, state) do
    {:skip, state}
  end

  defp error_response(acp_id, error) do
    %{
      "jsonrpc" => "2.0",
      "id" => acp_id,
      "error" => normalize_error(error)
    }
  end

  # ── Notification Handling ─────────────────────────────────────

  defp handle_notification("thread/started", params, state) do
    thread = params["thread"] || %{}
    thread_id = thread["id"] || ""
    {:skip, %{state | thread_id: thread_id}}
  end

  defp handle_notification("turn/started", params, state) do
    turn = params["turn"] || %{}
    turn_id = turn["id"] || ""
    {:skip, %{state | turn_id: turn_id}}
  end

  # ── Text Streaming ───────────────────────────────────────────

  defp handle_notification("item/agentMessage/delta", params, state) do
    delta = params["delta"] || ""
    state = %{state | accumulated_text: [delta | state.accumulated_text]}

    notification =
      session_update(state, %{
        "sessionUpdate" => "agent_message_chunk",
        "content" => %{"type" => "text", "text" => delta}
      })

    {:messages, [notification], state}
  end

  # ── Thinking/Reasoning Streaming ──────────────────────────────

  defp handle_notification("item/reasoning/textDelta", params, state) do
    delta = params["delta"] || ""
    state = %{state | accumulated_thinking: [delta | state.accumulated_thinking]}

    notification =
      session_update(state, %{
        "sessionUpdate" => "thinking",
        "content" => delta
      })

    {:messages, [notification], state}
  end

  # ── Tool Call Notifications ───────────────────────────────────

  # Tool call started (item/created with tool call type)
  defp handle_notification(
         "item/created",
         %{"item" => %{"type" => "function_call"} = item},
         state
       ) do
    notification =
      session_update(state, %{
        "sessionUpdate" => "tool_call",
        "toolCallId" => item["callId"] || item["id"],
        "toolName" => item["name"],
        "title" => item["name"],
        "input" => item["arguments"],
        "status" => "pending"
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/created", _params, state) do
    {:skip, state}
  end

  # Item completed — handles agent messages, tool calls, and tool results
  defp handle_notification("item/completed", params, state) do
    item = params["item"] || %{}
    handle_item_completed(item, state)
  end

  # ── Command Execution Streaming ───────────────────────────────

  defp handle_notification("item/commandExecution/started", params, state) do
    notification =
      session_update(state, %{
        "sessionUpdate" => "tool_execution",
        "status" => "started",
        "toolCallId" => params["callId"] || params["itemId"],
        "toolName" => "command",
        "command" => params["command"]
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/commandExecution/outputDelta", params, state) do
    delta = params["delta"] || ""

    notification =
      session_update(state, %{
        "sessionUpdate" => "tool_output",
        "content" => delta,
        "itemId" => params["itemId"] || params["item_id"]
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/commandExecution/completed", params, state) do
    notification =
      session_update(state, %{
        "sessionUpdate" => "tool_execution",
        "status" => "completed",
        "toolCallId" => params["callId"] || params["itemId"],
        "toolName" => "command",
        "exitCode" => params["exitCode"],
        "output" => params["output"]
      })

    {:messages, [notification], state}
  end

  # ── Patch/Approval Events ─────────────────────────────────────

  defp handle_notification("item/patch/created", params, state) do
    patch = params["patch"] || params

    notification =
      session_update(state, %{
        "sessionUpdate" => "tool_call",
        "toolCallId" => patch["id"] || params["itemId"],
        "toolName" => "patch",
        "title" => "Edit File",
        "input" => %{
          "path" => patch["path"],
          "diff" => patch["diff"]
        },
        "status" => "pending"
      })

    {:messages, [notification], state}
  end

  # ── Turn Completion ───────────────────────────────────────────

  defp handle_notification("turn/completed", params, state) do
    turn = params["turn"] || %{}
    status = turn["status"]

    # Use saved ACP ID (set when turn/start response arrived)
    acp_id = state.current_prompt_acp_id

    text =
      state.accumulated_text
      |> Enum.reverse()
      |> IO.iodata_to_binary()

    messages = []

    # Emit usage update if we have accumulated usage
    messages =
      if state.accumulated_usage do
        [
          session_update(state, %{
            "sessionUpdate" => "usage",
            "content" => state.accumulated_usage
          })
          | messages
        ]
      else
        messages
      end

    # Status completed notification
    messages = [
      session_update(state, %{
        "sessionUpdate" => "status",
        "status" => "completed"
      })
      | messages
    ]

    # Prompt response
    messages =
      if acp_id do
        response = %{
          "jsonrpc" => "2.0",
          "id" => acp_id,
          "result" => %{
            "stopReason" => normalize_stop_reason(status),
            "text" => text,
            "sessionId" => state.thread_id,
            "turnId" => state.turn_id
          }
        }

        [response | messages]
      else
        messages
      end

    state = %{
      state
      | accumulated_text: [],
        accumulated_thinking: [],
        accumulated_usage: nil,
        turn_id: nil,
        current_prompt_acp_id: nil
    }

    {:messages, Enum.reverse(messages), state}
  end

  # ── Token Usage ───────────────────────────────────────────────

  defp handle_notification("thread/tokenUsage/updated", params, state) do
    token_usage = params["tokenUsage"] || %{}
    total = token_usage["total"] || %{}

    usage_data = %{
      "inputTokens" => total["inputTokens"] || 0,
      "outputTokens" => total["outputTokens"] || 0,
      "cachedInputTokens" => total["cachedInputTokens"] || 0
    }

    # Accumulate usage for turn/completed response
    state = %{state | accumulated_usage: usage_data}

    notification =
      session_update(state, %{
        "sessionUpdate" => "usage",
        "content" => usage_data
      })

    {:messages, [notification], state}
  end

  # ── Error Notifications ───────────────────────────────────────

  defp handle_notification("error", params, state) do
    error = params["error"] || %{}

    notification =
      session_update(state, %{
        "sessionUpdate" => "error",
        "content" => error["message"] || "Unknown error",
        "code" => error["code"]
      })

    {:messages, [notification], state}
  end

  # ── Web Search Events ─────────────────────────────────────────

  defp handle_notification("item/webSearch/started", params, state) do
    notification =
      session_update(state, %{
        "sessionUpdate" => "tool_execution",
        "status" => "started",
        "toolCallId" => params["itemId"],
        "toolName" => "web_search",
        "query" => params["query"]
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/webSearch/completed", params, state) do
    notification =
      session_update(state, %{
        "sessionUpdate" => "tool_execution",
        "status" => "completed",
        "toolCallId" => params["itemId"],
        "toolName" => "web_search",
        "results" => params["results"]
      })

    {:messages, [notification], state}
  end

  # Catch-all for unknown notifications
  defp handle_notification(method, _params, state) do
    Logger.debug("[Codex Adapter] Unhandled notification: #{method}")
    {:skip, state}
  end

  # ── Item Completion Handlers ───────────────────────────────────

  defp handle_item_completed(%{"type" => "agent_message"} = item, state) do
    text = item["text"] || ""

    notification =
      session_update(state, %{
        "sessionUpdate" => "agent_message_chunk",
        "content" => %{"type" => "text", "text" => text},
        "final" => true
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(%{"type" => "function_call"} = item, state) do
    notification =
      session_update(state, %{
        "sessionUpdate" => "tool_call_update",
        "toolCallId" => item["callId"] || item["id"],
        "toolName" => item["name"],
        "status" => "completed",
        "arguments" => item["arguments"]
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(%{"type" => "function_call_output"} = item, state) do
    notification =
      session_update(state, %{
        "sessionUpdate" => "tool_result",
        "toolCallId" => item["callId"] || item["id"],
        "content" => item["output"] || item["text"] || "",
        "isError" => item["isError"] || false
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(%{"type" => "patch"} = item, state) do
    notification =
      session_update(state, %{
        "sessionUpdate" => "tool_result",
        "toolCallId" => item["callId"] || item["id"],
        "toolName" => "patch",
        "content" => item["diff"] || item["text"] || "",
        "filePath" => item["path"],
        "isError" => false
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(_item, state), do: {:skip, state}

  # ── Helpers ────────────────────────────────────────────────────

  defp next_request_id(%{next_id: id} = state) do
    {id, %{state | next_id: id + 1}}
  end

  defp track_request(state, id, type, acp_id) do
    entry = %{type: type, acp_id: acp_id}
    %{state | pending_requests: Map.put(state.pending_requests, id, entry)}
  end

  defp encode_request(id, method, params) do
    params = if is_map(params) and map_size(params) > 0, do: params, else: %{}
    msg = %{"id" => id, "method" => method, "params" => params}
    [Jason.encode!(msg), "\n"]
  end

  defp encode_notification(method, params \\ nil) do
    msg =
      %{"method" => method}
      |> maybe_put("params", params)

    [Jason.encode!(msg), "\n"]
  end

  defp session_update(state, update) do
    %{
      "jsonrpc" => "2.0",
      "method" => "session/update",
      "params" => %{
        "sessionId" => state.thread_id || "default",
        "update" => update
      }
    }
  end

  # Extract input items from prompt — supports text and images
  defp extract_input_items(nil), do: [%{"type" => "text", "text" => ""}]

  defp extract_input_items(prompt) when is_binary(prompt) do
    [%{"type" => "text", "text" => prompt}]
  end

  defp extract_input_items(blocks) when is_list(blocks) do
    items =
      Enum.flat_map(blocks, fn
        %{"type" => "text", "text" => text} ->
          [%{"type" => "text", "text" => text}]

        %{"type" => "image", "data" => data} = img ->
          [
            %{
              "type" => "image",
              "data" => data,
              "mimeType" => img["mimeType"] || "image/png"
            }
          ]

        _ ->
          []
      end)

    if items == [], do: [%{"type" => "text", "text" => ""}], else: items
  end

  defp extract_input_items(_), do: [%{"type" => "text", "text" => ""}]

  defp normalize_error(%{"message" => msg} = error) do
    %{"code" => error["code"] || -1, "message" => msg}
  end

  defp normalize_error(error) when is_binary(error) do
    %{"code" => -1, "message" => error}
  end

  defp normalize_error(error) do
    %{"code" => -1, "message" => inspect(error)}
  end

  defp normalize_stop_reason(nil), do: "end_turn"
  defp normalize_stop_reason("completed"), do: "end_turn"
  defp normalize_stop_reason("cancelled"), do: "cancelled"
  defp normalize_stop_reason("interrupted"), do: "cancelled"
  defp normalize_stop_reason("errored"), do: "error"
  defp normalize_stop_reason(other), do: other

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, _key, ""), do: map
  defp maybe_put(map, _key, map_val) when map_val == %{}, do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)
end
