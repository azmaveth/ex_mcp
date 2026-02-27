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
  | `session/prompt` | `turn/start` request |
  | `session/cancel` | `turn/interrupt` request |
  | `item/agentMessage/delta` | `session/update` (kind: text) |
  | `item/reasoning/textDelta` | `session/update` (kind: thinking) |
  | `turn/completed` | prompt response result |
  """

  @behaviour ExMCP.ACP.Adapter

  require Logger

  defstruct [
    :model,
    :thread_id,
    :turn_id,
    next_id: 1,
    phase: :initializing,
    pending_requests: %{},
    accumulated_text: [],
    accumulated_thinking: [],
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
      "streaming" => true,
      "supportedModes" => ["thread"]
    }
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

  # Outbound: ACP → Codex

  @impl true
  def translate_outbound(%{"method" => "initialize"}, state) do
    # Handled by post_connect + bridge synthetic init
    {:ok, :skip, state}
  end

  def translate_outbound(%{"method" => "session/new", "id" => acp_id, "params" => params}, state) do
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
        %{"method" => "session/prompt", "id" => acp_id, "params" => params},
        state
      ) do
    thread_id = params["sessionId"] || state.thread_id

    if thread_id do
      {id, state} = next_request_id(state)
      content = extract_prompt_text(params["content"])

      wire_params =
        %{
          "threadId" => thread_id,
          "input" => content
        }
        |> maybe_put("model", params["model"] || state.model)
        |> maybe_put("cwd", params["cwd"] || Keyword.get(state.opts, :cwd))

      request = encode_request(id, "turn/start", wire_params)

      state =
        state
        |> track_request(id, :turn_start, acp_id)
        |> Map.put(:accumulated_text, [])
        |> Map.put(:accumulated_thinking, [])

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

  def translate_outbound(_msg, state) do
    {:ok, :skip, state}
  end

  # Inbound: Codex → ACP

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

  # Response handling

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

  defp handle_typed_response(:turn_start, _entry, {:ok, result}, state) do
    turn = result["turn"] || %{}
    turn_id = turn["id"] || ""
    {:skip, %{state | turn_id: turn_id}}
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

  # Notification handling

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

  defp handle_notification("item/agentMessage/delta", params, state) do
    delta = params["delta"] || ""
    state = %{state | accumulated_text: [delta | state.accumulated_text]}
    session_id = state.thread_id || "default"

    notification = %{
      "jsonrpc" => "2.0",
      "method" => "session/update",
      "params" => %{
        "sessionId" => session_id,
        "kind" => "text",
        "content" => delta
      }
    }

    {:messages, [notification], state}
  end

  defp handle_notification("item/reasoning/textDelta", params, state) do
    delta = params["delta"] || ""
    state = %{state | accumulated_thinking: [delta | state.accumulated_thinking]}
    session_id = state.thread_id || "default"

    notification = %{
      "jsonrpc" => "2.0",
      "method" => "session/update",
      "params" => %{
        "sessionId" => session_id,
        "kind" => "thinking",
        "content" => delta
      }
    }

    {:messages, [notification], state}
  end

  defp handle_notification("item/completed", params, state) do
    item = params["item"] || %{}

    case item["type"] do
      "agent_message" ->
        text = item["text"] || ""
        session_id = state.thread_id || "default"

        notification = %{
          "jsonrpc" => "2.0",
          "method" => "session/update",
          "params" => %{
            "sessionId" => session_id,
            "kind" => "text",
            "content" => text,
            "final" => true
          }
        }

        {:messages, [notification], state}

      _ ->
        {:skip, state}
    end
  end

  defp handle_notification("turn/completed", params, state) do
    turn = params["turn"] || %{}
    status = turn["status"]

    # Find the pending turn_start ACP request ID
    acp_id = find_turn_acp_id(state)

    text =
      state.accumulated_text
      |> Enum.reverse()
      |> IO.iodata_to_binary()

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

        [response]
      else
        []
      end

    state = %{state | accumulated_text: [], accumulated_thinking: [], turn_id: nil}
    {:messages, messages, state}
  end

  defp handle_notification("thread/tokenUsage/updated", params, state) do
    token_usage = params["tokenUsage"] || %{}
    total = token_usage["total"] || %{}
    session_id = state.thread_id || "default"

    notification = %{
      "jsonrpc" => "2.0",
      "method" => "session/update",
      "params" => %{
        "sessionId" => session_id,
        "kind" => "usage",
        "content" => %{
          "inputTokens" => total["inputTokens"] || 0,
          "outputTokens" => total["outputTokens"] || 0,
          "cachedInputTokens" => total["cachedInputTokens"] || 0
        }
      }
    }

    {:messages, [notification], state}
  end

  defp handle_notification("error", params, state) do
    error = params["error"] || %{}
    session_id = state.thread_id || "default"

    notification = %{
      "jsonrpc" => "2.0",
      "method" => "session/update",
      "params" => %{
        "sessionId" => session_id,
        "kind" => "error",
        "content" => error["message"] || "Unknown error"
      }
    }

    {:messages, [notification], state}
  end

  defp handle_notification("item/commandExecution/outputDelta", params, state) do
    delta = params["delta"] || ""
    session_id = state.thread_id || "default"

    notification = %{
      "jsonrpc" => "2.0",
      "method" => "session/update",
      "params" => %{
        "sessionId" => session_id,
        "kind" => "tool_output",
        "content" => delta,
        "itemId" => params["itemId"] || params["item_id"]
      }
    }

    {:messages, [notification], state}
  end

  defp handle_notification(_method, _params, state) do
    {:skip, state}
  end

  # Helpers

  defp next_request_id(%{next_id: id} = state) do
    {id, %{state | next_id: id + 1}}
  end

  defp track_request(state, id, type, acp_id) do
    entry = %{type: type, acp_id: acp_id}
    %{state | pending_requests: Map.put(state.pending_requests, id, entry)}
  end

  defp find_turn_acp_id(state) do
    state.pending_requests
    |> Enum.find_value(fn
      {_id, %{type: :turn_start, acp_id: acp_id}} -> acp_id
      _ -> nil
    end)
  end

  defp encode_request(id, method, params) do
    msg =
      %{"id" => id, "method" => method}
      |> maybe_put("params", params)

    [Jason.encode!(msg), "\n"]
  end

  defp encode_notification(method, params \\ nil) do
    msg =
      %{"method" => method}
      |> maybe_put("params", params)

    [Jason.encode!(msg), "\n"]
  end

  defp extract_prompt_text(nil), do: ""

  defp extract_prompt_text(blocks) when is_list(blocks) do
    blocks
    |> Enum.filter(&(&1["type"] == "text"))
    |> Enum.map_join("\n", &(&1["text"] || ""))
  end

  defp extract_prompt_text(text) when is_binary(text), do: text

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
