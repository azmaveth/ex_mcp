defmodule ExMCP.ACP.Adapters.ClaudeSDK.Mapper do
  @moduledoc """
  Pure Claude SDK message to ACP message mapping.
  """

  alias ExMCP.ACP.Adapters.ClaudeSDK.Protocol, as: ClaudeProtocol
  alias ExMCP.ACP.Adapters.ClaudeSDK.ToolInfo
  alias ExMCP.ACP.Protocol, as: ACPProtocol
  alias ExMCP.ACP.{AdapterEvents, Envelope, PendingRequests, PromptQueue}

  @stop_reasons %{
    "end_turn" => "end_turn",
    "stop" => "end_turn",
    "tool_use" => "end_turn",
    "max_tokens" => "max_tokens",
    "refusal" => "refusal",
    "cancelled" => "cancelled"
  }

  @auth_errors ~w(authentication_failed oauth_org_not_allowed billing_error)

  @doc "Builds the dynamic session setup result for the current adapter state."
  @spec session_result(map(), String.t()) :: map()
  def session_result(state, session_id) do
    %{
      "sessionId" => session_id,
      "modes" => modes_result(state),
      "configOptions" => config_options(state)
    }
    |> compact()
  end

  @doc "Builds dynamic config options from SDK initialization/model state."
  @spec config_options(map()) :: [map()]
  def config_options(state) do
    [
      mode_option(state),
      model_option(state),
      effort_option(state),
      fast_mode_option(state),
      agent_option(state)
    ]
    |> Enum.reject(&is_nil/1)
  end

  @doc "Builds the dynamic ACP modes result."
  @spec modes_result(map()) :: map()
  def modes_result(state) do
    %{
      "availableModes" => modes(),
      "currentModeId" => permission_mode_to_mode(state.permission_mode || "default")
    }
  end

  @doc "Static mode list for Claude permission modes."
  @spec modes() :: [map()]
  def modes do
    [
      %{
        "id" => "default",
        "name" => "Default",
        "description" => "Standard behavior, prompts for dangerous operations"
      },
      %{
        "id" => "acceptEdits",
        "name" => "Accept Edits",
        "description" => "Auto-accept file edit operations"
      },
      %{
        "id" => "plan",
        "name" => "Plan Mode",
        "description" => "Planning mode, no actual tool execution"
      },
      %{
        "id" => "auto",
        "name" => "Auto",
        "description" => "Use a model classifier to approve/deny permission prompts"
      },
      %{
        "id" => "dontAsk",
        "name" => "Don't Ask",
        "description" => "Don't prompt for permissions, deny if not pre-approved"
      }
    ]
  end

  @doc "Classifies a Claude SDK result into an ACP stop reason."
  @spec stop_reason(map()) :: String.t()
  def stop_reason(%{"subtype" => subtype})
      when subtype in ["error_max_turns", "error_max_budget_usd"] do
    "max_turn_requests"
  end

  def stop_reason(%{"subtype" => "error_max_structured_output_retries"}), do: "max_turn_requests"

  def stop_reason(%{"stop_reason" => reason}) when is_binary(reason) do
    Map.get(@stop_reasons, reason, "end_turn")
  end

  def stop_reason(%{"is_error" => true}), do: "refusal"
  def stop_reason(_), do: "end_turn"

  @doc "Maps one decoded SDK stdout message into ACP messages and SDK writes."
  @spec reduce_message(map(), map()) :: {[map()], [iodata()], map()}
  def reduce_message(%{"type" => "control_response"} = event, state) do
    handle_control_response(event, state)
  end

  def reduce_message(
        %{"type" => "control_request", "request_id" => request_id, "request" => request},
        state
      ) do
    handle_control_request(request_id, request, state)
  end

  def reduce_message(%{"type" => "control_cancel_request", "request_id" => request_id}, state) do
    state = cancel_pending_client_request(state, request_id)
    {[], [], state}
  end

  def reduce_message(%{"type" => "stream_event", "event" => event} = wrapper, state) do
    state = maybe_set_session(state, wrapper)
    handle_stream_event(event, state)
  end

  def reduce_message(%{"type" => "assistant", "message" => message} = wrapper, state) do
    state = maybe_set_session(state, wrapper)
    handle_assistant(message, state)
  end

  def reduce_message(%{"type" => "user", "message" => message} = wrapper, state) do
    state = maybe_set_session(state, wrapper)
    handle_user(message, state)
  end

  def reduce_message(%{"type" => "result"} = event, state) do
    handle_result(event, maybe_set_session(state, event))
  end

  def reduce_message(%{"type" => "system"} = event, state) do
    handle_system(event, maybe_set_session(state, event))
  end

  def reduce_message(%{"type" => "tool_progress"} = event, state) do
    update =
      %{
        "toolCallId" => event["tool_use_id"],
        "status" => "in_progress",
        "_meta" => %{
          "ex_mcp.claude_sdk" => %{
            "toolName" => event["tool_name"],
            "elapsedTimeSeconds" => event["elapsed_time_seconds"],
            "taskId" => event["task_id"]
          }
        }
      }
      |> compact()

    {[AdapterEvents.tool_call_update(session_id(state), update)], [], state}
  end

  def reduce_message(%{"type" => "tool_use_summary"} = event, state) do
    update = %{"_meta" => %{"ex_mcp.claude_sdk" => %{"toolUseSummary" => event["summary"]}}}

    {[AdapterEvents.session_info_update(session_id(state), update)], [], state}
  end

  def reduce_message(%{"type" => "rate_limit_event"} = event, state) do
    update = %{
      "_meta" => %{
        "ex_mcp.claude_sdk" => %{
          "status" => "rate_limited",
          "rateLimitInfo" => event["rate_limit_info"]
        }
      }
    }

    {[AdapterEvents.session_info_update(session_id(state), update)], [], state}
  end

  def reduce_message(_event, state), do: {[], [], state}

  @doc """
  Replays persisted Claude JSONL transcript entries as ACP session updates.
  """
  @spec replay_messages([map()], map()) :: {[map()], map()}
  def replay_messages(events, state) when is_list(events) do
    Enum.reduce(events, {[], state}, fn event, {messages, acc} ->
      acc = remember_message_id(event, acc)
      {replay_messages, acc} = replay_message(event, acc)
      {messages ++ replay_messages, acc}
    end)
  end

  @doc "Maps a client JSON-RPC response back into a Claude control response."
  @spec client_response(map(), map()) :: {:ok, iodata(), map()} | :unknown
  def client_response(%{"id" => id, "result" => result}, state) do
    case pop_pending_client_request(state, id) do
      {nil, _state} ->
        :unknown

      {%{request_id: request_id, request: request, kind: :permission}, state} ->
        response = ClaudeProtocol.permission_result(result["outcome"] || result, request)

        {:ok, ClaudeProtocol.control_success(request_id, response) |> ClaudeProtocol.line(),
         state}

      {%{request_id: request_id, kind: :file_read, request: request}, state} ->
        response =
          %{
            "contents" => result["content"] || result["contents"] || "",
            "absPath" => result["absPath"] || request["path"]
          }
          |> compact()

        {:ok, ClaudeProtocol.control_success(request_id, response) |> ClaudeProtocol.line(),
         state}

      {%{request_id: request_id}, state} ->
        {:ok, ClaudeProtocol.control_success(request_id, result || %{}) |> ClaudeProtocol.line(),
         state}
    end
  end

  def client_response(%{"id" => id, "error" => error}, state) do
    case pop_pending_client_request(state, id) do
      {nil, _state} ->
        :unknown

      {%{request_id: request_id}, state} ->
        message = error["message"] || "ACP client request failed"
        {:ok, ClaudeProtocol.control_error(request_id, message) |> ClaudeProtocol.line(), state}
    end
  end

  def client_response(_msg, _state), do: :unknown

  defp replay_message(%{"type" => "user"} = event, state) do
    {tool_messages, _writes, state} = reduce_message(event, state)
    text_messages = replay_user_content(event, state)
    {text_messages ++ tool_messages, state}
  end

  defp replay_message(event, state) do
    {messages, _writes, state} = reduce_message(event, state)
    {messages, state}
  end

  defp replay_user_content(event, state) do
    event
    |> get_in(["message", "content"])
    |> replay_content_blocks()
    |> Enum.map(fn content ->
      AdapterEvents.content_chunk(session_id(state), "user_message_chunk", content,
        meta: replay_meta(event)
      )
    end)
  end

  defp replay_content_blocks(content) when is_binary(content),
    do: [%{"type" => "text", "text" => content}]

  defp replay_content_blocks(content) when is_list(content) do
    content
    |> Enum.reject(&(&1["type"] == "tool_result"))
    |> Enum.flat_map(&replay_content_block/1)
  end

  defp replay_content_blocks(_content), do: []

  defp replay_content_block(%{"type" => "text", "text" => text}) when is_binary(text),
    do: [%{"type" => "text", "text" => text}]

  defp replay_content_block(%{"type" => "image", "source" => source}) when is_map(source) do
    [
      %{
        "type" => "image",
        "mimeType" => source["media_type"] || source["mimeType"] || "image/png",
        "data" => source["data"] || ""
      }
    ]
  end

  defp replay_content_block(_block), do: []

  defp replay_meta(event) do
    %{
      "ex_mcp.claude_sdk" =>
        %{
          "replay" => true,
          "messageUuid" => message_uuid(event),
          "parentUuid" => event["parentUuid"] || event["parent_uuid"],
          "timestamp" => event["timestamp"]
        }
        |> compact()
    }
  end

  defp remember_message_id(event, state) do
    uuid = message_uuid(event)

    if is_binary(uuid) and Map.has_key?(state, :message_ids) do
      %{state | message_ids: Map.put(state.message_ids, uuid, event)}
    else
      state
    end
  end

  defp message_uuid(event) do
    event["uuid"] || event["message_uuid"] || event["messageUuid"] ||
      get_in(event, ["message", "id"])
  end

  defp handle_control_response(
         %{"response" => %{"request_id" => request_id, "response" => response}},
         state
       ) do
    case Map.pop(state.pending_controls, request_id) do
      {nil, _pending} ->
        {[], [], state}

      {:initialize, pending} ->
        state =
          %{state | pending_controls: pending}
          |> put_init_response(response || %{})

        messages = initialization_updates(state)
        {messages, [], state}

      {_kind, pending} ->
        {[], [], %{state | pending_controls: pending}}
    end
  end

  defp handle_control_response(%{"response" => %{"request_id" => request_id}}, state) do
    {_kind, pending} = Map.pop(state.pending_controls, request_id)
    {[], [], %{state | pending_controls: pending}}
  end

  defp handle_control_response(_event, state), do: {[], [], state}

  defp handle_control_request(request_id, %{"subtype" => "can_use_tool"} = request, state) do
    acp_id = ACPProtocol.generate_id()
    tool_call = ClaudeProtocol.permission_tool_call(request, state.cwd)
    options = ClaudeProtocol.permission_options(request)
    tool_call_id = tool_call["toolCallId"]

    message =
      ACPProtocol.encode_permission_request(session_id(state), tool_call, options)
      |> Map.put("id", acp_id)

    tool_message =
      if Map.has_key?(state.tool_calls, tool_call_id) do
        nil
      else
        AdapterEvents.tool_call(session_id(state), Map.put(tool_call, "status", "pending"))
      end

    state =
      state
      |> put_pending_client_request(acp_id, request_id, :permission, request)
      |> Map.update!(:tool_calls, fn tool_calls ->
        Map.put_new(tool_calls, tool_call_id, %{
          name: request["tool_name"],
          input: request["input"] || %{}
        })
      end)

    {[tool_message, message] |> Enum.reject(&is_nil/1), [], state}
  end

  defp handle_control_request(request_id, %{"subtype" => "read_file"} = request, state) do
    acp_id = ACPProtocol.generate_id()

    message =
      ACPProtocol.encode_file_read_request(session_id(state), request["path"],
        max_bytes: request["max_bytes"]
      )
      |> Map.put("id", acp_id)

    state = put_pending_client_request(state, acp_id, request_id, :file_read, request)
    {[message], [], state}
  end

  defp handle_control_request(request_id, %{"subtype" => subtype}, state) do
    error = "Claude SDK control request #{subtype} is not supported by ExMCP yet"
    {[], [ClaudeProtocol.control_error(request_id, error) |> ClaudeProtocol.line()], state}
  end

  defp handle_control_request(request_id, _request, state) do
    {[],
     [
       ClaudeProtocol.control_error(request_id, "Malformed Claude SDK control request")
       |> ClaudeProtocol.line()
     ], state}
  end

  defp handle_stream_event(%{"type" => "content_block_start", "content_block" => block}, state) do
    case block do
      %{"type" => "tool_use"} ->
        {messages, state} = emit_tool_pending(block, state)
        {messages, [], state}

      %{"type" => type} ->
        {[], [], %{state | current_block_type: type}}

      _ ->
        {[], [], state}
    end
  end

  defp handle_stream_event(%{"type" => "content_block_delta", "delta" => delta}, state) do
    case delta do
      %{"type" => "text_delta", "text" => text} ->
        state = %{state | text_acc: [text | state.text_acc]}

        {[AdapterEvents.agent_message_chunk(session_id(state), text)], [], state}

      %{"type" => "thinking_delta", "thinking" => thinking} ->
        state = %{
          state
          | thinking_acc: [thinking | state.thinking_acc],
            current_block_type: "thinking"
        }

        {[AdapterEvents.agent_thought_chunk(session_id(state), thinking)], [], state}

      %{"type" => "input_json_delta"} ->
        {[], [], state}

      _ ->
        {[], [], state}
    end
  end

  defp handle_stream_event(%{"type" => "content_block_stop"}, state) do
    {[], [], finalize_block(state)}
  end

  defp handle_stream_event(_event, state), do: {[], [], state}

  defp handle_assistant(%{"content" => content} = message, state) when is_list(content) do
    state =
      state
      |> maybe_set(:model, message["model"])
      |> maybe_set(:session_id, message["session_id"])

    Enum.reduce(content, {[], [], state}, fn block, {messages, writes, acc} ->
      {new_messages, new_writes, acc} = handle_assistant_block(block, acc)
      {messages ++ new_messages, writes ++ new_writes, acc}
    end)
  end

  defp handle_assistant(_message, state), do: {[], [], state}

  defp handle_assistant_block(%{"type" => "text", "text" => text}, state) do
    state =
      if state.text_acc == [] do
        %{state | text_acc: [text]}
      else
        state
      end

    {[AdapterEvents.agent_message_chunk(session_id(state), text)], [], state}
  end

  defp handle_assistant_block(%{"type" => "thinking", "thinking" => thinking}, state) do
    state = %{
      state
      | thinking_blocks: [%{text: thinking, signature: nil} | state.thinking_blocks]
    }

    {[], [], state}
  end

  defp handle_assistant_block(%{"type" => "tool_use"} = block, state) do
    {pending_messages, state} = emit_tool_pending(block, state)
    {update_messages, state} = emit_tool_update(block, state)

    plan_messages =
      if block["name"] == "TodoWrite" do
        entries = ToolInfo.plan_entries(block["input"] || %{})

        if entries == [],
          do: [],
          else: [AdapterEvents.plan(session_id(state), entries)]
      else
        []
      end

    {pending_messages ++ update_messages ++ plan_messages, [], state}
  end

  defp handle_assistant_block(_block, state), do: {[], [], state}

  defp handle_user(%{"content" => content}, state) when is_list(content) do
    {messages, state} =
      content
      |> Enum.filter(&(&1["type"] == "tool_result"))
      |> Enum.reduce({[], state}, fn result, {messages, acc} ->
        tool_call_id = result["tool_use_id"]
        tool = Map.get(acc.tool_calls, tool_call_id, %{})
        update = tool_result_update(result, tool)
        acc = %{acc | tool_calls: Map.delete(acc.tool_calls, tool_call_id)}

        {[AdapterEvents.tool_call_update(session_id(acc), update) | messages], acc}
      end)

    {Enum.reverse(messages), [], state}
  end

  defp handle_user(_message, state), do: {[], [], state}

  defp handle_result(result, state) do
    state =
      state
      |> finalize_block()
      |> Map.put(
        :fast_mode_enabled,
        fast_mode_enabled?(result["fast_mode_state"], state.fast_mode_enabled)
      )

    session_id = result["session_id"] || state.session_id || "default"
    usage = format_usage(result["usage"] || %{})

    text =
      case state.text_acc do
        [] -> result["result"] || ""
        acc -> IO.iodata_to_binary(Enum.reverse(acc))
      end

    response =
      %{
        "stopReason" => stop_reason(result),
        "usage" => usage,
        "_meta" => %{
          "ex_mcp.claude_sdk" =>
            %{
              "text" => text,
              "sessionId" => session_id,
              "modelUsage" => result["modelUsage"],
              "totalCostUsd" => result["total_cost_usd"],
              "errors" => result["errors"]
            }
            |> compact()
        }
      }
      |> maybe_put_error_meta(result)

    messages =
      [
        usage_update(session_id, usage, result, state),
        config_option_update(state),
        AdapterEvents.session_info_update(session_id, %{
          "_meta" => %{"ex_mcp.claude_sdk" => %{"status" => "completed"}}
        }),
        result_text_chunk(session_id, text, state)
      ]
      |> Enum.reject(&is_nil/1)

    messages =
      if state.pending_prompt_id do
        [Envelope.response(state.pending_prompt_id, response) | messages]
      else
        messages
      end

    state = %{
      state
      | pending_prompt_id: nil,
        active_prompt_session_id: nil,
        text_acc: [],
        thinking_acc: [],
        thinking_blocks: [],
        current_block_type: nil,
        session_id: session_id
    }

    {writes, state} = start_next_queued_prompt(state)

    {Enum.reverse(messages), writes, state}
  end

  defp result_text_chunk(session_id, text, %{pending_prompt_id: pending_prompt_id, text_acc: []})
       when not is_nil(pending_prompt_id) and is_binary(text) and text != "" do
    AdapterEvents.agent_message_chunk(session_id, text)
  end

  defp result_text_chunk(_session_id, _text, _state), do: nil

  defp handle_system(%{"subtype" => "init"} = event, state) do
    state =
      state
      |> maybe_set(:session_id, event["session_id"])
      |> maybe_set(:model, event["model"])
      |> maybe_set(:permission_mode, event["permissionMode"])
      |> Map.put(
        :fast_mode_enabled,
        fast_mode_enabled?(event["fast_mode_state"], state.fast_mode_enabled)
      )

    updates =
      [
        AdapterEvents.session_info_update(session_id(state), %{
          "_meta" => %{
            "ex_mcp.claude_sdk" =>
              %{
                "status" => "initialized",
                "claudeCodeVersion" => event["claude_code_version"],
                "cwd" => event["cwd"],
                "tools" => event["tools"],
                "mcpServers" => event["mcp_servers"]
              }
              |> compact()
          }
        }),
        current_mode_update(state),
        config_option_update(state),
        available_commands_update(state, event["slash_commands"] || [])
      ]
      |> Enum.reject(&is_nil/1)

    {updates, [], state}
  end

  defp handle_system(%{"subtype" => "status"} = event, state) do
    state = maybe_set(state, :permission_mode, event["permissionMode"])

    update =
      %{
        "_meta" => %{
          "ex_mcp.claude_sdk" =>
            %{
              "status" => event["status"],
              "compactResult" => event["compact_result"],
              "compactError" => event["compact_error"]
            }
            |> compact()
        }
      }

    messages =
      [AdapterEvents.session_info_update(session_id(state), update), current_mode_update(state)]
      |> Enum.reject(&is_nil/1)

    {messages, [], state}
  end

  defp handle_system(%{"subtype" => "session_state_changed"} = event, state) do
    update = %{"_meta" => %{"ex_mcp.claude_sdk" => %{"sessionState" => event["state"]}}}

    {[AdapterEvents.session_info_update(session_id(state), update)], [], state}
  end

  defp handle_system(%{"subtype" => "commands_changed", "commands" => commands}, state) do
    {[available_commands_update(state, commands)], [], state}
  end

  defp handle_system(%{"subtype" => subtype} = event, state)
       when subtype in ["task_started", "task_progress", "task_updated", "task_notification"] do
    {[AdapterEvents.plan(session_id(state), task_plan_entries(event))], [], state}
  end

  defp handle_system(%{"subtype" => "permission_denied"} = event, state) do
    update =
      %{
        "toolCallId" => event["tool_use_id"],
        "status" => "failed",
        "rawOutput" => event["message"],
        "_meta" => %{
          "ex_mcp.claude_sdk" =>
            %{
              "toolName" => event["tool_name"],
              "decisionReason" => event["decision_reason"],
              "decisionReasonType" => event["decision_reason_type"]
            }
            |> compact()
        }
      }

    {[AdapterEvents.tool_call_update(session_id(state), update)], [], state}
  end

  defp handle_system(%{"subtype" => subtype} = event, state) do
    update = %{
      "_meta" => %{"ex_mcp.claude_sdk" => %{"systemSubtype" => subtype, "event" => event}}
    }

    {[AdapterEvents.session_info_update(session_id(state), update)], [], state}
  end

  defp emit_tool_pending(%{"id" => id, "name" => name, "input" => input}, state) do
    if Map.has_key?(state.tool_calls, id) do
      {[], state}
    else
      info = ToolInfo.from_use(name, input || %{}, id, state.cwd)

      update =
        info
        |> Map.take(["title", "kind", "content", "locations", "rawInput", "_meta"])
        |> Map.put("toolCallId", id)
        |> Map.put("status", "pending")
        |> compact()

      state = %{
        state
        | tool_calls: Map.put(state.tool_calls, id, %{name: name, input: input || %{}})
      }

      {[AdapterEvents.tool_call(session_id(state), update)], state}
    end
  end

  defp emit_tool_pending(_block, state), do: {[], state}

  defp emit_tool_update(%{"id" => id, "name" => name, "input" => input}, state) do
    info = ToolInfo.from_use(name, input || %{}, id, state.cwd)

    update =
      info
      |> Map.take(["title", "kind", "content", "locations", "rawInput", "_meta"])
      |> Map.put("toolCallId", id)
      |> Map.put("status", "in_progress")
      |> compact()

    {[AdapterEvents.tool_call_update(session_id(state), update)], state}
  end

  defp emit_tool_update(_block, state), do: {[], state}

  defp tool_result_update(result, %{name: "Bash"}) do
    is_error = result["is_error"] || false
    output = parse_tool_result_raw(result["content"]) || ""
    exit_code = bash_exit_code(result["content"], is_error)
    tool_call_id = result["tool_use_id"]

    %{
      "toolCallId" => tool_call_id,
      "status" => if(is_error, do: "failed", else: "completed"),
      "content" => [%{"type" => "terminal", "terminalId" => tool_call_id}],
      "rawOutput" => output,
      "_meta" => %{
        "terminal_output" => %{"terminal_id" => tool_call_id, "data" => output},
        "terminal_exit" => %{
          "terminal_id" => tool_call_id,
          "exit_code" => exit_code,
          "signal" => nil
        },
        "ex_mcp.claude_sdk" => %{"isError" => is_error}
      }
    }
    |> compact()
  end

  defp tool_result_update(result, tool) do
    is_error = result["is_error"] || false

    %{
      "toolCallId" => result["tool_use_id"],
      "status" => if(is_error, do: "failed", else: "completed"),
      "content" => parse_tool_result_content(result["content"]),
      "rawOutput" => parse_tool_result_raw(result["content"]),
      "_meta" =>
        %{
          "ex_mcp.claude_sdk" => %{
            "isError" => is_error,
            "toolName" => tool[:name],
            "toolInput" => tool[:input]
          }
        }
        |> compact()
    }
    |> compact()
  end

  defp initialization_updates(%{session_id: nil}), do: []

  defp initialization_updates(state) do
    [
      available_commands_update(state, state.available_commands),
      config_option_update(state),
      current_mode_update(state)
    ]
    |> Enum.reject(&is_nil/1)
  end

  defp put_init_response(state, response) do
    %{
      state
      | init_response: response,
        available_commands: normalize_commands(response["commands"] || []),
        available_models: response["models"] || [],
        available_agents:
          normalize_agents(response["agents"] || response["supportedAgents"] || []),
        fast_mode_enabled:
          fast_mode_enabled?(response["fast_mode_state"], state.fast_mode_enabled)
    }
  end

  defp mode_option(state) do
    %{
      "id" => "mode",
      "name" => "Mode",
      "description" => "Session permission mode",
      "category" => "mode",
      "type" => "select",
      "currentValue" => permission_mode_to_mode(state.permission_mode || "default"),
      "options" =>
        Enum.map(modes(), fn mode ->
          %{
            "name" => mode["name"],
            "value" => mode["id"],
            "description" => mode["description"]
          }
          |> compact()
        end)
    }
  end

  defp model_option(state) do
    options =
      case state.available_models do
        [] ->
          [
            %{"name" => "Default", "value" => "default"},
            %{"name" => "Sonnet", "value" => "sonnet"},
            %{"name" => "Opus", "value" => "opus"}
          ]

        models ->
          Enum.map(models, fn model ->
            %{
              "name" =>
                model["displayName"] || model["display_name"] || model["name"] || model["id"] ||
                  model["value"],
              "value" => model["value"] || model["id"] || model["name"],
              "description" => model["description"]
            }
            |> compact()
          end)
      end

    %{
      "id" => "model",
      "name" => "Model",
      "description" => "AI model to use",
      "category" => "model",
      "type" => "select",
      "currentValue" => state.model || "default",
      "options" => options
    }
  end

  defp effort_option(state) do
    model = current_model_info(state)
    levels = model["supportedEffortLevels"] || model["supported_effort_levels"] || []

    options =
      if levels == [] do
        [
          %{"name" => "Default", "value" => "default"},
          %{"name" => "Low", "value" => "low"},
          %{"name" => "Medium", "value" => "medium"},
          %{"name" => "High", "value" => "high"}
        ]
      else
        [%{"name" => "Default", "value" => "default"}] ++
          Enum.map(levels, fn level ->
            %{"name" => humanize_config_value(level), "value" => level}
          end)
      end

    %{
      "id" => "effort",
      "name" => "Effort",
      "description" => "Available effort levels for this model",
      "category" => "thought_level",
      "type" => "select",
      "currentValue" => state.effort || "default",
      "options" => options
    }
  end

  defp fast_mode_option(state) do
    model = current_model_info(state)

    if model["supportsFastMode"] == true or model["supports_fast_mode"] == true do
      if client_supports_boolean_config?(state.client_capabilities) do
        %{
          "id" => "fast",
          "name" => "Fast mode",
          "description" => "Faster responses on supported models",
          "category" => "model_config",
          "type" => "boolean",
          "currentValue" => state.fast_mode_enabled == true
        }
      else
        %{
          "id" => "fast",
          "name" => "Fast mode",
          "description" => "Faster responses on supported models",
          "category" => "model_config",
          "type" => "select",
          "currentValue" => if(state.fast_mode_enabled == true, do: "on", else: "off"),
          "options" => [
            %{"name" => "On", "value" => "on"},
            %{"name" => "Off", "value" => "off"}
          ]
        }
      end
    end
  end

  defp agent_option(%{available_agents: agents} = state) when is_list(agents) and agents != [] do
    %{
      "id" => "agent",
      "name" => "Agent",
      "description" => "Main-thread agent persona",
      "type" => "select",
      "currentValue" => state.current_agent || "default",
      "options" =>
        [
          %{
            "name" => "Default",
            "value" => "default",
            "description" => "Standard Claude Code agent"
          }
        ] ++
          Enum.map(agents, fn agent ->
            %{
              "name" => agent["name"],
              "value" => agent["name"],
              "description" => agent["description"]
            }
            |> compact()
          end)
    }
  end

  defp agent_option(_state), do: nil

  defp current_mode_update(state) do
    AdapterEvents.current_mode_update(
      session_id(state),
      permission_mode_to_mode(state.permission_mode || "default")
    )
  end

  defp config_option_update(state) do
    AdapterEvents.config_option_update(session_id(state), config_options(state))
  end

  defp available_commands_update(_state, []), do: nil

  defp available_commands_update(state, commands) do
    AdapterEvents.available_commands_update(session_id(state), normalize_commands(commands))
  end

  defp normalize_commands(commands) do
    Enum.map(commands, fn
      command when is_binary(command) ->
        %{"name" => command, "description" => command}

      %{"name" => _} = command ->
        command

      %{"id" => id} = command ->
        Map.put_new(command, "name", id)

      other ->
        %{"name" => inspect(other), "description" => inspect(other)}
    end)
  end

  defp normalize_agents(agents) do
    agents
    |> Enum.map(fn
      %{"name" => name} = agent when is_binary(name) ->
        agent

      %{name: name} = agent when is_binary(name) ->
        agent
        |> Enum.map(fn {key, value} -> {to_string(key), value} end)
        |> Map.new()

      name when is_binary(name) ->
        %{"name" => name}

      _other ->
        nil
    end)
    |> Enum.reject(fn
      nil ->
        true

      agent ->
        agent["name"] in [
          "claude",
          "general-purpose",
          "Explore",
          "Plan",
          "statusline-setup",
          "default"
        ]
    end)
  end

  defp current_model_info(state) do
    current = state.model || "default"

    Enum.find(state.available_models || [], fn model ->
      (model["value"] || model["id"] || model["name"]) == current
    end) || %{}
  end

  defp fast_mode_enabled?("off", _fallback), do: false
  defp fast_mode_enabled?(state, _fallback) when state in ["on", "cooldown"], do: true
  defp fast_mode_enabled?(nil, fallback), do: fallback == true
  defp fast_mode_enabled?(_, fallback), do: fallback == true

  defp client_supports_boolean_config?(capabilities) do
    get_in(capabilities || %{}, ["session", "configOptions", "boolean"]) != nil
  end

  defp humanize_config_value(value) when is_binary(value) do
    value
    |> String.split(~r/[_-]+/, trim: true)
    |> Enum.map_join(" ", fn
      <<first::binary-size(1), rest::binary>> -> String.upcase(first) <> rest
      "" -> ""
    end)
  end

  defp humanize_config_value(value), do: to_string(value)

  defp task_plan_entries(%{"subtype" => "task_started"} = event) do
    [
      %{
        "content" => event["description"] || event["prompt"] || "Task started",
        "priority" => "medium",
        "status" => "in_progress"
      }
    ]
  end

  defp task_plan_entries(%{"subtype" => "task_progress"} = event) do
    [
      %{
        "content" => event["summary"] || event["description"] || "Task running",
        "priority" => "medium",
        "status" => "in_progress"
      }
    ]
  end

  defp task_plan_entries(%{"subtype" => "task_notification"} = event) do
    status = if event["status"] == "completed", do: "completed", else: "pending"

    [
      %{
        "content" => event["summary"] || "Task #{event["status"]}",
        "priority" => "medium",
        "status" => status
      }
    ]
  end

  defp task_plan_entries(%{"patch" => patch}) do
    status = if patch["status"] == "completed", do: "completed", else: "in_progress"

    [
      %{
        "content" => patch["description"] || patch["error"] || "Task updated",
        "priority" => "medium",
        "status" => status
      }
    ]
  end

  defp finalize_block(%{current_block_type: "thinking", thinking_acc: acc} = state)
       when acc != [] do
    text = IO.iodata_to_binary(Enum.reverse(acc))

    %{
      state
      | thinking_blocks: [%{text: text, signature: nil} | state.thinking_blocks],
        thinking_acc: [],
        current_block_type: nil
    }
  end

  defp finalize_block(state), do: %{state | current_block_type: nil}

  defp parse_tool_result_content(content) when is_list(content) do
    Enum.map(content, fn
      %{"type" => "text", "text" => text} ->
        %{"type" => "content", "content" => %{"type" => "text", "text" => text}}

      %{"text" => text} ->
        %{"type" => "content", "content" => %{"type" => "text", "text" => text}}

      other ->
        %{"type" => "content", "content" => %{"type" => "text", "text" => inspect(other)}}
    end)
  end

  defp parse_tool_result_content(content) when is_binary(content) do
    [%{"type" => "content", "content" => %{"type" => "text", "text" => content}}]
  end

  defp parse_tool_result_content(_), do: []

  defp parse_tool_result_raw(content) when is_binary(content), do: content

  defp parse_tool_result_raw(%{"type" => "bash_code_execution_result"} = content) do
    [content["stdout"], content["stderr"]]
    |> Enum.filter(&(is_binary(&1) and &1 != ""))
    |> Enum.join("\n")
  end

  defp parse_tool_result_raw(content) when is_list(content) do
    Enum.map_join(content, "\n", fn
      %{"text" => text} -> text
      other -> inspect(other)
    end)
  end

  defp parse_tool_result_raw(_), do: nil

  defp bash_exit_code(%{"type" => "bash_code_execution_result", "return_code" => code}, _is_error)
       when is_integer(code),
       do: code

  defp bash_exit_code(_content, true), do: 1
  defp bash_exit_code(_content, _is_error), do: 0

  defp format_usage(raw) do
    %{
      "inputTokens" => raw["input_tokens"] || 0,
      "outputTokens" => raw["output_tokens"] || 0,
      "cacheReadTokens" => raw["cache_read_input_tokens"] || 0,
      "cacheCreationTokens" => raw["cache_creation_input_tokens"] || 0
    }
  end

  defp usage_update(session_id, usage, result, state) do
    used =
      usage
      |> Map.values()
      |> Enum.filter(&is_integer/1)
      |> Enum.sum()

    size = context_window_size(result, state)

    if used > 0 and is_integer(size) and size > 0 do
      AdapterEvents.session_update_type(session_id, "usage_update", %{
        "used" => used,
        "size" => size
      })
    end
  end

  defp context_window_size(%{"modelUsage" => model_usage}, state) when is_map(model_usage) do
    current_model = state.model || "default"

    model_usage
    |> Enum.find_value(fn {model, usage} ->
      if String.starts_with?(to_string(model), to_string(current_model)) and is_map(usage) do
        usage["contextWindow"] || usage["context_window"]
      end
    end)
    |> case do
      nil ->
        model_usage
        |> Map.values()
        |> Enum.find_value(fn
          %{"contextWindow" => size} -> size
          %{"context_window" => size} -> size
          _ -> nil
        end)

      size ->
        size
    end
  end

  defp context_window_size(_result, state) do
    model = current_model_info(state)

    [state.model, model["displayName"], model["display_name"], model["description"]]
    |> Enum.any?(fn text -> is_binary(text) and String.match?(text, ~r/\b1m\b/i) end)
    |> if(do: 1_000_000, else: nil)
  end

  defp maybe_put_error_meta(response, %{"error" => error}) when error in @auth_errors do
    put_in(response, ["_meta", "ex_mcp.claude_sdk", "authError"], error)
  end

  defp maybe_put_error_meta(response, %{"subtype" => subtype}) when is_binary(subtype) do
    put_in(response, ["_meta", "ex_mcp.claude_sdk", "resultSubtype"], subtype)
  end

  defp maybe_put_error_meta(response, _result), do: response

  defp put_pending_client_request(state, acp_id, request_id, kind, request) do
    pending =
      PendingRequests.put(state.pending_client_requests, acp_id, %{
        request_id: request_id,
        kind: kind,
        request: request
      })

    %{state | pending_client_requests: pending}
  end

  defp pop_pending_client_request(state, acp_id) do
    {request, pending} = PendingRequests.pop(state.pending_client_requests, acp_id)
    {request, %{state | pending_client_requests: pending}}
  end

  defp cancel_pending_client_request(state, request_id) do
    pending =
      state.pending_client_requests
      |> Enum.reject(fn {_id, pending} -> pending.request_id == request_id end)
      |> Map.new()

    %{state | pending_client_requests: pending}
  end

  defp start_next_queued_prompt(state) do
    case PromptQueue.pop(state.prompt_queue) do
      {:value, queued, rest} ->
        state =
          %{
            state
            | pending_prompt_id: queued.id,
              active_prompt_session_id: queued.session_id || state.session_id,
              session_id: queued.session_id || state.session_id,
              prompt_queue: rest,
              text_acc: [],
              thinking_acc: [],
              thinking_blocks: [],
              current_block_type: nil,
              tool_calls: %{}
          }

        {[ClaudeProtocol.line(queued.message)], state}

      :empty ->
        {[], state}
    end
  end

  defp session_id(%{session_id: nil}), do: "default"
  defp session_id(%{session_id: session_id}), do: session_id

  defp maybe_set_session(state, %{"session_id" => session_id})
       when is_binary(session_id) and session_id != "" do
    %{state | session_id: session_id}
  end

  defp maybe_set_session(state, _event), do: state

  defp permission_mode_to_mode("acceptEdits"), do: "acceptEdits"
  defp permission_mode_to_mode("plan"), do: "plan"
  defp permission_mode_to_mode("auto"), do: "auto"
  defp permission_mode_to_mode("dontAsk"), do: "dontAsk"
  defp permission_mode_to_mode(_), do: "default"

  defp maybe_set(state, _key, nil), do: state
  defp maybe_set(state, key, value), do: Map.put(state, key, value)

  defp compact(map) do
    map
    |> Enum.reject(fn {_key, value} -> value in [nil, [], %{}] end)
    |> Map.new()
  end
end
