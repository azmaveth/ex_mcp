defmodule ExMCP.ACP.Adapters.Pi do
  @moduledoc """
  ACP adapter for the Pi coding agent.

  The adapter translates ACP JSON-RPC to Pi's RPC NDJSON protocol. Pi runs as a
  persistent subprocess in `--mode rpc` and the adapter correlates Pi RPC
  responses with ACP lifecycle/control requests.
  """

  @behaviour ExMCP.ACP.Adapter

  require Logger

  alias ExMCP.ACP.{AdapterEvents, Envelope, Types}
  alias ExMCP.ACP.Adapters.Pi.{Prompt, SessionStore, SlashCommands, Tools}

  @thinking_levels ~w(off minimal low medium high xhigh)
  @auth_method_id "pi_terminal_login"
  @default_thinking_level "medium"

  defstruct [
    :session_id,
    :session_file,
    :session_dir,
    :session_map_path,
    :cwd,
    :thinking_level,
    :current_model_id,
    opts: [],
    available_models: [],
    file_commands: [],
    available_commands: [],
    text_acc: [],
    pending_prompt: nil,
    prompt_queue: :queue.new(),
    active_tool_executions: %{},
    current_tool_calls: %{},
    edit_snapshots: %{},
    pending_controls: %{},
    control_groups: %{},
    msg_counter: 0
  ]

  @impl true
  def init(opts) do
    {:ok,
     %__MODULE__{
       opts: opts,
       thinking_level: Keyword.get(opts, :thinking_level, @default_thinking_level),
       session_dir: Keyword.get(opts, :session_dir),
       session_map_path: Keyword.get(opts, :session_map_path, SessionStore.default_map_path()),
       cwd: Keyword.get(opts, :cwd)
     }}
  end

  @impl true
  def command(opts) do
    cli_path = Keyword.get(opts, :cli_path, "pi")

    args =
      ["--mode", "rpc", "--no-themes"]
      |> append_opt(opts, :model, "--model")
      |> append_opt(opts, :cwd, "--cwd")
      |> append_opt(opts, :system_prompt, "--system-prompt")
      |> append_opt(opts, :session_path, "--session")
      |> append_opt(opts, :session_dir, "--session-dir")

    args = if Keyword.get(opts, :no_session, false), do: args ++ ["--no-session"], else: args
    {cli_path, args}
  end

  @impl true
  def capabilities do
    %{
      "loadSession" => true,
      "mcpCapabilities" => %{"http" => false, "sse" => false},
      "promptCapabilities" => %{
        "image" => true,
        "audio" => false,
        "embeddedContext" => System.get_env("PI_ACP_ENABLE_EMBEDDED_CONTEXT") == "true"
      },
      "_meta" => %{
        "ex_mcp.pi" => %{
          "sessionStore" => SessionStore.default_map_path(),
          "thinkingLevels" => @thinking_levels,
          "features" => %{
            "slashCommands" => true,
            "terminalAuth" => true,
            "modelSelection" => true,
            "sessionLoad" => true,
            "structuredDiffs" => true
          }
        }
      }
    }
  end

  @impl true
  def auth_methods(opts) do
    command = Keyword.get(opts, :cli_path, "pi")

    [
      %{
        "id" => @auth_method_id,
        "name" => "Launch pi in the terminal",
        "description" => "Start pi interactively to configure API keys or login",
        "type" => "terminal",
        "args" => [],
        "env" => %{},
        "_meta" => %{
          "terminal-auth" => %{
            "command" => command,
            "args" => [],
            "label" => "Launch pi"
          }
        }
      }
    ]
  end

  @impl true
  def modes do
    Enum.map(@thinking_levels, fn level ->
      %{"id" => level, "name" => "Thinking: #{level}", "description" => nil}
    end)
  end

  @impl true
  def config_options do
    [
      %{
        "id" => "auto_compaction",
        "name" => "Auto Compaction",
        "category" => "other",
        "description" => "Automatically compact context when nearly full",
        "type" => "select",
        "currentValue" => "true",
        "options" => boolean_options()
      },
      %{
        "id" => "auto_retry",
        "name" => "Auto Retry",
        "category" => "other",
        "description" => "Automatically retry on transient errors",
        "type" => "select",
        "currentValue" => "true",
        "options" => boolean_options()
      },
      %{
        "id" => "steering_mode",
        "name" => "Steering Mode",
        "category" => "other",
        "description" => "How steering messages are delivered",
        "type" => "select",
        "currentValue" => "all",
        "options" => mode_options()
      },
      %{
        "id" => "follow_up_mode",
        "name" => "Follow-up Mode",
        "category" => "other",
        "description" => "How follow-up messages are delivered",
        "type" => "select",
        "currentValue" => "all",
        "options" => mode_options()
      }
    ]
  end

  @impl true
  def list_sessions(params, state) do
    cursor = params["cursor"] || "0"
    offset = parse_cursor(cursor)
    page_size = 50
    cwd = params["cwd"]

    sessions =
      [session_dir: state.session_dir]
      |> SessionStore.list_pi_sessions()
      |> filter_sessions_by_cwd(cwd)
      |> Enum.slice(offset, page_size)
      |> Enum.map(&Map.drop(&1, ["sessionFile"]))

    {:ok, sessions, state}
  end

  @impl true
  def translate_outbound(%{"method" => "initialize"}, state), do: {:ok, :skip, state}

  def translate_outbound(%{"method" => "authenticate"}, state), do: {:reply, %{}, state}

  def translate_outbound(%{"method" => "session/new", "id" => acp_id, "params" => params}, state) do
    cwd = params["cwd"] || state.cwd || Keyword.get(state.opts, :cwd) || File.cwd!()

    case require_absolute_cwd(cwd) do
      :ok -> start_session_new(acp_id, cwd, state)
      {:error, reason} -> {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/load", "id" => acp_id, "params" => params}, state) do
    session_id = params["sessionId"]
    cwd = params["cwd"] || state.cwd || Keyword.get(state.opts, :cwd) || File.cwd!()

    with :ok <- require_absolute_cwd(cwd),
         session_file when is_binary(session_file) <- find_session_file(session_id, state) do
      file_commands = SlashCommands.load(cwd)

      {switch_id, switch_session} = rpc("switch_session", %{"sessionPath" => session_file})
      {messages_id, get_messages} = rpc("get_messages")
      {state_id, get_state} = rpc("get_state")
      {models_id, get_models} = rpc("get_available_models")
      {commands_id, get_commands} = rpc("get_commands")

      group = %{
        type: :session_load,
        acp_id: acp_id,
        session_id: session_id,
        cwd: cwd,
        session_file: session_file,
        file_commands: file_commands,
        refs: MapSet.new([switch_id, messages_id, state_id, models_id, commands_id]),
        responses: %{}
      }

      state =
        state
        |> put_group(group)
        |> put_control(switch_id, :switch, group)
        |> put_control(messages_id, :messages, group)
        |> put_control(state_id, :state, group)
        |> put_control(models_id, :models, group)
        |> put_control(commands_id, :commands, group)

      {:ok, encode_many([switch_session, get_messages, get_state, get_models, get_commands]),
       state}
    else
      {:error, reason} -> {:error, reason, state}
      _ -> {:error, "Unknown sessionId: #{session_id}", state}
    end
  end

  def translate_outbound(
        %{"method" => "session/prompt", "id" => acp_id, "params" => params},
        state
      ) do
    {message, images} = Prompt.to_pi_message(params["prompt"])
    translate_prompt_message(message, images, acp_id, params, state)
  end

  def translate_outbound(%{"method" => "session/cancel"}, state) do
    had_queued = not :queue.is_empty(state.prompt_queue)
    {queued_responses, state} = cancel_queued_prompts(state)
    state = mark_pending_cancel_requested(state)
    messages = queued_responses ++ queue_cleared_messages(state, had_queued)
    {:messages_and_write, messages, encode_rpc(%{"type" => "abort"}), state}
  end

  def translate_outbound(%{"method" => "session/set_mode", "params" => params}, state) do
    mode = params["modeId"]

    if mode in @thinking_levels do
      state = %{state | thinking_level: mode}

      update =
        AdapterEvents.session_update(params["sessionId"] || state.session_id, %{
          "sessionUpdate" => "current_mode_update",
          "currentModeId" => mode
        })

      {:messages_and_write, [update],
       encode_rpc(%{"type" => "set_thinking_level", "level" => mode}), state}
    else
      {:error, "Unknown modeId: #{inspect(mode)}", state}
    end
  end

  def translate_outbound(%{"method" => "session/set_model", "params" => params}, state) do
    case resolve_model(params["modelId"], state.available_models) do
      {:ok, provider, model_id, current_model_id} ->
        rpc_msg = %{"type" => "set_model", "provider" => provider, "modelId" => model_id}
        {:ok, encode_rpc(rpc_msg), %{state | current_model_id: current_model_id}}

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/set_config_option", "params" => params}, state) do
    translate_config_option(params["configId"], params["value"], state)
  end

  def translate_outbound(%{"method" => method}, state) do
    if String.starts_with?(method, "_ex_mcp.pi/") or String.starts_with?(method, "pi/") do
      {:error, "Pi extension methods were removed; use ACP session methods or slash commands",
       state}
    else
      {:ok, :skip, state}
    end
  end

  @impl true
  def translate_inbound(line, state) do
    trimmed = String.trim(line)

    if trimmed == "" do
      {:skip, state}
    else
      case Jason.decode(trimmed) do
        {:ok, event} -> process_event(event, state)
        {:error, _reason} -> {:skip, state}
      end
    end
  end

  defp translate_prompt_message(message, images, acp_id, params, state) do
    slash = if images == [], do: SlashCommands.parse(message), else: :error

    cond do
      state.pending_prompt ->
        queued = %{acp_id: acp_id, message: message, images: images, params: params}
        queue = :queue.in(queued, state.prompt_queue)

        notice =
          AdapterEvents.session_update(params["sessionId"] || state.session_id, %{
            "sessionUpdate" => "agent_message_chunk",
            "content" => %{
              "type" => "text",
              "text" => "Queued message (position #{:queue.len(queue)})."
            }
          })

        info =
          AdapterEvents.session_update(params["sessionId"] || state.session_id, %{
            "sessionUpdate" => "session_info_update",
            "_meta" => %{
              "ex_mcp" => %{"pi" => %{"queueDepth" => :queue.len(queue), "running" => true}}
            }
          })

        {:messages, [notice, info], %{state | prompt_queue: queue}}

      match?({:ok, _, _}, slash) ->
        translate_slash_command(slash, acp_id, params, state)

      true ->
        start_prompt(acp_id, message, images, params, state)
    end
  end

  defp start_prompt(acp_id, message, images, params, state) do
    msg_id = "msg-#{state.msg_counter + 1}"

    rpc_msg =
      %{"type" => "prompt", "id" => msg_id, "message" => message}
      |> maybe_put_non_empty("images", images)
      |> maybe_put_present("streamingBehavior", params["streamingBehavior"])

    state = %{
      state
      | session_id: params["sessionId"] || state.session_id,
        pending_prompt: %{acp_id: acp_id, msg_id: msg_id, cancel_requested: false},
        text_acc: [],
        msg_counter: state.msg_counter + 1
    }

    {:ok, encode_rpc(rpc_msg), state}
  end

  defp start_session_new(acp_id, cwd, state) do
    file_commands = SlashCommands.load(cwd)

    {new_id, new_session} = rpc("new_session")
    {state_id, get_state} = rpc("get_state")
    {models_id, get_models} = rpc("get_available_models")
    {commands_id, get_commands} = rpc("get_commands")

    group = %{
      type: :session_new,
      acp_id: acp_id,
      cwd: cwd,
      file_commands: file_commands,
      refs: MapSet.new([new_id, state_id, models_id, commands_id]),
      responses: %{}
    }

    state =
      state
      |> put_group(group)
      |> put_control(new_id, :new_session, group)
      |> put_control(state_id, :state, group)
      |> put_control(models_id, :models, group)
      |> put_control(commands_id, :commands, group)

    {:ok, encode_many([new_session, get_state, get_models, get_commands]), state}
  end

  defp translate_slash_command({:ok, name, args}, acp_id, params, state) do
    context = %{
      acp_id: acp_id,
      params: params,
      session_id: params["sessionId"] || state.session_id
    }

    route_slash_command(name, args, context, state)
  end

  defp route_slash_command(name, args, context, state)
       when name in ["compact", "autocompact", "export", "session", "name"] do
    case name do
      "compact" -> slash_compact(args, context, state)
      "autocompact" -> slash_autocompact(args, context, state)
      "export" -> slash_export(context, state)
      "session" -> slash_session(context, state)
      "name" -> slash_name(args, context, state)
    end
  end

  defp route_slash_command(name, args, context, state)
       when name in ["steering", "follow-up", "model", "thinking"] do
    case name do
      "steering" -> slash_steering(args, context, state)
      "follow-up" -> slash_follow_up(args, context, state)
      "model" -> slash_model_notice(context, state)
      "thinking" -> slash_thinking_notice(context, state)
    end
  end

  defp route_slash_command(name, args, context, state) do
    slash_file_or_prompt(name, args, context, state)
  end

  defp slash_compact(args, context, state) do
    custom = args |> Enum.join(" ") |> blank_to_nil()

    start_control_command(
      :slash_compact,
      context.acp_id,
      context.session_id,
      "compact",
      %{"customInstructions" => custom},
      state
    )
  end

  defp slash_autocompact([mode | _], context, state)
       when mode in ["on", "true", "enable", "enabled"] do
    start_control_command(
      :slash_autocompact_on,
      context.acp_id,
      context.session_id,
      "set_auto_compaction",
      %{"enabled" => true},
      state
    )
  end

  defp slash_autocompact([mode | _], context, state)
       when mode in ["off", "false", "disable", "disabled"] do
    start_control_command(
      :slash_autocompact_off,
      context.acp_id,
      context.session_id,
      "set_auto_compaction",
      %{"enabled" => false},
      state
    )
  end

  defp slash_autocompact(_args, context, state) do
    start_control_command(
      :slash_autocompact_toggle_get,
      context.acp_id,
      context.session_id,
      "get_state",
      %{},
      state
    )
  end

  defp slash_export(context, state) do
    safe_id = (context.session_id || "default") |> String.replace(~r/[^A-Za-z0-9_-]/, "_")
    output_path = Path.join(state.cwd || File.cwd!(), "pi-session-#{safe_id}.html")

    start_control_command(
      :slash_export,
      context.acp_id,
      context.session_id,
      "export_html",
      %{"outputPath" => output_path},
      state
    )
  end

  defp slash_session(context, state) do
    start_control_command(
      :slash_session,
      context.acp_id,
      context.session_id,
      "get_session_stats",
      %{},
      state
    )
  end

  defp slash_name([], context, state) do
    prompt_message(context.acp_id, context.session_id, "Usage: /name <name>", state)
  end

  defp slash_name(name_parts, context, state) do
    name_value = Enum.join(name_parts, " ")

    start_control_command(
      :slash_name,
      context.acp_id,
      context.session_id,
      "set_session_name",
      %{"name" => name_value},
      state
    )
  end

  defp slash_steering([], context, state) do
    start_control_command(
      :slash_steering_get,
      context.acp_id,
      context.session_id,
      "get_state",
      %{},
      state
    )
  end

  defp slash_steering([mode | _], context, state) when mode in ["all", "one-at-a-time"] do
    start_control_command(
      :slash_steering_set,
      context.acp_id,
      context.session_id,
      "set_steering_mode",
      %{"mode" => mode},
      state
    )
  end

  defp slash_steering(_args, context, state) do
    prompt_message(
      context.acp_id,
      context.session_id,
      "Usage: /steering all | /steering one-at-a-time",
      state
    )
  end

  defp slash_follow_up([], context, state) do
    start_control_command(
      :slash_follow_up_get,
      context.acp_id,
      context.session_id,
      "get_state",
      %{},
      state
    )
  end

  defp slash_follow_up([mode | _], context, state) when mode in ["all", "one-at-a-time"] do
    start_control_command(
      :slash_follow_up_set,
      context.acp_id,
      context.session_id,
      "set_follow_up_mode",
      %{"mode" => mode},
      state
    )
  end

  defp slash_follow_up(_args, context, state) do
    prompt_message(
      context.acp_id,
      context.session_id,
      "Usage: /follow-up all | /follow-up one-at-a-time",
      state
    )
  end

  defp slash_model_notice(context, state) do
    prompt_message(
      context.acp_id,
      context.session_id,
      "Use the ACP model selector to change models.",
      state
    )
  end

  defp slash_thinking_notice(context, state) do
    prompt_message(
      context.acp_id,
      context.session_id,
      "Use the ACP mode selector to change thinking level.",
      state
    )
  end

  defp slash_file_or_prompt(name, args, context, state) do
    case SlashCommands.expand_file_command(name, args, state.file_commands) do
      nil ->
        start_prompt(context.acp_id, "/" <> name <> slash_args(args), [], context.params, state)

      expanded ->
        start_prompt(context.acp_id, expanded, [], context.params, state)
    end
  end

  defp start_control_command(type, acp_id, session_id, command, params, state) do
    {rpc_id, rpc_msg} = rpc(command, compact(params))

    group = %{
      type: type,
      acp_id: acp_id,
      session_id: session_id,
      refs: MapSet.new([rpc_id]),
      responses: %{}
    }

    state =
      state
      |> put_group(group)
      |> put_control(rpc_id, :result, group)

    {:ok, encode_rpc(rpc_msg), state}
  end

  defp prompt_message(_acp_id, session_id, text, state) do
    message =
      AdapterEvents.session_update(session_id, %{
        "sessionUpdate" => "agent_message_chunk",
        "content" => %{"type" => "text", "text" => text}
      })

    {:messages_and_reply, [message], %{"stopReason" => "end_turn"}, state}
  end

  defp process_event(%{"type" => "response", "id" => id} = event, state) when is_binary(id) do
    case Map.pop(state.pending_controls, id) do
      {nil, pending_controls} ->
        process_untracked_response(event, %{state | pending_controls: pending_controls})

      {control, pending_controls} ->
        state = %{state | pending_controls: pending_controls}
        handle_control_response(control, event, state)
    end
  end

  defp process_event(
         %{
           "type" => "message_update",
           "assistantMessageEvent" => %{"type" => "text_delta", "delta" => delta}
         },
         state
       ) do
    state = %{state | text_acc: [delta | state.text_acc]}

    notification =
      AdapterEvents.session_update(state.session_id, %{
        "sessionUpdate" => "agent_message_chunk",
        "content" => %{"type" => "text", "text" => delta}
      })

    {:messages, [notification], state}
  end

  defp process_event(
         %{
           "type" => "message_update",
           "assistantMessageEvent" => %{"type" => "thinking_delta", "delta" => delta}
         },
         state
       ) do
    notification =
      AdapterEvents.session_update(state.session_id, %{
        "sessionUpdate" => "agent_thought_chunk",
        "content" => %{"type" => "text", "text" => delta}
      })

    {:messages, [notification], state}
  end

  defp process_event(
         %{
           "type" => "message_update",
           "assistantMessageEvent" => %{"type" => type} = tool_event
         },
         state
       )
       when type in ["toolcall_start", "toolcall_delta", "toolcall_end", "tool_call"] do
    {notification, state} = tool_call_stream_update(tool_event, state)

    if notification do
      {:messages, [notification], state}
    else
      {:skip, state}
    end
  end

  defp process_event(
         %{
           "type" => "tool_execution_start",
           "toolCallId" => tool_call_id,
           "toolName" => tool_name
         } = event,
         state
       ) do
    args = event["args"] || %{}
    {line, state} = maybe_snapshot_edit(tool_call_id, tool_name, args, state)
    locations = Tools.locations(args, state.cwd, line)

    update = %{
      "toolCallId" => tool_call_id,
      "title" => tool_name,
      "kind" => Tools.kind(tool_name),
      "status" => "in_progress",
      "locations" => locations,
      "rawInput" => args
    }

    {session_update, current_tool_calls} =
      if Map.has_key?(state.current_tool_calls, tool_call_id) do
        {Map.put(update, "sessionUpdate", "tool_call_update"),
         Map.put(state.current_tool_calls, tool_call_id, "in_progress")}
      else
        {Map.put(update, "sessionUpdate", "tool_call"),
         Map.put(state.current_tool_calls, tool_call_id, "in_progress")}
      end

    notification = AdapterEvents.session_update(state.session_id, compact(session_update))

    state = %{
      state
      | active_tool_executions:
          Map.put(state.active_tool_executions, tool_call_id, %{name: tool_name, args: args}),
        current_tool_calls: current_tool_calls
    }

    {:messages, [notification], state}
  end

  defp process_event(
         %{
           "type" => "tool_execution_update",
           "toolCallId" => tool_call_id
         } = event,
         state
       ) do
    text = Tools.result_text(event["partialResult"])

    notification =
      AdapterEvents.session_update(state.session_id, %{
        "sessionUpdate" => "tool_call_update",
        "toolCallId" => tool_call_id,
        "status" => "in_progress",
        "content" => Tools.text_content(text),
        "rawOutput" => event["partialResult"]
      })

    {:messages, [compact(notification)], state}
  end

  defp process_event(
         %{
           "type" => "tool_execution_end",
           "toolCallId" => tool_call_id
         } = event,
         state
       ) do
    result = event["result"]
    is_error = event["isError"] == true
    text = Tools.result_text(result)
    {content, state} = tool_result_content(tool_call_id, text, is_error, state)

    notification =
      AdapterEvents.session_update(state.session_id, %{
        "sessionUpdate" => "tool_call_update",
        "toolCallId" => tool_call_id,
        "status" => if(is_error, do: "failed", else: "completed"),
        "content" => content,
        "rawOutput" => result
      })

    state = %{
      state
      | active_tool_executions: Map.delete(state.active_tool_executions, tool_call_id),
        current_tool_calls: Map.delete(state.current_tool_calls, tool_call_id)
    }

    {:messages, [compact(notification)], state}
  end

  defp process_event(%{"type" => "agent_end"} = event, state) do
    text = state.text_acc |> Enum.reverse() |> Enum.join("")
    usage = usage_from_agent_end(event)

    stop_reason =
      if state.pending_prompt && state.pending_prompt.cancel_requested,
        do: "cancelled",
        else: "end_turn"

    acp_id = get_in(state.pending_prompt, [:acp_id])

    response =
      Envelope.response(acp_id, %{
        "stopReason" => stop_reason,
        "usage" => usage,
        "_meta" => %{"ex_mcp" => %{"text" => text, "sessionId" => state.session_id || "default"}}
      })

    state = %{state | pending_prompt: nil, text_acc: []}

    case start_next_queued_prompt(state) do
      {:ok, messages, write_data, state} ->
        {:messages_and_write, [response | messages], write_data, state}

      :empty ->
        {:messages, [response], state}
    end
  end

  defp process_event(%{"type" => type} = event, state)
       when type in [
              "auto_compaction_start",
              "auto_compaction_end",
              "auto_retry_start",
              "auto_retry_end"
            ] do
    notification =
      AdapterEvents.session_update(state.session_id, %{
        "sessionUpdate" => "session_info_update",
        "_meta" => %{"ex_mcp" => %{"pi" => event}}
      })

    {:messages, [notification], state}
  end

  defp process_event(%{"type" => "extension_ui_request"} = event, state) do
    notification =
      AdapterEvents.session_update(state.session_id, %{
        "sessionUpdate" => "session_info_update",
        "_meta" => %{"ex_mcp" => %{"pi" => %{"extensionUiRequest" => event}}}
      })

    {:messages, [notification], state}
  end

  defp process_event(%{"type" => type}, state)
       when type in [
              "agent_start",
              "turn_start",
              "turn_end",
              "message_start",
              "message_end",
              "message_update"
            ] do
    {:skip, state}
  end

  defp process_event(event, state) do
    Logger.debug("[Pi Adapter] Unhandled event: #{inspect(event["type"])}")
    {:skip, state}
  end

  defp handle_control_response(%{group_id: group_id, kind: kind, rpc_id: rpc_id}, event, state) do
    group = Map.fetch!(state.control_groups, group_id)

    cond do
      event["success"] == false and auth_error?(event["error"]) ->
        finish_control_error(group, auth_required_error(group.acp_id, state), state)

      event["success"] == false ->
        finish_control_error(
          group,
          Envelope.error(group.acp_id, -32_603, to_string(event["error"])),
          state
        )

      true ->
        group =
          group
          |> update_in([:responses], &Map.put(&1, kind, event["data"] || %{}))
          |> update_in([:refs], &MapSet.delete(&1, rpc_id))

        state = %{state | control_groups: Map.put(state.control_groups, group_id, group)}
        maybe_finish_control_group(group, state)
    end
  end

  defp maybe_finish_control_group(%{refs: refs} = group, state) do
    if MapSet.size(refs) == 0 do
      finish_control_group(group, state)
    else
      {:skip, state}
    end
  end

  defp finish_control_group(%{type: :session_new} = group, state) do
    state = delete_group(state, group)
    state_data = group.responses[:state] || %{}
    models_data = group.responses[:models] || %{}

    if empty_models?(models_data) do
      {:messages, [auth_required_error(group.acp_id, state)], state}
    else
      session_id = state_data["sessionId"] || "pi-#{System.unique_integer([:positive])}"
      session_file = state_data["sessionFile"]
      cwd = state_data["cwd"] || group.cwd
      models = model_state(models_data, state_data)
      modes = thinking_state(state_data)
      commands = command_state(group.responses[:commands], group.file_commands)

      maybe_store_session(state.session_map_path, session_id, cwd, session_file)

      state = %{
        state
        | session_id: session_id,
          session_file: session_file,
          cwd: cwd,
          thinking_level: modes["currentModeId"],
          current_model_id: get_in(models, ["currentModelId"]),
          available_models: Map.get(models || %{}, "availableModels", []),
          file_commands: group.file_commands,
          available_commands: commands
      }

      response =
        Envelope.response(group.acp_id, %{
          "sessionId" => session_id,
          "models" => models,
          "modes" => modes,
          "_meta" => %{"ex_mcp" => %{"pi" => compact(%{"sessionFile" => session_file})}}
        })

      commands_update = available_commands_update(session_id, commands)
      {:messages, [response, commands_update], state}
    end
  end

  defp finish_control_group(%{type: :session_load} = group, state) do
    state = delete_group(state, group)
    state_data = group.responses[:state] || %{}
    models_data = group.responses[:models] || %{}
    session_id = group.session_id
    cwd = state_data["cwd"] || group.cwd
    session_file = state_data["sessionFile"] || group.session_file
    models = model_state(models_data, state_data)
    modes = thinking_state(state_data)
    commands = command_state(group.responses[:commands], group.file_commands)

    maybe_store_session(state.session_map_path, session_id, cwd, session_file)

    state = %{
      state
      | session_id: session_id,
        session_file: session_file,
        cwd: cwd,
        thinking_level: modes["currentModeId"],
        current_model_id: get_in(models, ["currentModelId"]),
        available_models: Map.get(models || %{}, "availableModels", []),
        file_commands: group.file_commands,
        available_commands: commands
    }

    replay = replay_messages(group.responses[:messages], session_id)

    response =
      Envelope.response(group.acp_id, %{
        "sessionId" => session_id,
        "models" => models,
        "modes" => modes,
        "_meta" => %{"ex_mcp" => %{"pi" => compact(%{"sessionFile" => session_file})}}
      })

    {:messages, replay ++ [response, available_commands_update(session_id, commands)], state}
  end

  defp finish_control_group(%{type: :slash_autocompact_toggle_get} = group, state) do
    data = group.responses[:result] || %{}
    enabled = not truthy?(data["autoCompactionEnabled"])
    {rpc_id, rpc_msg} = rpc("set_auto_compaction", %{"enabled" => enabled})

    group =
      group
      |> Map.put(:type, :slash_autocompact_toggle_set)
      |> Map.put(:enabled, enabled)
      |> Map.put(:refs, MapSet.new([rpc_id]))
      |> Map.put(:responses, %{})

    state =
      state
      |> put_group(group)
      |> put_control(rpc_id, :result, group)

    {:skip_and_write, encode_rpc(rpc_msg), state}
  end

  defp finish_control_group(group, state) do
    state = delete_group(state, group)
    text = slash_result_text(group)

    messages =
      [
        AdapterEvents.session_update(group.session_id || state.session_id, %{
          "sessionUpdate" => "agent_message_chunk",
          "content" => %{"type" => "text", "text" => text}
        })
      ]
      |> maybe_add_name_update(group, state)

    response = Envelope.response(group.acp_id, %{"stopReason" => "end_turn"})
    {:messages, messages ++ [response], state}
  end

  defp finish_control_error(group, error, state) do
    state = delete_group(state, group)
    {:messages, [error], state}
  end

  defp process_untracked_response(%{"command" => "get_state", "data" => data}, state)
       when is_map(data) do
    {:skip, update_session_state_from_pi(data, state)}
  end

  defp process_untracked_response(_event, state), do: {:skip, state}

  defp tool_call_stream_update(tool_event, state) do
    tool_call =
      tool_event["toolCall"] ||
        get_in(tool_event, ["partial", "content", tool_event["contentIndex"] || 0]) ||
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
        |> compact()

      if existing_status do
        {AdapterEvents.session_update(
           state.session_id,
           Map.put(update, "sessionUpdate", "tool_call_update")
         ), state}
      else
        state = %{
          state
          | current_tool_calls: Map.put(state.current_tool_calls, tool_call_id, "pending")
        }

        {AdapterEvents.session_update(
           state.session_id,
           Map.put(update, "sessionUpdate", "tool_call")
         ), state}
      end
    else
      {nil, state}
    end
  end

  defp tool_raw_input(%{"arguments" => args}) when is_map(args), do: args
  defp tool_raw_input(%{"args" => args}) when is_map(args), do: args

  defp tool_raw_input(%{"partialArgs" => partial}) when is_binary(partial) do
    case Jason.decode(partial) do
      {:ok, args} when is_map(args) -> args
      _ -> %{"partialArgs" => partial}
    end
  end

  defp tool_raw_input(_tool_call), do: %{}

  defp maybe_snapshot_edit(tool_call_id, "edit", %{"path" => path} = args, state)
       when is_binary(path) do
    abs =
      if Path.type(path) == :absolute, do: path, else: Path.expand(path, state.cwd || File.cwd!())

    case File.read(abs) do
      {:ok, old_text} ->
        line = Tools.find_unique_line_number(old_text, args["oldText"] || "")
        snapshot = %{path: path, old_text: old_text}
        {line, %{state | edit_snapshots: Map.put(state.edit_snapshots, tool_call_id, snapshot)}}

      _ ->
        {nil, state}
    end
  end

  defp maybe_snapshot_edit(_tool_call_id, _tool_name, _args, state), do: {nil, state}

  defp tool_result_content(tool_call_id, text, is_error, state) do
    snapshot = state.edit_snapshots[tool_call_id]
    state = %{state | edit_snapshots: Map.delete(state.edit_snapshots, tool_call_id)}

    content =
      if !is_error && snapshot do
        abs =
          if Path.type(snapshot.path) == :absolute,
            do: snapshot.path,
            else: Path.expand(snapshot.path, state.cwd || File.cwd!())

        case File.read(abs) do
          {:ok, new_text} when new_text != snapshot.old_text ->
            [
              %{
                "type" => "diff",
                "path" => snapshot.path,
                "oldText" => snapshot.old_text,
                "newText" => new_text
              }
            ] ++ (Tools.text_content(text) || [])

          _ ->
            Tools.text_content(text)
        end
      else
        Tools.text_content(text)
      end

    {content, state}
  end

  defp replay_messages(data, session_id) do
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
            AdapterEvents.session_update(session_id, %{
              "sessionUpdate" => "tool_call",
              "toolCallId" => tool_call_id,
              "title" => tool_name,
              "kind" => Tools.kind(tool_name),
              "status" => "completed",
              "rawOutput" => message
            }),
            AdapterEvents.session_update(session_id, %{
              "sessionUpdate" => "tool_call_update",
              "toolCallId" => tool_call_id,
              "status" => if(message["isError"], do: "failed", else: "completed"),
              "content" => Tools.text_content(text),
              "rawOutput" => message
            })
            |> compact()
          ]

        _ ->
          []
      end
    end)
  end

  defp replay_text_update(_session_id, _type, ""), do: []

  defp replay_text_update(session_id, type, text) do
    [
      AdapterEvents.session_update(session_id, %{
        "sessionUpdate" => type,
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

  defp start_next_queued_prompt(state) do
    case :queue.out(state.prompt_queue) do
      {{:value, queued}, rest} ->
        state = %{state | prompt_queue: rest}

        {:ok, data, state} =
          start_prompt(queued.acp_id, queued.message, queued.images, queued.params, state)

        messages = [
          AdapterEvents.session_update(state.session_id, %{
            "sessionUpdate" => "agent_message_chunk",
            "content" => %{
              "type" => "text",
              "text" => "Starting queued message. (#{:queue.len(rest)} remaining)"
            }
          }),
          AdapterEvents.session_update(state.session_id, %{
            "sessionUpdate" => "session_info_update",
            "_meta" => %{
              "ex_mcp" => %{"pi" => %{"queueDepth" => :queue.len(rest), "running" => true}}
            }
          })
        ]

        {:ok, messages, data, state}

      {:empty, _} ->
        :empty
    end
  end

  defp cancel_queued_prompts(state) do
    {queued, queue} = :queue.to_list(state.prompt_queue) |> then(&{&1, :queue.new()})

    responses =
      Enum.map(queued, fn queued ->
        Envelope.response(queued.acp_id, %{"stopReason" => "cancelled"})
      end)

    {responses, %{state | prompt_queue: queue}}
  end

  defp mark_pending_cancel_requested(%{pending_prompt: nil} = state), do: state

  defp mark_pending_cancel_requested(state) do
    put_in(state.pending_prompt[:cancel_requested], true)
  end

  defp queue_cleared_messages(state, had_queued) do
    if had_queued do
      [
        AdapterEvents.session_update(state.session_id, %{
          "sessionUpdate" => "agent_message_chunk",
          "content" => %{"type" => "text", "text" => "Cleared queued prompts."}
        }),
        AdapterEvents.session_update(state.session_id, %{
          "sessionUpdate" => "session_info_update",
          "_meta" => %{
            "ex_mcp" => %{
              "pi" => %{"queueDepth" => 0, "running" => not is_nil(state.pending_prompt)}
            }
          }
        })
      ]
    else
      []
    end
  end

  defp usage_from_agent_end(event) do
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

  defp slash_result_text(%{type: :slash_compact, responses: %{result: result}}) do
    summary = if is_map(result), do: result["summary"], else: nil
    tokens = if is_map(result), do: result["tokensBefore"], else: nil

    [
      "Compaction completed.",
      if(is_number(tokens), do: "Tokens before: #{tokens}", else: nil),
      summary
    ]
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n")
  end

  defp slash_result_text(%{type: :slash_autocompact_on}), do: "Auto-compaction enabled."
  defp slash_result_text(%{type: :slash_autocompact_off}), do: "Auto-compaction disabled."

  defp slash_result_text(%{type: :slash_autocompact_toggle_set, enabled: enabled}),
    do: "Auto-compaction #{if(enabled, do: "enabled", else: "disabled")}."

  defp slash_result_text(%{type: :slash_export, responses: %{result: result}}) do
    path = if is_map(result), do: result["path"], else: nil

    if is_binary(path) and path != "",
      do: "Session exported: file://#{path}",
      else: "Session export completed."
  end

  defp slash_result_text(%{type: :slash_session, responses: %{result: stats}})
       when is_map(stats) do
    [
      maybe_line("Session", stats["sessionId"]),
      maybe_line("Session file", stats["sessionFile"]),
      maybe_line("Messages", stats["totalMessages"]),
      maybe_line("Cost", stats["cost"])
    ]
    |> Enum.reject(&is_nil/1)
    |> case do
      [] -> "Session stats:\n#{Jason.encode!(stats, pretty: true)}"
      lines -> Enum.join(lines, "\n")
    end
  end

  defp slash_result_text(%{type: :slash_name}), do: "Session name set."

  defp slash_result_text(%{type: :slash_steering_get, responses: %{result: state}}),
    do: "Steering mode: #{state["steeringMode"] || "unknown"}"

  defp slash_result_text(%{type: :slash_steering_set}), do: "Steering mode updated."

  defp slash_result_text(%{type: :slash_follow_up_get, responses: %{result: state}}),
    do: "Follow-up mode: #{state["followUpMode"] || "unknown"}"

  defp slash_result_text(%{type: :slash_follow_up_set}), do: "Follow-up mode updated."
  defp slash_result_text(_group), do: "Command completed."

  defp maybe_add_name_update(
         messages,
         %{type: :slash_name, session_id: session_id, responses: %{result: result}},
         _state
       ) do
    title = if is_map(result), do: result["name"], else: nil

    if is_binary(title) and title != "" do
      [
        AdapterEvents.session_update(session_id, %{
          "sessionUpdate" => "session_info_update",
          "title" => title,
          "updatedAt" => DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601()
        })
        | messages
      ]
    else
      messages
    end
  end

  defp maybe_add_name_update(messages, _group, _state), do: messages

  defp model_state(data, state_data) do
    available =
      data
      |> Map.get("models", [])
      |> Enum.flat_map(fn model ->
        provider = model["provider"] |> to_string_or_nil()
        id = model["id"] |> to_string_or_nil()

        if provider && id do
          name = model["name"] || id

          [
            %{
              "modelId" => "#{provider}/#{id}",
              "name" => "#{provider}/#{name}",
              "description" => nil
            }
          ]
        else
          []
        end
      end)

    current =
      case state_data["model"] do
        %{"provider" => provider, "id" => id} when is_binary(provider) and is_binary(id) ->
          "#{provider}/#{id}"

        _ ->
          get_in(available, [Access.at(0), "modelId"])
      end

    if available == [] and is_nil(current) do
      nil
    else
      %{"availableModels" => available, "currentModelId" => current || "default"}
    end
  end

  defp thinking_state(state_data) do
    current = state_data["thinkingLevel"] || @default_thinking_level
    current = if current in @thinking_levels, do: current, else: @default_thinking_level
    %{"availableModes" => modes(), "currentModeId" => current}
  end

  defp command_state(data, file_commands) do
    pi_commands =
      data
      |> pi_commands()
      |> Enum.reject(&(&1["source"] == "extension"))
      |> Enum.map(fn command ->
        %{"name" => command["name"], "description" => command["description"] || "(command)"}
      end)

    (pi_commands ++ SlashCommands.available_commands(file_commands))
    |> Enum.reduce({MapSet.new(), []}, fn command, {seen, acc} ->
      name = command["name"]

      if is_binary(name) and name != "" and not MapSet.member?(seen, name) do
        {MapSet.put(seen, name), [command | acc]}
      else
        {seen, acc}
      end
    end)
    |> elem(1)
    |> Enum.reverse()
  end

  defp pi_commands(%{"commands" => commands}) when is_list(commands), do: commands
  defp pi_commands(%{"data" => %{"commands" => commands}}) when is_list(commands), do: commands
  defp pi_commands(_data), do: []

  defp available_commands_update(session_id, commands) do
    AdapterEvents.session_update(session_id, %{
      "sessionUpdate" => "available_commands_update",
      "availableCommands" => commands
    })
  end

  defp empty_models?(%{"models" => models}) when is_list(models), do: models == []
  defp empty_models?(_data), do: false

  defp update_session_state_from_pi(data, state) do
    state
    |> maybe_set(:session_id, data["sessionId"])
    |> maybe_set(:session_file, data["sessionFile"])
    |> maybe_set(:thinking_level, data["thinkingLevel"])
  end

  defp resolve_model(model_id, _available_models)
       when not is_binary(model_id) or model_id == "" do
    {:error, "session/set_model requires modelId"}
  end

  defp resolve_model(model_id, available_models) when is_binary(model_id) do
    if String.contains?(model_id, "/") do
      [provider | rest] = String.split(model_id, "/")
      model = Enum.join(rest, "/")
      {:ok, provider, model, model_id}
    else
      case find_available_model(model_id, available_models) do
        {:ok, current_model_id} -> resolve_model(current_model_id, available_models)
        :error -> {:error, "Unknown modelId: #{model_id}"}
      end
    end
  end

  defp find_available_model(model_id, available_models) when is_list(available_models) do
    Enum.find_value(available_models, :error, fn model ->
      advertised_id = model["modelId"]

      cond do
        advertised_id == model_id ->
          {:ok, advertised_id}

        is_binary(advertised_id) and List.last(String.split(advertised_id, "/")) == model_id ->
          {:ok, advertised_id}

        true ->
          false
      end
    end)
  end

  defp find_available_model(_model_id, _available_models), do: :error

  defp require_absolute_cwd(cwd) when is_binary(cwd) do
    if Path.type(cwd) == :absolute,
      do: :ok,
      else: {:error, "cwd must be an absolute path: #{cwd}"}
  end

  defp require_absolute_cwd(_cwd), do: {:error, "cwd is required"}

  defp find_session_file(session_id, state) when is_binary(session_id) do
    case SessionStore.get(state.session_map_path, session_id) do
      %{"sessionFile" => session_file} when is_binary(session_file) ->
        session_file

      _ ->
        SessionStore.find_pi_session_file(session_id, session_dir: state.session_dir)
    end
  end

  defp find_session_file(_session_id, _state), do: nil

  defp maybe_store_session(_map_path, _session_id, _cwd, nil), do: :ok

  defp maybe_store_session(map_path, session_id, cwd, session_file) do
    SessionStore.upsert(map_path, %{
      "sessionId" => session_id,
      "cwd" => cwd,
      "sessionFile" => session_file
    })
  end

  defp put_group(state, group) do
    group_id =
      Map.get(group, :group_id) || "group-#{System.unique_integer([:positive, :monotonic])}"

    group = Map.put(group, :group_id, group_id)
    %{state | control_groups: Map.put(state.control_groups, group_id, group)}
  end

  defp put_control(state, rpc_id, kind, group) do
    group_id = group[:group_id] || latest_group_id(state, group)

    control = %{
      rpc_id: rpc_id,
      kind: kind,
      group_id: group_id,
      inserted_at: System.monotonic_time(:millisecond)
    }

    %{state | pending_controls: Map.put(state.pending_controls, rpc_id, control)}
  end

  defp latest_group_id(state, group) do
    state.control_groups
    |> Enum.find_value(fn {id, existing} ->
      if existing.acp_id == group.acp_id and existing.type == group.type, do: id
    end)
  end

  defp delete_group(state, group) do
    refs = group.refs || MapSet.new()

    pending_controls =
      Enum.reduce(refs, state.pending_controls, fn rpc_id, pending ->
        Map.delete(pending, rpc_id)
      end)

    %{
      state
      | pending_controls: pending_controls,
        control_groups: Map.delete(state.control_groups, group.group_id)
    }
  end

  defp rpc(type, fields \\ %{}) do
    id = "pi-#{System.unique_integer([:positive, :monotonic])}"
    {id, fields |> Map.put("type", type) |> Map.put("id", id)}
  end

  defp encode_many(messages), do: messages |> Enum.map(&encode_rpc/1) |> IO.iodata_to_binary()
  defp encode_rpc(msg), do: Jason.encode!(compact(msg)) <> "\n"

  defp append_opt(args, opts, key, flag) do
    case Keyword.get(opts, key) do
      nil -> args
      value -> args ++ [flag, to_string(value)]
    end
  end

  defp translate_config_option("auto_compaction", value, state) when is_boolean(value) do
    {:ok, encode_rpc(%{"type" => "set_auto_compaction", "enabled" => value}), state}
  end

  defp translate_config_option("auto_compaction", value, state) when value in ["true", "false"],
    do: translate_config_option("auto_compaction", value == "true", state)

  defp translate_config_option("auto_retry", value, state) when is_boolean(value) do
    {:ok, encode_rpc(%{"type" => "set_auto_retry", "enabled" => value}), state}
  end

  defp translate_config_option("auto_retry", value, state) when value in ["true", "false"],
    do: translate_config_option("auto_retry", value == "true", state)

  defp translate_config_option("steering_mode", value, state)
       when value in ["all", "one-at-a-time"] do
    {:ok, encode_rpc(%{"type" => "set_steering_mode", "mode" => value}), state}
  end

  defp translate_config_option("follow_up_mode", value, state)
       when value in ["all", "one-at-a-time"] do
    {:ok, encode_rpc(%{"type" => "set_follow_up_mode", "mode" => value}), state}
  end

  defp translate_config_option(config_id, _value, state),
    do: {:error, "Unknown Pi config option: #{config_id}", state}

  defp filter_sessions_by_cwd(sessions, nil), do: sessions
  defp filter_sessions_by_cwd(sessions, cwd), do: Enum.filter(sessions, &(&1["cwd"] == cwd))

  defp parse_cursor(cursor) when is_binary(cursor) do
    case Integer.parse(cursor) do
      {offset, ""} when offset > 0 -> offset
      _ -> 0
    end
  end

  defp parse_cursor(_cursor), do: 0

  defp maybe_put_present(map, _key, nil), do: map
  defp maybe_put_present(map, key, value), do: Map.put(map, key, value)

  defp maybe_put_non_empty(map, _key, []), do: map
  defp maybe_put_non_empty(map, _key, nil), do: map
  defp maybe_put_non_empty(map, key, value), do: Map.put(map, key, value)

  defp maybe_set(state, _key, nil), do: state
  defp maybe_set(state, key, value), do: Map.put(state, key, value)

  defp compact(map) when is_map(map) do
    map
    |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    |> Map.new()
  end

  defp compact(value), do: value

  defp boolean_options do
    [%{"value" => "true", "name" => "On"}, %{"value" => "false", "name" => "Off"}]
  end

  defp mode_options do
    [
      %{"value" => "all", "name" => "All"},
      %{"value" => "one-at-a-time", "name" => "One at a time"}
    ]
  end

  defp auth_required_error(id, state) do
    Envelope.error(
      id,
      Types.auth_required_code(),
      "Configure an API key or log in with an OAuth provider.",
      %{"authMethods" => auth_methods(state.opts)}
    )
  end

  defp auth_error?(message) do
    text = message |> to_string() |> String.downcase()

    Enum.any?(
      [
        "api key",
        "apikey",
        "missing key",
        "no key",
        "not configured",
        "unauthorized",
        "authentication",
        "permission denied",
        "forbidden",
        "401",
        "403"
      ],
      &String.contains?(text, &1)
    )
  end

  defp truthy?(value), do: value in [true, "true", 1, "1"]
  defp blank_to_nil(""), do: nil
  defp blank_to_nil(value), do: value

  defp slash_args([]), do: ""
  defp slash_args(args), do: " " <> Enum.join(args, " ")

  defp maybe_line(_label, nil), do: nil
  defp maybe_line(label, value), do: "#{label}: #{value}"

  defp to_string_or_nil(nil), do: nil
  defp to_string_or_nil(""), do: nil
  defp to_string_or_nil(value), do: to_string(value)
end
