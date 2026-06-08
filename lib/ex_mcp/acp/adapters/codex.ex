defmodule ExMCP.ACP.Adapters.Codex do
  @moduledoc """
  Adapter for Codex CLI using `codex app-server` persistent mode.

  Translates between ACP JSON-RPC and Codex's app-server JSON-RPC protocol.
  The adapter keeps Codex app-server as the subprocess boundary and owns the
  pure protocol mapping needed to present a stable ACP surface.
  """

  @behaviour ExMCP.ACP.Adapter

  require Logger

  alias ExMCP.ACP.{AdapterEvents, Envelope}

  @default_mode "auto"
  @default_reasoning_effort "medium"

  @mode_profiles %{
    "read-only" => %{permissions: ":read-only", approval: "on-request"},
    "auto" => %{permissions: ":workspace", approval: "on-request"},
    "full-access" => %{permissions: ":danger-no-sandbox", approval: "never"}
  }

  @legacy_mode_aliases %{
    "suggest" => "read-only",
    "auto-edit" => "auto",
    "full-auto" => "full-access"
  }

  @reasoning_efforts [
    {"minimal", "Minimal"},
    {"low", "Low"},
    {"medium", "Medium"},
    {"high", "High"}
  ]

  defstruct [
    :model,
    :mode_id,
    :reasoning_effort,
    next_id: 1,
    phase: :initializing,
    pending_requests: %{},
    pending_client_requests: %{},
    sessions: %{},
    opts: []
  ]

  @impl true
  def init(opts) do
    {:ok,
     %__MODULE__{
       opts: opts,
       model: Keyword.get(opts, :model),
       mode_id: normalize_mode_id(Keyword.get(opts, :mode_id, @default_mode)),
       reasoning_effort: Keyword.get(opts, :reasoning_effort, @default_reasoning_effort)
     }}
  end

  @impl true
  def command(_opts), do: {"codex", ["app-server"]}

  @impl true
  def capabilities do
    %{
      "promptCapabilities" => %{"image" => true, "embeddedContext" => true},
      "mcpCapabilities" => %{
        "http" => true,
        "_meta" => %{"ex_mcp.codex" => %{"stdioMcpServers" => true}}
      },
      "loadSession" => true,
      "auth" => %{"logout" => %{}},
      "sessionCapabilities" => %{
        "list" => %{},
        "resume" => %{},
        "close" => %{}
      }
    }
  end

  @impl true
  def auth_methods(opts) do
    env_methods = [
      env_auth_method("codex-api-key", "Use CODEX_API_KEY", "CODEX_API_KEY"),
      env_auth_method("openai-api-key", "Use OPENAI_API_KEY", "OPENAI_API_KEY")
    ]

    if Keyword.get(opts, :no_browser, false) || System.get_env("NO_BROWSER") do
      env_methods
    else
      [
        %{
          "id" => "chatgpt",
          "name" => "Login with ChatGPT",
          "description" =>
            "Use your ChatGPT login with Codex CLI (requires a paid ChatGPT subscription)"
        }
        | env_methods
      ]
    end
  end

  @impl true
  def modes do
    [
      %{
        "id" => "read-only",
        "name" => "Read Only",
        "description" => "Inspect files and propose changes without writing to the workspace"
      },
      %{
        "id" => "auto",
        "name" => "Auto",
        "description" => "Edit within the workspace and request approval for sensitive actions"
      },
      %{
        "id" => "full-access",
        "name" => "Full Access",
        "description" => "Run without sandbox restrictions"
      }
    ]
  end

  @impl true
  def config_options do
    []
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

  # Outbound: ACP -> Codex app-server

  @impl true
  def translate_outbound(%{"method" => "initialize"}, state), do: {:ok, :skip, state}

  def translate_outbound(%{"method" => "authenticate", "id" => acp_id, "params" => params}, state) do
    method_id = params["methodId"] || params["provider"] || params["id"]

    case auth_request_params(method_id, state) do
      {:ok, codex_params} ->
        {id, state} = next_request_id(state)
        request = encode_request(id, "account/login/start", codex_params)
        state = track_request(state, id, :authenticate, acp_id, %{method_id: method_id})
        {:ok, request, state}

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "authenticate"}, state),
    do: {:error, "authenticate requires methodId", state}

  def translate_outbound(%{"method" => "logout"}, state) do
    {id, state} = next_request_id(state)
    request = encode_request(id, "account/logout", %{})
    state = track_request(state, id, :logout, nil)
    {:reply_and_write, %{}, request, state}
  end

  def translate_outbound(%{"method" => "session/new", "id" => acp_id, "params" => params}, state) do
    mode_id = normalize_mode_id(params["modeId"] || params["approvalPolicy"] || state.mode_id)
    mcp_config = mcp_config(params["mcpServers"], params["cwd"] || Keyword.get(state.opts, :cwd))

    wire_params =
      %{}
      |> maybe_put("model", params["model"] || state.model)
      |> maybe_put("cwd", params["cwd"] || Keyword.get(state.opts, :cwd))
      |> maybe_put("sandbox", params["sandbox"])
      |> maybe_put("config", mcp_config)
      |> merge_mode_wire_params(mode_id)

    {id, state} = next_request_id(state)
    request = encode_request(id, "thread/start", wire_params)
    state = track_request(state, id, :thread_start, acp_id, %{mode_id: mode_id})
    {:ok, request, state}
  end

  def translate_outbound(%{"method" => "session/load", "id" => acp_id, "params" => params}, state) do
    case fetch_session_id(params) do
      {:ok, session_id} ->
        mode_id = normalize_mode_id(params["modeId"] || params["approvalPolicy"] || state.mode_id)

        mcp_config =
          mcp_config(params["mcpServers"], params["cwd"] || Keyword.get(state.opts, :cwd))

        wire_params =
          %{
            "threadId" => session_id,
            "initialTurnsPage" => %{"limit" => 100, "itemsView" => "full"}
          }
          |> maybe_put("model", params["model"] || state.model)
          |> maybe_put("cwd", params["cwd"] || Keyword.get(state.opts, :cwd))
          |> maybe_put("config", mcp_config)
          |> merge_mode_wire_params(mode_id)

        {id, state} = next_request_id(state)
        request = encode_request(id, "thread/resume", wire_params)
        state = track_request(state, id, :thread_resume, acp_id, %{mode_id: mode_id})
        {:ok, request, state}

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  def translate_outbound(
        %{"method" => "session/resume", "id" => acp_id, "params" => params},
        state
      ) do
    case fetch_session_id(params) do
      {:ok, session_id} ->
        wire_params =
          %{"threadId" => session_id, "excludeTurns" => true}
          |> maybe_put("model", params["model"] || state.model)
          |> maybe_put("cwd", params["cwd"] || Keyword.get(state.opts, :cwd))
          |> maybe_put("config", mcp_config(params["mcpServers"], params["cwd"]))

        {id, state} = next_request_id(state)
        request = encode_request(id, "thread/resume", wire_params)
        state = track_request(state, id, :thread_resume, acp_id, %{})
        {:ok, request, state}

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/list", "id" => acp_id, "params" => params}, state) do
    {id, state} = next_request_id(state)

    wire_params =
      %{}
      |> maybe_put("cwd", params["cwd"])
      |> maybe_put("cursor", params["cursor"])
      |> maybe_put("limit", params["limit"])
      |> maybe_put("archived", false)
      |> maybe_put("sortKey", "updatedAt")
      |> maybe_put("sortDirection", "desc")

    request = encode_request(id, "thread/list", wire_params)
    state = track_request(state, id, :session_list, acp_id)
    {:ok, request, state}
  end

  def translate_outbound(%{"method" => "session/close", "params" => params}, state) do
    with {:ok, session_id} <- fetch_session_id(params),
         {:ok, session} <- fetch_session(state, session_id) do
      {id, state} = next_request_id(state)

      close_request = encode_request(id, "thread/unsubscribe", %{"threadId" => session_id})
      state = track_request(state, id, :thread_unsubscribe, nil, %{session_id: session_id})

      {state, data} =
        if session[:turn_id] do
          {interrupt_id, state} = next_request_id(state)

          interrupt_request =
            encode_request(interrupt_id, "turn/interrupt", %{
              "threadId" => session_id,
              "turnId" => session[:turn_id]
            })

          state =
            track_request(state, interrupt_id, :turn_interrupt, nil, %{session_id: session_id})

          {state, [interrupt_request, close_request]}
        else
          {state, close_request}
        end

      state = %{state | sessions: Map.delete(state.sessions, session_id)}
      {:reply_and_write, %{}, data, state}
    else
      {:error, reason} -> {:error, reason, state}
    end
  end

  def translate_outbound(
        %{"method" => "session/prompt", "id" => acp_id, "params" => params},
        state
      ) do
    with {:ok, session_id} <- fetch_session_id(params),
         {:ok, session} <- fetch_session(state, session_id) do
      {id, state} = next_request_id(state)

      wire_params =
        %{
          "threadId" => session_id,
          "input" => extract_input_items(params["prompt"])
        }
        |> maybe_put("model", params["model"] || session[:model] || state.model)
        |> maybe_put("cwd", params["cwd"] || session[:cwd] || Keyword.get(state.opts, :cwd))

      request = encode_request(id, "turn/start", wire_params)

      session =
        session
        |> Map.put(:active_prompt_acp_id, acp_id)
        |> Map.put(:accumulated_text, [])
        |> Map.put(:accumulated_thinking, [])
        |> Map.put(:accumulated_usage, nil)

      state =
        state
        |> put_session(session_id, session)
        |> track_request(id, :turn_start, acp_id, %{session_id: session_id})

      {:ok, request, state}
    else
      {:error, reason} -> {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/cancel", "params" => params}, state) do
    with {:ok, session_id} <- fetch_session_id(params),
         {:ok, session} <- fetch_session(state, session_id),
         {:ok, turn_id} <- fetch_turn_id(params, session) do
      {id, state} = next_request_id(state)

      request =
        encode_request(id, "turn/interrupt", %{
          "threadId" => session_id,
          "turnId" => turn_id
        })

      state = track_request(state, id, :turn_interrupt, nil, %{session_id: session_id})
      {:ok, request, state}
    else
      {:error, reason} -> {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/set_mode", "params" => params}, state) do
    with {:ok, session_id} <- fetch_session_id(params),
         {:ok, session} <- fetch_session(state, session_id),
         {:ok, mode_id} <- normalize_requested_mode(params["modeId"]) do
      {id, state} = next_request_id(state)

      request =
        encode_request(
          id,
          "thread/settings/update",
          %{"threadId" => session_id} |> merge_mode_wire_params(mode_id)
        )

      session = Map.put(session, :mode_id, mode_id)

      messages = [
        session_update(session_id, %{
          "sessionUpdate" => "current_mode_update",
          "currentModeId" => mode_id
        })
      ]

      state =
        state
        |> put_session(session_id, session)
        |> Map.put(:mode_id, mode_id)
        |> track_request(id, :settings_update, nil, %{session_id: session_id})

      {:messages_and_write, messages, request, state}
    else
      {:error, reason} -> {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/set_config_option", "params" => params}, state) do
    with {:ok, session_id} <- fetch_session_id(params),
         {:ok, session} <- fetch_session(state, session_id),
         {:ok, update} <- config_update(params) do
      {id, state} = next_request_id(state)

      request =
        encode_request(
          id,
          "thread/settings/update",
          Map.merge(%{"threadId" => session_id}, update.wire)
        )

      session = Map.merge(session, update.session)
      result = %{"configOptions" => config_options_for_session(session, state)}

      state =
        state
        |> put_session(session_id, session)
        |> track_request(id, :settings_update, nil, %{session_id: session_id})

      {:reply_and_write, result, request, state}
    else
      {:error, reason} -> {:error, reason, state}
    end
  end

  def translate_outbound(%{"id" => response_id} = response, state) do
    case Map.pop(state.pending_client_requests, response_id) do
      {nil, _pending} ->
        {:ok, :skip, state}

      {entry, pending} ->
        state = %{state | pending_client_requests: pending}
        {:ok, encode_response(entry.codex_id, permission_response(entry, response)), state}
    end
  end

  def translate_outbound(_msg, state), do: {:ok, :skip, state}

  # Inbound: Codex app-server -> ACP

  @impl true
  def translate_inbound(line, state) do
    case Jason.decode(line) do
      {:ok, msg} -> handle_inbound_message(msg, state)
      {:error, _} -> {:skip, state}
    end
  end

  defp handle_inbound_message(%{"id" => id, "result" => result}, state) do
    handle_response(state, id, {:ok, result})
  end

  defp handle_inbound_message(%{"id" => id, "error" => error}, state) do
    handle_response(state, id, {:error, error})
  end

  defp handle_inbound_message(%{"id" => id, "method" => method, "params" => params}, state)
       when is_binary(method) do
    handle_server_request(id, method, params || %{}, state)
  end

  defp handle_inbound_message(%{"method" => method, "params" => params}, state)
       when is_binary(method) do
    handle_notification(method, params || %{}, state)
  end

  defp handle_inbound_message(%{"method" => method}, state) when is_binary(method) do
    handle_notification(method, %{}, state)
  end

  defp handle_inbound_message(_msg, state), do: {:skip, state}

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
    {:skip_and_write, encode_notification("initialized"), state}
  end

  defp handle_typed_response(:authenticate, %{acp_id: acp_id}, {:ok, result}, state) do
    response =
      result
      |> auth_response_result()
      |> then(&Envelope.response(acp_id, &1))

    {:messages, [response], state}
  end

  defp handle_typed_response(:authenticate, %{acp_id: acp_id}, {:error, error}, state) do
    {:messages, [error_response(acp_id, error)], state}
  end

  defp handle_typed_response(:logout, _entry, _reply, state), do: {:skip, state}

  defp handle_typed_response(type, %{acp_id: acp_id} = entry, {:ok, result}, state)
       when type in [:thread_start, :thread_resume] do
    thread = result["thread"] || %{}
    session_id = thread_id(thread, result)
    meta = Map.get(entry, :meta, %{})
    mode_id = mode_id_from_result(result) || meta[:mode_id] || state.mode_id || @default_mode

    session =
      session_from_result(session_id, result, state)
      |> Map.put(:mode_id, mode_id)

    state = put_session(state, session_id, session)

    replay_messages =
      if type == :thread_resume do
        replay_thread_history(session_id, result)
      else
        []
      end

    response = Envelope.response(acp_id, session_result(session_id, result, session, state))
    {:messages, replay_messages ++ [response], state}
  end

  defp handle_typed_response(type, %{acp_id: acp_id}, {:error, error}, state)
       when type in [:thread_start, :thread_resume, :session_list] do
    {:messages, [error_response(acp_id, error)], state}
  end

  defp handle_typed_response(:session_list, %{acp_id: acp_id}, {:ok, result}, state) do
    response =
      Envelope.response(acp_id, %{
        "sessions" => Enum.map(result["data"] || [], &session_info/1)
      })
      |> put_optional_result("nextCursor", result["nextCursor"])

    {:messages, [response], state}
  end

  defp handle_typed_response(:turn_start, %{acp_id: acp_id} = entry, {:ok, result}, state) do
    session_id =
      get_in(entry, [:meta, :session_id]) || result["threadId"] || current_session_id(state)

    turn = result["turn"] || %{}
    turn_id = turn["id"] || result["turnId"]

    state =
      update_session(state, session_id, fn session ->
        session
        |> Map.put(:turn_id, turn_id)
        |> Map.put(:active_prompt_acp_id, acp_id)
      end)

    {:skip, state}
  end

  defp handle_typed_response(:turn_start, %{acp_id: acp_id}, {:error, error}, state) do
    {:messages, [error_response(acp_id, error)], state}
  end

  defp handle_typed_response(:turn_interrupt, _entry, _reply, state), do: {:skip, state}
  defp handle_typed_response(:settings_update, _entry, _reply, state), do: {:skip, state}
  defp handle_typed_response(:thread_unsubscribe, _entry, _reply, state), do: {:skip, state}
  defp handle_typed_response(_type, _entry, _reply, state), do: {:skip, state}

  # Notifications

  defp handle_notification("thread/started", params, state) do
    thread = params["thread"] || %{}
    session_id = thread_id(thread, params)
    session = session_from_result(session_id, params, state)
    {:skip, put_session(state, session_id, session)}
  end

  defp handle_notification("thread/settings/updated", %{"threadId" => session_id} = params, state) do
    state =
      update_session(state, session_id, fn session ->
        session
        |> Map.put(:model, params["settings"]["model"] || session[:model])
        |> Map.put(
          :reasoning_effort,
          params["settings"]["reasoningEffort"] || session[:reasoning_effort]
        )
        |> Map.put(:mode_id, mode_id_from_result(params) || session[:mode_id])
      end)

    {:skip, state}
  end

  defp handle_notification("turn/started", params, state) do
    session_id = params["threadId"] || params["sessionId"] || current_session_id(state)
    turn = params["turn"] || %{}
    turn_id = turn["id"] || params["turnId"]

    state =
      update_session(state, session_id, fn session ->
        Map.put(session, :turn_id, turn_id)
      end)

    {:skip, state}
  end

  defp handle_notification("item/agentMessage/delta", params, state) do
    session_id = session_id_from_params(params, state)
    delta = params["delta"] || ""

    state =
      update_session(state, session_id, fn session ->
        Map.update(session, :accumulated_text, [delta], &[delta | &1])
      end)

    {:messages,
     [
       session_update(session_id, %{
         "sessionUpdate" => "agent_message_chunk",
         "content" => %{"type" => "text", "text" => delta}
       })
     ], state}
  end

  defp handle_notification("agent_message/delta", params, state),
    do: handle_notification("item/agentMessage/delta", params, state)

  defp handle_notification("item/reasoning/textDelta", params, state) do
    session_id = session_id_from_params(params, state)
    delta = params["delta"] || params["text"] || ""

    state =
      update_session(state, session_id, fn session ->
        Map.update(session, :accumulated_thinking, [delta], &[delta | &1])
      end)

    {:messages,
     [
       session_update(session_id, %{
         "sessionUpdate" => "agent_thought_chunk",
         "content" => %{"type" => "text", "text" => delta}
       })
     ], state}
  end

  defp handle_notification(
         "item/created",
         %{"item" => %{"type" => "function_call"} = item} = params,
         state
       ) do
    session_id = session_id_from_params(params, state)
    {:messages, [tool_call_started(session_id, item)], state}
  end

  defp handle_notification("item/created", _params, state), do: {:skip, state}

  defp handle_notification("item/completed", params, state) do
    session_id = session_id_from_params(params, state)
    handle_item_completed(session_id, params["item"] || %{}, state)
  end

  defp handle_notification("item/commandExecution/started", params, state) do
    session_id = session_id_from_params(params, state)

    notification =
      session_update(session_id, %{
        "sessionUpdate" => "tool_call",
        "toolCallId" => params["callId"] || params["itemId"],
        "title" => command_title(params["command"]),
        "kind" => "execute",
        "status" => "in_progress",
        "rawInput" => %{"command" => params["command"]}
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/commandExecution/outputDelta", params, state) do
    session_id = session_id_from_params(params, state)
    delta = params["delta"] || ""

    notification =
      session_update(session_id, %{
        "sessionUpdate" => "tool_call_update",
        "toolCallId" => params["callId"] || params["itemId"] || params["item_id"],
        "content" => [tool_text_content(delta)]
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/commandExecution/completed", params, state) do
    session_id = session_id_from_params(params, state)

    notification =
      session_update(session_id, %{
        "sessionUpdate" => "tool_call_update",
        "status" => "completed",
        "toolCallId" => params["callId"] || params["itemId"],
        "rawOutput" => %{
          "exitCode" => params["exitCode"],
          "output" => params["output"]
        },
        "content" => [tool_text_content(params["output"] || "")]
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/patch/created", params, state) do
    session_id = session_id_from_params(params, state)
    patch = params["patch"] || params

    notification =
      session_update(session_id, %{
        "sessionUpdate" => "tool_call",
        "toolCallId" => patch["id"] || params["itemId"],
        "title" => "Edit File",
        "kind" => "edit",
        "rawInput" => %{
          "path" => patch["path"],
          "diff" => patch["diff"]
        },
        "status" => "pending"
      })

    {:messages, [notification], state}
  end

  defp handle_notification("turn/completed", params, state) do
    session_id = session_id_from_params(params, state)
    session = Map.get(state.sessions, session_id, empty_session(session_id, state))
    turn = params["turn"] || %{}

    text =
      session
      |> Map.get(:accumulated_text, [])
      |> Enum.reverse()
      |> IO.iodata_to_binary()

    messages = [
      session_update(session_id, %{
        "sessionUpdate" => "session_info_update",
        "_meta" => %{"ex_mcp" => %{"adapter" => "codex", "status" => "completed"}}
      })
    ]

    messages =
      if session[:active_prompt_acp_id] do
        result =
          %{
            "stopReason" => normalize_stop_reason(turn["status"] || params["status"]),
            "_meta" => %{
              "ex_mcp" => %{
                "text" => text,
                "sessionId" => session_id,
                "turnId" => session[:turn_id]
              }
            }
          }
          |> maybe_put("usage", session[:accumulated_usage])

        [Envelope.response(session[:active_prompt_acp_id], result) | messages]
      else
        messages
      end

    state =
      update_session(state, session_id, fn session ->
        session
        |> Map.put(:accumulated_text, [])
        |> Map.put(:accumulated_thinking, [])
        |> Map.put(:accumulated_usage, nil)
        |> Map.put(:turn_id, nil)
        |> Map.put(:active_prompt_acp_id, nil)
      end)

    {:messages, Enum.reverse(messages), state}
  end

  defp handle_notification("thread/tokenUsage/updated", params, state) do
    session_id = session_id_from_params(params, state)
    token_usage = params["tokenUsage"] || %{}
    total = token_usage["total"] || %{}

    usage_data = %{
      "inputTokens" => total["inputTokens"] || 0,
      "outputTokens" => total["outputTokens"] || 0,
      "cachedInputTokens" => total["cachedInputTokens"] || 0
    }

    state =
      update_session(state, session_id, fn session ->
        Map.put(session, :accumulated_usage, usage_data)
      end)

    {:skip, state}
  end

  defp handle_notification("error", params, state) do
    session_id = session_id_from_params(params, state)
    error = params["error"] || %{}

    notification =
      session_update(session_id, %{
        "sessionUpdate" => "session_info_update",
        "_meta" => %{
          "ex_mcp" => %{
            "adapter" => "codex",
            "error" => %{
              "message" => error["message"] || "Unknown error",
              "code" => error["code"]
            }
          }
        }
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/webSearch/started", params, state) do
    session_id = session_id_from_params(params, state)

    notification =
      session_update(session_id, %{
        "sessionUpdate" => "tool_call",
        "toolCallId" => params["itemId"],
        "title" => "Web Search",
        "kind" => "fetch",
        "status" => "in_progress",
        "rawInput" => %{"query" => params["query"]}
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/webSearch/completed", params, state) do
    session_id = session_id_from_params(params, state)

    notification =
      session_update(session_id, %{
        "sessionUpdate" => "tool_call_update",
        "status" => "completed",
        "toolCallId" => params["itemId"],
        "rawOutput" => params["results"],
        "content" => [tool_text_content(format_web_search_results(params["results"]))]
      })

    {:messages, [notification], state}
  end

  defp handle_notification("thread/plan/updated", params, state) do
    session_id = session_id_from_params(params, state)
    entries = params["entries"] || params["plan"] || []

    {:messages,
     [
       session_update(session_id, %{
         "sessionUpdate" => "plan",
         "entries" => entries
       })
     ], state}
  end

  defp handle_notification("thread/availableCommands/updated", params, state) do
    session_id = session_id_from_params(params, state)
    commands = params["commands"] || params["availableCommands"] || []

    {:messages,
     [
       session_update(session_id, %{
         "sessionUpdate" => "available_commands_update",
         "availableCommands" => commands
       })
     ], state}
  end

  defp handle_notification(method, _params, state) do
    Logger.debug("[Codex Adapter] Unhandled notification: #{method}")
    {:skip, state}
  end

  # Codex app-server requests that need ACP client interaction.

  defp handle_server_request(codex_id, method, params, state)
       when method in [
              "item/commandExecution/requestApproval",
              "item/fileChange/requestApproval",
              "execCommandApproval",
              "applyPatchApproval",
              "item/permissions/requestApproval",
              "mcpServer/elicitation/request"
            ] do
    session_id = session_id_from_params(params, state)
    acp_id = "codex-permission-#{System.unique_integer([:positive])}"

    request =
      Envelope.request(
        "session/request_permission",
        %{
          "sessionId" => session_id,
          "toolCall" => permission_tool_call(method, params),
          "options" => permission_options(method, params),
          "_meta" => %{"ex_mcp" => %{"codex" => %{"method" => method, "params" => params}}}
        },
        acp_id
      )

    state =
      put_in(state.pending_client_requests[acp_id], %{
        codex_id: codex_id,
        method: method,
        params: params,
        session_id: session_id
      })

    {:messages, [request], state}
  end

  defp handle_server_request(codex_id, method, _params, state) do
    Logger.debug("[Codex Adapter] Rejecting unsupported app-server request: #{method}")

    {:skip_and_write,
     encode_error(codex_id, -32_601, "Unsupported app-server request: #{method}"), state}
  end

  # Item completion / replay helpers

  defp handle_item_completed(session_id, %{"type" => "agent_message"} = item, state) do
    text = item["text"] || item["message"] || ""

    notification =
      session_update(session_id, %{
        "sessionUpdate" => "agent_message_chunk",
        "content" => %{"type" => "text", "text" => text},
        "_meta" => %{"ex_mcp" => %{"final" => true}}
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "function_call"} = item, state) do
    notification =
      session_update(session_id, %{
        "sessionUpdate" => "tool_call_update",
        "toolCallId" => item["callId"] || item["id"],
        "status" => "completed",
        "kind" => codex_tool_kind(item["name"]),
        "rawInput" => item["arguments"]
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "function_call_output"} = item, state) do
    notification =
      session_update(session_id, %{
        "sessionUpdate" => "tool_call_update",
        "toolCallId" => item["callId"] || item["id"],
        "status" => if(item["isError"], do: "failed", else: "completed"),
        "content" => [tool_text_content(item["output"] || item["text"] || "")],
        "rawOutput" => item["output"] || item["text"] || ""
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "patch"} = item, state) do
    notification =
      session_update(session_id, %{
        "sessionUpdate" => "tool_call_update",
        "toolCallId" => item["callId"] || item["id"],
        "kind" => "edit",
        "status" => "completed",
        "content" => [tool_diff_content(item["path"], item["diff"] || item["text"] || "")]
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(_session_id, _item, state), do: {:skip, state}

  defp replay_thread_history(session_id, result) do
    turns =
      get_in(result, ["initialTurnsPage", "data"]) ||
        get_in(result, ["thread", "turns"]) ||
        []

    Enum.flat_map(turns, fn turn ->
      turn
      |> Map.get("items", [])
      |> Enum.flat_map(&replay_item(session_id, &1))
    end)
  end

  defp replay_item(session_id, %{"type" => "agent_message"} = item) do
    [
      session_update(session_id, %{
        "sessionUpdate" => "agent_message_chunk",
        "content" => %{"type" => "text", "text" => item["text"] || item["message"] || ""},
        "_meta" => %{"ex_mcp" => %{"replay" => true}}
      })
    ]
  end

  defp replay_item(session_id, %{"type" => "reasoning"} = item) do
    [
      session_update(session_id, %{
        "sessionUpdate" => "agent_thought_chunk",
        "content" => %{"type" => "text", "text" => item["text"] || item["summary"] || ""},
        "_meta" => %{"ex_mcp" => %{"replay" => true}}
      })
    ]
  end

  defp replay_item(_session_id, _item), do: []

  # State helpers

  defp empty_session(session_id, state) do
    %{
      id: session_id,
      model: state.model,
      mode_id: state.mode_id || @default_mode,
      reasoning_effort: state.reasoning_effort || @default_reasoning_effort,
      accumulated_text: [],
      accumulated_thinking: [],
      accumulated_usage: nil
    }
  end

  defp session_from_result(session_id, result, state) do
    thread = result["thread"] || %{}

    empty_session(session_id, state)
    |> Map.merge(%{
      id: session_id,
      thread: thread,
      cwd: result["cwd"] || thread["cwd"],
      model: result["model"] || state.model,
      reasoning_effort:
        result["reasoningEffort"] || state.reasoning_effort || @default_reasoning_effort
    })
  end

  defp fetch_session_id(%{"sessionId" => session_id})
       when is_binary(session_id) and session_id != "",
       do: {:ok, session_id}

  defp fetch_session_id(_params), do: {:error, "sessionId is required"}

  defp fetch_session(state, session_id) do
    case Map.fetch(state.sessions, session_id) do
      {:ok, session} -> {:ok, session}
      :error -> {:error, "Unknown Codex session: #{session_id}"}
    end
  end

  defp fetch_turn_id(%{"turnId" => turn_id}, _session) when is_binary(turn_id) and turn_id != "",
    do: {:ok, turn_id}

  defp fetch_turn_id(_params, %{turn_id: turn_id}) when is_binary(turn_id) and turn_id != "",
    do: {:ok, turn_id}

  defp fetch_turn_id(_params, _session), do: {:error, "No active Codex turn for session"}

  defp put_session(state, nil, _session), do: state
  defp put_session(state, "", _session), do: state

  defp put_session(state, session_id, session),
    do: %{state | sessions: Map.put(state.sessions, session_id, session)}

  defp update_session(state, nil, _fun), do: state
  defp update_session(state, "", _fun), do: state

  defp update_session(state, session_id, fun) do
    session = Map.get(state.sessions, session_id, empty_session(session_id, state))
    put_session(state, session_id, fun.(session))
  end

  defp current_session_id(%{sessions: sessions}) when map_size(sessions) == 1 do
    sessions |> Map.keys() |> hd()
  end

  defp current_session_id(_state), do: nil

  defp session_id_from_params(params, state) do
    params["threadId"] || params["sessionId"] || get_in(params, ["turn", "threadId"]) ||
      current_session_id(state)
  end

  # Result builders

  defp session_result(session_id, result, session, state) do
    %{
      "sessionId" => session_id,
      "modes" => %{
        "availableModes" => modes(),
        "currentModeId" => session[:mode_id] || state.mode_id || @default_mode
      },
      "configOptions" => config_options_for_session(session, state),
      "_meta" => %{"ex_mcp" => %{"codex" => %{"thread" => result["thread"] || %{}}}}
    }
  end

  defp auth_response_result(%{"authUrl" => _} = result) do
    %{"_meta" => %{"ex_mcp" => %{"codex" => %{"auth" => result}}}}
  end

  defp auth_response_result(%{"verificationUrl" => _} = result) do
    %{"_meta" => %{"ex_mcp" => %{"codex" => %{"auth" => result}}}}
  end

  defp auth_response_result(_result), do: %{}

  defp put_optional_result(%{"result" => _result} = response, _key, nil), do: response

  defp put_optional_result(%{"result" => result} = response, key, value),
    do: %{response | "result" => Map.put(result, key, value)}

  defp session_info(thread) do
    %{
      "sessionId" => thread["id"] || thread["sessionId"],
      "cwd" => thread["cwd"] || "",
      "title" => thread["name"] || thread["preview"],
      "updatedAt" => timestamp_to_iso8601(thread["updatedAt"])
    }
    |> reject_nil_values()
  end

  defp timestamp_to_iso8601(nil), do: nil

  defp timestamp_to_iso8601(value) when is_integer(value) do
    case DateTime.from_unix(value) do
      {:ok, dt} -> DateTime.to_iso8601(dt)
      {:error, _} -> nil
    end
  end

  defp timestamp_to_iso8601(value) when is_binary(value), do: value
  defp timestamp_to_iso8601(_value), do: nil

  defp config_options_for_session(session, state) do
    model = session[:model] || state.model
    effort = session[:reasoning_effort] || state.reasoning_effort || @default_reasoning_effort

    []
    |> maybe_add_model_option(model)
    |> Kernel.++([reasoning_effort_option(effort)])
  end

  defp maybe_add_model_option(options, nil), do: options
  defp maybe_add_model_option(options, ""), do: options

  defp maybe_add_model_option(options, model) do
    options ++
      [
        %{
          "id" => "model",
          "name" => "Model",
          "type" => "select",
          "category" => "model",
          "currentValue" => model,
          "options" => [%{"value" => model, "name" => model}]
        }
      ]
  end

  defp reasoning_effort_option(current) do
    %{
      "id" => "reasoning_effort",
      "name" => "Reasoning Effort",
      "type" => "select",
      "category" => "thought_level",
      "currentValue" => current || @default_reasoning_effort,
      "options" =>
        Enum.map(@reasoning_efforts, fn {value, name} ->
          %{"value" => value, "name" => name}
        end)
    }
  end

  defp config_update(%{"configId" => "model", "value" => value}) when is_binary(value) do
    {:ok, %{wire: %{"model" => value}, session: %{model: value}}}
  end

  defp config_update(%{"configId" => "reasoning_effort", "value" => value})
       when is_binary(value) do
    if value in Enum.map(@reasoning_efforts, &elem(&1, 0)) do
      {:ok, %{wire: %{"effort" => value}, session: %{reasoning_effort: value}}}
    else
      {:error, "Unsupported reasoning_effort: #{value}"}
    end
  end

  defp config_update(%{"configId" => id}), do: {:error, "Unsupported Codex config option: #{id}"}
  defp config_update(_params), do: {:error, "configId and value are required"}

  # Auth helpers

  defp env_auth_method(id, name, var) do
    %{
      "id" => id,
      "name" => name,
      "type" => "env_var",
      "description" => "Uses #{var} supplied explicitly in the adapter environment.",
      "vars" => [%{"name" => var, "label" => var, "secret" => true}]
    }
  end

  defp auth_request_params("chatgpt", _state), do: {:ok, %{"type" => "chatgpt"}}

  defp auth_request_params("codex-api-key", state) do
    explicit_api_key(state, "CODEX_API_KEY")
  end

  defp auth_request_params("openai-api-key", state) do
    explicit_api_key(state, "OPENAI_API_KEY")
  end

  defp auth_request_params(nil, _state), do: {:error, "authenticate requires methodId"}

  defp auth_request_params(method_id, _state),
    do: {:error, "Unsupported Codex auth method: #{method_id}"}

  defp explicit_api_key(state, name) do
    case explicit_env_value(state.opts, name) do
      nil ->
        {:error, "#{name} must be supplied explicitly in adapter_opts[:env] before authenticate"}

      value ->
        {:ok, %{"type" => "apiKey", "apiKey" => value}}
    end
  end

  defp explicit_env_value(opts, name) do
    opts
    |> Keyword.get(:env, [])
    |> normalize_env_map()
    |> Map.get(name)
  end

  defp normalize_env_map(env) when is_map(env) do
    Map.new(env, fn {key, value} -> {to_string(key), to_string(value)} end)
  end

  defp normalize_env_map(env) when is_list(env) do
    Map.new(env, fn {key, value} -> {to_string(key), to_string(value)} end)
  end

  defp normalize_env_map(_env), do: %{}

  # MCP mapping

  defp mcp_config(nil, _cwd), do: nil
  defp mcp_config([], _cwd), do: nil

  defp mcp_config(servers, cwd) when is_list(servers) do
    servers
    |> Enum.reduce(%{}, fn server, acc ->
      case mcp_server_config(server, cwd) do
        nil -> acc
        {name, config} -> Map.put(acc, name, config)
      end
    end)
    |> case do
      config when map_size(config) == 0 -> nil
      config -> %{"mcp_servers" => config}
    end
  end

  defp mcp_config(_servers, _cwd), do: nil

  defp mcp_server_config(%{"type" => "http"} = server, _cwd) do
    name = sanitize_mcp_server_name(server["name"])

    {name,
     %{}
     |> Map.put("url", server["url"])
     |> maybe_put("headers", headers_to_map(server["headers"]))}
  end

  defp mcp_server_config(%{"type" => "stdio"} = server, cwd) do
    name = sanitize_mcp_server_name(server["name"])

    {name,
     %{}
     |> Map.put("command", server["command"])
     |> maybe_put("args", server["args"] || [])
     |> maybe_put("env", env_to_map(server["env"]))
     |> maybe_put("cwd", cwd)}
  end

  defp mcp_server_config(%{"type" => "sse"}, _cwd), do: nil
  defp mcp_server_config(_server, _cwd), do: nil

  defp sanitize_mcp_server_name(nil), do: "mcp_server"

  defp sanitize_mcp_server_name(name) do
    name
    |> to_string()
    |> String.trim()
    |> String.replace(~r/\s+/, "_")
    |> case do
      "" -> "mcp_server"
      sanitized -> sanitized
    end
  end

  defp headers_to_map(headers), do: name_value_list_to_map(headers)
  defp env_to_map(env), do: name_value_list_to_map(env)

  defp name_value_list_to_map(values) when is_list(values) do
    Map.new(values, fn
      %{"name" => name, "value" => value} -> {name, value}
      {name, value} -> {to_string(name), to_string(value)}
    end)
  end

  defp name_value_list_to_map(_values), do: nil

  # Mode mapping

  defp normalize_requested_mode(mode_id) do
    mode_id = normalize_mode_id(mode_id)

    if Map.has_key?(@mode_profiles, mode_id) do
      {:ok, mode_id}
    else
      {:error, "Unsupported Codex mode: #{inspect(mode_id)}"}
    end
  end

  defp normalize_mode_id(nil), do: @default_mode

  defp normalize_mode_id(mode_id) do
    mode_id = to_string(mode_id)
    Map.get(@legacy_mode_aliases, mode_id, mode_id)
  end

  defp merge_mode_wire_params(map, nil), do: map

  defp merge_mode_wire_params(map, mode_id) do
    case Map.get(@mode_profiles, mode_id) do
      nil ->
        map

      %{permissions: permissions, approval: approval} ->
        map
        |> Map.put("permissions", permissions)
        |> Map.put("approvalPolicy", approval)
    end
  end

  defp mode_id_from_result(result) do
    active_profile =
      get_in(result, ["activePermissionProfile", "id"]) ||
        get_in(result, ["settings", "activePermissionProfile", "id"])

    case active_profile do
      ":read-only" -> "read-only"
      ":workspace" -> "auto"
      ":danger-no-sandbox" -> "full-access"
      _ -> nil
    end
  end

  # Permission mapping

  defp permission_tool_call(method, params) do
    %{
      "toolCallId" => params["itemId"] || params["callId"] || params["approvalId"] || method,
      "toolName" => permission_tool_name(method, params),
      "kind" => permission_tool_kind(method),
      "title" => permission_title(method, params),
      "status" => "pending",
      "rawInput" => params
    }
  end

  defp permission_tool_name("item/commandExecution/requestApproval", _params), do: "execute"
  defp permission_tool_name("execCommandApproval", _params), do: "execute"
  defp permission_tool_name("item/fileChange/requestApproval", _params), do: "edit"
  defp permission_tool_name("applyPatchApproval", _params), do: "edit"
  defp permission_tool_name("item/permissions/requestApproval", _params), do: "permissions"

  defp permission_tool_name("mcpServer/elicitation/request", params),
    do: "mcp:#{params["serverName"]}"

  defp permission_tool_name(_method, _params), do: "codex"

  defp permission_tool_kind(method)
       when method in ["item/commandExecution/requestApproval", "execCommandApproval"],
       do: "execute"

  defp permission_tool_kind(method)
       when method in ["item/fileChange/requestApproval", "applyPatchApproval"], do: "edit"

  defp permission_tool_kind("mcpServer/elicitation/request"), do: "other"
  defp permission_tool_kind(_method), do: "other"

  defp permission_title(method, params)
       when method in ["item/commandExecution/requestApproval", "execCommandApproval"] do
    command_title(params["command"])
  end

  defp permission_title(method, _params)
       when method in ["item/fileChange/requestApproval", "applyPatchApproval"],
       do: "Approve File Changes"

  defp permission_title("item/permissions/requestApproval", _params), do: "Approve Permissions"

  defp permission_title("mcpServer/elicitation/request", params),
    do: params["message"] || "MCP Elicitation"

  defp permission_title(_method, _params), do: "Codex Permission"

  defp permission_options(_method, %{"availableDecisions" => decisions})
       when is_list(decisions) do
    Enum.map(decisions, &decision_to_option/1)
  end

  defp permission_options(_method, _params) do
    [
      %{"optionId" => "allow_once", "name" => "Allow Once", "kind" => "allow_once"},
      %{"optionId" => "allow_always", "name" => "Allow for Session", "kind" => "allow_always"},
      %{"optionId" => "reject_once", "name" => "Reject", "kind" => "reject_once"}
    ]
  end

  defp decision_to_option(%{"id" => id, "name" => name}) do
    %{"optionId" => id, "name" => name, "kind" => option_kind(id)}
  end

  defp decision_to_option(decision) when is_binary(decision) do
    %{
      "optionId" => decision,
      "name" => humanize_option(decision),
      "kind" => option_kind(decision)
    }
  end

  defp decision_to_option(decision), do: decision_to_option(to_string(decision))

  defp permission_response(%{method: method}, %{
         "result" => %{"outcome" => %{"outcome" => "cancelled"}}
       }) do
    codex_cancel_response(method)
  end

  defp permission_response(%{method: method}, %{
         "result" => %{"outcome" => %{"optionId" => option_id}}
       }) do
    codex_decision_response(method, option_id)
  end

  defp permission_response(%{method: method}, %{"error" => _error}) do
    codex_cancel_response(method)
  end

  defp permission_response(%{method: method}, _response), do: codex_cancel_response(method)

  defp codex_decision_response(method, option_id)
       when method in ["execCommandApproval", "applyPatchApproval"] do
    %{"decision" => legacy_review_decision(option_id)}
  end

  defp codex_decision_response("item/permissions/requestApproval", option_id) do
    if allow_option?(option_id) do
      %{
        "permissions" => %{},
        "scope" => if(always_option?(option_id), do: "session", else: "turn")
      }
    else
      %{"permissions" => %{}, "scope" => "turn"}
    end
  end

  defp codex_decision_response("mcpServer/elicitation/request", option_id) do
    if allow_option?(option_id), do: %{"action" => "accept"}, else: %{"action" => "decline"}
  end

  defp codex_decision_response(_method, option_id) do
    %{"decision" => app_server_decision(option_id)}
  end

  defp codex_cancel_response(method) when method in ["execCommandApproval", "applyPatchApproval"],
    do: %{"decision" => "abort"}

  defp codex_cancel_response("mcpServer/elicitation/request"), do: %{"action" => "cancel"}

  defp codex_cancel_response("item/permissions/requestApproval"),
    do: %{"permissions" => %{}, "scope" => "turn"}

  defp codex_cancel_response(_method), do: %{"decision" => "cancel"}

  defp app_server_decision(option_id) do
    cond do
      always_option?(option_id) -> "acceptForSession"
      allow_option?(option_id) -> "accept"
      String.contains?(to_string(option_id), "cancel") -> "cancel"
      true -> "decline"
    end
  end

  defp legacy_review_decision(option_id) do
    cond do
      always_option?(option_id) -> "approved_for_session"
      allow_option?(option_id) -> "approved"
      String.contains?(to_string(option_id), "cancel") -> "abort"
      true -> "denied"
    end
  end

  defp option_kind(option_id) do
    cond do
      always_option?(option_id) -> "allow_always"
      allow_option?(option_id) -> "allow_once"
      String.contains?(to_string(option_id), "always") -> "reject_always"
      true -> "reject_once"
    end
  end

  defp allow_option?(option_id) do
    option_id = to_string(option_id)

    String.contains?(option_id, "allow") || String.contains?(option_id, "accept") ||
      String.contains?(option_id, "approved")
  end

  defp always_option?(option_id) do
    option_id = to_string(option_id)
    String.contains?(option_id, "always") || String.contains?(option_id, "session")
  end

  defp humanize_option(option_id) do
    option_id
    |> to_string()
    |> String.replace("_", " ")
    |> String.replace("-", " ")
    |> String.split()
    |> Enum.map_join(" ", &String.capitalize/1)
  end

  # Prompt mapping

  defp extract_input_items(nil), do: [%{"type" => "text", "text" => ""}]

  defp extract_input_items(prompt) when is_binary(prompt),
    do: [%{"type" => "text", "text" => prompt}]

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

        %{"type" => "resource_link"} = resource ->
          [%{"type" => "text", "text" => format_uri_as_link(resource["name"], resource["uri"])}]

        %{"type" => "resource", "resource" => %{"text" => text, "uri" => uri}} ->
          [
            %{
              "type" => "text",
              "text" =>
                "#{format_uri_as_link(nil, uri)}\n<context ref=\"#{uri}\">\n#{text}\n</context>"
            }
          ]

        _ ->
          []
      end)

    if items == [], do: [%{"type" => "text", "text" => ""}], else: items
  end

  defp extract_input_items(_), do: [%{"type" => "text", "text" => ""}]

  defp format_uri_as_link(name, uri) when is_binary(name) and name != "", do: "[@#{name}](#{uri})"

  defp format_uri_as_link(_name, "file://" <> path = uri) do
    name = path |> String.split("/") |> List.last()
    "[@#{name}](#{uri})"
  end

  defp format_uri_as_link(_name, uri) when is_binary(uri), do: uri
  defp format_uri_as_link(_name, nil), do: ""

  # General helpers

  defp next_request_id(%{next_id: id} = state), do: {id, %{state | next_id: id + 1}}

  defp track_request(state, id, type, acp_id, meta \\ %{}) do
    entry = %{type: type, acp_id: acp_id, meta: meta}
    %{state | pending_requests: Map.put(state.pending_requests, id, entry)}
  end

  defp thread_id(thread, result) do
    thread["id"] || thread["sessionId"] || result["threadId"] || result["sessionId"] || ""
  end

  defp tool_call_started(session_id, item) do
    session_update(session_id, %{
      "sessionUpdate" => "tool_call",
      "toolCallId" => item["callId"] || item["id"],
      "title" => item["name"],
      "kind" => codex_tool_kind(item["name"]),
      "rawInput" => item["arguments"],
      "status" => "pending"
    })
  end

  defp tool_text_content(text) do
    %{
      "type" => "content",
      "content" => %{"type" => "text", "text" => to_string(text || "")}
    }
  end

  defp tool_diff_content(path, new_text) do
    %{
      "type" => "diff",
      "path" => path || "",
      "oldText" => nil,
      "newText" => to_string(new_text || "")
    }
  end

  defp command_title(command) when is_binary(command) and command != "", do: command
  defp command_title(_), do: "Run Command"

  defp codex_tool_kind(name) when is_binary(name) do
    name = String.downcase(name)

    cond do
      String.contains?(name, ["read", "view", "open"]) -> "read"
      String.contains?(name, ["write", "edit", "patch", "update"]) -> "edit"
      String.contains?(name, ["delete", "remove"]) -> "delete"
      String.contains?(name, ["move", "rename"]) -> "move"
      String.contains?(name, ["search", "grep", "find"]) -> "search"
      String.contains?(name, ["exec", "command", "bash", "shell"]) -> "execute"
      String.contains?(name, ["think", "reason"]) -> "think"
      String.contains?(name, ["fetch", "web"]) -> "fetch"
      true -> "other"
    end
  end

  defp codex_tool_kind(_), do: "other"

  defp format_web_search_results(results) when is_binary(results), do: results
  defp format_web_search_results(nil), do: ""
  defp format_web_search_results(results), do: Jason.encode!(results)

  defp session_update(session_id, update), do: AdapterEvents.session_update(session_id, update)

  defp error_response(acp_id, error), do: Envelope.error(acp_id, normalize_error(error))

  defp normalize_error(%{"message" => msg} = error),
    do: %{"code" => error["code"] || -1, "message" => msg}

  defp normalize_error(error) when is_binary(error), do: %{"code" => -1, "message" => error}
  defp normalize_error(error), do: %{"code" => -1, "message" => inspect(error)}

  defp normalize_stop_reason(nil), do: "end_turn"
  defp normalize_stop_reason("completed"), do: "end_turn"
  defp normalize_stop_reason("cancelled"), do: "cancelled"
  defp normalize_stop_reason("interrupted"), do: "cancelled"
  defp normalize_stop_reason("errored"), do: "refusal"

  defp normalize_stop_reason(other)
       when other in ["end_turn", "max_tokens", "max_turn_requests", "refusal", "cancelled"],
       do: other

  defp normalize_stop_reason(_other), do: "end_turn"

  defp encode_request(id, method, params) do
    msg = %{"id" => id, "method" => method, "params" => params || %{}}
    [Jason.encode!(msg), "\n"]
  end

  defp encode_response(id, result), do: [Jason.encode!(%{"id" => id, "result" => result}), "\n"]

  defp encode_error(id, code, message) do
    [Jason.encode!(%{"id" => id, "error" => %{"code" => code, "message" => message}}), "\n"]
  end

  defp encode_notification(method, params \\ nil) do
    msg = %{"method" => method} |> maybe_put("params", params)
    [Jason.encode!(msg), "\n"]
  end

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, _key, ""), do: map
  defp maybe_put(map, _key, value) when value == %{}, do: map
  defp maybe_put(map, _key, value) when value == [], do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp reject_nil_values(map) do
    Map.reject(map, fn {_key, value} -> is_nil(value) end)
  end
end
