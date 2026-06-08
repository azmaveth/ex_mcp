defmodule ExMCP.ACP.Adapters.Codex do
  @moduledoc """
  Adapter for Codex CLI using `codex app-server` persistent mode.

  Translates between ACP JSON-RPC and Codex's app-server JSON-RPC protocol.
  The adapter keeps Codex app-server as the subprocess boundary and owns the
  pure protocol mapping needed to present a stable ACP surface.
  """

  @behaviour ExMCP.ACP.Adapter

  require Logger

  alias ExMCP.ACP.Adapters.Codex.{Config, Events, Sessions, SlashCommands}
  alias ExMCP.ACP.{AdapterEvents, Envelope, PendingRequests}
  alias ExMCP.Internal.{Maps, NameValue}

  defstruct [
    :model,
    :mode_id,
    :reasoning_effort,
    models: [],
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
       mode_id: Config.normalize_mode_id(Keyword.get(opts, :mode_id, Config.default_mode())),
       reasoning_effort: Keyword.get(opts, :reasoning_effort, Config.default_reasoning_effort())
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
        "close" => %{},
        "setModel" => %{}
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
  def modes, do: Config.modes()

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
    mode_id =
      Config.normalize_mode_id(params["modeId"] || params["approvalPolicy"] || state.mode_id)

    mcp_config = mcp_config(params["mcpServers"], params["cwd"] || Keyword.get(state.opts, :cwd))

    wire_params =
      %{}
      |> maybe_put("model", params["model"] || state.model)
      |> maybe_put("cwd", params["cwd"] || Keyword.get(state.opts, :cwd))
      |> maybe_put("sandbox", params["sandbox"])
      |> maybe_put("config", mcp_config)
      |> Config.merge_mode_wire_params(mode_id)

    {id, state} = next_request_id(state)
    request = encode_request(id, "thread/start", wire_params)
    state = track_request(state, id, :thread_start, acp_id, %{mode_id: mode_id})
    {:ok, request, state}
  end

  def translate_outbound(%{"method" => "session/load", "id" => acp_id, "params" => params}, state) do
    case Sessions.fetch_id(params) do
      {:ok, session_id} ->
        mode_id =
          Config.normalize_mode_id(params["modeId"] || params["approvalPolicy"] || state.mode_id)

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
          |> Config.merge_mode_wire_params(mode_id)

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
    case Sessions.fetch_id(params) do
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
    with {:ok, session_id} <- Sessions.fetch_id(params),
         {:ok, session} <- Sessions.fetch(state, session_id) do
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
    with {:ok, session_id} <- Sessions.fetch_id(params),
         {:ok, session} <- Sessions.fetch(state, session_id) do
      input_items = extract_input_items(params["prompt"])

      case SlashCommands.parse(input_items) do
        {:ok, command} ->
          translate_slash_command(command, acp_id, session_id, session, params, state)

        :error ->
          translate_user_prompt(input_items, acp_id, session_id, session, params, state)
      end
    else
      {:error, reason} -> {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/cancel", "params" => params}, state) do
    with {:ok, session_id} <- Sessions.fetch_id(params),
         {:ok, session} <- Sessions.fetch(state, session_id),
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
    with {:ok, session_id} <- Sessions.fetch_id(params),
         {:ok, session} <- Sessions.fetch(state, session_id),
         {:ok, mode_id} <- Config.normalize_requested_mode(params["modeId"]) do
      {id, state} = next_request_id(state)

      request =
        encode_request(
          id,
          "thread/settings/update",
          %{"threadId" => session_id} |> Config.merge_mode_wire_params(mode_id)
        )

      session = Map.put(session, :mode_id, mode_id)

      messages = [AdapterEvents.current_mode_update(session_id, mode_id)]

      state =
        state
        |> Sessions.put(session_id, session)
        |> Map.put(:mode_id, mode_id)
        |> track_request(id, :settings_update, nil, %{session_id: session_id})

      {:messages_and_write, messages, request, state}
    else
      {:error, reason} -> {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/set_model", "params" => params}, state) do
    with {:ok, session_id} <- Sessions.fetch_id(params),
         {:ok, session} <- Sessions.fetch(state, session_id),
         {:ok, selection} <- model_selection(params["modelId"], session, state) do
      {id, state} = next_request_id(state)

      request =
        encode_request(
          id,
          "thread/settings/update",
          %{"threadId" => session_id}
          |> Map.put("model", selection.model)
          |> maybe_put("effort", selection.effort)
        )

      session = Map.merge(session, selection.session)
      result = session_config_result(session, state)

      state =
        state
        |> Sessions.put(session_id, session)
        |> Map.put(:model, selection.model)
        |> Map.put(:reasoning_effort, selection.effort || state.reasoning_effort)
        |> track_request(id, :settings_update, nil, %{session_id: session_id})

      {:reply_and_write, result, request, state}
    else
      {:error, reason} -> {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/set_config_option", "params" => params}, state) do
    with {:ok, session_id} <- Sessions.fetch_id(params),
         {:ok, session} <- Sessions.fetch(state, session_id),
         {:ok, update} <- config_update(params, session, state) do
      {id, state} = next_request_id(state)

      request =
        encode_request(
          id,
          "thread/settings/update",
          Map.merge(%{"threadId" => session_id}, update.wire)
        )

      session = Map.merge(session, update.session)
      result = session_config_result(session, state)

      state =
        state
        |> Sessions.put(session_id, session)
        |> Map.merge(update.state)
        |> track_request(id, :settings_update, nil, %{session_id: session_id})

      {:reply_and_write, result, request, state}
    else
      {:error, reason} -> {:error, reason, state}
    end
  end

  def translate_outbound(%{"id" => response_id} = response, state) do
    case PendingRequests.pop(state.pending_client_requests, response_id) do
      {nil, _pending} ->
        {:ok, :skip, state}

      {entry, pending} ->
        state = %{state | pending_client_requests: pending}
        {:ok, encode_response(entry.codex_id, permission_response(entry, response)), state}
    end
  end

  def translate_outbound(_msg, state), do: {:ok, :skip, state}

  defp translate_user_prompt(input_items, acp_id, session_id, session, params, state) do
    {id, state} = next_request_id(state)

    wire_params =
      %{
        "threadId" => session_id,
        "input" => input_items
      }
      |> maybe_put("model", params["model"] || session[:model] || state.model)
      |> maybe_put("cwd", params["cwd"] || session[:cwd] || Keyword.get(state.opts, :cwd))

    request = encode_request(id, "turn/start", wire_params)

    state =
      state
      |> Sessions.put(session_id, reset_prompt_accumulators(session, acp_id))
      |> track_request(id, :turn_start, acp_id, %{session_id: session_id})

    {:ok, request, state}
  end

  defp translate_slash_command({:compact, _rest}, acp_id, session_id, session, _params, state) do
    track_prompt_command(acp_id, session_id, session, state, "thread/compact/start", %{
      "threadId" => session_id
    })
  end

  defp translate_slash_command({:init, _rest}, acp_id, session_id, session, params, state) do
    input_items = SlashCommands.init_input_items()
    translate_user_prompt(input_items, acp_id, session_id, session, params, state)
  end

  defp translate_slash_command({:review, rest}, acp_id, session_id, session, _params, state) do
    target =
      case String.trim(rest) do
        "" -> %{"type" => "uncommittedChanges"}
        instructions -> %{"type" => "custom", "instructions" => instructions}
      end

    review_command(acp_id, session_id, session, state, target)
  end

  defp translate_slash_command(
         {:"review-branch", rest},
         acp_id,
         session_id,
         session,
         _params,
         state
       ) do
    case String.trim(rest) do
      "" ->
        translate_user_prompt(
          [%{"type" => "text", "text" => "/review-branch #{rest}"}],
          acp_id,
          session_id,
          session,
          %{},
          state
        )

      branch ->
        review_command(acp_id, session_id, session, state, %{
          "type" => "baseBranch",
          "branch" => branch
        })
    end
  end

  defp translate_slash_command(
         {:"review-commit", rest},
         acp_id,
         session_id,
         session,
         _params,
         state
       ) do
    case String.trim(rest) do
      "" ->
        translate_user_prompt(
          [%{"type" => "text", "text" => "/review-commit #{rest}"}],
          acp_id,
          session_id,
          session,
          %{},
          state
        )

      sha ->
        review_command(acp_id, session_id, session, state, %{
          "type" => "commit",
          "sha" => sha,
          "title" => nil
        })
    end
  end

  defp translate_slash_command({:logout, _rest}, acp_id, session_id, session, _params, state) do
    {id, state} = next_request_id(state)
    request = encode_request(id, "account/logout", %{})

    state =
      state
      |> Sessions.put(session_id, reset_prompt_accumulators(session, acp_id))
      |> track_request(id, :logout, nil, %{session_id: session_id})

    result = %{
      "stopReason" => "refusal",
      "_meta" => %{
        "ex_mcp" => %{
          "adapter" => "codex",
          "authRequired" => true,
          "message" => "Codex logout completed; authenticate again before prompting."
        }
      }
    }

    {:reply_and_write, result, request, state}
  end

  defp review_command(acp_id, session_id, session, state, target) do
    track_prompt_command(acp_id, session_id, session, state, "review/start", %{
      "threadId" => session_id,
      "target" => target,
      "delivery" => "inline"
    })
  end

  defp track_prompt_command(acp_id, session_id, session, state, method, params) do
    {id, state} = next_request_id(state)
    request = encode_request(id, method, params)

    state =
      state
      |> Sessions.put(session_id, reset_prompt_accumulators(session, acp_id))
      |> track_request(id, :prompt_command_start, acp_id, %{session_id: session_id})

    {:ok, request, state}
  end

  defp reset_prompt_accumulators(session, acp_id) do
    session
    |> Map.put(:active_prompt_acp_id, acp_id)
    |> Map.put(:accumulated_text, [])
    |> Map.put(:accumulated_thinking, [])
    |> Map.put(:accumulated_usage, nil)
  end

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
    case PendingRequests.pop(state.pending_requests, id) do
      {nil, _} ->
        {:skip, state}

      {%{type: type} = entry, pending} ->
        state = %{state | pending_requests: pending}
        handle_typed_response(type, entry, reply, state)
    end
  end

  defp handle_typed_response(:initialize, _entry, _reply, state) do
    {id, state} = next_request_id(%{state | phase: :ready})
    request = encode_request(id, "model/list", %{"includeHidden" => false})
    state = track_request(state, id, :model_list, nil)
    {:skip_and_write, [encode_notification("initialized"), request], state}
  end

  defp handle_typed_response(:model_list, _entry, {:ok, result}, state) do
    {:skip, %{state | models: normalize_model_catalog(result["data"] || [])}}
  end

  defp handle_typed_response(:model_list, _entry, {:error, _error}, state), do: {:skip, state}

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
    session_id = Sessions.thread_id(thread, result)
    meta = Map.get(entry, :meta, %{})

    mode_id =
      Config.mode_id_from_result(result) || meta[:mode_id] || state.mode_id ||
        Config.default_mode()

    session =
      session_from_result(session_id, result, state)
      |> Map.put(:mode_id, mode_id)

    state = Sessions.put(state, session_id, session)

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
      get_in(entry, [:meta, :session_id]) || result["threadId"] || Sessions.current_id(state)

    turn = result["turn"] || %{}
    turn_id = turn["id"] || result["turnId"]

    state =
      Sessions.update(state, session_id, fn session ->
        session
        |> Map.put(:turn_id, turn_id)
        |> Map.put(:active_prompt_acp_id, acp_id)
      end)

    {:skip, state}
  end

  defp handle_typed_response(:turn_start, %{acp_id: acp_id}, {:error, error}, state) do
    {:messages, [error_response(acp_id, error)], state}
  end

  defp handle_typed_response(:prompt_command_start, _entry, {:ok, _result}, state),
    do: {:skip, state}

  defp handle_typed_response(:prompt_command_start, %{acp_id: acp_id}, {:error, error}, state) do
    {:messages, [error_response(acp_id, error)], state}
  end

  defp handle_typed_response(:turn_interrupt, _entry, _reply, state), do: {:skip, state}
  defp handle_typed_response(:settings_update, _entry, _reply, state), do: {:skip, state}
  defp handle_typed_response(:thread_unsubscribe, _entry, _reply, state), do: {:skip, state}
  defp handle_typed_response(_type, _entry, _reply, state), do: {:skip, state}

  # Notifications

  defp handle_notification("thread/started", params, state) do
    thread = params["thread"] || %{}
    session_id = Sessions.thread_id(thread, params)
    session = session_from_result(session_id, params, state)
    {:skip, Sessions.put(state, session_id, session)}
  end

  defp handle_notification("thread/settings/updated", %{"threadId" => session_id} = params, state) do
    settings = params["threadSettings"] || params["settings"] || %{}

    state =
      Sessions.update(state, session_id, fn session ->
        effort = settings["effort"] || settings["reasoningEffort"] || session[:reasoning_effort]
        model = settings["model"] || session[:model]

        session
        |> Map.put(:model, model)
        |> Map.put(:reasoning_effort, effort)
        |> Map.put(
          :model_id,
          model_id_for_session(%{session | model: model, reasoning_effort: effort}, state)
        )
        |> Map.put(
          :mode_id,
          Config.mode_id_from_result(params) || Config.mode_id_from_result(settings) ||
            session[:mode_id]
        )
      end)

    {:skip, state}
  end

  defp handle_notification("turn/started", params, state) do
    session_id = params["threadId"] || params["sessionId"] || Sessions.current_id(state)
    turn = params["turn"] || %{}
    turn_id = turn["id"] || params["turnId"]

    state =
      Sessions.update(state, session_id, fn session ->
        Map.put(session, :turn_id, turn_id)
      end)

    {:skip, state}
  end

  defp handle_notification("item/agentMessage/delta", params, state) do
    session_id = Sessions.id_from_params(params, state)
    delta = params["delta"] || ""

    state =
      Sessions.update(state, session_id, fn session ->
        Map.update(session, :accumulated_text, [delta], &[delta | &1])
      end)

    {:messages, [AdapterEvents.agent_message_chunk(session_id, delta)], state}
  end

  defp handle_notification("agent_message/delta", params, state),
    do: handle_notification("item/agentMessage/delta", params, state)

  defp handle_notification("item/reasoning/textDelta", params, state) do
    session_id = Sessions.id_from_params(params, state)
    delta = params["delta"] || params["text"] || ""

    state =
      Sessions.update(state, session_id, fn session ->
        Map.update(session, :accumulated_thinking, [delta], &[delta | &1])
      end)

    {:messages, [AdapterEvents.agent_thought_chunk(session_id, delta)], state}
  end

  defp handle_notification("item/reasoning/summaryTextDelta", params, state),
    do: handle_notification("item/reasoning/textDelta", params, state)

  defp handle_notification("item/reasoning/summaryPartAdded", params, state) do
    text = params["text"] || params["summary"] || params["part"] || ""
    handle_notification("item/reasoning/textDelta", Map.put(params, "delta", text), state)
  end

  defp handle_notification("item/started", %{"item" => item} = params, state) do
    session_id = Sessions.id_from_params(params, state)
    handle_item_started(session_id, item, params, state)
  end

  defp handle_notification(
         "item/created",
         %{"item" => %{"type" => "function_call"} = item} = params,
         state
       ) do
    session_id = Sessions.id_from_params(params, state)
    {:messages, [Events.tool_call_started(session_id, item)], state}
  end

  defp handle_notification("item/created", _params, state), do: {:skip, state}

  defp handle_notification("item/completed", params, state) do
    session_id = Sessions.id_from_params(params, state)
    handle_item_completed(session_id, params["item"] || %{}, state)
  end

  defp handle_notification("item/commandExecution/started", params, state) do
    session_id = Sessions.id_from_params(params, state)

    notification =
      AdapterEvents.tool_call(session_id, %{
        "toolCallId" => params["callId"] || params["itemId"],
        "title" => Events.command_title(params["command"]),
        "kind" => "execute",
        "status" => "in_progress",
        "rawInput" => %{"command" => params["command"]}
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/commandExecution/outputDelta", params, state) do
    session_id = Sessions.id_from_params(params, state)
    delta = params["delta"] || ""

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => params["callId"] || params["itemId"] || params["item_id"],
        "content" => [Events.tool_text_content(delta)]
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/commandExecution/terminalInteraction", params, state) do
    session_id = Sessions.id_from_params(params, state)
    text = params["text"] || params["input"] || params["delta"] || Events.format_raw(params)

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => params["callId"] || params["itemId"] || params["item_id"],
        "content" => [Events.tool_text_content(text)],
        "rawOutput" => params
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/commandExecution/completed", params, state) do
    session_id = Sessions.id_from_params(params, state)

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "status" => "completed",
        "toolCallId" => params["callId"] || params["itemId"],
        "rawOutput" => %{
          "exitCode" => params["exitCode"],
          "output" => params["output"]
        },
        "content" => [Events.tool_text_content(params["output"] || "")]
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/fileChange/outputDelta", params, state) do
    session_id = Sessions.id_from_params(params, state)
    delta = params["delta"] || params["text"] || params["output"] || ""

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => params["callId"] || params["itemId"],
        "content" => [Events.tool_text_content(delta)],
        "rawOutput" => params
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/fileChange/patchUpdated", params, state) do
    session_id = Sessions.id_from_params(params, state)
    patch = params["patch"] || params

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => params["callId"] || params["itemId"] || patch["id"],
        "kind" => "edit",
        "content" => [
          Events.tool_diff_content(
            patch["path"] || params["path"],
            patch["diff"] || patch["text"] || params["delta"] || ""
          )
        ],
        "rawOutput" => params
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/mcpToolCall/progress", params, state) do
    session_id = Sessions.id_from_params(params, state)
    text = params["message"] || params["delta"] || Events.format_raw(params["progress"] || params)

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => params["callId"] || params["itemId"],
        "status" => "in_progress",
        "content" => [Events.tool_text_content(text)],
        "rawOutput" => params
      })

    {:messages, [notification], state}
  end

  defp handle_notification("serverRequest/resolved", params, state) do
    request_id = params["requestId"]

    pending =
      Map.reject(state.pending_client_requests, fn {_acp_id, entry} ->
        entry.codex_id == request_id
      end)

    {:skip, %{state | pending_client_requests: pending}}
  end

  defp handle_notification("item/patch/created", params, state) do
    session_id = Sessions.id_from_params(params, state)
    patch = params["patch"] || params

    notification =
      AdapterEvents.tool_call(session_id, %{
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
    session_id = Sessions.id_from_params(params, state)
    session = Map.get(state.sessions, session_id, Sessions.empty(session_id, state))
    turn = params["turn"] || %{}

    text =
      session
      |> Map.get(:accumulated_text, [])
      |> Enum.reverse()
      |> IO.iodata_to_binary()

    messages = [
      AdapterEvents.session_info_update(session_id, %{
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
      Sessions.update(state, session_id, fn session ->
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
    session_id = Sessions.id_from_params(params, state)
    token_usage = params["tokenUsage"] || %{}
    total = token_usage["total"] || %{}

    usage_data = %{
      "inputTokens" => total["inputTokens"] || 0,
      "outputTokens" => total["outputTokens"] || 0,
      "cachedInputTokens" => total["cachedInputTokens"] || 0
    }

    state =
      Sessions.update(state, session_id, fn session ->
        Map.put(session, :accumulated_usage, usage_data)
      end)

    {:skip, state}
  end

  defp handle_notification("error", params, state) do
    session_id = Sessions.id_from_params(params, state)
    error = params["error"] || %{}

    notification =
      AdapterEvents.session_info_update(session_id, %{
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

  defp handle_notification("warning", params, state) do
    session_id = Sessions.id_from_params(params, state)
    text = params["message"] || params["warning"] || Events.format_raw(params)

    {:messages, [AdapterEvents.agent_message_chunk(session_id, text)], state}
  end

  defp handle_notification("guardianWarning", params, state),
    do: handle_notification("warning", params, state)

  defp handle_notification("item/webSearch/started", params, state) do
    session_id = Sessions.id_from_params(params, state)

    notification =
      AdapterEvents.tool_call(session_id, %{
        "toolCallId" => params["itemId"],
        "title" => "Web Search",
        "kind" => "fetch",
        "status" => "in_progress",
        "rawInput" => %{"query" => params["query"]}
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/webSearch/completed", params, state) do
    session_id = Sessions.id_from_params(params, state)

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "status" => "completed",
        "toolCallId" => params["itemId"],
        "rawOutput" => params["results"],
        "content" => [
          Events.tool_text_content(Events.format_web_search_results(params["results"]))
        ]
      })

    {:messages, [notification], state}
  end

  defp handle_notification("thread/plan/updated", params, state) do
    session_id = Sessions.id_from_params(params, state)
    entries = params["entries"] || params["plan"] || []

    {:messages, [AdapterEvents.plan(session_id, entries)], state}
  end

  defp handle_notification("turn/plan/updated", params, state),
    do: handle_notification("thread/plan/updated", params, state)

  defp handle_notification("item/plan/delta", params, state) do
    session_id = Sessions.id_from_params(params, state)
    delta = params["delta"] || params["text"] || ""

    {:messages, [AdapterEvents.agent_thought_chunk(session_id, delta)], state}
  end

  defp handle_notification("thread/compacted", params, state) do
    session_id = Sessions.id_from_params(params, state)

    {:messages, [AdapterEvents.agent_message_chunk(session_id, "Context compacted\n")], state}
  end

  defp handle_notification("thread/status/changed", params, state) do
    session_id = Sessions.id_from_params(params, state)

    {:messages,
     [
       AdapterEvents.session_info_update(session_id, %{
         "_meta" => %{
           "ex_mcp" => %{
             "adapter" => "codex",
             "status" => params["status"] || params["threadStatus"] || params["state"]
           }
         }
       })
     ], state}
  end

  defp handle_notification("thread/goal/updated", params, state) do
    session_id = Sessions.id_from_params(params, state)

    {:messages,
     [
       AdapterEvents.session_info_update(session_id, %{
         "_meta" => %{"ex_mcp" => %{"adapter" => "codex", "goal" => params["goal"] || params}}
       })
     ], state}
  end

  defp handle_notification("thread/goal/cleared", params, state) do
    session_id = Sessions.id_from_params(params, state)

    {:messages,
     [
       AdapterEvents.session_info_update(session_id, %{
         "_meta" => %{"ex_mcp" => %{"adapter" => "codex", "goalCleared" => true}}
       })
     ], state}
  end

  defp handle_notification(method, params, state)
       when method in ["model/rerouted", "model/verification", "turn/moderationMetadata"] do
    session_id = Sessions.id_from_params(params, state)

    {:messages,
     [
       AdapterEvents.session_info_update(session_id, %{
         "_meta" => %{"ex_mcp" => %{"adapter" => "codex", "event" => method, "params" => params}}
       })
     ], state}
  end

  defp handle_notification("thread/availableCommands/updated", params, state) do
    session_id = Sessions.id_from_params(params, state)
    commands = params["commands"] || params["availableCommands"] || []

    {:messages, [AdapterEvents.available_commands_update(session_id, commands)], state}
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
    session_id = Sessions.id_from_params(params, state)
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

    entry = %{
      codex_id: codex_id,
      method: method,
      params: params,
      session_id: session_id
    }

    state = %{
      state
      | pending_client_requests: PendingRequests.put(state.pending_client_requests, acp_id, entry)
    }

    {:messages, [request], state}
  end

  defp handle_server_request(codex_id, method, _params, state)
       when method in [
              "item/tool/requestUserInput",
              "item/tool/call",
              "account/chatgptAuthTokens/refresh",
              "attestation/generate"
            ] do
    Logger.debug("[Codex Adapter] Rejecting unsupported app-server request: #{method}")

    {:skip_and_write,
     encode_error(codex_id, -32_601, "Unsupported app-server request: #{method}"), state}
  end

  defp handle_server_request(codex_id, method, _params, state) do
    Logger.debug("[Codex Adapter] Rejecting unsupported app-server request: #{method}")

    {:skip_and_write,
     encode_error(codex_id, -32_601, "Unsupported app-server request: #{method}"), state}
  end

  # Item completion / replay helpers

  defp handle_item_started(session_id, item, params, state) do
    case Events.item_type(item) do
      type when type in ["function_call", "functionCall"] ->
        {:messages, [Events.tool_call_started(session_id, item)], state}

      "commandExecution" ->
        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => Events.command_title(item["command"]),
            "kind" => "execute",
            "status" => Events.normalize_tool_status(item["status"], "in_progress"),
            "rawInput" => %{"command" => item["command"], "cwd" => item["cwd"]}
          })

        {:messages, [notification], state}

      "fileChange" ->
        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => "Edit File",
            "kind" => "edit",
            "status" => Events.normalize_tool_status(item["status"], "in_progress"),
            "rawInput" => %{"changes" => item["changes"]}
          })

        {:messages, [notification], state}

      "mcpToolCall" ->
        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => Events.mcp_tool_title(item),
            "kind" => "other",
            "status" => Events.normalize_tool_status(item["status"], "in_progress"),
            "rawInput" => item["arguments"]
          })

        {:messages, [notification], state}

      "dynamicToolCall" ->
        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => Events.dynamic_tool_title(item),
            "kind" => Events.tool_kind(item["tool"]),
            "status" => Events.normalize_tool_status(item["status"], "in_progress"),
            "rawInput" => item["arguments"]
          })

        {:messages, [notification], state}

      "webSearch" ->
        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => "Web Search",
            "kind" => "fetch",
            "status" => "in_progress",
            "rawInput" => %{"query" => item["query"], "action" => item["action"]}
          })

        {:messages, [notification], state}

      "imageGeneration" ->
        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => "Generate Image",
            "kind" => "other",
            "status" => Events.normalize_tool_status(item["status"], "in_progress"),
            "rawInput" => %{"revisedPrompt" => item["revisedPrompt"]}
          })

        {:messages, [notification], state}

      _ ->
        {:skip, state}
    end
  end

  defp handle_item_completed(session_id, %{"type" => "agent_message"} = item, state) do
    text = item["text"] || item["message"] || ""

    notification =
      AdapterEvents.agent_message_chunk(session_id, text, meta: %{"ex_mcp" => %{"final" => true}})

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "agentMessage"} = item, state) do
    handle_item_completed(session_id, Map.put(item, "type", "agent_message"), state)
  end

  defp handle_item_completed(session_id, %{"type" => "reasoning"} = item, state) do
    text =
      (item["content"] || item["summary"] || [])
      |> List.wrap()
      |> Enum.join("\n")

    notification =
      AdapterEvents.agent_thought_chunk(session_id, text, meta: %{"ex_mcp" => %{"final" => true}})

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "function_call"} = item, state) do
    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["callId"] || item["id"],
        "status" => "completed",
        "kind" => Events.tool_kind(item["name"]),
        "rawInput" => item["arguments"]
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "functionCall"} = item, state) do
    handle_item_completed(session_id, Map.put(item, "type", "function_call"), state)
  end

  defp handle_item_completed(session_id, %{"type" => "function_call_output"} = item, state) do
    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["callId"] || item["id"],
        "status" => if(item["isError"], do: "failed", else: "completed"),
        "content" => [Events.tool_text_content(item["output"] || item["text"] || "")],
        "rawOutput" => item["output"] || item["text"] || ""
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "commandExecution"} = item, state) do
    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["id"],
        "status" => Events.normalize_tool_status(item["status"], "completed"),
        "rawOutput" => %{
          "exitCode" => item["exitCode"],
          "output" => item["aggregatedOutput"],
          "durationMs" => item["durationMs"]
        },
        "content" => [Events.tool_text_content(item["aggregatedOutput"] || "")]
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "patch"} = item, state) do
    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["callId"] || item["id"],
        "kind" => "edit",
        "status" => "completed",
        "content" => [Events.tool_diff_content(item["path"], item["diff"] || item["text"] || "")]
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "fileChange"} = item, state) do
    changes = item["changes"] || []

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["id"],
        "kind" => "edit",
        "status" => Events.normalize_tool_status(item["status"], "completed"),
        "content" => Enum.map(changes, &Events.file_change_content/1),
        "rawOutput" => %{"changes" => changes, "status" => item["status"]}
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "mcpToolCall"} = item, state) do
    output = item["result"] || item["error"] || %{}

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["id"],
        "status" =>
          Events.normalize_tool_status(
            item["status"],
            if(item["error"], do: "failed", else: "completed")
          ),
        "content" => [Events.tool_text_content(Events.format_raw(output))],
        "rawOutput" => output
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "dynamicToolCall"} = item, state) do
    output = item["contentItems"] || []

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["id"],
        "status" =>
          Events.normalize_tool_status(
            item["status"],
            if(item["success"] == false, do: "failed", else: "completed")
          ),
        "content" => Events.dynamic_tool_content(output),
        "rawOutput" => output
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "webSearch"} = item, state) do
    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["id"],
        "status" => "completed",
        "rawOutput" => item,
        "content" => [
          Events.tool_text_content(item["query"] || Events.format_raw(item["action"] || item))
        ]
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "imageGeneration"} = item, state) do
    text = item["savedPath"] || item["result"] || item["revisedPrompt"] || ""

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["id"],
        "status" => Events.normalize_tool_status(item["status"], "completed"),
        "content" => [Events.tool_text_content(text)],
        "rawOutput" => item
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "contextCompaction"} = _item, state) do
    {:messages, [AdapterEvents.agent_message_chunk(session_id, "Context compacted\n")], state}
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
      AdapterEvents.agent_message_chunk(session_id, item["text"] || item["message"] || "",
        meta: %{"ex_mcp" => %{"replay" => true}}
      )
    ]
  end

  defp replay_item(session_id, %{"type" => "reasoning"} = item) do
    [
      AdapterEvents.agent_thought_chunk(session_id, item["text"] || item["summary"] || "",
        meta: %{"ex_mcp" => %{"replay" => true}}
      )
    ]
  end

  defp replay_item(session_id, item) do
    case handle_item_completed(session_id, item, nil) do
      {:messages, messages, _state} -> Enum.map(messages, &Events.mark_replay/1)
      {:skip, _state} -> []
    end
  end

  # State helpers

  defp session_from_result(session_id, result, state) do
    Sessions.from_result(session_id, result, state, &model_id_for_session(&1, state))
  end

  defp fetch_turn_id(%{"turnId" => turn_id}, _session) when is_binary(turn_id) and turn_id != "",
    do: {:ok, turn_id}

  defp fetch_turn_id(_params, %{turn_id: turn_id}) when is_binary(turn_id) and turn_id != "",
    do: {:ok, turn_id}

  defp fetch_turn_id(_params, _session), do: {:error, "No active Codex turn for session"}

  # Result builders

  defp session_result(session_id, result, session, state) do
    %{
      "sessionId" => session_id,
      "modes" => %{
        "availableModes" => modes(),
        "currentModeId" => session[:mode_id] || state.mode_id || Config.default_mode()
      },
      "models" => models_for_session(session, state),
      "configOptions" => config_options_for_session(session, state),
      "_meta" => %{"ex_mcp" => %{"codex" => %{"thread" => result["thread"] || %{}}}}
    }
  end

  defp session_config_result(session, state) do
    %{
      "models" => models_for_session(session, state),
      "configOptions" => config_options_for_session(session, state)
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

    effort =
      session[:reasoning_effort] || state.reasoning_effort || Config.default_reasoning_effort()

    []
    |> Kernel.++([mode_option(session[:mode_id] || state.mode_id || Config.default_mode())])
    |> maybe_add_model_option(model, state)
    |> maybe_add_reasoning_effort_option(effort, session, state)
  end

  defp mode_option(current) do
    %{
      "id" => "mode",
      "name" => "Approval Preset",
      "type" => "select",
      "category" => "mode",
      "currentValue" => current || Config.default_mode(),
      "description" => "Choose an approval and sandboxing preset for your session",
      "options" =>
        Enum.map(modes(), fn mode ->
          %{
            "value" => mode["id"],
            "name" => mode["name"],
            "description" => mode["description"]
          }
        end)
    }
  end

  defp maybe_add_model_option(options, nil, %{models: []}), do: options
  defp maybe_add_model_option(options, "", %{models: []}), do: options

  defp maybe_add_model_option(options, model, state) do
    select_options =
      model_select_options(model, state)
      |> ensure_current_model_option(model)

    options ++
      [
        %{
          "id" => "model",
          "name" => "Model",
          "type" => "select",
          "category" => "model",
          "description" => "Choose which model Codex should use",
          "currentValue" => model,
          "options" => select_options
        }
      ]
  end

  defp maybe_add_reasoning_effort_option(options, current, session, state) do
    options ++ [reasoning_effort_option(current, session, state)]
  end

  defp reasoning_effort_option(current, session, state) do
    efforts =
      case current_model(session, state) do
        nil -> []
        model -> model_reasoning_efforts(model)
      end
      |> case do
        [] ->
          Enum.map(Config.reasoning_efforts(), fn {value, name} ->
            %{"value" => value, "name" => name}
          end)

        efforts ->
          Enum.map(efforts, fn effort ->
            %{
              "value" => effort["value"],
              "name" => effort["name"] || humanize_option(effort["value"]),
              "description" => effort["description"]
            }
            |> reject_nil_values()
          end)
      end

    %{
      "id" => "reasoning_effort",
      "name" => "Reasoning Effort",
      "type" => "select",
      "category" => "thought_level",
      "currentValue" => current || Config.default_reasoning_effort(),
      "description" => "Choose how much reasoning effort the model should use",
      "options" => efforts
    }
  end

  defp config_update(%{"configId" => "mode", "value" => value}, _session, _state)
       when is_binary(value) do
    case Config.normalize_requested_mode(value) do
      {:ok, mode_id} ->
        {:ok,
         %{
           wire: Config.merge_mode_wire_params(%{}, mode_id),
           session: %{mode_id: mode_id},
           state: %{mode_id: mode_id}
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp config_update(%{"configId" => "model", "value" => value}, session, state)
       when is_binary(value) do
    case model_selection(value, session, state) do
      {:ok, selection} ->
        {:ok,
         %{
           wire: %{"model" => selection.model} |> maybe_put("effort", selection.effort),
           session: selection.session,
           state: %{
             model: selection.model,
             reasoning_effort: selection.effort || state.reasoning_effort
           }
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp config_update(%{"configId" => "reasoning_effort", "value" => value}, session, state)
       when is_binary(value) do
    if reasoning_effort_supported?(value, session, state) do
      {:ok,
       %{
         wire: %{"effort" => value},
         session: %{
           reasoning_effort: value,
           model_id: model_id_for_session(%{session | reasoning_effort: value}, state)
         },
         state: %{reasoning_effort: value}
       }}
    else
      {:error, "Unsupported reasoning_effort: #{value}"}
    end
  end

  defp config_update(%{"configId" => id}, _session, _state),
    do: {:error, "Unsupported Codex config option: #{id}"}

  defp config_update(_params, _session, _state), do: {:error, "configId and value are required"}

  # Model catalog mapping

  defp normalize_model_catalog(models) when is_list(models) do
    models
    |> Enum.map(&normalize_model/1)
    |> Enum.reject(&is_nil/1)
  end

  defp normalize_model_catalog(_models), do: []

  defp normalize_model(%{"id" => id} = model) when is_binary(id) and id != "" do
    %{
      "id" => id,
      "model" => model["model"] || id,
      "displayName" => model["displayName"] || model["display_name"] || id,
      "description" => model["description"],
      "hidden" => model["hidden"] || false,
      "defaultReasoningEffort" =>
        model["defaultReasoningEffort"] || model["default_reasoning_effort"],
      "supportedReasoningEfforts" =>
        model
        |> Map.get("supportedReasoningEfforts", model["supported_reasoning_efforts"] || [])
        |> normalize_reasoning_efforts()
    }
  end

  defp normalize_model(_model), do: nil

  defp normalize_reasoning_efforts(efforts) when is_list(efforts) do
    efforts
    |> Enum.map(&normalize_reasoning_effort/1)
    |> Enum.reject(&is_nil/1)
  end

  defp normalize_reasoning_efforts(_efforts), do: []

  defp normalize_reasoning_effort(%{} = effort) do
    value = effort["reasoningEffort"] || effort["effort"] || effort["value"]

    if is_binary(value) and value != "" do
      %{
        "value" => value,
        "name" => effort["name"] || humanize_option(value),
        "description" => effort["description"]
      }
      |> reject_nil_values()
    end
  end

  defp normalize_reasoning_effort(value) when is_binary(value) and value != "" do
    %{"value" => value, "name" => humanize_option(value)}
  end

  defp normalize_reasoning_effort(_effort), do: nil

  defp model_selection(model_id, session, state) when is_binary(model_id) do
    model_id = String.trim(model_id)

    if model_id == "",
      do: {:error, "modelId is required"},
      else: do_model_selection(model_id, session, state)
  end

  defp model_selection(_model_id, _session, _state), do: {:error, "modelId is required"}

  defp do_model_selection(model_id, session, state) do
    case parse_catalog_model_id(model_id) do
      {catalog_id, effort} ->
        case find_model_by_id(state.models, catalog_id) do
          nil ->
            raw_model_selection(model_id, session, state)

          model ->
            effort = supported_or_default_effort(model, effort, session, state)
            catalog_model_selection(model, effort)
        end

      nil ->
        case find_model_by_id(state.models, model_id) ||
               find_model_by_wire(state.models, model_id) do
          nil ->
            raw_model_selection(model_id, session, state)

          model ->
            effort =
              supported_or_default_effort(model, session[:reasoning_effort], session, state)

            catalog_model_selection(model, effort)
        end
    end
  end

  defp catalog_model_selection(model, effort) do
    wire_model = model["model"] || model["id"]
    model_id = catalog_model_id(model, effort)

    {:ok,
     %{
       model: wire_model,
       effort: effort,
       session: %{model: wire_model, model_id: model_id, reasoning_effort: effort}
     }}
  end

  defp raw_model_selection(model_id, session, state) do
    effort = session[:reasoning_effort] || state.reasoning_effort

    {:ok,
     %{
       model: model_id,
       effort: effort,
       session: %{model: model_id, model_id: model_id, reasoning_effort: effort}
     }}
  end

  defp parse_catalog_model_id(model_id) when is_binary(model_id) do
    case String.split(model_id, "/", parts: 2) do
      [catalog_id, effort] when catalog_id != "" and effort != "" -> {catalog_id, effort}
      _ -> nil
    end
  end

  defp parse_catalog_model_id(_model_id), do: nil

  defp model_select_options(current_model, state) do
    state.models
    |> Enum.filter(&visible_model?(&1, current_model))
    |> Enum.map(fn model ->
      %{
        "value" => model["id"],
        "name" => model["displayName"] || model["id"],
        "description" => model["description"]
      }
      |> reject_nil_values()
    end)
  end

  defp ensure_current_model_option(options, nil), do: options
  defp ensure_current_model_option(options, ""), do: options

  defp ensure_current_model_option(options, current_model) do
    if Enum.any?(options, &(&1["value"] == current_model)) do
      options
    else
      [%{"value" => current_model, "name" => current_model} | options]
    end
  end

  defp models_for_session(session, state) do
    current = model_id_for_session(session, state)

    available =
      case state.models do
        [] ->
          if current, do: [%{"modelId" => current, "name" => current}], else: []

        models ->
          models
          |> Enum.filter(&visible_model?(&1, session[:model] || state.model))
          |> Enum.flat_map(&model_infos/1)
          |> ensure_current_model_info(current)
      end

    %{
      "currentModelId" => current,
      "availableModels" => available
    }
    |> reject_nil_values()
  end

  defp model_infos(model) do
    case model_reasoning_efforts(model) do
      [] ->
        [
          %{
            "modelId" => model["id"],
            "name" => model["displayName"] || model["id"],
            "description" => model["description"]
          }
          |> reject_nil_values()
        ]

      efforts ->
        Enum.map(efforts, fn effort ->
          %{
            "modelId" => catalog_model_id(model, effort["value"]),
            "name" => "#{model["displayName"] || model["id"]} (#{effort["value"]})",
            "description" =>
              [model["description"], effort["description"]]
              |> Enum.reject(&is_nil/1)
              |> Enum.join(" ")
          }
          |> reject_nil_values()
        end)
    end
  end

  defp ensure_current_model_info(options, nil), do: options

  defp ensure_current_model_info(options, current) do
    if Enum.any?(options, &(&1["modelId"] == current)) do
      options
    else
      [%{"modelId" => current, "name" => current} | options]
    end
  end

  defp model_id_for_session(session, state) do
    session[:model_id] ||
      case current_model(session, state) do
        nil ->
          session[:model] || state.model

        model ->
          catalog_model_id(
            model,
            supported_or_default_effort(model, session[:reasoning_effort], session, state)
          )
      end
  end

  defp catalog_model_id(model, nil), do: model["id"]
  defp catalog_model_id(model, effort), do: "#{model["id"]}/#{effort}"

  defp current_model(session, state) do
    find_model_by_wire(state.models, session[:model] || state.model) ||
      find_model_by_id(state.models, session[:model_id]) ||
      session[:model_id]
      |> parse_catalog_model_id()
      |> case do
        {catalog_id, _effort} -> find_model_by_id(state.models, catalog_id)
        nil -> nil
      end
  end

  defp find_model_by_id(models, id) when is_binary(id) do
    Enum.find(models, &(&1["id"] == id))
  end

  defp find_model_by_id(_models, _id), do: nil

  defp find_model_by_wire(models, model) when is_binary(model) do
    Enum.find(models, &(&1["model"] == model || &1["id"] == model))
  end

  defp find_model_by_wire(_models, _model), do: nil

  defp visible_model?(model, current_model) do
    model["hidden"] != true || model["model"] == current_model || model["id"] == current_model
  end

  defp model_reasoning_efforts(model) do
    model["supportedReasoningEfforts"] || []
  end

  defp supported_or_default_effort(model, requested, session, state) do
    supported = model_reasoning_efforts(model)
    values = Enum.map(supported, & &1["value"])
    current = requested || session[:reasoning_effort] || state.reasoning_effort

    cond do
      current in values -> current
      model["defaultReasoningEffort"] in values -> model["defaultReasoningEffort"]
      values != [] -> hd(values)
      true -> nil
    end
  end

  defp reasoning_effort_supported?(value, session, state) do
    case current_model(session, state) do
      nil ->
        value in Enum.map(Config.reasoning_efforts(), &elem(&1, 0))

      model ->
        case model_reasoning_efforts(model) do
          [] -> true
          efforts -> value in Enum.map(efforts, & &1["value"])
        end
    end
  end

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
    |> NameValue.map()
    |> Map.get(name)
  end

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
    Events.command_title(params["command"])
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
    %{state | pending_requests: PendingRequests.put(state.pending_requests, id, entry)}
  end

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

  defp maybe_put(map, key, value), do: Maps.put_non_empty(map, key, value)

  defp reject_nil_values(map) do
    Map.reject(map, fn {_key, value} -> is_nil(value) end)
  end
end
