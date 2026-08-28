defmodule ExMCP.ACP.Adapters.Codex do
  @moduledoc """
  Adapter for Codex CLI using `codex app-server` persistent mode.

  Translates between ACP JSON-RPC and Codex's app-server JSON-RPC protocol.
  The adapter keeps Codex app-server as the subprocess boundary and owns the
  pure protocol mapping needed to present a stable ACP surface.

  Session-provided workspace paths and MCP servers are treated as untrusted.
  `:workspace_roots` defaults to the adapter working directory. Deployments
  may provide `:authorize_workspace` and `:authorize_mcp_server` callbacks;
  `:trust_authorized_workspaces` must also be set to opt into Codex's trusted
  project setting. `:trusted_mcp_servers` accepts exact server maps; `:all` is
  an explicit unsafe compatibility escape hatch. Names alone never authorize
  caller-controlled connection details.
  """

  @behaviour ExMCP.ACP.Adapter

  require Logger

  alias ExMCP.ACP.Adapters.Codex.{Config, Events, FileChanges, Protocol, Sessions, SlashCommands}
  alias ExMCP.ACP.{AdapterEvents, Envelope, PendingRequests}
  alias ExMCP.Internal.{Maps, NameValue, WorkspacePath}

  @structured_decision_prefix "codex:decision:"

  defstruct [
    :model,
    :mode_id,
    :reasoning_effort,
    models: [],
    next_id: 1,
    phase: :initializing,
    pending_requests: %{},
    pending_client_requests: %{},
    client_capabilities: %{},
    closed_sessions: %{},
    url_elicitations: %{},
    pending_auth: nil,
    sessions: %{},
    gateway_config: nil,
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
  def command(opts) do
    {Keyword.get(opts, :codex_path) || System.get_env("CODEX_PATH") || "codex", ["app-server"]}
  end

  @impl true
  def capabilities do
    %{
      "promptCapabilities" => %{"image" => true, "embeddedContext" => true},
      "mcpCapabilities" => %{
        "acp" => false,
        "http" => true,
        "sse" => false,
        "_meta" => %{"ex_mcp.codex" => %{"stdioMcpServers" => true}}
      },
      "loadSession" => true,
      "auth" => %{"logout" => %{}},
      "sessionCapabilities" => %{
        "list" => %{},
        "resume" => %{},
        "close" => %{},
        "delete" => %{},
        "additionalDirectories" => %{},
        "setModel" => %{}
      }
    }
  end

  @impl true
  def auth_methods(opts) do
    methods = [
      %{
        "id" => "api-key",
        "name" => "API Key",
        "description" => "Use an API key to authenticate",
        "_meta" => %{"api-key" => %{"provider" => "openai"}}
      }
    ]

    methods =
      if Keyword.get(opts, :no_browser, false) || System.get_env("NO_BROWSER") do
        methods
      else
        methods ++
          [
            %{
              "id" => "chat-gpt",
              "name" => "ChatGPT",
              "description" => "Use ChatGPT to authenticate"
            }
          ]
      end

    methods =
      if Keyword.get(opts, :gateway_auth, false) do
        methods ++
          [
            %{
              "id" => "gateway",
              "name" => "Custom model gateway",
              "description" => "Use a custom gateway to authenticate and access models",
              "_meta" => %{
                "gateway" => %{"protocol" => "openai", "restartRequired" => "false"}
              }
            }
          ]
      else
        methods
      end

    with_legacy_auth_methods(methods)
  end

  @impl true
  def auth_methods(opts, state) do
    if client_supports_elicitation?(state, "url") do
      auth_methods(opts) ++
        [
          %{
            "id" => "chat-gpt-device-code",
            "name" => "ChatGPT (device code)",
            "description" =>
              "Sign in to ChatGPT by opening a verification page and entering a one-time code"
          }
        ]
    else
      auth_methods(opts)
    end
  end

  defp legacy_auth_methods do
    [
      env_auth_method("codex-api-key", "Use CODEX_API_KEY", "CODEX_API_KEY"),
      env_auth_method("openai-api-key", "Use OPENAI_API_KEY", "OPENAI_API_KEY"),
      %{
        "id" => "chatgpt",
        "name" => "Login with ChatGPT",
        "description" =>
          "Use your ChatGPT login with Codex CLI (requires a paid ChatGPT subscription)"
      }
    ]
  end

  defp with_legacy_auth_methods(methods) do
    if Application.get_env(:ex_mcp, :codex_legacy_auth_methods, false) do
      methods ++ legacy_auth_methods()
    else
      methods
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
      Protocol.encode_request(id, Protocol.method(:initialize), %{
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
  def translate_outbound(%{"method" => "initialize", "params" => params}, state) do
    {:ok, :skip, %{state | client_capabilities: params["clientCapabilities"] || %{}}}
  end

  def translate_outbound(%{"method" => "authenticate", "id" => acp_id, "params" => params}, state) do
    method_id = params["methodId"] || params["provider"] || params["id"]

    case auth_request_params(method_id, params, state) do
      {:ok, {:gateway, gateway_config}} ->
        {:reply, %{}, %{state | gateway_config: gateway_config}}

      {:ok, codex_params} ->
        {id, state} = next_request_id(state)
        request = Protocol.encode_request(id, Protocol.method(:account_login_start), codex_params)
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
    request = Protocol.encode_request(id, Protocol.method(:account_logout), %{})
    state = track_request(state, id, :logout, nil)
    {:reply_and_write, %{}, request, state}
  end

  def translate_outbound(%{"method" => "session/new", "id" => acp_id, "params" => params}, state) do
    mode_id =
      Config.normalize_mode_id(params["modeId"] || params["approvalPolicy"] || state.mode_id)

    cwd = params["cwd"] || Keyword.get(state.opts, :cwd)

    case session_config(params, cwd, state) do
      {:ok, config, additional_directories} ->
        wire_params =
          %{}
          |> maybe_put("model", params["model"] || state.model)
          |> maybe_put("modelProvider", model_provider(state))
          |> maybe_put("cwd", cwd)
          |> maybe_put("config", config)
          |> Config.merge_thread_mode_wire_params(mode_id)

        {id, state} = next_request_id(state)
        request = Protocol.encode_request(id, Protocol.method(:thread_start), wire_params)

        state =
          track_request(state, id, :thread_start, acp_id, %{
            mode_id: mode_id,
            additional_directories: additional_directories
          })

        {:ok, request, state}

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/load", "id" => acp_id, "params" => params}, state) do
    case Sessions.fetch_id(params) do
      {:ok, session_id} ->
        mode_id =
          Config.normalize_mode_id(params["modeId"] || params["approvalPolicy"] || state.mode_id)

        cwd = params["cwd"] || Keyword.get(state.opts, :cwd)

        case session_config(params, cwd, state) do
          {:ok, config, additional_directories} ->
            wire_params =
              %{
                "threadId" => session_id,
                "initialTurnsPage" => %{"limit" => 100, "itemsView" => "full"}
              }
              |> maybe_put("model", params["model"] || state.model)
              |> maybe_put("modelProvider", resume_model_provider(state))
              |> maybe_put("cwd", cwd)
              |> maybe_put("config", config)
              |> Config.merge_thread_mode_wire_params(mode_id)

            {id, state} = next_request_id(state)
            request = Protocol.encode_request(id, Protocol.method(:thread_resume), wire_params)

            state =
              track_request(state, id, :thread_resume, acp_id, %{
                mode_id: mode_id,
                additional_directories: additional_directories
              })

            {:ok, request, state}

          {:error, reason} ->
            {:error, reason, state}
        end

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
        mode_id =
          Config.normalize_mode_id(params["modeId"] || params["approvalPolicy"] || state.mode_id)

        cwd = params["cwd"] || Keyword.get(state.opts, :cwd)

        case session_config(params, cwd, state) do
          {:ok, config, additional_directories} ->
            wire_params =
              %{"threadId" => session_id, "excludeTurns" => true}
              |> maybe_put("model", params["model"] || state.model)
              |> maybe_put("modelProvider", resume_model_provider(state))
              |> maybe_put("cwd", cwd)
              |> maybe_put("config", config)
              |> Config.merge_thread_mode_wire_params(mode_id)

            {id, state} = next_request_id(state)
            request = Protocol.encode_request(id, Protocol.method(:thread_resume), wire_params)

            state =
              track_request(state, id, :thread_resume, acp_id, %{
                mode_id: mode_id,
                additional_directories: additional_directories
              })

            {:ok, request, state}

          {:error, reason} ->
            {:error, reason, state}
        end

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/list", "id" => acp_id, "params" => params}, state) do
    cwd = session_list_cwd(params, state)

    case authorize_optional_workspace(cwd, :session_list, state) do
      :ok ->
        {id, state} = next_request_id(state)

        wire_params =
          %{}
          |> maybe_put("cwd", cwd)
          |> maybe_put("cursor", params["cursor"])
          |> maybe_put("limit", params["limit"])
          |> maybe_put("archived", false)

        request = Protocol.encode_request(id, Protocol.method(:thread_list), wire_params)
        state = track_request(state, id, :session_list, acp_id)
        {:ok, request, state}

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/close", "params" => params}, state) do
    with {:ok, session_id} <- Sessions.fetch_id(params),
         {:ok, session} <- Sessions.fetch(state, session_id) do
      {id, state} = next_request_id(state)

      close_request =
        Protocol.encode_request(id, Protocol.method(:thread_unsubscribe), %{
          "threadId" => session_id
        })

      state = track_request(state, id, :thread_unsubscribe, nil, %{session_id: session_id})

      {state, data} =
        if session[:turn_id] do
          {interrupt_id, state} = next_request_id(state)

          interrupt_request =
            Protocol.encode_request(interrupt_id, Protocol.method(:turn_interrupt), %{
              "threadId" => session_id,
              "turnId" => session[:turn_id]
            })

          state =
            track_request(state, interrupt_id, :turn_interrupt, nil, %{session_id: session_id})

          {state, [interrupt_request, close_request]}
        else
          {state, close_request}
        end

      {messages, pending_responses, state} = close_session_state(session_id, session, state)
      {:messages_and_write, messages, List.wrap(data) ++ pending_responses, state}
    else
      {:error, reason} -> {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/delete", "params" => params}, state) do
    case Sessions.fetch_id(params) do
      {:ok, session_id} ->
        session = Map.get(state.sessions, session_id)

        {requests, state} =
          if session && session[:turn_id] do
            {interrupt_id, state} = next_request_id(state)

            interrupt_request =
              Protocol.encode_request(interrupt_id, Protocol.method(:turn_interrupt), %{
                "threadId" => session_id,
                "turnId" => session[:turn_id]
              })

            state =
              track_request(state, interrupt_id, :turn_interrupt, nil, %{session_id: session_id})

            {[interrupt_request], state}
          else
            {[], state}
          end

        {requests, state} =
          if session do
            {unsubscribe_id, state} = next_request_id(state)

            unsubscribe_request =
              Protocol.encode_request(unsubscribe_id, Protocol.method(:thread_unsubscribe), %{
                "threadId" => session_id
              })

            state =
              track_request(state, unsubscribe_id, :thread_unsubscribe, nil, %{
                session_id: session_id
              })

            {requests ++ [unsubscribe_request], state}
          else
            {requests, state}
          end

        {archive_id, state} = next_request_id(state)

        archive_request =
          Protocol.encode_request(archive_id, Protocol.method(:thread_archive), %{
            "threadId" => session_id
          })

        {messages, pending_responses, state} =
          close_session_state(session_id, session, state)

        state = track_request(state, archive_id, :thread_archive, nil, %{session_id: session_id})

        {:messages_and_write, messages, requests ++ pending_responses ++ [archive_request], state}

      {:error, reason} ->
        {:error, reason, state}
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
        Protocol.encode_request(id, Protocol.method(:turn_interrupt), %{
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
      session = Map.put(session, :mode_id, mode_id)
      messages = [AdapterEvents.current_mode_update(session_id, mode_id)]

      state =
        state
        |> Sessions.put(session_id, session)
        |> Map.put(:mode_id, mode_id)

      {:messages_and_reply, messages, %{}, state}
    else
      {:error, reason} -> {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/set_model", "params" => params}, state) do
    with {:ok, session_id} <- Sessions.fetch_id(params),
         {:ok, session} <- Sessions.fetch(state, session_id),
         {:ok, selection} <- model_selection(params["modelId"], session, state) do
      session = Map.merge(session, selection.session)
      result = session_config_result(session, state)

      state =
        state
        |> Sessions.put(session_id, session)
        |> Map.put(:model, selection.model)
        |> Map.put(:reasoning_effort, selection.effort || state.reasoning_effort)

      {:reply, result, state}
    else
      {:error, reason} -> {:error, reason, state}
    end
  end

  def translate_outbound(%{"method" => "session/set_config_option", "params" => params}, state) do
    with {:ok, session_id} <- Sessions.fetch_id(params),
         {:ok, session} <- Sessions.fetch(state, session_id),
         {:ok, update} <- config_update(params, session, state) do
      session = Map.merge(session, update.session)
      result = session_config_result(session, state)

      state =
        state
        |> Sessions.put(session_id, session)
        |> Map.merge(update.state)

      {:reply, result, state}
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

        case client_request_result(entry, response, state) do
          {:native, result, state} ->
            {:ok, Protocol.encode_response(entry.codex_id, result), state}

          {:defer, state} ->
            {:ok, :skip, state}

          {:messages_and_write, messages, data, state} ->
            {:messages_and_write, messages, data, state}
        end
    end
  end

  def translate_outbound(_msg, state), do: {:ok, :skip, state}

  defp close_session_state(session_id, session, state) do
    prompt_messages =
      case session && session[:active_prompt_acp_id] do
        nil -> []
        acp_id -> [Envelope.response(acp_id, %{"stopReason" => "cancelled"})]
      end

    {cancelled, pending} =
      Enum.split_with(state.pending_client_requests, fn {_acp_id, entry} ->
        Map.get(entry, :session_id) == session_id
      end)

    native_responses =
      Enum.map(cancelled, fn {_acp_id, entry} ->
        Protocol.encode_response(entry.codex_id, cancelled_client_request_result(entry))
      end)

    {closed_elicitations, open_elicitations} =
      Enum.split_with(state.url_elicitations, fn {_request_id, elicitation} ->
        elicitation.session_id == session_id
      end)

    completion_messages =
      Enum.map(closed_elicitations, fn {_request_id, elicitation} ->
        Envelope.notification("elicitation/complete", %{
          "elicitationId" => elicitation.elicitation_id
        })
      end)

    state = %{
      state
      | sessions: Map.delete(state.sessions, session_id),
        pending_client_requests: Map.new(pending),
        closed_sessions: Map.put(state.closed_sessions, session_id, true),
        url_elicitations: Map.new(open_elicitations)
    }

    {prompt_messages ++ completion_messages, native_responses, state}
  end

  defp cancelled_client_request_result(%{kind: :user_input}), do: %{"answers" => %{}}
  defp cancelled_client_request_result(%{kind: :elicitation}), do: %{"action" => "cancel"}
  defp cancelled_client_request_result(%{method: method}), do: codex_cancel_response(method)

  defp closed_session_params?(params, state) do
    session_id = params["threadId"] || params["sessionId"] || get_in(params, ["turn", "threadId"])
    is_binary(session_id) and Map.has_key?(state.closed_sessions, session_id)
  end

  defp translate_user_prompt(input_items, acp_id, session_id, session, params, state) do
    {id, state} = next_request_id(state)
    additional_directories = session[:additional_directories] || []

    wire_params =
      %{
        "threadId" => session_id,
        "input" => input_items,
        "summary" => params["summary"] || "auto"
      }
      |> maybe_put("model", params["model"] || session[:model] || state.model)
      |> maybe_put("effort", session[:reasoning_effort] || state.reasoning_effort)
      |> maybe_put("serviceTier", service_tier_for_session(session, state))
      |> maybe_put("cwd", params["cwd"] || session[:cwd] || Keyword.get(state.opts, :cwd))
      |> Config.merge_turn_mode_wire_params(
        session[:mode_id] || state.mode_id || Config.default_mode(),
        additional_directories
      )

    request = Protocol.encode_request(id, Protocol.method(:turn_start), wire_params)

    state =
      state
      |> Sessions.put(session_id, reset_prompt_accumulators(session, acp_id))
      |> track_request(id, :turn_start, acp_id, %{session_id: session_id})

    {:ok, request, state}
  end

  defp translate_slash_command({:compact, _rest}, acp_id, session_id, session, _params, state) do
    track_prompt_command(
      acp_id,
      session_id,
      session,
      state,
      Protocol.method(:thread_compact_start),
      %{
        "threadId" => session_id
      }
    )
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
    request = Protocol.encode_request(id, Protocol.method(:account_logout), %{})

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

  defp translate_slash_command({:status, _rest}, _acp_id, session_id, session, _params, state) do
    message =
      AdapterEvents.agent_message_chunk(session_id, status_message(session_id, session, state))

    result =
      %{
        "stopReason" => "end_turn",
        "_meta" => %{"ex_mcp" => %{"adapter" => "codex", "command" => "status"}}
      }
      |> maybe_put("usage", session[:accumulated_usage])

    {:messages_and_reply, [message], result, state}
  end

  defp translate_slash_command(
         {:unknown, name, _rest},
         _acp_id,
         session_id,
         _session,
         _params,
         state
       ) do
    commands =
      ["compact", "init", "review", "review-branch", "review-commit", "status", "logout"]
      |> Enum.map_join("\n", &"- /#{&1}")

    message =
      AdapterEvents.agent_message_chunk(
        session_id,
        "Unknown command \"/#{name}\".\nAvailable commands:\n#{commands}"
      )

    result = %{
      "stopReason" => "end_turn",
      "_meta" => %{"ex_mcp" => %{"adapter" => "codex", "command" => "unknown"}}
    }

    {:messages_and_reply, [message], result, state}
  end

  defp review_command(acp_id, session_id, session, state, target) do
    track_prompt_command(acp_id, session_id, session, state, Protocol.method(:review_start), %{
      "threadId" => session_id,
      "target" => target,
      "delivery" => "inline"
    })
  end

  defp track_prompt_command(acp_id, session_id, session, state, method, params) do
    {id, state} = next_request_id(state)
    request = Protocol.encode_request(id, method, params)

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
    |> Map.put(:rate_limits, %{})
    |> Map.put(:prompt_activity, false)
  end

  # Inbound: Codex app-server -> ACP

  @impl true
  def translate_inbound(line, state) do
    case Protocol.decode_line(line) do
      {:response, id, reply} ->
        handle_response(state, id, reply)

      {:request, id, method, params} ->
        handle_server_request(id, method, params, state)

      {:notification, method, params} ->
        if closed_session_params?(params, state) do
          {:skip, state}
        else
          handle_notification(method, params, state)
        end

      :unknown ->
        {:skip, state}
    end
  end

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

    request =
      Protocol.encode_request(id, Protocol.method(:model_list), %{"includeHidden" => false})

    state = track_request(state, id, :model_list, nil)

    {:skip_and_write, [Protocol.encode_notification(Protocol.method(:initialized)), request],
     state}
  end

  defp handle_typed_response(:model_list, _entry, {:ok, result}, state) do
    {:skip, %{state | models: normalize_model_catalog(result["data"] || [])}}
  end

  defp handle_typed_response(:model_list, _entry, {:error, _error}, state), do: {:skip, state}

  defp handle_typed_response(
         :authenticate,
         %{acp_id: acp_id, meta: %{method_id: "chat-gpt-device-code"}},
         {:ok, result},
         state
       )
       when is_map(result) do
    case result["verificationUrl"] do
      url when is_binary(url) and url != "" ->
        start_auth_url_elicitation(acp_id, result, state)

      _missing_url ->
        {:messages,
         [
           Envelope.error(
             acp_id,
             -32_603,
             "Codex device-code authentication did not return a verification URL"
           )
         ], state}
    end
  end

  defp handle_typed_response(:authenticate, %{acp_id: acp_id}, {:ok, result}, state)
       when is_map(result),
       do: finish_authenticate(acp_id, result, state)

  defp handle_typed_response(:authenticate, %{acp_id: acp_id}, {:ok, result}, state) do
    finish_authenticate(acp_id, result, state)
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
      |> Map.put(:additional_directories, meta[:additional_directories] || [])

    state = Sessions.put(state, session_id, session)
    state = %{state | closed_sessions: Map.delete(state.closed_sessions, session_id)}

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
  defp handle_typed_response(:thread_archive, _entry, _reply, state), do: {:skip, state}
  defp handle_typed_response(_type, _entry, _reply, state), do: {:skip, state}

  defp finish_authenticate(acp_id, result, state) do
    response =
      result
      |> auth_response_result()
      |> then(&Envelope.response(acp_id, &1))

    {:messages, [response], state}
  end

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
        session
        |> Map.update(:accumulated_text, [delta], &[delta | &1])
        |> Map.put(:prompt_activity, true)
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
        session
        |> Map.update(:accumulated_thinking, [delta], &[delta | &1])
        |> Map.put(:prompt_activity, true)
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
    handle_item_started(session_id, item, params, mark_prompt_activity(state, session_id))
  end

  defp handle_notification(
         "item/created",
         %{"item" => %{"type" => "function_call"} = item} = params,
         state
       ) do
    session_id = Sessions.id_from_params(params, state)
    state = mark_prompt_activity(state, session_id)
    {:messages, [Events.tool_call_started(session_id, item)], state}
  end

  defp handle_notification("item/created", _params, state), do: {:skip, state}

  defp handle_notification("item/completed", params, state) do
    session_id = Sessions.id_from_params(params, state)
    state = mark_prompt_activity(state, session_id)
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
    tool_call_id = params["callId"] || params["itemId"] || params["item_id"]

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => tool_call_id,
        "_meta" => Events.terminal_output_delta(tool_call_id, delta)
      })

    {:messages, [notification], state}
  end

  defp handle_notification("item/commandExecution/terminalInteraction", params, state) do
    session_id = Sessions.id_from_params(params, state)

    text =
      params["stdin"] || params["text"] || params["input"] || params["delta"] ||
        Events.format_raw(params)

    tool_call_id = params["callId"] || params["itemId"] || params["item_id"]

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => tool_call_id,
        "_meta" => Events.terminal_output_delta(tool_call_id, "\n#{text}\n"),
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
          "exit_code" => params["exitCode"],
          "formatted_output" => params["output"] || ""
        },
        "_meta" => Events.terminal_exit(params["callId"] || params["itemId"], params["exitCode"])
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

    {:messages, [FileChanges.patch_updated(session_id, params)], state}
  end

  defp handle_notification("item/mcpToolCall/progress", params, state) do
    session_id = Sessions.id_from_params(params, state)
    text = params["message"] || params["delta"] || Events.format_raw(params["progress"] || params)

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => params["callId"] || params["itemId"],
        "status" => "in_progress",
        "_meta" => %{"mcp_output_delta" => %{"data" => String.trim(text)}}
      })

    {:messages, [notification], state}
  end

  defp handle_notification("serverRequest/resolved", params, state) do
    request_id = params["requestId"]

    pending =
      Map.reject(state.pending_client_requests, fn {_acp_id, entry} ->
        entry.codex_id == request_id
      end)

    {elicitation, url_elicitations} = Map.pop(state.url_elicitations, request_id)

    messages =
      case elicitation do
        %{elicitation_id: elicitation_id} ->
          [Envelope.notification("elicitation/complete", %{"elicitationId" => elicitation_id})]

        nil ->
          []
      end

    state = %{
      state
      | pending_client_requests: pending,
        url_elicitations: url_elicitations
    }

    if messages == [], do: {:skip, state}, else: {:messages, messages, state}
  end

  defp handle_notification("account/login/completed", params, %{pending_auth: pending} = state)
       when is_map(pending) do
    if is_nil(params["loginId"]) or params["loginId"] == pending.login_id do
      {_, client_requests} =
        PendingRequests.pop(state.pending_client_requests, pending.client_request_acp_id)

      auth_response =
        if params["success"] == true do
          Envelope.response(pending.auth_acp_id, %{})
        else
          Envelope.error(
            pending.auth_acp_id,
            -32_603,
            params["error"] || "Codex authentication failed"
          )
        end

      messages = [
        Envelope.notification("elicitation/complete", %{
          "elicitationId" => pending.elicitation_id
        }),
        auth_response
      ]

      {:messages, messages,
       %{state | pending_auth: nil, pending_client_requests: client_requests}}
    else
      {:skip, state}
    end
  end

  defp handle_notification("account/login/completed", _params, state), do: {:skip, state}

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
        response =
          case capacity_failure(session, text) do
            {:error, error} ->
              Envelope.error(session[:active_prompt_acp_id], error)

            :ok ->
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

              Envelope.response(session[:active_prompt_acp_id], result)
          end

        [response | messages]
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
    last = token_usage["last"] || %{}
    total = token_usage["total"] || %{}

    usage_data = usage_data(total)
    used = token_total(last)
    size = token_usage["modelContextWindow"]

    state =
      Sessions.update(state, session_id, fn session ->
        session
        |> Map.put(:accumulated_usage, usage_data)
        |> Map.put(:model_context_window, size)
        |> Map.put(:prompt_activity, session[:prompt_activity] == true or positive_usage?(last))
      end)

    if is_integer(used) and is_integer(size) and size > 0 do
      {:messages,
       [
         AdapterEvents.session_update_type(session_id, "usage_update", %{
           "used" => used,
           "size" => size
         })
       ], state}
    else
      {:skip, state}
    end
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

    if session_id do
      notification =
        AdapterEvents.session_info_update(session_id, %{
          "_meta" => %{
            "ex_mcp" => %{
              "adapter" => "codex",
              "warning" => %{"message" => text}
            }
          }
        })

      {:messages, [notification], state}
    else
      {:skip, state}
    end
  end

  defp handle_notification("guardianWarning", params, state),
    do: handle_notification("warning", params, state)

  defp handle_notification("account/rateLimits/updated", params, state) do
    session_id = Sessions.current_id(state)
    rate_limits = params["rateLimits"] || %{}

    state =
      Sessions.update(state, session_id, fn session ->
        Map.put(session, :rate_limits, rate_limits)
      end)

    if session_id do
      notification =
        AdapterEvents.session_info_update(session_id, %{
          "_meta" => %{
            "ex_mcp" => %{
              "adapter" => "codex",
              "rateLimits" => rate_limits
            }
          }
        })

      {:messages, [notification], state}
    else
      {:skip, state}
    end
  end

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

  defp handle_notification("thread/name/updated", params, state) do
    session_id = Sessions.id_from_params(params, state)

    {:messages,
     [AdapterEvents.session_info_update(session_id, %{"title" => params["threadName"] || nil})],
     state}
  end

  defp handle_notification(method, params, state)
       when method in ["thread/archived", "thread/unarchived", "thread/closed"] do
    session_id = Sessions.id_from_params(params, state)

    metadata =
      case method do
        "thread/archived" -> %{"archived" => true}
        "thread/unarchived" -> %{"archived" => false}
        "thread/closed" -> %{"closed" => true}
      end

    {:messages,
     [
       AdapterEvents.session_info_update(session_id, %{
         "_meta" => %{"codex" => metadata}
       })
     ], state}
  end

  defp handle_notification("thread/goal/updated", params, state) do
    session_id = Sessions.id_from_params(params, state)

    {:messages,
     [
       AdapterEvents.session_info_update(session_id, %{
         "_meta" => %{"codex" => %{"goal" => normalize_goal(params["goal"] || params)}}
       })
     ], state}
  end

  defp handle_notification("thread/goal/cleared", params, state) do
    session_id = Sessions.id_from_params(params, state)

    {:messages,
     [
       AdapterEvents.session_info_update(session_id, %{
         "_meta" => %{"codex" => %{"goal" => nil}}
       })
     ], state}
  end

  defp handle_notification("model/rerouted", params, state) do
    session_id = Sessions.id_from_params(params, state)
    from_model = params["fromModel"] || params["from"]
    to_model = params["toModel"] || params["to"]
    reason = params["reason"] || "unknown"

    {:messages,
     [
       AdapterEvents.agent_thought_chunk(
         session_id,
         "Model rerouted from #{from_model} to #{to_model} (#{reason}).\n\n"
       )
     ], state}
  end

  defp handle_notification(method, params, state)
       when method in ["model/verification", "turn/moderationMetadata"] do
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

  defp handle_notification(method, _params, state)
       when method in [
              "remoteControl/status/changed",
              "mcpServer/startupStatus/updated",
              "account/updated",
              "skills/changed",
              "deprecationNotice"
            ],
       do: {:skip, state}

  defp handle_notification(method, _params, state) do
    Logger.debug("[Codex Adapter] Unhandled notification: #{method}")
    {:skip, state}
  end

  # Codex app-server requests that need ACP client interaction.

  defp handle_server_request(codex_id, method, %{"threadId" => session_id}, state)
       when is_map_key(state.closed_sessions, session_id) do
    {:skip_and_write, Protocol.encode_response(codex_id, late_server_request_result(method)),
     state}
  end

  defp handle_server_request(codex_id, method, params, state)
       when method in [
              "item/commandExecution/requestApproval",
              "item/fileChange/requestApproval",
              "execCommandApproval",
              "applyPatchApproval",
              "item/permissions/requestApproval"
            ] do
    request_permission_from_client(codex_id, method, params, state)
  end

  defp handle_server_request(codex_id, "mcpServer/elicitation/request" = method, params, state) do
    mode = normalize_elicitation_mode(params["mode"])

    if mode && client_supports_elicitation?(state, mode) do
      start_elicitation_request(codex_id, params, mcp_elicitation_request(params, mode), state)
    else
      request_permission_from_client(codex_id, method, params, state)
    end
  end

  defp handle_server_request(codex_id, "item/tool/requestUserInput", params, state) do
    cond do
      not client_supports_elicitation?(state, "form") ->
        {:skip_and_write, Protocol.encode_response(codex_id, %{"answers" => %{}}), state}

      Enum.any?(List.wrap(params["questions"]), &(&1["isSecret"] == true)) ->
        Logger.warning(
          "Codex secret user-input request was not forwarded through form elicitation"
        )

        {:skip_and_write, Protocol.encode_response(codex_id, %{"answers" => %{}}), state}

      true ->
        start_user_input_request(codex_id, params, state)
    end
  end

  defp handle_server_request(codex_id, method, _params, state)
       when method in [
              "item/tool/call",
              "account/chatgptAuthTokens/refresh",
              "attestation/generate"
            ] do
    Logger.debug("[Codex Adapter] Rejecting unsupported app-server request: #{method}")

    {:skip_and_write,
     Protocol.encode_error(codex_id, -32_601, "Unsupported app-server request: #{method}"), state}
  end

  defp handle_server_request(codex_id, method, _params, state) do
    Logger.debug("[Codex Adapter] Rejecting unsupported app-server request: #{method}")

    {:skip_and_write,
     Protocol.encode_error(codex_id, -32_601, "Unsupported app-server request: #{method}"), state}
  end

  defp request_permission_from_client(codex_id, method, params, state) do
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

  defp normalize_elicitation_mode("form"), do: "form"
  defp normalize_elicitation_mode("url"), do: "url"
  defp normalize_elicitation_mode(_mode), do: nil

  defp client_supports_elicitation?(state, mode) do
    is_map(get_in(state.client_capabilities, ["elicitation", mode]))
  end

  defp start_elicitation_request(codex_id, params, elicitation_params, state) do
    acp_id = "codex-elicitation-#{System.unique_integer([:positive])}"

    entry = %{
      kind: :elicitation,
      codex_id: codex_id,
      method: "mcpServer/elicitation/request",
      params: params,
      session_id: elicitation_params["sessionId"],
      mode: elicitation_params["mode"],
      elicitation_id: elicitation_params["elicitationId"]
    }

    state = %{
      state
      | pending_client_requests: PendingRequests.put(state.pending_client_requests, acp_id, entry)
    }

    request = Envelope.request("elicitation/create", elicitation_params, acp_id)
    {:messages, [request], state}
  end

  defp mcp_elicitation_request(params, "form") do
    %{
      "sessionId" => params["threadId"] || params["sessionId"],
      "mode" => "form",
      "message" => params["message"] || "Input requested",
      "requestedSchema" => normalize_elicitation_schema(params["requestedSchema"]),
      "_meta" => params["_meta"]
    }
    |> maybe_put("toolCallId", params["toolCallId"] || params["itemId"])
  end

  defp mcp_elicitation_request(params, "url") do
    %{
      "sessionId" => params["threadId"] || params["sessionId"],
      "mode" => "url",
      "message" => params["message"] || "Open the requested URL to continue",
      "url" => params["url"],
      "elicitationId" => params["elicitationId"],
      "_meta" => params["_meta"]
    }
  end

  defp normalize_elicitation_schema(%{} = schema) do
    schema
    |> normalize_elicitation_schema_value()
    |> Map.put("type", "object")
    |> Map.put_new("properties", %{})
  end

  defp normalize_elicitation_schema(_schema), do: %{"type" => "object", "properties" => %{}}

  defp normalize_elicitation_schema_value(value) when is_list(value) do
    Enum.map(value, &normalize_elicitation_schema_value/1)
  end

  defp normalize_elicitation_schema_value(%{} = value) do
    normalized =
      Map.new(value, fn {key, nested} -> {key, normalize_elicitation_schema_value(nested)} end)

    if normalized["type"] == "string" and is_list(normalized["enum"]) and
         is_list(normalized["enumNames"]) and not is_list(normalized["oneOf"]) do
      names = normalized["enumNames"]

      one_of =
        normalized["enum"]
        |> Enum.with_index()
        |> Enum.map(fn {enum_value, index} ->
          title = Enum.at(names, index, enum_value)
          %{"const" => to_string(enum_value), "title" => to_string(title)}
        end)

      normalized
      |> Map.drop(["enum", "enumNames"])
      |> Map.put("oneOf", one_of)
    else
      normalized
    end
  end

  defp normalize_elicitation_schema_value(value), do: value

  defp start_user_input_request(codex_id, params, state) do
    questions =
      params["questions"]
      |> List.wrap()
      |> Enum.filter(fn question ->
        is_map(question) and is_binary(question["id"]) and question["id"] != ""
      end)

    {properties, required, other_fields} = user_input_schema(questions)
    session_id = Sessions.id_from_params(params, state)
    acp_id = "codex-user-input-#{System.unique_integer([:positive])}"

    request_params = %{
      "sessionId" => session_id,
      "toolCallId" => params["itemId"],
      "mode" => "form",
      "message" => user_input_message(params, questions),
      "requestedSchema" =>
        %{"type" => "object", "properties" => properties}
        |> maybe_put("required", if(required == [], do: nil, else: required)),
      "_meta" => %{
        "codexAcp" => %{
          "autoResolutionMs" => params["autoResolutionMs"],
          "isBlocking" => params["isBlocking"]
        }
      }
    }

    entry = %{
      kind: :user_input,
      codex_id: codex_id,
      method: "item/tool/requestUserInput",
      params: params,
      questions: questions,
      other_fields: other_fields,
      session_id: session_id
    }

    state = %{
      state
      | pending_client_requests: PendingRequests.put(state.pending_client_requests, acp_id, entry)
    }

    {:messages, [Envelope.request("elicitation/create", request_params, acp_id)], state}
  end

  defp user_input_schema(questions) do
    question_ids = MapSet.new(questions, & &1["id"])

    Enum.reduce(questions, {%{}, [], %{}}, fn question, {properties, required, other_fields} ->
      id = question["id"]

      if is_binary(id) and id != "" do
        property = user_input_property(question)
        properties = Map.put(properties, id, property)
        has_other_answer = question["isOther"] == true and List.wrap(question["options"]) != []

        {properties, other_fields} =
          if has_other_answer do
            other_id = user_input_other_field_id(id, question_ids)

            property = %{
              "type" => "string",
              "title" => "Other",
              "description" => "Type your own answer instead of choosing an option above.",
              "_meta" => %{
                "codex" => %{
                  "questionId" => id,
                  "isOtherAnswer" => true,
                  "isSecret" => question["isSecret"] == true
                }
              }
            }

            {Map.put(properties, other_id, property), Map.put(other_fields, id, other_id)}
          else
            {properties, other_fields}
          end

        required = if has_other_answer, do: required, else: required ++ [id]
        {properties, required, other_fields}
      else
        {properties, required, other_fields}
      end
    end)
  end

  defp user_input_message(params, [question]) do
    params["message"] || question["question"] || "Input requested"
  end

  defp user_input_message(params, _questions), do: params["message"] || "Input requested"

  defp user_input_other_field_id(question_id, question_ids, suffix \\ "") do
    candidate = question_id <> "__other" <> suffix

    if MapSet.member?(question_ids, candidate) do
      next_suffix =
        if suffix == "", do: "1", else: Integer.to_string(String.to_integer(suffix) + 1)

      user_input_other_field_id(question_id, question_ids, next_suffix)
    else
      candidate
    end
  end

  defp user_input_property(question) do
    base = %{
      "type" => "string",
      "title" => question["header"] || "Input",
      "description" => question["question"],
      "_meta" => %{
        "codex" => %{
          "isOther" => question["isOther"] == true,
          "isSecret" => question["isSecret"] == true
        }
      }
    }

    case question["options"] do
      options when is_list(options) and options != [] ->
        Map.put(base, "oneOf", Enum.map(options, &user_input_option/1))

      _no_options ->
        base
    end
  end

  defp user_input_option(option) do
    %{"const" => option["label"], "title" => option["label"]}
    |> maybe_put("description", option["description"])
  end

  defp client_request_result(%{kind: :user_input} = entry, response, state) do
    {:native, user_input_response(entry, response), state}
  end

  defp client_request_result(%{kind: :elicitation} = entry, response, state) do
    {result, accepted?} = elicitation_response(response)

    state =
      if accepted? and entry.mode == "url" and is_binary(entry.elicitation_id) do
        update_in(state.url_elicitations, fn elicitations ->
          Map.put(elicitations, entry.codex_id, %{
            session_id: entry.session_id,
            elicitation_id: entry.elicitation_id
          })
        end)
      else
        state
      end

    {:native, result, state}
  end

  defp client_request_result(%{kind: :auth_url} = entry, response, state) do
    case elicitation_response(response) do
      {_result, true} ->
        {:defer, put_in(state.pending_auth[:consented], true)}

      {_result, false} ->
        {cancel_id, state} = next_request_id(state)

        cancel =
          Protocol.encode_request(cancel_id, Protocol.method(:account_login_cancel), %{
            "loginId" => entry.login_id
          })

        error =
          Envelope.error(entry.auth_acp_id, -32_603, "Codex authentication was cancelled")

        {:messages_and_write, [error], cancel, %{state | pending_auth: nil}}
    end
  end

  defp client_request_result(entry, response, state) do
    {:native, permission_response(entry, response), state}
  end

  defp elicitation_response(%{"result" => %{"action" => "accept"} = result}) do
    content = result["content"]

    if is_nil(content) or is_map(content) do
      {result, true}
    else
      {%{"action" => "cancel"}, false}
    end
  end

  defp elicitation_response(%{"result" => %{"action" => action} = result})
       when action in ["decline", "cancel"],
       do: {result, false}

  defp elicitation_response(_response), do: {%{"action" => "cancel"}, false}

  defp user_input_response(entry, %{"result" => %{"action" => "accept", "content" => content}})
       when is_map(content) do
    answers =
      Enum.reduce(entry.questions, %{}, fn question, answers ->
        id = question["id"]
        custom = content[entry.other_fields[id]]
        value = if is_binary(custom) and String.trim(custom) != "", do: custom, else: content[id]

        values =
          case value do
            value when is_binary(value) and value != "" -> [value]
            values when is_list(values) -> Enum.filter(values, &is_binary/1)
            _missing -> []
          end

        if values == [], do: answers, else: Map.put(answers, id, %{"answers" => values})
      end)

    %{"answers" => answers}
  end

  defp user_input_response(_entry, _response), do: %{"answers" => %{}}

  defp late_server_request_result("item/tool/requestUserInput"), do: %{"answers" => %{}}
  defp late_server_request_result("mcpServer/elicitation/request"), do: %{"action" => "cancel"}
  defp late_server_request_result(method), do: codex_cancel_response(method)

  # Item completion / replay helpers

  defp handle_item_started(session_id, item, params, state) do
    case Events.item_type(item) do
      type when type in ["function_call", "functionCall"] ->
        {:messages, [Events.tool_call_started(session_id, item)], state}

      "commandExecution" ->
        tool_call_id = Events.item_id(params, item)

        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => tool_call_id,
            "title" => Events.command_title(item["command"]),
            "kind" => "execute",
            "status" => Events.normalize_tool_status(item["status"], "in_progress"),
            "rawInput" => %{"command" => item["command"], "cwd" => item["cwd"]},
            "content" => [%{"type" => "terminal", "terminalId" => tool_call_id}],
            "_meta" => Events.terminal_info(tool_call_id, item["cwd"])
          })

        {:messages, [notification], state}

      "fileChange" ->
        {:messages, [FileChanges.started(session_id, params, item)], state}

      "mcpToolCall" ->
        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => Events.mcp_tool_title(item),
            "kind" => "execute",
            "status" => Events.normalize_tool_status(item["status"], "in_progress"),
            "rawInput" => Events.mcp_raw_input(item),
            "_meta" => %{"is_mcp_tool_call" => true}
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
            "title" => Events.web_search_title(item),
            "kind" => "search",
            "status" => "in_progress",
            "rawInput" => item
          })

        {:messages, [notification], state}

      "imageView" ->
        path = item["path"] || ""

        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => "View Image #{path}",
            "kind" => "read",
            "status" => "completed",
            "content" => [
              %{
                "type" => "content",
                "content" => %{"type" => "resource_link", "name" => path, "uri" => path}
              }
            ],
            "locations" => [%{"path" => path}],
            "rawInput" => %{"path" => path}
          })

        {:messages, [notification], state}

      "imageGeneration" ->
        notification =
          AdapterEvents.tool_call(session_id, %{
            "toolCallId" => Events.item_id(params, item),
            "title" => "Image generation",
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
    tool_call_id = item["id"]

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => tool_call_id,
        "status" => Events.normalize_tool_status(item["status"], "completed"),
        "rawOutput" => %{
          "exit_code" => item["exitCode"],
          "formatted_output" => item["aggregatedOutput"] || ""
        },
        "_meta" => Events.terminal_exit(tool_call_id, item["exitCode"])
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
    {:messages, [FileChanges.completed(session_id, item)], state}
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
        "rawInput" => Events.mcp_raw_input(item),
        "rawOutput" => Events.mcp_raw_output(item) || output
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
        "title" => Events.web_search_title(item),
        "status" => "completed",
        "rawInput" => item
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "imageView"} = item, state) do
    handle_item_started(session_id, item, %{}, state)
  end

  defp handle_item_completed(session_id, %{"type" => "imageGeneration"} = item, state) do
    content =
      []
      |> maybe_add_image_revised_prompt(item["revisedPrompt"])
      |> maybe_add_generated_image(item)

    notification =
      AdapterEvents.tool_call_update(session_id, %{
        "toolCallId" => item["id"],
        "status" => Events.normalize_tool_status(item["status"], "completed"),
        "content" => content,
        "rawOutput" => item
      })

    {:messages, [notification], state}
  end

  defp handle_item_completed(session_id, %{"type" => "contextCompaction"} = _item, state) do
    {:messages, [AdapterEvents.agent_message_chunk(session_id, "Context compacted\n")], state}
  end

  defp handle_item_completed(_session_id, _item, state), do: {:skip, state}

  defp maybe_add_image_revised_prompt(content, prompt) when is_binary(prompt) and prompt != "" do
    content ++ [Events.tool_text_content("Revised prompt: #{prompt}")]
  end

  defp maybe_add_image_revised_prompt(content, _prompt), do: content

  defp maybe_add_generated_image(content, %{"result" => result} = item)
       when is_binary(result) and result != "" do
    image =
      %{"type" => "image", "data" => result, "mimeType" => "image/png"}
      |> maybe_put("uri", item["savedPath"])

    content ++ [%{"type" => "content", "content" => image}]
  end

  defp maybe_add_generated_image(content, _item), do: content

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

  defp status_message(session_id, session, state) do
    mode_id = session[:mode_id] || state.mode_id || Config.default_mode()
    profile = status_mode_profile(mode_id)
    model = model_id_for_session(session, state) || session[:model] || state.model || "default"
    cwd = session[:cwd] || Keyword.get(state.opts, :cwd) || ""
    usage = format_usage(session[:accumulated_usage])

    [
      "**Model:** #{model}",
      "**Directory:** #{cwd}",
      "**Approval:** #{profile.approval}",
      "**Sandbox:** #{profile.sandbox}",
      "**Session:** `#{session_id}`",
      "",
      "**Token usage:** #{usage}"
    ]
    |> Enum.join("  \n")
  end

  defp status_mode_profile("read-only"), do: %{approval: "on-request", sandbox: "read-only"}

  defp status_mode_profile("agent-full-access"),
    do: %{approval: "never", sandbox: "danger-full-access"}

  defp status_mode_profile(_mode_id), do: %{approval: "on-request", sandbox: "workspace-write"}

  defp format_usage(nil), do: "data not available yet"

  defp format_usage(%{"inputTokens" => input, "outputTokens" => output} = usage) do
    cached = usage["cachedInputTokens"] || 0
    total = input + output
    "#{total} total (#{input} input + #{cached} cached input, #{output} output)"
  end

  defp format_usage(_usage), do: "data not available yet"

  defp mark_prompt_activity(state, session_id) do
    Sessions.update(state, session_id, &Map.put(&1, :prompt_activity, true))
  end

  defp capacity_failure(session, text) do
    if text == "" and session[:prompt_activity] != true and
         not positive_usage?(session[:accumulated_usage]) and
         rate_limit_exhausted?(session[:rate_limits]) do
      {:error,
       %{
         "code" => -32_029,
         "message" => "Codex rate limit exhausted before the model produced a response",
         "data" => %{
           "kind" => "rate_limit_exhausted",
           "provider" => "codex",
           "rateLimits" => session[:rate_limits]
         }
       }}
    else
      :ok
    end
  end

  defp rate_limit_exhausted?(rate_limits) when is_map(rate_limits) do
    reached_type =
      rate_limits["rateLimitReachedType"] || rate_limits["rate_limit_reached_type"]

    reached_type not in [nil, ""] or
      (Enum.any?([rate_limits["primary"], rate_limits["secondary"]], &window_exhausted?/1) and
         not credits_available?(rate_limits["credits"]))
  end

  defp rate_limit_exhausted?(_rate_limits), do: false

  defp window_exhausted?(window) when is_map(window) do
    used_percent = window["usedPercent"] || window["used_percent"]
    is_number(used_percent) and used_percent >= 100
  end

  defp window_exhausted?(_window), do: false

  defp credits_available?(credits) when is_map(credits) do
    credits["unlimited"] == true or credits["hasCredits"] == true or
      credits["has_credits"] == true
  end

  defp credits_available?(_credits), do: false

  defp positive_usage?(usage) when is_map(usage) do
    case token_total(usage) do
      total when is_integer(total) -> total > 0
      _other -> false
    end
  end

  defp positive_usage?(_usage), do: false

  defp usage_data(token_counts) when is_map(token_counts) do
    %{
      "inputTokens" => token_counts["inputTokens"] || 0,
      "outputTokens" => token_counts["outputTokens"] || 0,
      "cachedInputTokens" => token_counts["cachedInputTokens"] || 0
    }
  end

  defp usage_data(_token_counts), do: usage_data(%{})

  defp token_total(%{"totalTokens" => total}) when is_integer(total), do: total

  defp token_total(token_counts) when is_map(token_counts) do
    input = token_counts["inputTokens"] || 0
    output = token_counts["outputTokens"] || 0

    if is_integer(input) and is_integer(output), do: input + output
  end

  defp token_total(_token_counts), do: nil

  defp normalize_goal(%{"objective" => objective} = goal) when is_binary(objective) do
    %{
      "objective" => String.trim(objective),
      "status" => goal["status"],
      "tokenBudget" => goal["tokenBudget"]
    }
  end

  defp normalize_goal(goal), do: goal

  defp auth_response_result(%{"authUrl" => _} = result) do
    %{"_meta" => %{"ex_mcp" => %{"codex" => %{"auth" => result}}}}
  end

  defp auth_response_result(%{"verificationUrl" => _} = result) do
    %{"_meta" => %{"ex_mcp" => %{"codex" => %{"auth" => result}}}}
  end

  defp auth_response_result(_result), do: %{}

  defp start_auth_url_elicitation(auth_acp_id, result, state) do
    login_id = result["loginId"] || "codex-login-#{System.unique_integer([:positive])}"
    elicitation_id = to_string(login_id)
    client_request_acp_id = "codex-auth-elicitation-#{System.unique_integer([:positive])}"

    request =
      Envelope.request(
        "elicitation/create",
        %{
          "requestId" => auth_acp_id,
          "mode" => "url",
          "elicitationId" => elicitation_id,
          "url" => auth_url(result),
          "message" => auth_url_message(result)
        },
        client_request_acp_id
      )

    entry = %{
      kind: :auth_url,
      codex_id: nil,
      auth_acp_id: auth_acp_id,
      client_request_acp_id: client_request_acp_id,
      login_id: login_id,
      session_id: nil
    }

    pending_auth = %{
      auth_acp_id: auth_acp_id,
      client_request_acp_id: client_request_acp_id,
      elicitation_id: elicitation_id,
      login_id: login_id,
      consented: false
    }

    state = %{
      state
      | pending_auth: pending_auth,
        pending_client_requests:
          PendingRequests.put(state.pending_client_requests, client_request_acp_id, entry)
    }

    {:messages, [request], state}
  end

  defp auth_url(result), do: result["verificationUrl"] || result["authUrl"]

  defp auth_url_message(result) do
    case result["userCode"] do
      code when is_binary(code) and code != "" ->
        "Sign in to ChatGPT and enter this code: #{code}"

      _no_code ->
        "Sign in to ChatGPT to continue."
    end
  end

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
    |> maybe_add_fast_mode_option(session, state)
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

  defp maybe_add_fast_mode_option(options, session, state) do
    if model_supports_fast?(current_model(session, state)) do
      options ++
        [
          %{
            "id" => "fast-mode",
            "name" => "Fast mode",
            "description" => "1.5x speed, increased usage",
            "type" => "select",
            "category" => "model_config",
            "currentValue" => if(session[:fast_mode_enabled], do: "on", else: "off"),
            "options" => [
              %{
                "value" => "off",
                "name" => "Off",
                "description" => "Default speed, normal usage"
              },
              %{"value" => "on", "name" => "On", "description" => "1.5x speed, increased usage"}
            ]
          }
        ]
    else
      options
    end
  end

  defp config_update(%{"configId" => "mode", "value" => value}, _session, _state)
       when is_binary(value) do
    case Config.normalize_requested_mode(value) do
      {:ok, mode_id} ->
        {:ok,
         %{
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

  defp config_update(%{"configId" => "fast-mode", "value" => value}, _session, _state) do
    case normalize_fast_mode_value(value) do
      {:ok, enabled} ->
        {:ok, %{session: %{fast_mode_enabled: enabled}, state: %{}}}

      :error ->
        {:error, "Unsupported fast-mode value: #{inspect(value)}"}
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
      "additionalSpeedTiers" => model["additionalSpeedTiers"] || [],
      "inputModalities" => model["inputModalities"] || ["text", "image"],
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

  defp normalize_fast_mode_value(value) when value in [true, "on"], do: {:ok, true}
  defp normalize_fast_mode_value(value) when value in [false, "off"], do: {:ok, false}
  defp normalize_fast_mode_value(_value), do: :error

  defp service_tier_for_session(session, state) do
    if session[:fast_mode_enabled] && model_supports_fast?(current_model(session, state)) do
      "fast"
    end
  end

  defp model_supports_fast?(%{"additionalSpeedTiers" => tiers}) when is_list(tiers),
    do: "fast" in tiers

  defp model_supports_fast?(_model), do: false

  defp model_provider(%{gateway_config: %{model_provider: provider}}), do: provider

  defp model_provider(state),
    do: Keyword.get(state.opts, :model_provider) || System.get_env("MODEL_PROVIDER")

  defp resume_model_provider(state), do: model_provider(state) || "openai"

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

  defp auth_request_params("chat-gpt", _params, _state), do: {:ok, %{"type" => "chatgpt"}}
  defp auth_request_params("chatgpt", _params, _state), do: {:ok, %{"type" => "chatgpt"}}

  defp auth_request_params("chat-gpt-device-code", _params, state) do
    if client_supports_elicitation?(state, "url") do
      {:ok, %{"type" => "chatgptDeviceCode"}}
    else
      {:error, "ChatGPT device-code authentication requires ACP URL elicitation support"}
    end
  end

  defp auth_request_params("api-key", params, state) do
    case explicit_api_key_from_request(params) do
      nil ->
        explicit_api_key(state, ["CODEX_API_KEY", "OPENAI_API_KEY"])

      value ->
        {:ok, %{"type" => "apiKey", "apiKey" => value}}
    end
  end

  defp auth_request_params("gateway", params, _state) do
    meta = get_in(params, ["_meta", "gateway"])

    if is_map(meta) do
      gateway_auth_params(meta)
    else
      {:error, "gateway auth requires adapter_opts[:gateway]"}
    end
  end

  defp auth_request_params("codex-api-key", _params, state) do
    explicit_api_key(state, ["CODEX_API_KEY"])
  end

  defp auth_request_params("openai-api-key", _params, state) do
    explicit_api_key(state, ["OPENAI_API_KEY"])
  end

  defp auth_request_params(nil, _params, _state), do: {:error, "authenticate requires methodId"}

  defp auth_request_params(method_id, _params, _state),
    do: {:error, "Unsupported Codex auth method: #{method_id}"}

  defp explicit_api_key(state, names) do
    case Enum.find_value(names, &explicit_env_value(state.opts, &1)) do
      nil ->
        {:error,
         "#{Enum.join(names, " or ")} must be supplied explicitly in adapter_opts[:env] before authenticate"}

      value ->
        {:ok, %{"type" => "apiKey", "apiKey" => value}}
    end
  end

  defp explicit_api_key_from_request(params) do
    get_in(params, ["_meta", "api-key", "apiKey"])
  end

  defp gateway_auth_params(%{"baseUrl" => base_url} = meta) when is_binary(base_url) do
    provider_name =
      case meta["providerName"] do
        name when is_binary(name) and name != "" -> name
        _ -> "User-provided gateway"
      end

    headers = Map.merge(%{"X-Client-Feature-ID" => "codex"}, meta["headers"] || %{})

    {:ok,
     {:gateway,
      %{
        model_provider: "custom-gateway",
        provider_config: %{
          "name" => provider_name,
          "base_url" => base_url,
          "http_headers" => headers,
          "wire_api" => "responses"
        }
      }}}
  end

  defp gateway_auth_params(_meta), do: {:error, "gateway auth requires baseUrl"}

  defp explicit_env_value(opts, name) do
    opts
    |> Keyword.get(:env, [])
    |> NameValue.map()
    |> Map.get(name)
  end

  # Session config / MCP mapping

  defp session_config(params, cwd, state) do
    with :ok <- authorize_workspace(cwd, :cwd, state),
         {:ok, additional_directories} <- additional_directories(params, cwd),
         :ok <- authorize_additional_directories(additional_directories, cwd, state),
         {:ok, mcp_config} <- mcp_config(params["mcpServers"], cwd, state) do
      config =
        state.opts
        |> codex_config()
        |> merge_gateway_config(state.gateway_config)
        |> maybe_merge_trusted_projects(cwd, additional_directories, state.opts)
        |> merge_sandbox_workspace_roots(additional_directories)
        |> merge_config(mcp_config)

      {:ok, empty_to_nil(config), additional_directories}
    end
  end

  defp maybe_merge_trusted_projects(config, cwd, additional_directories, opts) do
    if Keyword.get(opts, :trust_authorized_workspaces, false) do
      merge_trusted_projects(config, cwd, additional_directories)
    else
      config
    end
  end

  defp codex_config(opts) do
    case Keyword.get(opts, :codex_config) || System.get_env("CODEX_CONFIG") do
      config when is_map(config) ->
        config

      config when is_binary(config) and config != "" ->
        case Jason.decode(config) do
          {:ok, decoded} when is_map(decoded) -> decoded
          _ -> %{}
        end

      _ ->
        %{}
    end
  end

  defp merge_gateway_config(config, nil), do: config

  defp merge_gateway_config(config, %{
         model_provider: model_provider,
         provider_config: provider_config
       }) do
    providers =
      config
      |> Map.get("model_providers", %{})
      |> case do
        providers when is_map(providers) -> providers
        _ -> %{}
      end
      |> Map.put(model_provider, provider_config)

    Map.put(config, "model_providers", providers)
  end

  defp merge_trusted_projects(config, cwd, additional_directories) do
    roots =
      [cwd | additional_directories]
      |> Enum.filter(&(is_binary(&1) and &1 != ""))
      |> Enum.uniq()

    if roots == [] do
      config
    else
      projects =
        config
        |> Map.get("projects", %{})
        |> case do
          projects when is_map(projects) -> projects
          _ -> %{}
        end
        |> Map.merge(Map.new(roots, &{&1, %{"trust_level" => "trusted"}}))

      Map.put(config, "projects", projects)
    end
  end

  defp merge_sandbox_workspace_roots(config, []), do: config

  defp merge_sandbox_workspace_roots(config, additional_directories) do
    sandbox =
      config
      |> Map.get("sandbox_workspace_write", %{})
      |> case do
        sandbox when is_map(sandbox) -> sandbox
        _ -> %{}
      end

    roots =
      sandbox
      |> Map.get("writable_roots", [])
      |> List.wrap()
      |> Enum.filter(&is_binary/1)
      |> Enum.concat(additional_directories)
      |> Enum.uniq()

    Map.put(config, "sandbox_workspace_write", Map.put(sandbox, "writable_roots", roots))
  end

  defp merge_config(config, nil), do: config
  defp merge_config(config, mcp_config), do: Map.merge(config, mcp_config)

  defp empty_to_nil(config) when map_size(config) == 0, do: nil
  defp empty_to_nil(config), do: config

  defp additional_directories(params, cwd) do
    raw = params["additionalDirectories"] || get_in(params, ["_meta", "additionalRoots"])

    cond do
      is_nil(raw) ->
        {:ok, []}

      not is_list(raw) ->
        {:error, "additionalDirectories must be a list of absolute paths"}

      true ->
        raw
        |> Enum.reduce_while({:ok, [], MapSet.new([cwd])}, fn
          directory, {:ok, acc, seen} when is_binary(directory) ->
            directory = String.trim(directory)

            cond do
              directory == "" ->
                {:halt, {:error, "additionalDirectories entries must not be empty"}}

              Path.type(directory) != :absolute ->
                {:halt, {:error, "additionalDirectories entries must be absolute paths"}}

              MapSet.member?(seen, directory) ->
                {:cont, {:ok, acc, seen}}

              true ->
                {:cont, {:ok, acc ++ [directory], MapSet.put(seen, directory)}}
            end

          _directory, _acc ->
            {:halt, {:error, "additionalDirectories entries must be strings"}}
        end)
        |> case do
          {:ok, dirs, _seen} -> {:ok, dirs}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  defp authorize_optional_workspace(nil, _kind, _state), do: :ok
  defp authorize_optional_workspace(path, kind, state), do: authorize_workspace(path, kind, state)

  defp session_list_cwd(params, state) do
    cond do
      is_binary(params["cwd"]) -> params["cwd"]
      Keyword.get(state.opts, :allow_unscoped_session_list, false) -> nil
      true -> Keyword.get(state.opts, :cwd, File.cwd!())
    end
  end

  defp authorize_workspace(path, kind, state) do
    if is_binary(path) and path != "" and Path.type(path) == :absolute do
      context = %{kind: kind, adapter: __MODULE__}

      case Keyword.get(state.opts, :authorize_workspace) do
        callback when is_function(callback, 2) ->
          callback
          |> safe_authorize(path, context)
          |> authorization_result("Workspace path is not authorized")

        callback when is_function(callback, 1) ->
          callback
          |> safe_authorize(path)
          |> authorization_result("Workspace path is not authorized")

        nil ->
          if within_workspace_roots?(path, state.opts),
            do: :ok,
            else: {:error, "Workspace path is not authorized"}

        _invalid ->
          {:error, "Invalid workspace authorization callback"}
      end
    else
      {:error, "Workspace paths must be absolute"}
    end
  end

  defp authorize_additional_directories(directories, cwd, state) do
    Enum.reduce_while(directories, :ok, fn directory, :ok ->
      case authorize_workspace(directory, {:additional_directory, cwd}, state) do
        :ok -> {:cont, :ok}
        {:error, _reason} = error -> {:halt, error}
      end
    end)
  end

  defp within_workspace_roots?(path, opts) do
    roots =
      case Keyword.get(opts, :workspace_roots) do
        roots when is_list(roots) -> roots
        nil -> [Keyword.get(opts, :cwd, File.cwd!())]
        root -> [root]
      end

    Enum.any?(roots, fn
      root when is_binary(root) and root != "" ->
        Path.type(root) == :absolute and WorkspacePath.within?(path, root)

      _invalid ->
        false
    end)
  end

  defp authorize_mcp_server(server, cwd, state) do
    context = %{cwd: cwd, transport: server["type"], adapter: __MODULE__}

    result =
      case Keyword.get(state.opts, :authorize_mcp_server) do
        callback when is_function(callback, 2) -> safe_authorize(callback, server, context)
        callback when is_function(callback, 1) -> safe_authorize(callback, server)
        nil -> trusted_mcp_server?(server, Keyword.get(state.opts, :trusted_mcp_servers, []))
        _invalid -> false
      end

    authorization_result(result, "MCP server is not authorized")
  end

  defp trusted_mcp_server?(_server, :all), do: true

  defp trusted_mcp_server?(server, trusted) when is_list(trusted) do
    Enum.any?(trusted, fn
      trusted_server when is_map(trusted_server) -> trusted_server == server
      _other -> false
    end)
  end

  defp trusted_mcp_server?(_server, _trusted), do: false

  defp safe_authorize(callback, value, context) do
    callback.(value, context)
  rescue
    exception ->
      Logger.warning("Codex authorization callback failed", error_class: exception.__struct__)
      false
  catch
    _kind, _reason -> false
  end

  defp safe_authorize(callback, value) do
    callback.(value)
  rescue
    exception ->
      Logger.warning("Codex authorization callback failed", error_class: exception.__struct__)
      false
  catch
    _kind, _reason -> false
  end

  defp authorization_result(result, _message) when result in [:ok, true], do: :ok
  defp authorization_result({:ok, _value}, _message), do: :ok
  defp authorization_result(_result, message), do: {:error, message}

  defp validate_http_mcp_server(server) do
    uri = if is_binary(server["url"]), do: URI.parse(server["url"]), else: %URI{}

    if valid_mcp_name?(server["name"]) and uri.scheme in ["http", "https"] and
         is_binary(uri.host) and uri.host != "" and valid_name_value_list?(server["headers"]) do
      :ok
    else
      {:error, "Invalid HTTP MCP server configuration"}
    end
  end

  defp validate_stdio_mcp_server(server) do
    if valid_mcp_name?(server["name"]) and is_binary(server["command"]) and
         server["command"] != "" and Path.type(server["command"]) == :absolute and
         is_list(server["args"]) and Enum.all?(server["args"], &is_binary/1) and
         valid_name_value_list?(server["env"]) do
      :ok
    else
      {:error, "Invalid stdio MCP server configuration"}
    end
  end

  defp valid_mcp_name?(name), do: is_binary(name) and String.trim(name) != ""

  defp valid_name_value_list?(values) when is_list(values) do
    Enum.all?(values, fn
      %{"name" => name, "value" => value} -> is_binary(name) and is_binary(value)
      {name, value} -> is_binary(name) and is_binary(value)
      _other -> false
    end)
  end

  defp valid_name_value_list?(_values), do: false

  defp mcp_config(nil, _cwd, _state), do: {:ok, nil}
  defp mcp_config([], _cwd, _state), do: {:ok, nil}

  defp mcp_config(servers, cwd, state) when is_list(servers) do
    Enum.reduce_while(servers, {:ok, %{}}, fn server, {:ok, acc} ->
      case mcp_server_config(server, cwd, state) do
        {:ok, {name, _config}} when is_map_key(acc, name) ->
          {:halt, {:error, "MCP server names must be unique"}}

        {:ok, {name, config}} ->
          {:cont, {:ok, Map.put(acc, name, config)}}

        {:error, reason} ->
          {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:ok, config} when map_size(config) == 0 -> {:ok, nil}
      {:ok, config} -> {:ok, %{"mcp_servers" => config}}
      {:error, reason} -> {:error, reason}
    end
  end

  defp mcp_config(_servers, _cwd, _state), do: {:error, "mcpServers must be a list"}

  defp mcp_server_config(%{"type" => "http"} = server, cwd, state) do
    with :ok <- validate_http_mcp_server(server),
         :ok <- authorize_mcp_server(server, cwd, state) do
      name = sanitize_mcp_server_name(server["name"])

      {:ok,
       {name,
        %{}
        |> Map.put("url", server["url"])
        |> maybe_put("http_headers", headers_to_map(server["headers"]))}}
    end
  end

  defp mcp_server_config(%{"type" => "stdio"} = server, cwd, state) do
    with :ok <- validate_stdio_mcp_server(server),
         :ok <- authorize_mcp_server(server, cwd, state) do
      name = sanitize_mcp_server_name(server["name"])

      {:ok,
       {name,
        %{}
        |> Map.put("command", server["command"])
        |> maybe_put("args", server["args"])
        |> maybe_put("env", env_to_map(server["env"]))}}
    end
  end

  defp mcp_server_config(%{"type" => "sse"}, _cwd, _state),
    do: {:error, "Codex doesn't support MCP SSE transport protocol"}

  defp mcp_server_config(%{"type" => "acp"}, _cwd, _state),
    do: {:error, "Codex doesn't support MCP ACP transport protocol"}

  defp mcp_server_config(server, cwd, state) when is_map(server) do
    if Map.has_key?(server, "command") do
      mcp_server_config(Map.put(server, "type", "stdio"), cwd, state)
    else
      {:error, "Unsupported MCP server transport"}
    end
  end

  defp mcp_server_config(_server, _cwd, _state), do: {:error, "Invalid MCP server"}

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

  defp permission_tool_kind("item/fileChange/requestApproval"), do: "edit"
  defp permission_tool_kind("applyPatchApproval"), do: "edit"

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

  defp decision_to_option(decision) when is_map(decision) do
    %{
      "optionId" => @structured_decision_prefix <> Jason.encode!(decision),
      "name" => structured_decision_name(decision),
      "kind" => structured_decision_kind(decision)
    }
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

  defp permission_response(
         %{
           method: method,
           params: %{"availableDecisions" => available_decisions}
         },
         %{"result" => %{"outcome" => %{"optionId" => option_id}}}
       ) do
    case selected_structured_decision(option_id, available_decisions) do
      {:ok, decision} -> structured_permission_response(method, decision)
      :plain -> codex_decision_response(method, option_id)
      :error -> invalid_structured_permission_response(method)
    end
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

  defp selected_structured_decision(option_id, available_decisions) do
    if structured_decision_option_id?(option_id) do
      with true <- is_list(available_decisions),
           {:ok, decision} <- decode_structured_decision(option_id),
           true <- Enum.member?(available_decisions, decision) do
        {:ok, decision}
      else
        _ -> :error
      end
    else
      :plain
    end
  end

  defp structured_permission_response("item/permissions/requestApproval", decision),
    do: decision

  defp structured_permission_response(_method, decision), do: %{"decision" => decision}

  defp invalid_structured_permission_response("item/permissions/requestApproval"),
    do: codex_cancel_response("item/permissions/requestApproval")

  defp invalid_structured_permission_response(_method), do: %{"decision" => "decline"}

  defp app_server_decision(option_id) do
    cond do
      always_option?(option_id) -> "acceptForSession"
      allow_option?(option_id) -> "accept"
      String.contains?(to_string(option_id), "cancel") -> "cancel"
      true -> "decline"
    end
  end

  defp structured_decision_option_id?(option_id) when is_binary(option_id),
    do: String.starts_with?(option_id, @structured_decision_prefix)

  defp structured_decision_option_id?(_option_id), do: false

  defp decode_structured_decision(option_id) when is_binary(option_id) do
    with true <- String.starts_with?(option_id, @structured_decision_prefix),
         encoded <- String.replace_prefix(option_id, @structured_decision_prefix, ""),
         {:ok, decision} when is_map(decision) <- Jason.decode(encoded) do
      {:ok, decision}
    else
      _ -> :error
    end
  end

  defp structured_decision_name(decision) do
    case Map.keys(decision) do
      [name] when is_binary(name) -> name |> Macro.underscore() |> humanize_option()
      _ -> "Codex Decision"
    end
  end

  defp structured_decision_kind(%{"acceptWithExecpolicyAmendment" => _amendment}),
    do: "allow_always"

  defp structured_decision_kind(%{"applyNetworkPolicyAmendment" => amendment}) do
    policy = amendment["network_policy_amendment"] || amendment["networkPolicyAmendment"] || %{}

    case policy["action"] do
      "allow" -> "allow_always"
      "deny" -> "reject_always"
      _ -> "reject_once"
    end
  end

  defp structured_decision_kind(%{"permissions" => permissions, "scope" => scope})
       when is_map(permissions) do
    if scope == "session", do: "allow_always", else: "allow_once"
  end

  defp structured_decision_kind(decision) when is_map(decision) do
    case Map.keys(decision) do
      [name] when is_binary(name) -> structured_option_kind(name)
      _ -> "reject_once"
    end
  end

  defp structured_option_kind(name) do
    normalized = String.downcase(name)

    cond do
      String.contains?(normalized, ["deny", "decline", "reject", "cancel"]) ->
        "reject_once"

      String.contains?(normalized, ["accept", "allow", "approve"]) ->
        if String.contains?(normalized, ["session", "amendment", "grant"]),
          do: "allow_always",
          else: "allow_once"

      true ->
        "reject_once"
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
    do: [text_input(prompt)]

  defp extract_input_items(blocks) when is_list(blocks) do
    items =
      Enum.flat_map(blocks, fn
        %{"type" => "text", "text" => text} ->
          [text_input(text)]

        %{"type" => "image"} = img ->
          [image_input(img)]

        %{"type" => "resource_link"} = resource ->
          [text_input(format_uri_as_link(resource["name"], resource["uri"]))]

        %{"type" => "resource", "resource" => %{"text" => text, "uri" => uri}} ->
          [
            text_input(
              "#{format_uri_as_link(nil, uri)}\n<context ref=\"#{uri}\">\n#{text}\n</context>"
            )
          ]

        %{
          "type" => "resource",
          "resource" => %{"blob" => blob, "mimeType" => mime_type, "uri" => uri}
        } ->
          if image_mime_type?(mime_type) do
            [%{"type" => "image", "url" => "data:#{mime_type};base64,#{blob}"}]
          else
            mime_type = mime_type || "application/octet-stream"

            context =
              [
                format_uri_as_link(nil, uri),
                ~s(<context ref="#{uri}" mimeType="#{mime_type}" encoding="base64">),
                blob,
                "</context>"
              ]
              |> Enum.join("\n")

            [
              text_input(context)
            ]
          end

        _ ->
          []
      end)

    if items == [], do: [text_input("")], else: items
  end

  defp extract_input_items(_), do: [text_input("")]

  defp text_input(text),
    do: %{"type" => "text", "text" => to_string(text || ""), "text_elements" => []}

  defp image_input(%{"uri" => uri}) when is_binary(uri) and uri != "" do
    %{"type" => "image", "url" => uri}
  end

  defp image_input(%{"data" => data} = img) do
    mime_type = img["mimeType"] || "image/png"
    %{"type" => "image", "url" => "data:#{mime_type};base64,#{data}"}
  end

  defp image_input(_img), do: %{"type" => "image", "url" => ""}

  defp image_mime_type?(mime_type) when is_binary(mime_type),
    do: String.starts_with?(mime_type, "image/")

  defp image_mime_type?(_mime_type), do: false

  defp format_uri_as_link(name, uri) when is_binary(name) and name != "", do: "[@#{name}](#{uri})"

  defp format_uri_as_link(_name, "file://" <> path = uri) do
    name = path |> String.split("/") |> List.last()
    "[@#{name}](#{uri})"
  end

  defp format_uri_as_link(_name, uri) when is_binary(uri), do: uri
  defp format_uri_as_link(_name, nil), do: ""

  # General helpers

  defp next_request_id(%{next_id: id} = state) do
    {id, next} = Protocol.next_id(id)
    {id, %{state | next_id: next}}
  end

  defp track_request(state, id, type, acp_id, meta \\ %{}) do
    entry = Protocol.request_entry(type, acp_id, meta)
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

  defp maybe_put(map, key, value), do: Maps.put_non_empty(map, key, value)

  defp reject_nil_values(map) do
    Map.reject(map, fn {_key, value} -> is_nil(value) end)
  end
end
