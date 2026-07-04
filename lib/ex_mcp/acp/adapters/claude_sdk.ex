defmodule ExMCP.ACP.Adapters.ClaudeSDK do
  @moduledoc """
  Adapter for Claude Code using the Claude Agent SDK process protocol.

  This adapter launches Claude Code with the same stream-json flags used by
  `@anthropic-ai/claude-agent-sdk`, including the SDK entrypoint environment and
  stdio permission prompt control channel.
  """

  @behaviour ExMCP.ACP.Adapter

  require Logger

  alias ExMCP.ACP.Adapters.ClaudeSDK.Mapper
  alias ExMCP.ACP.Adapters.ClaudeSDK.Protocol, as: ClaudeProtocol
  alias ExMCP.ACP.Adapters.ClaudeSDK.SessionStore
  alias ExMCP.ACP.Envelope
  alias ExMCP.ACP.PromptQueue

  defstruct [
    :session_id,
    :model,
    :permission_mode,
    :effort,
    :cwd,
    :client_capabilities,
    :gateway_auth,
    :fast_mode_enabled,
    :current_agent,
    :pending_prompt_id,
    :active_prompt_session_id,
    :init_response,
    opts: [],
    pending_controls: %{},
    pending_client_requests: %{},
    prompt_queue: PromptQueue.new(),
    available_commands: [],
    available_models: [],
    available_agents: [],
    text_acc: [],
    thinking_acc: [],
    thinking_blocks: [],
    current_block_type: nil,
    tool_calls: %{},
    message_ids: %{}
  ]

  @impl true
  def init(opts) do
    {:ok,
     %__MODULE__{
       opts: opts,
       cwd: Keyword.get(opts, :cwd),
       session_id: Keyword.get(opts, :session_id) || Keyword.get(opts, :resume),
       model: Keyword.get(opts, :model),
       permission_mode: encode_permission_mode(Keyword.get(opts, :permission_mode, :default)),
       effort: Keyword.get(opts, :effort),
       fast_mode_enabled: Keyword.get(opts, :fast_mode, false),
       current_agent: Keyword.get(opts, :agent, "default")
     }}
  end

  @impl true
  def command(opts), do: ClaudeProtocol.command(opts)

  @impl true
  def env(opts), do: ClaudeProtocol.env(opts)

  @impl true
  def post_connect(state) do
    request_id = control_id("initialize")

    state = %{
      state
      | pending_controls: Map.put(state.pending_controls, request_id, :initialize)
    }

    {:ok, ClaudeProtocol.initialize_request(state.opts, request_id) |> ClaudeProtocol.line(),
     state}
  end

  @impl true
  def capabilities do
    %{
      "loadSession" => true,
      "promptCapabilities" => %{
        "image" => true,
        "embeddedContext" => true
      },
      "mcpCapabilities" => %{
        "acp" => true,
        "http" => true,
        "sse" => true,
        "_meta" => %{
          "ex_mcp.mcpCapabilities" => %{
            "beam" => true
          }
        }
      },
      "auth" => %{
        "logout" => %{}
      },
      "sessionCapabilities" => %{
        "list" => %{},
        "resume" => %{},
        "close" => %{},
        "delete" => %{},
        "fork" => %{},
        "additionalDirectories" => %{}
      },
      "_meta" => %{
        "ex_mcp.claude_sdk" => %{
          "streaming" => true,
          "controlProtocol" => true
        }
      }
    }
  end

  @impl true
  def auth_methods(opts), do: auth_methods(opts, %__MODULE__{})

  @impl true
  def auth_methods(opts, state) do
    []
    |> maybe_add_terminal_auth_methods(opts, state)
    |> maybe_add_gateway_auth_methods(opts, state)
  end

  @impl true
  def modes, do: Mapper.modes()

  @impl true
  def config_options do
    Mapper.config_options(%{
      available_models: [],
      model: "default",
      permission_mode: "default",
      effort: "medium"
    })
  end

  @impl true
  def list_sessions(params, state) do
    params
    |> session_store_opts(state)
    |> SessionStore.list_acp_sessions()
    |> case do
      {:ok, sessions} -> {:ok, sessions, state}
      {:error, reason} -> {:error, reason, state}
    end
  end

  @impl true
  def fork_session(params, state) do
    session_id = params["sessionId"] || state.session_id

    params
    |> session_store_opts(state)
    |> then(&SessionStore.fork_session(session_id, &1))
    |> case do
      {:ok, forked_session_id} ->
        state = %{state | session_id: forked_session_id, cwd: params["cwd"] || state.cwd}
        {:ok, Mapper.session_result(state, forked_session_id), state}

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  @impl true
  def translate_outbound(%{"id" => _id, "method" => method} = msg, state)
      when is_binary(method) do
    handle_request(method, msg, state)
  end

  def translate_outbound(%{"method" => method} = msg, state) when is_binary(method) do
    handle_notification(method, msg, state)
  end

  def translate_outbound(%{"id" => _id} = msg, state) do
    case Mapper.client_response(msg, state) do
      {:ok, data, state} -> {:ok, data, state}
      :unknown -> {:ok, :skip, state}
    end
  end

  def translate_outbound(_msg, state), do: {:ok, :skip, state}

  defp handle_request("session/new", %{"id" => _id, "params" => params}, state) do
    session_id = state.session_id || params["sessionId"] || generated_session_id()
    state = %{state | session_id: session_id, cwd: params["cwd"] || state.cwd}
    {:reply, Mapper.session_result(state, session_id), state}
  end

  defp handle_request("initialize", %{"params" => params}, state) do
    {:ok, :skip, %{state | client_capabilities: params["clientCapabilities"] || %{}}}
  end

  defp handle_request("authenticate", %{"params" => %{"methodId" => method_id} = params}, state)
       when method_id in ["gateway", "gateway-bedrock"] do
    {:reply, %{}, %{state | gateway_auth: params}}
  end

  defp handle_request("authenticate", %{"params" => %{"methodId" => method_id}}, state)
       when method_id in ["claude-login", "claude-ai-login", "console-login"] do
    {:reply, %{}, state}
  end

  defp handle_request("authenticate", %{"params" => %{"methodId" => method_id}}, state) do
    {:error, "Unsupported Claude auth method: #{method_id}", state}
  end

  defp handle_request("authenticate", _msg, state) do
    {:error, "authenticate requires params.methodId", state}
  end

  defp handle_request("logout", _msg, state) do
    state = %{state | gateway_auth: nil}

    if Keyword.get(state.opts, :logout_cli, true) do
      case System.cmd(ClaudeProtocol.cli_path(state.opts), ["auth", "logout"],
             stderr_to_stdout: true
           ) do
        {_output, 0} ->
          {:reply, %{}, state}

        {output, status} ->
          {:error, "claude auth logout failed with status #{status}: #{String.trim(output)}",
           state}
      end
    else
      {:reply, %{}, state}
    end
  end

  defp handle_request("session/load", %{"params" => params}, state) do
    session_id = params["sessionId"] || state.session_id || generated_session_id()
    state = %{state | session_id: session_id, cwd: params["cwd"] || state.cwd}

    case SessionStore.read_session_messages(session_id, session_store_opts(params, state)) do
      {:ok, events} ->
        {messages, state} = Mapper.replay_messages(events, state)
        {:messages_and_reply, messages, Mapper.session_result(state, session_id), state}

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  defp handle_request("session/resume", %{"params" => params}, state) do
    session_id = params["sessionId"] || state.session_id || generated_session_id()
    state = %{state | session_id: session_id, cwd: params["cwd"] || state.cwd}
    {:reply, Mapper.session_result(state, session_id), state}
  end

  defp handle_request("session/close", %{"params" => %{"sessionId" => session_id}}, state) do
    {messages, state} = cleanup_session(state, session_id)

    if messages == [] do
      {:reply, %{}, state}
    else
      {:messages_and_reply, messages, %{}, state}
    end
  end

  defp handle_request("session/close", _msg, state), do: {:reply, %{}, state}

  defp handle_request("session/list", %{"params" => params}, state) do
    case list_sessions(params, state) do
      {:ok, sessions, state} -> {:reply, %{"sessions" => sessions}, state}
      {:error, reason, state} -> {:error, reason, state}
    end
  end

  defp handle_request("session/list", _msg, state) do
    handle_request("session/list", %{"params" => %{}}, state)
  end

  defp handle_request(
         "session/delete",
         %{"params" => %{"sessionId" => session_id} = params},
         state
       ) do
    params
    |> session_store_opts(state)
    |> then(&SessionStore.delete_session(session_id, &1))
    |> case do
      :ok ->
        {messages, state} = cleanup_session(state, session_id)
        state = clear_deleted_session(state, session_id)

        if messages == [] do
          {:reply, %{}, state}
        else
          {:messages_and_reply, messages, %{}, state}
        end

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  defp handle_request("session/delete", _msg, state) do
    {:error, "session/delete requires params.sessionId", state}
  end

  defp handle_request(
         "session/prompt",
         %{"id" => id, "params" => %{"sessionId" => session_id, "prompt" => prompt}},
         state
       ) do
    if state.pending_prompt_id do
      case ClaudeProtocol.user_message(session_id || state.session_id, prompt) do
        {:ok, message} -> {:ok, :skip, enqueue_prompt(state, id, session_id, message)}
        {:error, reason} -> {:error, reason, state}
      end
    else
      start_prompt(id, session_id, prompt, state)
    end
  end

  defp handle_request("session/set_mode", %{"params" => %{"modeId" => mode_id}}, state) do
    permission_mode = encode_permission_mode(mode_id)
    request_id = control_id("set_permission_mode")

    state = %{
      state
      | permission_mode: permission_mode,
        pending_controls: Map.put(state.pending_controls, request_id, :set_permission_mode)
    }

    data =
      ClaudeProtocol.control_request(request_id, %{
        "subtype" => "set_permission_mode",
        "mode" => permission_mode
      })
      |> ClaudeProtocol.line()

    {:reply_and_write, %{"modes" => Mapper.modes_result(state)}, data, state}
  end

  defp handle_request(
         "session/set_config_option",
         %{"params" => %{"configId" => "model", "value" => model}},
         state
       ) do
    request_id = control_id("set_model")

    state = %{
      state
      | model: model,
        pending_controls: Map.put(state.pending_controls, request_id, :set_model)
    }

    data =
      ClaudeProtocol.control_request(request_id, %{"subtype" => "set_model", "model" => model})
      |> ClaudeProtocol.line()

    {:reply_and_write, %{"configOptions" => Mapper.config_options(state)}, data, state}
  end

  defp handle_request(
         "session/set_config_option",
         %{"params" => %{"configId" => "mode", "value" => mode}},
         state
       ) do
    handle_request("session/set_mode", %{"params" => %{"modeId" => mode}}, state)
  end

  defp handle_request(
         "session/set_config_option",
         %{"params" => %{"configId" => "permission_mode", "value" => mode}},
         state
       ) do
    handle_request("session/set_mode", %{"params" => %{"modeId" => mode}}, state)
  end

  defp handle_request(
         "session/set_config_option",
         %{"params" => %{"configId" => "effort", "value" => effort}},
         state
       ) do
    request_id = control_id("apply_flag_settings")

    state = %{
      state
      | effort: effort,
        pending_controls: Map.put(state.pending_controls, request_id, :apply_flag_settings)
    }

    data =
      ClaudeProtocol.control_request(request_id, %{
        "subtype" => "apply_flag_settings",
        "settings" => %{"effortLevel" => to_sdk_effort_level(effort)}
      })
      |> ClaudeProtocol.line()

    {:reply_and_write, %{"configOptions" => Mapper.config_options(state)}, data, state}
  end

  defp handle_request(
         "session/set_config_option",
         %{"params" => %{"configId" => "fast"} = params},
         state
       ) do
    case resolve_fast_mode_enabled(params) do
      {:ok, fast_mode_enabled} ->
        request_id = control_id("apply_flag_settings")

        state = %{
          state
          | fast_mode_enabled: fast_mode_enabled,
            pending_controls: Map.put(state.pending_controls, request_id, :apply_flag_settings)
        }

        data =
          ClaudeProtocol.control_request(request_id, %{
            "subtype" => "apply_flag_settings",
            "settings" => %{"fastMode" => fast_mode_enabled}
          })
          |> ClaudeProtocol.line()

        {:reply_and_write, %{"configOptions" => Mapper.config_options(state)}, data, state}

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  defp handle_request(
         "session/set_config_option",
         %{"params" => %{"configId" => "agent", "value" => agent}},
         state
       ) do
    request_id = control_id("apply_flag_settings")
    agent = to_string(agent)
    sdk_agent = if agent == "default", do: nil, else: agent

    state = %{
      state
      | current_agent: agent,
        pending_controls: Map.put(state.pending_controls, request_id, :apply_flag_settings)
    }

    data =
      ClaudeProtocol.control_request(request_id, %{
        "subtype" => "apply_flag_settings",
        "settings" => %{"agent" => sdk_agent}
      })
      |> ClaudeProtocol.line()

    {:reply_and_write, %{"configOptions" => Mapper.config_options(state)}, data, state}
  end

  defp handle_request("session/set_config_option", _msg, state) do
    {:reply, %{"configOptions" => Mapper.config_options(state)}, state}
  end

  defp handle_request(_method, _msg, state), do: {:ok, :skip, state}

  defp handle_notification("session/cancel", %{"params" => params}, state) do
    session_id = params["sessionId"] || state.active_prompt_session_id || state.session_id
    {cancelled_messages, state} = cancel_queued_prompts(state, session_id)
    request_id = control_id("interrupt")
    state = %{state | pending_controls: Map.put(state.pending_controls, request_id, :interrupt)}

    data =
      ClaudeProtocol.control_request(request_id, %{"subtype" => "interrupt"})
      |> ClaudeProtocol.line()

    if cancelled_messages == [] do
      {:ok, data, state}
    else
      {:messages_and_write, cancelled_messages, data, state}
    end
  end

  defp handle_notification(_method, _msg, state), do: {:ok, :skip, state}

  @impl true
  def translate_inbound(line, state) do
    trimmed = String.trim(line)

    with false <- trimmed == "",
         {:ok, event} <- Jason.decode(trimmed) do
      {messages, writes, state} = Mapper.reduce_message(event, state)
      return_inbound(messages, writes, state)
    else
      true ->
        {:skip, state}

      {:error, _reason} ->
        Logger.debug("[ClaudeSDK Adapter] Non-JSON line: #{String.slice(trimmed, 0, 120)}")
        {:skip, state}
    end
  end

  defp return_inbound([], [], state), do: {:skip, state}
  defp return_inbound([], [write], state), do: {:skip_and_write, write, state}
  defp return_inbound([], writes, state), do: {:skip_and_write, writes, state}
  defp return_inbound(messages, [], state), do: {:messages, messages, state}
  defp return_inbound(messages, writes, state), do: {:messages_and_write, messages, writes, state}

  defp control_id(prefix),
    do: "ex_mcp_#{prefix}_#{System.unique_integer([:positive, :monotonic])}"

  defp generated_session_id do
    "claude_sdk_#{System.unique_integer([:positive, :monotonic])}"
  end

  defp session_store_opts(params, state) do
    params = params || %{}

    %{
      cwd: params["cwd"] || params[:cwd] || state.cwd || Keyword.get(state.opts, :cwd),
      cursor: params["cursor"] || params[:cursor],
      limit: params["limit"] || params[:limit],
      offset: params["offset"] || params[:offset],
      claude_config_dir: Keyword.get(state.opts, :claude_config_dir),
      env: Keyword.get(state.opts, :env, [])
    }
    |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    |> Map.new()
  end

  defp clear_deleted_session(%{session_id: session_id} = state, session_id) do
    %{
      state
      | session_id: nil,
        pending_prompt_id: nil,
        active_prompt_session_id: nil,
        prompt_queue: PromptQueue.new(),
        text_acc: [],
        thinking_acc: [],
        thinking_blocks: [],
        current_block_type: nil,
        tool_calls: %{}
    }
  end

  defp clear_deleted_session(state, _session_id), do: state

  defp start_prompt(id, session_id, prompt, state) do
    case ClaudeProtocol.user_message(session_id || state.session_id, prompt) do
      {:ok, message} ->
        state =
          %{
            state
            | pending_prompt_id: id,
              active_prompt_session_id: session_id || state.session_id,
              session_id: session_id || state.session_id,
              text_acc: [],
              thinking_acc: [],
              thinking_blocks: [],
              current_block_type: nil,
              tool_calls: %{}
          }

        {:ok, ClaudeProtocol.line(message), state}

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  defp enqueue_prompt(state, id, session_id, message) do
    queued = %{id: id, session_id: session_id, message: message}
    %{state | prompt_queue: PromptQueue.enqueue(state.prompt_queue, queued)}
  end

  defp cancel_queued_prompts(state, nil), do: {[], state}

  defp cancel_queued_prompts(state, session_id) do
    {cancelled, remaining} =
      PromptQueue.split(state.prompt_queue, &(&1.session_id == session_id))

    messages =
      Enum.map(cancelled, fn %{id: id} ->
        Envelope.response(id, %{"stopReason" => "cancelled"})
      end)

    {messages, %{state | prompt_queue: remaining}}
  end

  defp cleanup_session(state, session_id) do
    {queued_messages, state} = cancel_queued_prompts(state, session_id)
    {active_messages, state} = cancel_active_prompt(state, session_id)

    state =
      if state.session_id == session_id do
        %{
          state
          | session_id: nil,
            text_acc: [],
            thinking_acc: [],
            thinking_blocks: [],
            current_block_type: nil,
            tool_calls: %{}
        }
      else
        state
      end

    {active_messages ++ queued_messages, state}
  end

  defp cancel_active_prompt(%{pending_prompt_id: nil} = state, _session_id), do: {[], state}

  defp cancel_active_prompt(%{active_prompt_session_id: session_id} = state, session_id) do
    messages = [Envelope.response(state.pending_prompt_id, %{"stopReason" => "cancelled"})]

    state =
      %{
        state
        | pending_prompt_id: nil,
          active_prompt_session_id: nil,
          text_acc: [],
          thinking_acc: [],
          thinking_blocks: [],
          current_block_type: nil,
          tool_calls: %{}
      }

    {messages, state}
  end

  defp cancel_active_prompt(state, _session_id), do: {[], state}

  defp encode_permission_mode(:default), do: "default"
  defp encode_permission_mode(:accept_edits), do: "acceptEdits"
  defp encode_permission_mode(:plan), do: "plan"
  defp encode_permission_mode(:auto), do: "auto"
  defp encode_permission_mode(:dont_ask), do: "dontAsk"
  defp encode_permission_mode(:bypass), do: "bypassPermissions"
  defp encode_permission_mode(:bypass_permissions), do: "bypassPermissions"
  defp encode_permission_mode(mode) when is_binary(mode), do: mode
  defp encode_permission_mode(nil), do: "default"

  defp maybe_add_terminal_auth_methods(methods, opts, state) do
    caps = state.client_capabilities || %{}

    if terminal_auth_supported?(caps) do
      methods ++ terminal_auth_methods(opts, meta_terminal_auth_supported?(caps))
    else
      methods
    end
  end

  defp maybe_add_gateway_auth_methods(methods, opts, state) do
    if Keyword.get(opts, :gateway_auth, false) and
         get_in(state.client_capabilities || %{}, ["auth", "_meta", "gateway"]) == true do
      methods ++
        [
          %{
            "id" => "gateway",
            "name" => "Custom model gateway",
            "description" => "Use a custom gateway to authenticate and access models",
            "_meta" => %{"gateway" => %{"protocol" => "anthropic"}}
          },
          %{
            "id" => "gateway-bedrock",
            "name" => "Custom model gateway",
            "description" => "Use a custom gateway to authenticate and access models",
            "_meta" => %{"gateway" => %{"protocol" => "bedrock"}}
          }
        ]
    else
      methods
    end
  end

  defp terminal_auth_supported?(caps) do
    get_in(caps, ["auth", "terminal"]) == true or meta_terminal_auth_supported?(caps)
  end

  defp meta_terminal_auth_supported?(caps), do: get_in(caps, ["_meta", "terminal-auth"]) == true

  defp terminal_auth_methods(opts, include_meta?) do
    cli_path = ClaudeProtocol.cli_path(opts)

    if remote_environment?() do
      [
        terminal_auth_method(
          "claude-login",
          "Log in with Claude",
          "Run `claude /login` in the terminal",
          cli_path,
          [],
          "Claude Login",
          include_meta?
        )
      ]
    else
      [
        terminal_auth_method(
          "claude-ai-login",
          "Claude Subscription",
          "Use Claude subscription",
          cli_path,
          ["auth", "login", "--claudeai"],
          "Claude Login",
          include_meta?
        ),
        terminal_auth_method(
          "console-login",
          "Anthropic Console",
          "Use Anthropic Console (API usage billing)",
          cli_path,
          ["auth", "login", "--console"],
          "Anthropic Console Login",
          include_meta?
        )
      ]
    end
  end

  defp terminal_auth_method(id, name, description, command, args, label, include_meta?) do
    method = %{
      "id" => id,
      "name" => name,
      "description" => description,
      "type" => "terminal",
      "args" => args
    }

    if include_meta? do
      Map.put(method, "_meta", %{
        "terminal-auth" => %{
          "command" => command,
          "args" => args,
          "label" => label
        }
      })
    else
      method
    end
  end

  defp remote_environment? do
    Enum.any?(
      ~w(NO_BROWSER SSH_CONNECTION SSH_CLIENT SSH_TTY CLAUDE_CODE_REMOTE),
      &System.get_env/1
    )
  end

  defp resolve_fast_mode_enabled(%{"value" => value}) when is_boolean(value), do: {:ok, value}
  defp resolve_fast_mode_enabled(%{"value" => "on"}), do: {:ok, true}
  defp resolve_fast_mode_enabled(%{"value" => "off"}), do: {:ok, false}

  defp resolve_fast_mode_enabled(%{"value" => other}),
    do: {:error, "Invalid fast mode value: #{inspect(other)}"}

  defp to_sdk_effort_level("default"), do: nil
  defp to_sdk_effort_level(nil), do: nil
  defp to_sdk_effort_level(effort), do: effort
end
