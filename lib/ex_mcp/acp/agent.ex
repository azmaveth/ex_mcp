defmodule ExMCP.ACP.Agent do
  @moduledoc """
  Runtime for building native Elixir Agent Client Protocol agents.

  `ExMCP.ACP.Agent` is the ACP counterpart to an MCP server: it receives
  client requests such as `session/new` and `session/prompt`, streams
  `session/update` notifications, and may request client-side filesystem,
  terminal, or permission operations.

  Prompt callbacks may return immediately or keep the JSON-RPC request pending:

      def handle_prompt(session_id, prompt, ctx, state) do
        Task.start(fn ->
          ExMCP.ACP.Agent.agent_message(ctx.agent, session_id, "Working...")
          ExMCP.ACP.Agent.finish_prompt(ctx.agent, ctx.prompt_id, "end_turn")
        end)

        {:noreply, state}
      end
  """

  use GenServer

  require Logger

  alias ExMCP.ACP.AdapterEvents
  alias ExMCP.ACP.Agent.HandlerRunner
  alias ExMCP.ACP.Agent.Transport.{Memory, Stdio}
  alias ExMCP.ACP.Capabilities
  alias ExMCP.ACP.Maps
  alias ExMCP.ACP.NameValue
  alias ExMCP.ACP.Protocol

  @default_protocol_version 1
  @supported_protocol_versions [1]
  @default_timeout 30_000

  defstruct [
    :transport_mod,
    :transport_state,
    :receiver_pid,
    :handler_mod,
    :handler_pid,
    :agent_info,
    :agent_capabilities,
    :auth_methods,
    :client_info,
    :client_capabilities,
    :protocol_version,
    pending_callbacks: %{},
    pending_prompts: %{},
    active_prompts: %{},
    pending_client_requests: %{},
    status: :listening
  ]

  @agent_keys [
    :name,
    :handler,
    :handler_opts,
    :agent_info,
    :capabilities,
    :agent_capabilities,
    :auth_methods,
    :protocol_version,
    :transport,
    :transport_mod
  ]

  # Public API

  @doc "Starts an ACP agent runtime."
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    {gen_opts, agent_opts} = Keyword.split(opts, [:name])
    GenServer.start_link(__MODULE__, agent_opts, gen_opts)
  end

  @doc """
  Starts an ACP agent and blocks until it exits.

  Intended for stdio command-line entrypoints.
  """
  @spec run(keyword()) :: :ok | {:error, any()}
  def run(opts) do
    case start_link(opts) do
      {:ok, pid} ->
        ref = Process.monitor(pid)

        receive do
          {:DOWN, ^ref, :process, ^pid, :normal} -> :ok
          {:DOWN, ^ref, :process, ^pid, reason} -> {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc "Stops an ACP agent runtime."
  @spec stop(GenServer.server()) :: :ok
  def stop(agent), do: GenServer.stop(agent)

  @doc "Returns the runtime status."
  @spec status(GenServer.server()) :: atom()
  def status(agent), do: GenServer.call(agent, :status)

  @doc "Sends a raw `session/update` notification."
  @spec session_update(GenServer.server(), String.t(), map(), keyword()) ::
          :ok | {:error, any()}
  def session_update(agent, session_id, update, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    GenServer.call(agent, {:session_update, session_id, update}, timeout)
  end

  @doc "Streams an `agent_message_chunk` update."
  @spec agent_message(GenServer.server(), String.t(), String.t() | map(), keyword()) ::
          :ok | {:error, any()}
  def agent_message(agent, session_id, content, opts \\ []) do
    update =
      case content do
        text when is_binary(text) -> %{"type" => "text", "text" => text}
        block when is_map(block) -> block
      end

    session_update(
      agent,
      session_id,
      %{"sessionUpdate" => "agent_message_chunk", "content" => update},
      opts
    )
  end

  @doc "Streams an `agent_thought_chunk` update."
  @spec agent_thought(GenServer.server(), String.t(), String.t() | map(), keyword()) ::
          :ok | {:error, any()}
  def agent_thought(agent, session_id, content, opts \\ []) do
    update =
      case content do
        text when is_binary(text) -> %{"type" => "text", "text" => text}
        block when is_map(block) -> block
      end

    session_update(
      agent,
      session_id,
      %{"sessionUpdate" => "agent_thought_chunk", "content" => update},
      opts
    )
  end

  @doc "Sends a `tool_call` update."
  @spec tool_call(GenServer.server(), String.t(), map(), keyword()) :: :ok | {:error, any()}
  def tool_call(agent, session_id, tool_call, opts \\ []) do
    session_update(agent, session_id, Map.put_new(tool_call, "sessionUpdate", "tool_call"), opts)
  end

  @doc "Sends a `tool_call_update` update."
  @spec tool_call_update(GenServer.server(), String.t(), map(), keyword()) ::
          :ok | {:error, any()}
  def tool_call_update(agent, session_id, update, opts \\ []) do
    session_update(
      agent,
      session_id,
      Map.put_new(update, "sessionUpdate", "tool_call_update"),
      opts
    )
  end

  @doc "Sends a `plan` update."
  @spec plan(GenServer.server(), String.t(), [map()], keyword()) :: :ok | {:error, any()}
  def plan(agent, session_id, entries, opts \\ []) do
    update = get_in(AdapterEvents.plan(session_id, entries), ["params", "update"])
    session_update(agent, session_id, update, opts)
  end

  @doc "Sends an `available_commands_update` update."
  @spec available_commands(GenServer.server(), String.t(), [map()], keyword()) ::
          :ok | {:error, any()}
  def available_commands(agent, session_id, commands, opts \\ []) do
    session_update(
      agent,
      session_id,
      %{"sessionUpdate" => "available_commands_update", "availableCommands" => commands},
      opts
    )
  end

  @doc "Sends a `current_mode_update` update."
  @spec current_mode(GenServer.server(), String.t(), String.t(), keyword()) ::
          :ok | {:error, any()}
  def current_mode(agent, session_id, mode_id, opts \\ []) do
    session_update(
      agent,
      session_id,
      %{"sessionUpdate" => "current_mode_update", "currentModeId" => mode_id},
      opts
    )
  end

  @doc "Sends a `config_option_update` update."
  @spec config_options(GenServer.server(), String.t(), [map()], keyword()) ::
          :ok | {:error, any()}
  def config_options(agent, session_id, options, opts \\ []) do
    update = get_in(AdapterEvents.config_option_update(session_id, options), ["params", "update"])
    session_update(agent, session_id, update, opts)
  end

  @doc "Sends a `session_info_update` update."
  @spec session_info(GenServer.server(), String.t(), map(), keyword()) :: :ok | {:error, any()}
  def session_info(agent, session_id, info, opts \\ []) do
    session_update(
      agent,
      session_id,
      Map.put_new(info, "sessionUpdate", "session_info_update"),
      opts
    )
  end

  @doc "Sends a `usage_update` update."
  @spec usage(GenServer.server(), String.t(), non_neg_integer(), non_neg_integer(), keyword()) ::
          :ok | {:error, any()}
  def usage(agent, session_id, used, size, opts \\ []) do
    update =
      %{"sessionUpdate" => "usage_update", "used" => used, "size" => size}
      |> maybe_put("cost", Keyword.get(opts, :cost))

    session_update(agent, session_id, update, Keyword.drop(opts, [:cost]))
  end

  @doc """
  Completes a pending `session/prompt` request.

  `result_or_stop_reason` may be a stop reason string or a response map
  containing `"stopReason"`.
  """
  @spec finish_prompt(GenServer.server(), integer() | String.t(), String.t() | map(), keyword()) ::
          :ok | {:error, any()}
  def finish_prompt(agent, prompt_id, result_or_stop_reason, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    GenServer.call(agent, {:finish_prompt, prompt_id, result_or_stop_reason}, timeout)
  end

  @doc "Requests permission from the ACP client."
  @spec request_permission(GenServer.server(), String.t(), map(), [map()], keyword()) ::
          {:ok, map() | nil} | {:error, any()}
  def request_permission(agent, session_id, tool_call, options, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    msg = Protocol.encode_permission_request(session_id, tool_call, options)
    GenServer.call(agent, {:client_request, msg, :permission}, timeout)
  end

  @doc "Requests text file contents from the ACP client."
  @spec read_text_file(GenServer.server(), String.t(), String.t(), keyword()) ::
          {:ok, map() | nil} | {:error, any()}
  def read_text_file(agent, session_id, path, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    msg = Protocol.encode_file_read_request(session_id, path, opts)
    GenServer.call(agent, {:client_request, msg, :fs_read}, timeout)
  end

  @doc "Requests that the ACP client write a text file."
  @spec write_text_file(GenServer.server(), String.t(), String.t(), String.t(), keyword()) ::
          {:ok, map() | nil} | {:error, any()}
  def write_text_file(agent, session_id, path, content, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    msg = Protocol.encode_file_write_request(session_id, path, content)
    GenServer.call(agent, {:client_request, msg, :fs_write}, timeout)
  end

  @doc "Requests terminal creation from the ACP client."
  @spec terminal_create(GenServer.server(), String.t(), String.t() | map(), keyword()) ::
          {:ok, map() | nil} | {:error, any()}
  def terminal_create(agent, session_id, command_or_params, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)

    params =
      case command_or_params do
        command when is_binary(command) ->
          opts |> Keyword.drop([:timeout]) |> Map.new() |> Map.put("command", command)

        params when is_map(params) ->
          params
      end

    msg =
      Protocol.encode_terminal_request(
        "terminal/create",
        session_id,
        normalize_terminal_params(params)
      )

    GenServer.call(agent, {:client_request, msg, :terminal}, timeout)
  end

  @doc "Requests terminal output from the ACP client."
  @spec terminal_output(GenServer.server(), String.t(), String.t(), keyword()) ::
          {:ok, map() | nil} | {:error, any()}
  def terminal_output(agent, session_id, terminal_id, opts \\ []) do
    terminal_by_id(agent, "terminal/output", session_id, terminal_id, opts)
  end

  @doc "Waits for a terminal command to exit."
  @spec terminal_wait_for_exit(GenServer.server(), String.t(), String.t(), keyword()) ::
          {:ok, map() | nil} | {:error, any()}
  def terminal_wait_for_exit(agent, session_id, terminal_id, opts \\ []) do
    terminal_by_id(agent, "terminal/wait_for_exit", session_id, terminal_id, opts)
  end

  @doc "Kills a terminal command."
  @spec terminal_kill(GenServer.server(), String.t(), String.t(), keyword()) ::
          {:ok, map() | nil} | {:error, any()}
  def terminal_kill(agent, session_id, terminal_id, opts \\ []) do
    terminal_by_id(agent, "terminal/kill", session_id, terminal_id, opts)
  end

  @doc "Releases a terminal."
  @spec terminal_release(GenServer.server(), String.t(), String.t(), keyword()) ::
          {:ok, map() | nil} | {:error, any()}
  def terminal_release(agent, session_id, terminal_id, opts \\ []) do
    terminal_by_id(agent, "terminal/release", session_id, terminal_id, opts)
  end

  @impl true
  def init(opts) do
    Process.flag(:trap_exit, true)

    handler_mod = Keyword.fetch!(opts, :handler)
    handler_opts = Keyword.get(opts, :handler_opts, [])

    with {:ok, handler_pid} <- HandlerRunner.start_link(handler_mod, handler_opts, self()),
         {transport_mod, transport_opts} <- resolve_transport(opts),
         {:ok, transport_state} <- transport_mod.connect(transport_opts) do
      state = %__MODULE__{
        transport_mod: transport_mod,
        transport_state: transport_state,
        handler_mod: handler_mod,
        handler_pid: handler_pid,
        agent_info: agent_info(opts, handler_mod),
        agent_capabilities: agent_capabilities(opts, handler_mod),
        auth_methods: auth_methods(opts, handler_mod),
        protocol_version: Keyword.get(opts, :protocol_version, @default_protocol_version)
      }

      receiver_pid = start_receiver(self(), transport_mod, transport_state)
      {:ok, %{state | receiver_pid: receiver_pid}}
    end
  end

  @impl true
  def handle_call(:status, _from, state) do
    {:reply, state.status, state}
  end

  def handle_call({:session_update, session_id, update}, _from, state) do
    msg = Protocol.encode_session_update(session_id, update)
    reply_with_send(msg, state)
  end

  def handle_call({:finish_prompt, prompt_id, result}, _from, state) do
    case finish_prompt_response(prompt_id, result, state) do
      {:ok, state} -> {:reply, :ok, state}
      {:error, reason, state} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:client_request, msg, capability}, from, state) do
    case ensure_client_capability(state, capability) do
      :ok ->
        send_client_request(msg, from, state)

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_info({:transport_message, raw_message}, state) do
    case Protocol.parse_message(raw_message) do
      {:request, method, params, id} ->
        handle_client_request(method, params, id, state)

      {:notification, "session/cancel", params} ->
        {:noreply, handle_cancel_notification(params, state)}

      {:notification, method, _params} ->
        Logger.debug("ACP agent received unsupported notification: #{method}")
        {:noreply, state}

      {:result, result, id} ->
        {:noreply, resolve_client_request(id, {:ok, result}, state)}

      {:error, error, id} ->
        {:noreply, resolve_client_request(id, {:error, error}, state)}

      {:error, :invalid_message} ->
        msg = Protocol.encode_error(-32700, "Parse error", nil, nil)
        {:noreply, send_without_reply(msg, state)}
    end
  end

  def handle_info({:acp_agent_handler_result, ref, result}, state) do
    {:noreply, handle_handler_result(ref, result, state)}
  end

  def handle_info({:transport_closed, reason}, state) do
    Logger.info("ACP agent transport closed: #{inspect(reason)}")
    state = reply_all_pending({:error, :transport_closed}, state)
    {:stop, :normal, %{state | status: :disconnected}}
  end

  def handle_info({:transport_error, reason}, state) do
    Logger.warning("ACP agent transport error: #{inspect(reason)}")
    state = reply_all_pending({:error, {:transport_error, reason}}, state)
    {:stop, reason, %{state | status: :disconnected}}
  end

  def handle_info({:EXIT, pid, reason}, state) do
    cond do
      pid == state.receiver_pid ->
        if reason != :normal do
          Logger.warning("ACP agent receiver exited: #{inspect(reason)}")
        end

        state = reply_all_pending({:error, :receiver_exited}, state)
        {:stop, reason, %{state | status: :disconnected, receiver_pid: nil}}

      pid == state.handler_pid ->
        Logger.warning("ACP agent handler runner exited: #{inspect(reason)}")
        state = fail_pending_callbacks({:handler_exited, reason}, state)
        {:noreply, %{state | handler_pid: nil}}

      true ->
        {:noreply, state}
    end
  end

  def handle_info(_msg, state), do: {:noreply, state}

  @impl true
  def terminate(_reason, state) do
    if state.transport_mod && state.transport_state do
      state.transport_mod.close(state.transport_state)
    end

    :ok
  end

  defp terminal_by_id(agent, method, session_id, terminal_id, opts) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)

    params =
      opts
      |> Keyword.drop([:timeout])
      |> Map.new()
      |> Map.put("terminalId", terminal_id)
      |> Maps.stringify_keys()

    msg = Protocol.encode_terminal_request(method, session_id, params)
    GenServer.call(agent, {:client_request, msg, :terminal}, timeout)
  end

  defp resolve_transport(opts) do
    transport = Keyword.get(opts, :transport, :stdio)
    base_opts = Keyword.drop(opts, @agent_keys)

    cond do
      mod = Keyword.get(opts, :transport_mod) ->
        {mod, Keyword.put_new(base_opts, :role, :agent)}

      transport == :stdio ->
        {Stdio, base_opts}

      transport == :memory ->
        {Memory, Keyword.put_new(base_opts, :role, :agent)}

      match?({:memory, _}, transport) ->
        {:memory, peer} = transport
        {Memory, base_opts |> Keyword.put(:peer, peer) |> Keyword.put_new(:role, :agent)}

      is_atom(transport) ->
        {transport, Keyword.put_new(base_opts, :role, :agent)}
    end
  end

  defp agent_info(opts, handler_mod) do
    Keyword.get(opts, :agent_info) ||
      maybe_handler_static(handler_mod, :agent_info) ||
      %{"name" => inspect(handler_mod), "version" => app_version()}
  end

  defp agent_capabilities(opts, handler_mod) do
    Keyword.get(opts, :agent_capabilities) ||
      Keyword.get(opts, :capabilities) ||
      maybe_handler_static(handler_mod, :agent_capabilities) ||
      maybe_handler_static(handler_mod, :capabilities) ||
      Capabilities.from_handler(handler_mod)
  end

  defp auth_methods(opts, handler_mod) do
    Keyword.get(opts, :auth_methods) ||
      maybe_handler_static(handler_mod, :auth_methods) ||
      []
  end

  defp maybe_handler_static(handler_mod, function) do
    if function_exported?(handler_mod, function, 0), do: apply(handler_mod, function, [])
  end

  defp app_version do
    case Application.spec(:ex_mcp, :vsn) do
      nil -> "0.1.0"
      vsn -> to_string(vsn)
    end
  end

  defp start_receiver(parent, transport_mod, transport_state) do
    spawn_link(fn -> receiver_loop(parent, transport_mod, transport_state) end)
  end

  defp receiver_loop(parent, transport_mod, transport_state) do
    case transport_mod.receive_message(transport_state) do
      {:ok, message, new_state} ->
        send(parent, {:transport_message, message})
        receiver_loop(parent, transport_mod, new_state)

      {:error, :closed} ->
        send(parent, {:transport_closed, :normal})

      {:error, reason} ->
        send(parent, {:transport_error, reason})
    end
  end

  defp handle_client_request("initialize", params, id, state) do
    protocol_version =
      negotiate_protocol_version(params["protocolVersion"] || state.protocol_version)

    state = %{
      state
      | client_info: params["clientInfo"],
        client_capabilities: params["clientCapabilities"] || %{},
        protocol_version: protocol_version
    }

    if function_exported?(state.handler_mod, :handle_initialize, 3) do
      ctx = context(state, id)

      start_callback(
        :initialize,
        id,
        fn ref -> HandlerRunner.initialize(state.handler_pid, ref, params, ctx) end,
        state
      )
    else
      msg =
        Protocol.encode_initialize_response(
          id,
          state.agent_info,
          state.agent_capabilities,
          state.auth_methods,
          state.protocol_version
        )

      {:noreply, send_without_reply(msg, %{state | status: :ready})}
    end
  end

  defp handle_client_request("authenticate", params, id, state) do
    optional_callback(:authenticate, :handle_authenticate, id, state, fn ref, ctx ->
      HandlerRunner.authenticate(state.handler_pid, ref, params, ctx)
    end)
  end

  defp handle_client_request("logout", _params, id, state) do
    optional_capability_callback(:logout, :logout, :handle_logout, id, state, fn ref, ctx ->
      HandlerRunner.logout(state.handler_pid, ref, ctx)
    end)
  end

  defp handle_client_request("session/new", params, id, state) do
    if function_exported?(state.handler_mod, :handle_new_session, 3) do
      ctx = context(state, id)

      start_callback(
        :new_session,
        id,
        fn ref -> HandlerRunner.new_session(state.handler_pid, ref, params, ctx) end,
        state
      )
    else
      send_method_not_found("session/new", id, state)
    end
  end

  defp handle_client_request("session/load", params, id, state) do
    optional_capability_callback(
      :load_session,
      :load_session,
      :handle_load_session,
      id,
      state,
      fn ref, ctx ->
        HandlerRunner.load_session(state.handler_pid, ref, params, ctx)
      end
    )
  end

  defp handle_client_request("session/list", params, id, state) do
    optional_capability_callback(
      :list_sessions,
      :session_list,
      :handle_list_sessions,
      id,
      state,
      fn ref, ctx ->
        HandlerRunner.list_sessions(state.handler_pid, ref, params, ctx)
      end
    )
  end

  defp handle_client_request("session/resume", params, id, state) do
    optional_capability_callback(
      :resume_session,
      :session_resume,
      :handle_resume_session,
      id,
      state,
      fn ref, ctx ->
        HandlerRunner.resume_session(state.handler_pid, ref, params, ctx)
      end
    )
  end

  defp handle_client_request("session/fork", params, id, state) do
    optional_capability_callback(
      :fork_session,
      :session_fork,
      :handle_fork_session,
      id,
      state,
      fn ref, ctx ->
        HandlerRunner.fork_session(state.handler_pid, ref, params, ctx)
      end
    )
  end

  defp handle_client_request("session/close", %{"sessionId" => session_id}, id, state) do
    state = cancel_active_prompt(session_id, state)

    optional_capability_callback(
      :close_session,
      :session_close,
      :handle_close_session,
      id,
      state,
      fn ref, ctx ->
        HandlerRunner.close_session(
          state.handler_pid,
          ref,
          session_id,
          Map.put(ctx, :session_id, session_id)
        )
      end
    )
  end

  defp handle_client_request("session/delete", %{"sessionId" => session_id}, id, state) do
    optional_capability_callback(
      :delete_session,
      :session_delete,
      :handle_delete_session,
      id,
      state,
      fn ref, ctx ->
        HandlerRunner.delete_session(
          state.handler_pid,
          ref,
          session_id,
          Map.put(ctx, :session_id, session_id)
        )
      end
    )
  end

  defp handle_client_request(
         "session/prompt",
         %{"sessionId" => session_id, "prompt" => prompt},
         id,
         state
       )
       when is_binary(session_id) and is_list(prompt) do
    if Map.has_key?(state.active_prompts, session_id) do
      msg = Protocol.encode_error(-32603, "Prompt already active for session", nil, id)
      {:noreply, send_without_reply(msg, state)}
    else
      prompt_id = id
      ctx = state |> context(id, session_id) |> Map.put(:prompt_id, prompt_id)

      state = %{
        state
        | pending_prompts:
            Map.put(state.pending_prompts, prompt_id, %{
              request_id: id,
              session_id: session_id,
              cancelled?: false
            }),
          active_prompts: Map.put(state.active_prompts, session_id, prompt_id)
      }

      if function_exported?(state.handler_mod, :handle_prompt, 4) do
        start_callback(
          :prompt,
          id,
          fn ref ->
            HandlerRunner.prompt(state.handler_pid, ref, session_id, prompt, ctx)
          end,
          state,
          %{prompt_id: prompt_id, session_id: session_id}
        )
      else
        msg = Protocol.encode_error(-32601, "Method not found: session/prompt", nil, id)

        state =
          state
          |> cleanup_prompt(prompt_id)
          |> send_without_reply(msg)

        {:noreply, state}
      end
    end
  end

  defp handle_client_request("session/prompt", _params, id, state) do
    msg = Protocol.encode_error(-32602, "Invalid session/prompt params", nil, id)
    {:noreply, send_without_reply(msg, state)}
  end

  defp handle_client_request("session/set_mode", params, id, state) do
    case params do
      %{"sessionId" => session_id, "modeId" => mode_id} ->
        optional_callback(:set_mode, :handle_set_mode, id, state, fn ref, ctx ->
          HandlerRunner.set_mode(
            state.handler_pid,
            ref,
            session_id,
            mode_id,
            Map.put(ctx, :session_id, session_id)
          )
        end)

      _ ->
        msg = Protocol.encode_error(-32602, "Invalid session/set_mode params", nil, id)
        {:noreply, send_without_reply(msg, state)}
    end
  end

  defp handle_client_request("session/set_model", params, id, state) do
    case params do
      %{"sessionId" => session_id, "modelId" => model_id} ->
        optional_callback(:set_model, :handle_set_model, id, state, fn ref, ctx ->
          HandlerRunner.set_model(
            state.handler_pid,
            ref,
            session_id,
            model_id,
            Map.put(ctx, :session_id, session_id)
          )
        end)

      _ ->
        msg = Protocol.encode_error(-32602, "Invalid session/set_model params", nil, id)
        {:noreply, send_without_reply(msg, state)}
    end
  end

  defp handle_client_request("session/set_config_option", params, id, state) do
    case params do
      %{"sessionId" => session_id, "configId" => config_id, "value" => value} ->
        optional_callback(:set_config_option, :handle_set_config_option, id, state, fn ref, ctx ->
          HandlerRunner.set_config_option(
            state.handler_pid,
            ref,
            session_id,
            config_id,
            value,
            Map.put(ctx, :session_id, session_id)
          )
        end)

      _ ->
        msg = Protocol.encode_error(-32602, "Invalid session/set_config_option params", nil, id)
        {:noreply, send_without_reply(msg, state)}
    end
  end

  defp handle_client_request(method, _params, id, state) do
    send_method_not_found(method, id, state)
  end

  defp optional_capability_callback(kind, capability, callback, id, state, fun) do
    if Capabilities.supported?(state.agent_capabilities, capability) do
      optional_callback(kind, callback, id, state, fun)
    else
      send_method_not_found(method_name(kind), id, state)
    end
  end

  defp optional_callback(kind, callback, id, state, fun) do
    if function_exported?(state.handler_mod, callback, callback_arity(callback)) do
      ctx = context(state, id)
      start_callback(kind, id, fn ref -> fun.(ref, ctx) end, state)
    else
      send_method_not_found(method_name(kind), id, state)
    end
  end

  defp callback_arity(:handle_logout), do: 2
  defp callback_arity(:handle_close_session), do: 3
  defp callback_arity(:handle_delete_session), do: 3
  defp callback_arity(:handle_set_mode), do: 4
  defp callback_arity(:handle_set_model), do: 4
  defp callback_arity(:handle_set_config_option), do: 5
  defp callback_arity(_), do: 3

  defp method_name(:authenticate), do: "authenticate"
  defp method_name(:logout), do: "logout"
  defp method_name(:load_session), do: "session/load"
  defp method_name(:list_sessions), do: "session/list"
  defp method_name(:resume_session), do: "session/resume"
  defp method_name(:fork_session), do: "session/fork"
  defp method_name(:close_session), do: "session/close"
  defp method_name(:delete_session), do: "session/delete"
  defp method_name(:set_mode), do: "session/set_mode"
  defp method_name(:set_model), do: "session/set_model"
  defp method_name(:set_config_option), do: "session/set_config_option"

  defp negotiate_protocol_version(version) when version in @supported_protocol_versions,
    do: version

  defp negotiate_protocol_version(_version), do: @default_protocol_version

  defp start_callback(kind, request_id, starter, state, extra \\ %{}) do
    ref = make_ref()
    starter.(ref)

    pending =
      Map.put(
        state.pending_callbacks,
        ref,
        Map.merge(%{kind: kind, request_id: request_id}, extra)
      )

    {:noreply, %{state | pending_callbacks: pending}}
  end

  defp handle_cancel_notification(%{"sessionId" => session_id}, state) do
    cancel_active_prompt(session_id, state)
  end

  defp handle_cancel_notification(_params, state), do: state

  defp cancel_active_prompt(session_id, state) do
    case Map.get(state.active_prompts, session_id) do
      nil ->
        state

      prompt_id ->
        state = update_in(state.pending_prompts[prompt_id], &Map.put(&1, :cancelled?, true))

        if function_exported?(state.handler_mod, :handle_cancel, 3) do
          ctx = state |> context(prompt_id, session_id) |> Map.put(:prompt_id, prompt_id)
          ref = make_ref()
          HandlerRunner.cancel(state.handler_pid, ref, session_id, ctx)

          pending =
            Map.put(state.pending_callbacks, ref, %{
              kind: :cancel,
              request_id: prompt_id,
              prompt_id: prompt_id,
              session_id: session_id
            })

          %{state | pending_callbacks: pending}
        else
          case finish_prompt_response(prompt_id, "cancelled", state) do
            {:ok, state} -> state
            {:error, _reason, state} -> state
          end
        end
    end
  end

  defp handle_handler_result(ref, result, state) do
    case Map.pop(state.pending_callbacks, ref) do
      {nil, _pending} ->
        state

      {%{kind: :initialize, request_id: id}, pending} ->
        state = %{state | pending_callbacks: pending}
        handle_initialize_result(id, result, state)

      {%{kind: :new_session, request_id: id}, pending} ->
        state = %{state | pending_callbacks: pending}
        handle_session_result(id, result, state)

      {%{kind: kind, request_id: id}, pending}
      when kind in [:load_session, :resume_session, :fork_session] ->
        state = %{state | pending_callbacks: pending}
        handle_session_result(id, result, state)

      {%{kind: :list_sessions, request_id: id}, pending} ->
        state = %{state | pending_callbacks: pending}
        handle_list_sessions_result(id, result, state)

      {%{kind: :prompt, prompt_id: prompt_id}, pending} ->
        state = %{state | pending_callbacks: pending}
        handle_prompt_result(prompt_id, result, state)

      {%{kind: :cancel, prompt_id: prompt_id}, pending} ->
        state = %{state | pending_callbacks: pending}
        handle_cancel_result(prompt_id, result, state)

      {%{request_id: id}, pending} ->
        state = %{state | pending_callbacks: pending}
        handle_generic_result(id, result, state)
    end
  end

  defp handle_initialize_result(id, {:reply, result}, state) do
    result = Map.merge(default_initialize_result(state), result || %{})
    msg = Protocol.encode_response(result, id)
    send_without_reply(msg, %{state | status: :ready})
  end

  defp handle_initialize_result(id, {:error, reason}, state) do
    send_error(id, reason, state)
  end

  defp handle_initialize_result(id, :noreply, state) do
    msg = Protocol.encode_error(-32603, "initialize cannot return :noreply", nil, id)
    send_without_reply(msg, state)
  end

  defp handle_session_result(id, {:reply, result}, state) do
    msg = Protocol.encode_session_response(id, result)
    send_without_reply(msg, state)
  end

  defp handle_session_result(id, {:error, reason}, state), do: send_error(id, reason, state)
  defp handle_session_result(id, :noreply, state), do: invalid_noreply(id, state)

  defp handle_list_sessions_result(id, {:reply, sessions}, state) when is_list(sessions) do
    msg = Protocol.encode_session_list_response(id, sessions)
    send_without_reply(msg, state)
  end

  defp handle_list_sessions_result(id, {:reply, result}, state) do
    msg = Protocol.encode_response(result || %{"sessions" => []}, id)
    send_without_reply(msg, state)
  end

  defp handle_list_sessions_result(id, {:error, reason}, state), do: send_error(id, reason, state)
  defp handle_list_sessions_result(id, :noreply, state), do: invalid_noreply(id, state)

  defp handle_prompt_result(_prompt_id, :noreply, state), do: state

  defp handle_prompt_result(prompt_id, {:reply, result}, state) do
    case finish_prompt_response(prompt_id, result, state) do
      {:ok, state} -> state
      {:error, _reason, state} -> state
    end
  end

  defp handle_prompt_result(prompt_id, {:error, reason}, state) do
    case Map.get(state.pending_prompts, prompt_id) do
      nil ->
        state

      %{request_id: id} ->
        state
        |> cleanup_prompt(prompt_id)
        |> send_error(id, reason)
    end
  end

  defp handle_cancel_result(_prompt_id, :noreply, state), do: state

  defp handle_cancel_result(prompt_id, {:reply, result}, state),
    do: finish_cancel(prompt_id, result, state)

  defp handle_cancel_result(prompt_id, {:error, _reason}, state),
    do: finish_cancel(prompt_id, "cancelled", state)

  defp handle_generic_result(id, {:reply, result}, state) do
    send_without_reply(Protocol.encode_response(result || %{}, id), state)
  end

  defp handle_generic_result(id, {:error, reason}, state), do: send_error(id, reason, state)
  defp handle_generic_result(id, :noreply, state), do: invalid_noreply(id, state)

  defp finish_cancel(prompt_id, result, state) do
    result =
      case result do
        %{} = map -> Map.put_new(map, "stopReason", "cancelled")
        nil -> "cancelled"
        other -> other
      end

    case finish_prompt_response(prompt_id, result, state) do
      {:ok, state} -> state
      {:error, _reason, state} -> state
    end
  end

  defp finish_prompt_response(prompt_id, result, state) do
    case Map.get(state.pending_prompts, prompt_id) do
      nil ->
        {:error, :unknown_prompt, state}

      %{request_id: id} ->
        msg = Protocol.encode_prompt_response(id, result)

        case do_send(msg, state) do
          {:ok, state} -> {:ok, cleanup_prompt(state, prompt_id)}
          {:error, reason} -> {:error, reason, state}
        end
    end
  end

  defp cleanup_prompt(state, prompt_id) do
    case Map.pop(state.pending_prompts, prompt_id) do
      {nil, _pending} ->
        state

      {%{session_id: session_id}, pending} ->
        %{
          state
          | pending_prompts: pending,
            active_prompts: Map.delete(state.active_prompts, session_id)
        }
    end
  end

  defp send_client_request(msg, from, state) do
    id = msg["id"]

    case do_send(msg, state) do
      {:ok, state} ->
        pending = Map.put(state.pending_client_requests, id, from)
        {:noreply, %{state | pending_client_requests: pending}}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  defp resolve_client_request(id, reply, state) do
    case Map.pop(state.pending_client_requests, id) do
      {nil, _pending} ->
        Logger.debug("ACP agent received response for unknown client request #{inspect(id)}")
        state

      {from, pending} ->
        GenServer.reply(from, reply)
        %{state | pending_client_requests: pending}
    end
  end

  defp ensure_client_capability(_state, :permission), do: :ok

  defp ensure_client_capability(state, :fs_read) do
    if state.client_capabilities
       |> Maps.get("fs")
       |> Maps.get("readTextFile")
       |> Maps.truthy?() do
      :ok
    else
      {:error, {:unsupported_client_capability, :fs_read}}
    end
  end

  defp ensure_client_capability(state, :fs_write) do
    if state.client_capabilities
       |> Maps.get("fs")
       |> Maps.get("writeTextFile")
       |> Maps.truthy?() do
      :ok
    else
      {:error, {:unsupported_client_capability, :fs_write}}
    end
  end

  defp ensure_client_capability(state, :terminal) do
    if state.client_capabilities |> Maps.get("terminal") |> Maps.truthy?() do
      :ok
    else
      {:error, {:unsupported_client_capability, :terminal}}
    end
  end

  defp default_initialize_result(state) do
    %{
      "agentInfo" => state.agent_info,
      "agentCapabilities" => state.agent_capabilities,
      "authMethods" => state.auth_methods,
      "protocolVersion" => state.protocol_version
    }
  end

  defp context(state, request_id, session_id \\ nil) do
    %{
      agent: self(),
      request_id: request_id,
      session_id: session_id,
      client_info: state.client_info,
      client_capabilities: state.client_capabilities,
      protocol_version: state.protocol_version
    }
  end

  defp reply_with_send(msg, state) do
    case do_send(msg, state) do
      {:ok, state} -> {:reply, :ok, state}
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  defp send_without_reply(state, msg) when is_struct(state, __MODULE__),
    do: send_without_reply(msg, state)

  defp send_without_reply(msg, state) do
    case do_send(msg, state) do
      {:ok, state} ->
        state

      {:error, reason} ->
        Logger.warning("ACP agent send failed: #{inspect(reason)}")
        state
    end
  end

  defp send_method_not_found(method, id, state) do
    msg = Protocol.encode_error(-32601, "Method not found: #{method}", nil, id)
    {:noreply, send_without_reply(msg, state)}
  end

  defp send_error(state, id, reason) when is_struct(state, __MODULE__),
    do: send_error(id, reason, state)

  defp send_error(id, {code, message, data}, state)
       when is_integer(code) and is_binary(message) do
    send_without_reply(Protocol.encode_error(code, message, data, id), state)
  end

  defp send_error(id, reason, state) do
    send_without_reply(Protocol.encode_error(-32603, format_error(reason), nil, id), state)
  end

  defp invalid_noreply(id, state) do
    msg = Protocol.encode_error(-32603, "Only session/prompt may return :noreply", nil, id)
    send_without_reply(msg, state)
  end

  defp do_send(msg, state) do
    encoded = if is_binary(msg), do: msg, else: Jason.encode!(msg)

    case state.transport_mod.send_message(encoded, state.transport_state) do
      {:ok, transport_state} -> {:ok, %{state | transport_state: transport_state}}
      {:error, reason} -> {:error, reason}
    end
  end

  defp reply_all_pending(reply, state) do
    Enum.each(state.pending_client_requests, fn {_id, from} ->
      GenServer.reply(from, reply)
    end)

    reply
    |> elem(1)
    |> fail_pending_callbacks(state)
    |> Map.put(:pending_client_requests, %{})
  end

  defp fail_pending_callbacks(reason, state) do
    Enum.reduce(state.pending_callbacks, state, fn {_ref, callback}, acc ->
      case callback do
        %{kind: :prompt, prompt_id: prompt_id} ->
          handle_prompt_result(prompt_id, {:error, reason}, acc)

        %{request_id: id} ->
          send_error(id, reason, acc)
      end
    end)
    |> Map.put(:pending_callbacks, %{})
  end

  defp format_error({kind, reason, _stack}) when kind in [:error, :exit, :throw] do
    "#{kind}: #{inspect(reason)}"
  end

  defp format_error(reason) when is_binary(reason), do: reason
  defp format_error(reason), do: inspect(reason)

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp normalize_terminal_params(params) do
    params = Maps.stringify_keys(params)

    case Map.get(params, "env") do
      nil -> params
      env -> Map.put(params, "env", normalize_env(env))
    end
  end

  defp normalize_env(env) when is_map(env) do
    NameValue.list(env)
  end

  defp normalize_env(env) when is_list(env) do
    NameValue.list(env)
  end

  defp normalize_env(env), do: env
end
