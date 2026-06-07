defmodule ExMCP.ACP.Client do
  @moduledoc """
  GenServer client for the Agent Client Protocol (ACP).

  Manages connections to ACP-compatible coding agents over stdio, handling
  the initialize handshake, session lifecycle, and bidirectional communication
  (streaming updates from agent, permission/file requests from agent).

  ## Usage

      {:ok, client} = ExMCP.ACP.Client.start_link(
        command: ["gemini", "--acp"],
        handler: MyApp.ACPHandler
      )

      {:ok, %{"sessionId" => sid}} = ExMCP.ACP.Client.new_session(client, "/path/to/project")
      {:ok, %{"stopReason" => _}} = ExMCP.ACP.Client.prompt(client, sid, "Fix the bug in auth.ex")

  ## Options

  - `:command` — command list for the agent subprocess (required)
  - `:handler` — module implementing `ExMCP.ACP.Client.Handler` (default: `DefaultHandler`)
  - `:handler_opts` — options passed to `handler.init/1` (default: `[]`)
  - `:event_listener` — PID to receive `{:acp_session_update, session_id, update}` messages
  - `:client_info` — `%{"name" => ..., "version" => ...}` (default: `%{"name" => "ex_mcp", "version" => "0.1.0"}`)
  - `:capabilities` — client capabilities map
  - `:protocol_version` — integer (default: 1)
  - `:name` — GenServer name registration
  """

  use GenServer

  require Logger

  alias ExMCP.ACP.{Capabilities, LifecycleParams, Maps}
  alias ExMCP.ACP.Client.DefaultHandler
  alias ExMCP.ACP.Client.HandlerRunner
  alias ExMCP.ACP.Protocol
  alias ExMCP.Transport.Stdio

  @supported_protocol_versions [1]

  defstruct [
    :transport_mod,
    :transport_state,
    :receiver_pid,
    :agent_info,
    :agent_capabilities,
    :auth_methods,
    :handler_mod,
    :handler_pid,
    :event_listener,
    :protocol_version,
    pending_requests: %{},
    pending_agent_requests: %{},
    sessions: %{},
    # Accumulates streamed agent_message_chunk text per session so a synchronous
    # prompt/3 can return it — agents that stream the answer via session/update
    # otherwise leave the prompt result with no text.
    prompt_text: %{},
    status: :connecting
  ]

  # Public API

  @doc "Starts the ACP client and connects to the agent."
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    {gen_opts, client_opts} = Keyword.split(opts, [:name])
    GenServer.start_link(__MODULE__, client_opts, gen_opts)
  end

  @doc """
  Authenticates with the agent.

  Pass either a method ID advertised in the initialize response's
  `"authMethods"` list or a full params map for adapter compatibility.
  """
  @spec authenticate(GenServer.server(), String.t() | map(), keyword()) ::
          {:ok, map() | nil} | {:error, any()}
  def authenticate(client, method_id_or_params \\ %{}, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    GenServer.call(client, {:authenticate, method_id_or_params}, timeout)
  end

  @doc "Logs out of the current authenticated state if the agent supports `auth.logout`."
  @spec logout(GenServer.server(), keyword()) :: {:ok, map() | nil} | {:error, any()}
  def logout(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    GenServer.call(client, :logout, timeout)
  end

  @doc """
  Creates a new agent session.

  `cwd` is required per ACP spec
  (https://agentclientprotocol.com/protocol/session-setup).
  """
  @spec new_session(GenServer.server(), String.t(), keyword()) ::
          {:ok, map()} | {:error, any()}
  def new_session(client, cwd, opts \\ []) when is_binary(cwd) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    GenServer.call(client, {:new_session, cwd, LifecycleParams.client_opts(opts)}, timeout)
  end

  @doc """
  Loads an existing session and replays previous messages when the agent supports it.

  `cwd` is required per ACP spec.
  """
  @spec load_session(GenServer.server(), String.t(), String.t(), keyword()) ::
          {:ok, map()} | {:error, any()}
  def load_session(client, session_id, cwd, opts \\ []) when is_binary(cwd) do
    timeout = Keyword.get(opts, :timeout, 30_000)

    GenServer.call(
      client,
      {:load_session, session_id, cwd, LifecycleParams.client_opts(opts)},
      timeout
    )
  end

  @doc """
  Resumes an existing session without replaying previous messages.

  `cwd` is required per ACP spec.
  """
  @spec resume_session(GenServer.server(), String.t(), String.t(), keyword()) ::
          {:ok, map() | nil} | {:error, any()}
  def resume_session(client, session_id, cwd, opts \\ []) when is_binary(cwd) do
    timeout = Keyword.get(opts, :timeout, 30_000)

    GenServer.call(
      client,
      {:resume_session, session_id, cwd, LifecycleParams.client_opts(opts)},
      timeout
    )
  end

  @doc """
  Sends a prompt to the agent and blocks until the response arrives.

  Streaming `session/update` notifications are delivered to the handler and
  event listener as they arrive. The caller is unblocked when the agent sends
  the JSON-RPC result for the prompt request.
  """
  @spec prompt(GenServer.server(), String.t(), String.t() | [map()], keyword()) ::
          {:ok, map()} | {:error, any()}
  def prompt(client, session_id, content, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 300_000)
    GenServer.call(client, {:prompt, session_id, content}, timeout)
  end

  @doc "Lists available sessions from the agent. Stabilized in ACP spec March 9, 2026."
  @spec list_sessions(GenServer.server(), keyword()) :: {:ok, map()} | {:error, any()}
  def list_sessions(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    GenServer.call(client, {:list_sessions, opts}, timeout)
  end

  @doc "Cancels the current prompt in a session (fire-and-forget)."
  @spec cancel(GenServer.server(), String.t()) :: :ok
  def cancel(client, session_id) do
    GenServer.cast(client, {:cancel, session_id})
  end

  @doc "Closes an active session and frees agent-side resources."
  @spec close_session(GenServer.server(), String.t(), keyword()) ::
          {:ok, map() | nil} | {:error, any()}
  def close_session(client, session_id, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    GenServer.call(client, {:close_session, session_id}, timeout)
  end

  @doc "Deletes a session from the agent's session history."
  @spec delete_session(GenServer.server(), String.t(), keyword()) ::
          {:ok, map() | nil} | {:error, any()}
  def delete_session(client, session_id, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    GenServer.call(client, {:delete_session, session_id}, timeout)
  end

  @doc "Sets the agent mode for a session."
  @spec set_mode(GenServer.server(), String.t(), String.t()) :: {:ok, map()} | {:error, any()}
  def set_mode(client, session_id, mode_id) do
    GenServer.call(client, {:set_mode, session_id, mode_id})
  end

  @doc "Sets a config option for a session."
  @spec set_config_option(GenServer.server(), String.t(), String.t(), any()) ::
          {:ok, map()} | {:error, any()}
  def set_config_option(client, session_id, config_id, value) do
    GenServer.call(client, {:set_config_option, session_id, config_id, value})
  end

  @doc "Returns the agent's capabilities from the initialize handshake."
  @spec agent_capabilities(GenServer.server()) :: {:ok, map() | nil}
  def agent_capabilities(client) do
    GenServer.call(client, :agent_capabilities)
  end

  @doc "Returns the agent's authentication methods from the initialize handshake."
  @spec auth_methods(GenServer.server()) :: {:ok, [map()]}
  def auth_methods(client) do
    GenServer.call(client, :auth_methods)
  end

  @doc "Returns the client connection status."
  @spec status(GenServer.server()) :: atom()
  def status(client) do
    GenServer.call(client, :status)
  end

  @doc """
  Ends a session.

  Uses `session/close` when advertised by the agent, otherwise preserves the
  historical local telemetry-only behavior.
  """
  @spec end_session(GenServer.server(), String.t()) ::
          :ok | {:ok, map() | nil} | {:error, any()}
  def end_session(client, session_id) do
    GenServer.call(client, {:end_session, session_id})
  end

  @doc "Disconnects from the agent."
  @spec disconnect(GenServer.server()) :: :ok
  def disconnect(client) do
    GenServer.call(client, :disconnect)
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    Process.flag(:trap_exit, true)

    handler_mod = Keyword.get(opts, :handler, DefaultHandler)
    handler_opts = Keyword.get(opts, :handler_opts, [])

    case HandlerRunner.start_link(handler_mod, handler_opts, self()) do
      {:ok, handler_pid} ->
        state = %__MODULE__{
          transport_mod: Keyword.get(opts, :transport_mod, Stdio),
          handler_mod: handler_mod,
          handler_pid: handler_pid,
          event_listener: Keyword.get(opts, :event_listener),
          protocol_version: Keyword.get(opts, :protocol_version, 1)
        }

        # Allow skipping connection for tests
        if Keyword.get(opts, :_skip_connect) do
          {:ok, %{state | status: :ready}}
        else
          case connect_and_initialize(opts, state) do
            {:ok, state} -> {:ok, state}
            {:error, reason} -> {:stop, reason}
          end
        end

      {:error, reason} ->
        {:stop, {:handler_init_failed, reason}}
    end
  end

  @impl true
  def handle_call({:new_session, cwd, lifecycle_opts}, from, %{status: :ready} = state) do
    with :ok <- LifecycleParams.validate_cwd(cwd),
         :ok <- LifecycleParams.validate(lifecycle_opts, state.agent_capabilities) do
      msg = Protocol.encode_session_new(cwd, lifecycle_opts)
      send_request(msg, from, state, :new_session)
    else
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:authenticate, method_id_or_params}, from, %{status: :ready} = state) do
    msg = Protocol.encode_authenticate(method_id_or_params)
    send_request(msg, from, state)
  end

  def handle_call(:logout, from, %{status: :ready} = state) do
    case ensure_capability(state, :logout) do
      :ok ->
        msg = Protocol.encode_logout()
        send_request(msg, from, state)

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:list_sessions, opts}, from, %{status: :ready} = state) do
    case ensure_capability(state, :session_list) do
      :ok ->
        msg = Protocol.encode_session_list(opts)
        send_request(msg, from, state)

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(
        {:load_session, session_id, cwd, lifecycle_opts},
        from,
        %{status: :ready} = state
      ) do
    case ensure_capability(state, :load_session) do
      :ok ->
        with :ok <- LifecycleParams.validate_cwd(cwd),
             :ok <- LifecycleParams.validate(lifecycle_opts, state.agent_capabilities) do
          msg = Protocol.encode_session_load(session_id, cwd, lifecycle_opts)
          send_request(msg, from, state)
        else
          {:error, reason} -> {:reply, {:error, reason}, state}
        end

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(
        {:resume_session, session_id, cwd, lifecycle_opts},
        from,
        %{status: :ready} = state
      ) do
    case ensure_capability(state, :session_resume) do
      :ok ->
        with :ok <- LifecycleParams.validate_cwd(cwd),
             :ok <- LifecycleParams.validate(lifecycle_opts, state.agent_capabilities) do
          msg = Protocol.encode_session_resume(session_id, cwd, lifecycle_opts)
          send_request(msg, from, state)
        else
          {:error, reason} -> {:reply, {:error, reason}, state}
        end

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:prompt, session_id, content}, from, %{status: :ready} = state) do
    with {:ok, blocks} <- prompt_blocks(content),
         :ok <- validate_prompt_blocks(state, blocks) do
      :telemetry.execute(
        [:ex_mcp, :acp, :prompt, :sent],
        %{system_time: System.system_time()},
        %{session_id: session_id}
      )

      msg = Protocol.encode_session_prompt(session_id, blocks)
      state = %{state | prompt_text: Map.delete(state.prompt_text, session_id)}
      send_request(msg, from, state, {:prompt, session_id})
    else
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:set_mode, session_id, mode_id}, from, %{status: :ready} = state) do
    msg = Protocol.encode_session_set_mode(session_id, mode_id)
    send_request(msg, from, state)
  end

  def handle_call(
        {:set_config_option, session_id, config_id, value},
        from,
        %{status: :ready} = state
      ) do
    msg = Protocol.encode_session_set_config_option(session_id, config_id, value)
    send_request(msg, from, state)
  end

  def handle_call(:agent_capabilities, _from, state) do
    {:reply, {:ok, state.agent_capabilities}, state}
  end

  def handle_call(:auth_methods, _from, state) do
    {:reply, {:ok, state.auth_methods || []}, state}
  end

  def handle_call(:status, _from, state) do
    {:reply, state.status, state}
  end

  def handle_call({:close_session, session_id}, from, %{status: :ready} = state) do
    case ensure_capability(state, :session_close) do
      :ok ->
        msg = Protocol.encode_session_close(session_id)
        send_request(msg, from, state, {:close_session, session_id})

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:delete_session, session_id}, from, %{status: :ready} = state) do
    case ensure_capability(state, :session_delete) do
      :ok ->
        msg = Protocol.encode_session_delete(session_id)
        send_request(msg, from, state)

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:end_session, session_id}, from, %{status: :ready} = state) do
    case ensure_capability(state, :session_close) do
      :ok ->
        msg = Protocol.encode_session_close(session_id)
        send_request(msg, from, state, {:close_session, session_id})

      {:error, {:unsupported_capability, :session_close}} ->
        emit_session_ended(session_id)
        {:reply, :ok, state}
    end
  end

  def handle_call(:disconnect, _from, state) do
    state = do_disconnect(state)
    {:reply, :ok, state}
  end

  # Not ready
  def handle_call(_request, _from, %{status: status} = state) when status != :ready do
    {:reply, {:error, {:not_ready, status}}, state}
  end

  @impl true
  def handle_cast({:cancel, session_id}, state) do
    state = cancel_pending_permissions(session_id, state)
    msg = Protocol.encode_session_cancel(session_id)
    send_to_transport(msg, state)
    {:noreply, state}
  end

  @impl true
  def handle_info({:transport_message, raw_message}, state) do
    case Protocol.parse_message(raw_message) do
      {:result, result, id} ->
        state = resolve_pending(id, {:ok, result}, state)
        {:noreply, state}

      {:error, error, id} ->
        state = resolve_pending(id, {:error, error}, state)
        {:noreply, state}

      {:notification, "session/update", params} ->
        state = handle_session_update(params, state)
        {:noreply, state}

      {:request, method, params, id} ->
        state = handle_agent_request(method, params, id, state)
        {:noreply, state}

      other ->
        Logger.debug("ACP client received unexpected message: #{inspect(other)}")
        {:noreply, state}
    end
  end

  def handle_info({:acp_handler_result, ref, result}, state) do
    state = handle_handler_result(ref, result, state)
    {:noreply, state}
  end

  def handle_info({:transport_closed, _reason}, state) do
    Logger.info("ACP transport closed")
    state = reply_all_pending({:error, :transport_closed}, state)
    {:noreply, %{state | status: :disconnected}}
  end

  def handle_info({:transport_error, reason}, state) do
    Logger.warning("ACP transport error: #{inspect(reason)}")
    state = reply_all_pending({:error, {:transport_error, reason}}, state)
    {:noreply, %{state | status: :disconnected}}
  end

  def handle_info({:EXIT, pid, reason}, state) do
    cond do
      pid == state.receiver_pid ->
        if reason != :normal do
          Logger.warning("ACP receiver exited: #{inspect(reason)}")
        end

        state = reply_all_pending({:error, :receiver_exited}, state)
        {:noreply, %{state | status: :disconnected, receiver_pid: nil}}

      pid == state.handler_pid ->
        Logger.warning("ACP handler runner exited: #{inspect(reason)}")
        state = fail_pending_agent_requests("Handler unavailable", state)
        {:noreply, %{state | handler_pid: nil}}

      true ->
        {:noreply, state}
    end
  end

  def handle_info(_msg, state), do: {:noreply, state}

  @impl true
  def terminate(_reason, state) do
    do_disconnect(state)
    :ok
  end

  # Private helpers

  defp connect_and_initialize(opts, state) do
    transport_opts = build_transport_opts(opts)

    with {:ok, transport_state} <- state.transport_mod.connect(transport_opts) do
      state = %{state | transport_state: transport_state}

      # Start receiver loop
      receiver_pid = start_receiver(self(), state.transport_mod, transport_state)
      state = %{state | receiver_pid: receiver_pid}

      # Send initialize
      client_info =
        Keyword.get(opts, :client_info, %{"name" => "ex_mcp", "version" => "0.1.0"})

      # Per ACP spec: "capabilities omitted in initialize MUST be treated
      # as UNSUPPORTED." Auto-advertise fs/terminal capabilities based on
      # whether the handler module exports the corresponding callbacks —
      # otherwise the agent will never call them even if the handler can
      # answer. Explicit :capabilities opt takes precedence.
      auto_capabilities = auto_advertise_capabilities(state.handler_mod)
      explicit_capabilities = Keyword.get(opts, :capabilities)
      capabilities = Capabilities.merge(auto_capabilities, explicit_capabilities)

      init_msg = Protocol.encode_initialize(client_info, capabilities, state.protocol_version)

      with {:ok, _} <- do_send(init_msg, state),
           {:ok, result} <- receive_init_response(init_msg["id"]) do
        protocol_version = result["protocolVersion"] || state.protocol_version

        if protocol_version in @supported_protocol_versions do
          {:ok,
           %{
             state
             | agent_info: result["agentInfo"],
               agent_capabilities: result["agentCapabilities"],
               auth_methods: result["authMethods"] || [],
               protocol_version: protocol_version,
               status: :ready
           }}
        else
          do_disconnect(state)
          {:error, {:unsupported_protocol_version, protocol_version}}
        end
      end
    end
  end

  @client_keys [
    :name,
    :handler,
    :handler_opts,
    :event_listener,
    :client_info,
    :capabilities,
    :protocol_version,
    :transport_mod,
    :_skip_connect
  ]

  defp build_transport_opts(opts) do
    # Pass all non-client keys to the transport (command, cd, env, plus any test keys)
    Keyword.drop(opts, @client_keys)
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

  defp receive_init_response(request_id) do
    receive do
      {:transport_message, raw} ->
        case Protocol.parse_message(raw) do
          {:result, result, ^request_id} ->
            {:ok, result}

          {:error, error, ^request_id} ->
            {:error, {:agent_error, error}}

          _other ->
            # Skip non-matching messages during init
            receive_init_response(request_id)
        end
    after
      30_000 ->
        {:error, :init_timeout}
    end
  end

  defp send_request(msg, from, state, telemetry_tag \\ nil) do
    id = msg["id"]

    case do_send(msg, state) do
      {:ok, new_state} ->
        pending = Map.put(state.pending_requests, id, {from, telemetry_tag})
        {:noreply, %{new_state | pending_requests: pending}}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  defp send_to_transport(msg, state) do
    case do_send(msg, state) do
      {:ok, _state} -> :ok
      {:error, reason} -> Logger.warning("ACP send failed: #{inspect(reason)}")
    end
  end

  defp do_send(msg, state) do
    encoded = Jason.encode!(msg)

    case state.transport_mod.send_message(encoded, state.transport_state) do
      {:ok, new_transport_state} ->
        {:ok, %{state | transport_state: new_transport_state}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp resolve_pending(id, reply, state) do
    case Map.pop(state.pending_requests, id) do
      {nil, _pending} ->
        Logger.debug("ACP received response for unknown request #{id}")
        state

      {{from, telemetry_tag}, pending} ->
        {reply, state} = maybe_merge_prompt_text(telemetry_tag, reply, state)
        emit_resolve_telemetry(telemetry_tag, reply)
        GenServer.reply(from, reply)
        %{state | pending_requests: pending}

      {from, pending} ->
        GenServer.reply(from, reply)
        %{state | pending_requests: pending}
    end
  end

  # Fold any streamed agent_message_chunk text into the prompt result and clear the
  # buffer. Agents that return text inline keep theirs; others get the streamed text.
  defp maybe_merge_prompt_text({:prompt, session_id}, {:ok, result}, state) when is_map(result) do
    {buffered, prompt_text} = Map.pop(state.prompt_text, session_id)
    meta_text = get_in(result, ["_meta", "ex_mcp", "text"])

    result =
      case buffered do
        text when is_binary(text) and text != "" ->
          case result["text"] do
            existing when is_binary(existing) and existing != "" -> result
            _ -> Map.put(result, "text", text)
          end

        _ ->
          if is_binary(meta_text) and meta_text != "" do
            Map.put_new(result, "text", meta_text)
          else
            result
          end
      end

    {{:ok, result}, %{state | prompt_text: prompt_text}}
  end

  defp maybe_merge_prompt_text({:prompt, session_id}, reply, state) do
    {_, prompt_text} = Map.pop(state.prompt_text, session_id)
    {reply, %{state | prompt_text: prompt_text}}
  end

  defp maybe_merge_prompt_text(_tag, reply, state), do: {reply, state}

  defp emit_resolve_telemetry(:new_session, {:ok, result}) do
    session_id = result["sessionId"]

    :telemetry.execute(
      [:ex_mcp, :acp, :session, :started],
      %{system_time: System.system_time()},
      %{session_id: session_id}
    )
  end

  defp emit_resolve_telemetry({:prompt, session_id}, {:ok, result}) do
    stop_reason = result["stopReason"]

    :telemetry.execute(
      [:ex_mcp, :acp, :prompt, :completed],
      %{system_time: System.system_time()},
      %{session_id: session_id, stop_reason: stop_reason}
    )
  end

  defp emit_resolve_telemetry({:close_session, session_id}, {:ok, _result}) do
    emit_session_ended(session_id)
  end

  defp emit_resolve_telemetry(_, _), do: :ok

  # Build a clientCapabilities map reflecting which optional ACP callbacks
  # the handler module actually exports. Per spec, capabilities omitted in
  # initialize MUST be treated as unsupported — so a missing advertisement
  # means the agent will never invoke that capability.
  defp auto_advertise_capabilities(nil), do: nil

  defp auto_advertise_capabilities(handler_mod) when is_atom(handler_mod) do
    Code.ensure_loaded(handler_mod)

    fs =
      %{}
      |> maybe_put_cap("readTextFile", function_exported?(handler_mod, :handle_file_read, 4))
      |> maybe_put_cap("writeTextFile", function_exported?(handler_mod, :handle_file_write, 4))

    caps = %{}
    caps = if map_size(fs) > 0, do: Map.put(caps, "fs", fs), else: caps

    caps =
      if function_exported?(handler_mod, :handle_terminal_request, 4) do
        Map.put(caps, "terminal", true)
      else
        caps
      end

    if map_size(caps) > 0, do: caps, else: nil
  end

  defp maybe_put_cap(map, _key, false), do: map
  defp maybe_put_cap(map, key, true), do: Map.put(map, key, true)

  # Explicit :capabilities opt fully replaces auto-detected. Auto only
  # fills in when no explicit caps are passed. This preserves the
  # contract that `capabilities: %{}` means "advertise nothing" —
  # otherwise auto-fill from handler exports would override a caller's
  # explicit suppression.
  defp emit_session_ended(session_id) do
    :telemetry.execute(
      [:ex_mcp, :acp, :session, :ended],
      %{system_time: System.system_time()},
      %{session_id: session_id}
    )
  end

  defp ensure_capability(state, capability) do
    Capabilities.ensure(state.agent_capabilities || %{}, capability)
  end

  defp prompt_blocks(text) when is_binary(text), do: {:ok, [%{"type" => "text", "text" => text}]}
  defp prompt_blocks(blocks) when is_list(blocks), do: {:ok, blocks}
  defp prompt_blocks(_content), do: {:error, {:invalid_params, :prompt_must_be_a_list}}

  defp validate_prompt_blocks(state, blocks) when is_list(blocks) do
    Enum.reduce_while(blocks, :ok, fn block, :ok ->
      case validate_prompt_block(state.agent_capabilities || %{}, block) do
        :ok -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp validate_prompt_block(_caps, %{"type" => type}) when type in ["text", "resource_link"],
    do: :ok

  defp validate_prompt_block(caps, %{"type" => "image"}) do
    validate_prompt_capability(caps, "image", :image)
  end

  defp validate_prompt_block(caps, %{"type" => "audio"}) do
    validate_prompt_capability(caps, "audio", :audio)
  end

  defp validate_prompt_block(caps, %{"type" => "resource"}) do
    validate_prompt_capability(caps, "embeddedContext", :embedded_context)
  end

  defp validate_prompt_block(_caps, %{"type" => type}) do
    {:error, {:unsupported_prompt_content, type}}
  end

  defp validate_prompt_block(_caps, _block), do: {:error, {:invalid_params, :prompt_block}}

  defp validate_prompt_capability(caps, key, capability) do
    if caps |> Maps.get("promptCapabilities") |> Maps.get(key) |> Maps.truthy?() do
      :ok
    else
      {:error, {:unsupported_capability, {:prompt, capability}}}
    end
  end

  defp reply_all_pending(error, state) do
    for {_id, pending} <- state.pending_requests do
      case pending do
        {from, _tag} -> GenServer.reply(from, error)
        from -> GenServer.reply(from, error)
      end
    end

    %{state | pending_requests: %{}, pending_agent_requests: %{}}
  end

  defp handle_session_update(params, state) do
    session_id = params["sessionId"]
    update = params["update"]

    # Buffer streamed answer text so prompt/3 can return it (see prompt_text).
    state = accumulate_prompt_text(state, session_id, update)

    # Notify event listener from the client process so a slow handler cannot
    # stall the update stream.
    if state.event_listener do
      send(state.event_listener, {:acp_session_update, session_id, update})
    end

    if state.handler_pid do
      HandlerRunner.session_update(state.handler_pid, session_id, update)
    end

    state
  end

  # Append agent_message_chunk text to the per-session buffer. Only the assistant's
  # message text is buffered — thought chunks and other update types are ignored.
  defp accumulate_prompt_text(
         state,
         session_id,
         %{"sessionUpdate" => "agent_message_chunk"} = update
       )
       when is_binary(session_id) do
    if prompt_pending?(state, session_id) do
      case get_in(update, ["content", "text"]) do
        text when is_binary(text) and text != "" ->
          %{state | prompt_text: Map.update(state.prompt_text, session_id, text, &(&1 <> text))}

        _ ->
          state
      end
    else
      state
    end
  end

  defp accumulate_prompt_text(state, _session_id, _update), do: state

  defp prompt_pending?(state, session_id) do
    Enum.any?(state.pending_requests, fn
      {_id, {_from, {:prompt, ^session_id}}} -> true
      _ -> false
    end)
  end

  defp handle_agent_request("session/request_permission", params, id, state) do
    session_id = params["sessionId"]
    tool_call = params["toolCall"]
    options = params["options"] || []

    if state.handler_pid do
      ref = make_ref()
      HandlerRunner.permission_request(state.handler_pid, ref, session_id, tool_call, options)
      track_agent_request(state, ref, :permission, id, session_id)
    else
      response = Protocol.encode_error(-32603, "Handler unavailable", nil, id)
      send_to_transport(response, state)
      state
    end
  end

  defp handle_agent_request("fs/read_text_file", params, id, state) do
    session_id = params["sessionId"]
    path = params["path"]
    opts = Map.drop(params, ["sessionId", "path"])

    if state.handler_pid && function_exported?(state.handler_mod, :handle_file_read, 4) do
      ref = make_ref()
      HandlerRunner.file_read(state.handler_pid, ref, session_id, path, opts)
      track_agent_request(state, ref, :file_read, id, session_id)
    else
      response = Protocol.encode_error(-32601, "File read not supported", nil, id)
      send_to_transport(response, state)
      state
    end
  end

  defp handle_agent_request("fs/write_text_file", params, id, state) do
    session_id = params["sessionId"]
    path = params["path"]
    content = params["content"]

    if state.handler_pid && function_exported?(state.handler_mod, :handle_file_write, 4) do
      ref = make_ref()
      HandlerRunner.file_write(state.handler_pid, ref, session_id, path, content)
      track_agent_request(state, ref, :file_write, id, session_id)
    else
      response = Protocol.encode_error(-32601, "File write not supported", nil, id)
      send_to_transport(response, state)
      state
    end
  end

  # Terminal operations — spec-defined but delegated to handler
  defp handle_agent_request("terminal/" <> _ = method, params, id, state) do
    if state.handler_pid && function_exported?(state.handler_mod, :handle_terminal_request, 4) do
      ref = make_ref()
      HandlerRunner.terminal_request(state.handler_pid, ref, method, params, id)
      track_agent_request(state, ref, :terminal, id, params["sessionId"], %{method: method})
    else
      response = Protocol.encode_error(-32601, "Terminal operations not supported", nil, id)
      send_to_transport(response, state)
      state
    end
  end

  defp handle_agent_request(method, _params, id, state) do
    Logger.debug("ACP client received unknown agent request: #{method}")
    response = Protocol.encode_error(-32601, "Method not found: #{method}", nil, id)
    send_to_transport(response, state)
    state
  end

  defp handle_handler_result(ref, result, state) do
    case Map.pop(state.pending_agent_requests, ref) do
      {nil, _pending} ->
        state

      {request, pending} ->
        state = %{state | pending_agent_requests: pending}
        response = encode_handler_response(request, result)
        send_to_transport(response, state)
        state
    end
  end

  defp encode_handler_response(%{kind: :permission, id: id}, {:permission, {:ok, outcome}}) do
    Protocol.encode_permission_response(id, outcome)
  end

  defp encode_handler_response(%{kind: :file_read, id: id}, {:file_read, {:ok, content}}) do
    Protocol.encode_file_read_response(id, content)
  end

  defp encode_handler_response(%{kind: :file_write, id: id}, {:file_write, :ok}) do
    Protocol.encode_file_write_response(id)
  end

  defp encode_handler_response(
         %{kind: :terminal, id: id, method: "terminal/output"},
         {:terminal, {:ok, result}}
       )
       when is_map(result) do
    Protocol.encode_response(Map.put_new(result, "truncated", false), id)
  end

  defp encode_handler_response(%{kind: :terminal, id: id}, {:terminal, {:ok, result}}) do
    Protocol.encode_response(result, id)
  end

  defp encode_handler_response(%{id: id}, {_kind, {:error, reason}}) do
    Protocol.encode_error(-32603, format_handler_error(reason), nil, id)
  end

  defp encode_handler_response(%{id: id}, unexpected) do
    Protocol.encode_error(-32603, "Unexpected handler result: #{inspect(unexpected)}", nil, id)
  end

  defp track_agent_request(state, ref, kind, id, session_id, extra \\ %{}) do
    request = Map.merge(%{kind: kind, id: id, session_id: session_id}, extra)
    %{state | pending_agent_requests: Map.put(state.pending_agent_requests, ref, request)}
  end

  defp fail_pending_agent_requests(reason, state) do
    Enum.each(state.pending_agent_requests, fn {_ref, request} ->
      response = Protocol.encode_error(-32603, reason, nil, request.id)
      send_to_transport(response, state)
    end)

    %{state | pending_agent_requests: %{}}
  end

  defp cancel_pending_permissions(session_id, state) do
    {to_cancel, keep} =
      Enum.split_with(state.pending_agent_requests, fn {_ref, request} ->
        request.kind == :permission and request.session_id == session_id
      end)

    Enum.each(to_cancel, fn {_ref, request} ->
      response = Protocol.encode_permission_response(request.id, %{"outcome" => "cancelled"})
      send_to_transport(response, state)
    end)

    %{state | pending_agent_requests: Map.new(keep)}
  end

  defp format_handler_error(reason) when is_binary(reason), do: reason

  defp format_handler_error({kind, reason, stack}) when is_list(stack) do
    Exception.format(kind, reason, stack)
  end

  defp format_handler_error(reason), do: inspect(reason)

  defp do_disconnect(state) do
    if state.receiver_pid && Process.alive?(state.receiver_pid) do
      Process.exit(state.receiver_pid, :shutdown)
    end

    if state.transport_state do
      state.transport_mod.close(state.transport_state)
    end

    reply_all_pending({:error, :disconnected}, state)
    |> Map.merge(%{status: :disconnected, receiver_pid: nil, transport_state: nil})
  end
end
