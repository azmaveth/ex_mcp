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

  alias ExMCP.ACP.Client.DefaultHandler
  alias ExMCP.ACP.Protocol
  alias ExMCP.Transport.Stdio

  defstruct [
    :transport_mod,
    :transport_state,
    :receiver_pid,
    :agent_info,
    :agent_capabilities,
    :handler_mod,
    :handler_state,
    :event_listener,
    :protocol_version,
    pending_requests: %{},
    sessions: %{},
    status: :connecting
  ]

  # Public API

  @doc "Starts the ACP client and connects to the agent."
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    {gen_opts, client_opts} = Keyword.split(opts, [:name])
    GenServer.start_link(__MODULE__, client_opts, gen_opts)
  end

  @doc "Creates a new agent session."
  @spec new_session(GenServer.server(), String.t() | nil, keyword()) ::
          {:ok, map()} | {:error, any()}
  def new_session(client, cwd \\ nil, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    mcp_servers = Keyword.get(opts, :mcp_servers)
    GenServer.call(client, {:new_session, cwd, mcp_servers}, timeout)
  end

  @doc "Loads (resumes) an existing session."
  @spec load_session(GenServer.server(), String.t(), String.t() | nil, keyword()) ::
          {:ok, map()} | {:error, any()}
  def load_session(client, session_id, cwd \\ nil, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    mcp_servers = Keyword.get(opts, :mcp_servers)
    GenServer.call(client, {:load_session, session_id, cwd, mcp_servers}, timeout)
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

  @doc "Returns the client connection status."
  @spec status(GenServer.server()) :: atom()
  def status(client) do
    GenServer.call(client, :status)
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

    case handler_mod.init(handler_opts) do
      {:ok, handler_state} ->
        state = %__MODULE__{
          transport_mod: Keyword.get(opts, :transport_mod, Stdio),
          handler_mod: handler_mod,
          handler_state: handler_state,
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
  def handle_call({:new_session, cwd, mcp_servers}, from, %{status: :ready} = state) do
    msg = Protocol.encode_session_new(cwd, mcp_servers)
    send_request(msg, from, state)
  end

  def handle_call({:list_sessions, opts}, from, %{status: :ready} = state) do
    msg = Protocol.encode_session_list(opts)
    send_request(msg, from, state)
  end

  def handle_call({:load_session, session_id, cwd, mcp_servers}, from, %{status: :ready} = state) do
    msg = Protocol.encode_session_load(session_id, cwd, mcp_servers)
    send_request(msg, from, state)
  end

  def handle_call({:prompt, session_id, content}, from, %{status: :ready} = state) do
    blocks =
      case content do
        text when is_binary(text) -> [%{"type" => "text", "text" => text}]
        blocks when is_list(blocks) -> blocks
      end

    msg = Protocol.encode_session_prompt(session_id, blocks)
    send_request(msg, from, state)
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

  def handle_call(:status, _from, state) do
    {:reply, state.status, state}
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
    if pid == state.receiver_pid do
      if reason != :normal do
        Logger.warning("ACP receiver exited: #{inspect(reason)}")
      end

      state = reply_all_pending({:error, :receiver_exited}, state)
      {:noreply, %{state | status: :disconnected, receiver_pid: nil}}
    else
      {:noreply, state}
    end
  end

  def handle_info(_msg, state), do: {:noreply, state}

  @impl true
  def terminate(reason, state) do
    if function_exported?(state.handler_mod, :terminate, 2) do
      state.handler_mod.terminate(reason, state.handler_state)
    end

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

      capabilities = Keyword.get(opts, :capabilities)

      init_msg = Protocol.encode_initialize(client_info, capabilities, state.protocol_version)

      with {:ok, _} <- do_send(init_msg, state),
           {:ok, result} <- receive_init_response(init_msg["id"]) do
        {:ok,
         %{
           state
           | agent_info: result["agentInfo"],
             agent_capabilities: result["agentCapabilities"],
             protocol_version: result["protocolVersion"] || state.protocol_version,
             status: :ready
         }}
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

  defp send_request(msg, from, state) do
    id = msg["id"]

    case do_send(msg, state) do
      {:ok, new_state} ->
        pending = Map.put(state.pending_requests, id, from)
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

      {from, pending} ->
        GenServer.reply(from, reply)
        %{state | pending_requests: pending}
    end
  end

  defp reply_all_pending(error, state) do
    for {_id, from} <- state.pending_requests do
      GenServer.reply(from, error)
    end

    %{state | pending_requests: %{}}
  end

  defp handle_session_update(params, state) do
    session_id = params["sessionId"]
    update = params["update"]

    # Notify handler
    {:ok, new_handler_state} =
      state.handler_mod.handle_session_update(session_id, update, state.handler_state)

    # Notify event listener
    if state.event_listener do
      send(state.event_listener, {:acp_session_update, session_id, update})
    end

    %{state | handler_state: new_handler_state}
  end

  defp handle_agent_request("session/request_permission", params, id, state) do
    session_id = params["sessionId"]
    tool_call = params["toolCall"]
    options = params["options"] || []

    {:ok, outcome, new_handler_state} =
      state.handler_mod.handle_permission_request(
        session_id,
        tool_call,
        options,
        state.handler_state
      )

    response = Protocol.encode_permission_response(id, outcome)
    send_to_transport(response, state)

    %{state | handler_state: new_handler_state}
  end

  defp handle_agent_request("fs/read_text_file", params, id, state) do
    session_id = params["sessionId"]
    path = params["path"]
    opts = Map.drop(params, ["sessionId", "path"])

    if function_exported?(state.handler_mod, :handle_file_read, 4) do
      case state.handler_mod.handle_file_read(session_id, path, opts, state.handler_state) do
        {:ok, content, new_handler_state} ->
          response = Protocol.encode_file_read_response(id, content)
          send_to_transport(response, state)
          %{state | handler_state: new_handler_state}

        {:error, reason, new_handler_state} ->
          response = Protocol.encode_error(-32603, reason, nil, id)
          send_to_transport(response, state)
          %{state | handler_state: new_handler_state}
      end
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

    if function_exported?(state.handler_mod, :handle_file_write, 4) do
      case state.handler_mod.handle_file_write(session_id, path, content, state.handler_state) do
        {:ok, new_handler_state} ->
          response = Protocol.encode_file_write_response(id)
          send_to_transport(response, state)
          %{state | handler_state: new_handler_state}

        {:error, reason, new_handler_state} ->
          response = Protocol.encode_error(-32603, reason, nil, id)
          send_to_transport(response, state)
          %{state | handler_state: new_handler_state}
      end
    else
      response = Protocol.encode_error(-32601, "File write not supported", nil, id)
      send_to_transport(response, state)
      state
    end
  end

  # Terminal operations — spec-defined but delegated to handler
  defp handle_agent_request("terminal/" <> _ = method, params, id, state) do
    if function_exported?(state.handler_mod, :handle_terminal_request, 4) do
      case state.handler_mod.handle_terminal_request(method, params, id, state.handler_state) do
        {:ok, result, new_handler_state} ->
          response = Protocol.encode_response(result, id)
          send_to_transport(response, state)
          %{state | handler_state: new_handler_state}

        {:error, reason, new_handler_state} ->
          response = Protocol.encode_error(-32603, reason, nil, id)
          send_to_transport(response, state)
          %{state | handler_state: new_handler_state}
      end
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
