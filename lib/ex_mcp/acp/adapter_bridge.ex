defmodule ExMCP.ACP.AdapterBridge do
  @moduledoc """
  GenServer bridge between ACP clients and non-native CLI agents.

  Owns the Port subprocess and delegates translation to a pluggable
  `ExMCP.ACP.Adapter` implementation. Uses an outbox + waiters queue
  for synchronized message delivery.

  ## Modes

  - **Persistent** (default) — opens a Port on init, keeps it alive
  - **One-shot** — adapter manages subprocess per prompt (Codex pattern)

  ## Usage

      {:ok, bridge} = AdapterBridge.start_link(
        adapter: ExMCP.ACP.Adapters.Claude,
        adapter_opts: [model: "sonnet"]
      )

      :ok = AdapterBridge.send_message(bridge, json_rpc_string)
      {:ok, response} = AdapterBridge.receive_message(bridge)
  """

  use GenServer

  alias ExMCP.ACP.{Capabilities, Envelope, NameValue}

  @type t :: GenServer.server()

  defstruct [
    :adapter_mod,
    :adapter_state,
    :adapter_opts,
    :port,
    :outbox,
    :waiters,
    buffer: "",
    status: :connecting
  ]

  # Public API

  @doc "Start the bridge linked to the caller."
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    {gen_opts, bridge_opts} = Keyword.split(opts, [:name])
    GenServer.start_link(__MODULE__, bridge_opts, gen_opts)
  end

  @doc "Send a JSON-encoded ACP message to the agent."
  @spec send_message(t(), String.t()) :: :ok | {:error, term()}
  def send_message(bridge, json) do
    GenServer.call(bridge, {:send, json})
  end

  @doc "Receive the next ACP message from the agent. Blocks until available."
  @spec receive_message(t(), timeout()) :: {:ok, String.t()} | {:error, term()}
  def receive_message(bridge, timeout \\ 30_000) do
    GenServer.call(bridge, :receive, timeout)
  end

  @doc "Close the bridge and terminate the subprocess."
  @spec close(t()) :: :ok
  def close(bridge) do
    GenServer.call(bridge, :close)
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    Process.flag(:trap_exit, true)

    adapter_mod = Keyword.fetch!(opts, :adapter)
    adapter_opts = Keyword.get(opts, :adapter_opts, [])

    {:ok, adapter_state} = adapter_mod.init(adapter_opts)

    state = %__MODULE__{
      adapter_mod: adapter_mod,
      adapter_state: adapter_state,
      adapter_opts: adapter_opts,
      outbox: :queue.new(),
      waiters: :queue.new()
    }

    case adapter_mod.command(adapter_opts) do
      :one_shot ->
        # One-shot adapters don't open a Port on init
        # Init response is synthesized when the Client sends the initialize request
        {:ok, %{state | status: :ready}}

      {cmd, args} ->
        case open_port(cmd, args, adapter_opts, adapter_mod) do
          {:ok, port} ->
            state = %{state | port: port, status: :ready}
            state = maybe_post_connect(state)
            {:ok, state}

          {:error, reason} ->
            {:stop, reason}
        end
    end
  end

  @impl true
  def handle_call({:send, _json}, _from, %{status: :closed} = state) do
    {:reply, {:error, :closed}, state}
  end

  def handle_call({:send, json}, from, state) do
    case Jason.decode(json) do
      {:ok, msg} ->
        method = msg["method"]

        if method do
          :telemetry.execute(
            [:ex_mcp, :acp, :request, :received],
            %{system_time: System.system_time()},
            %{method: method}
          )
        end

        result = handle_outbound(msg, json, from, state)

        if method do
          :telemetry.execute(
            [:ex_mcp, :acp, :request, :completed],
            %{system_time: System.system_time()},
            %{method: method}
          )
        end

        result

      {:error, reason} ->
        {:reply, {:error, {:decode_error, reason}}, state}
    end
  end

  def handle_call(:receive, from, state) do
    case :queue.out(state.outbox) do
      {{:value, message}, rest} ->
        {:reply, {:ok, message}, %{state | outbox: rest}}

      {:empty, _} ->
        if state.status == :closed do
          {:reply, {:error, :closed}, state}
        else
          waiters = :queue.in(from, state.waiters)
          {:noreply, %{state | waiters: waiters}}
        end
    end
  end

  def handle_call(:close, _from, state) do
    state = do_close(state)
    {:stop, :normal, :ok, state}
  end

  @impl true
  def handle_info({port, {:data, data}}, %{port: port} = state) do
    state = process_port_data(state, data)
    {:noreply, state}
  end

  def handle_info({port, {:exit_status, _code}}, %{port: port} = state) do
    state = flush_buffer(state)
    state = reply_error_to_waiters(state, :port_exited)
    {:noreply, %{state | port: nil, status: :closed}}
  end

  def handle_info({port, :closed}, %{port: port} = state) do
    state = reply_error_to_waiters(state, :port_closed)
    {:noreply, %{state | port: nil, status: :closed}}
  end

  def handle_info({:EXIT, _pid, _reason}, state) do
    {:noreply, state}
  end

  def handle_info({:one_shot_result, messages}, state) do
    state = push_messages(state, messages)
    {:noreply, state}
  end

  def handle_info(_msg, state), do: {:noreply, state}

  @impl true
  def terminate(_reason, state) do
    do_close(state)
    :ok
  end

  # Private helpers

  defp open_port(cmd, args, opts, adapter_mod) do
    executable = System.find_executable(cmd)

    if executable do
      cwd = Keyword.get(opts, :cwd, File.cwd!())

      port_opts = [
        :binary,
        :exit_status,
        :use_stdio,
        :stderr_to_stdout,
        args: Enum.map(args, &to_charlist/1),
        cd: to_charlist(cwd),
        env: safe_env(opts, adapter_mod)
      ]

      try do
        port = Port.open({:spawn_executable, to_charlist(executable)}, port_opts)
        {:ok, port}
      catch
        :error, reason -> {:error, {:port_open_failed, reason}}
      end
    else
      {:error, {:executable_not_found, cmd}}
    end
  end

  defp synthesize_result(state, request_id, result) do
    push_message(state, request_id |> Envelope.response(result) |> Jason.encode!())
  end

  defp synthesize_error(state, request_id, code, message) do
    push_message(state, request_id |> Envelope.error(code, message) |> Jason.encode!())
  end

  defp synthesize_init_response(state, request_id) do
    caps =
      if function_exported?(state.adapter_mod, :capabilities, 0) do
        state.adapter_mod.capabilities()
      else
        %{}
      end

    caps = maybe_add_adapter_session_capabilities(caps, state.adapter_mod)

    init_result =
      Envelope.response(request_id, %{
        "agentInfo" => %{
          "name" => adapter_name(state.adapter_mod),
          "version" => "1.0.0"
        },
        "agentCapabilities" => caps,
        "authMethods" => adapter_auth_methods(state),
        "protocolVersion" => 1
      })

    push_message(state, Jason.encode!(init_result))
  end

  defp maybe_add_adapter_session_capabilities(caps, adapter_mod) do
    Capabilities.advertise_adapter_session_list(caps, adapter_mod)
    |> Capabilities.advertise_adapter_session_fork(adapter_mod)
  end

  defp advertised_capabilities(state) do
    if function_exported?(state.adapter_mod, :capabilities, 0) do
      state.adapter_mod.capabilities()
    else
      %{}
    end
    |> maybe_add_adapter_session_capabilities(state.adapter_mod)
  end

  defp ensure_capability(state, :load_session),
    do: state |> advertised_capabilities() |> Capabilities.supported?(:load_session)

  defp ensure_capability(state, :logout),
    do: state |> advertised_capabilities() |> Capabilities.supported?(:logout)

  defp ensure_capability(state, :session_list),
    do: state |> advertised_capabilities() |> Capabilities.supported?(:session_list)

  defp ensure_capability(state, :session_resume),
    do: state |> advertised_capabilities() |> Capabilities.supported?(:session_resume)

  defp ensure_capability(state, :session_close),
    do: state |> advertised_capabilities() |> Capabilities.supported?(:session_close)

  defp ensure_capability(state, :session_delete),
    do: state |> advertised_capabilities() |> Capabilities.supported?(:session_delete)

  defp ensure_capability(state, :session_fork),
    do: state |> advertised_capabilities() |> Capabilities.supported?(:session_fork)

  defp adapter_auth_methods(state) do
    if function_exported?(state.adapter_mod, :auth_methods, 1) do
      state.adapter_mod.auth_methods(state.adapter_opts)
    else
      []
    end
  end

  defp reject_unsupported_method(state, id, method) do
    synthesize_error(state, id, -32_601, "Method not found: #{method}")
  end

  defp session_result(state, session_id) do
    state
    |> session_state_result()
    |> Map.put("sessionId", session_id)
  end

  defp session_state_result(state) do
    %{}
    |> maybe_put_non_empty("modes", session_modes(state))
    |> maybe_put_non_empty("configOptions", adapter_config_options(state))
  end

  defp config_options_result(state) do
    %{"configOptions" => adapter_config_options(state)}
  end

  defp session_modes(state) do
    if function_exported?(state.adapter_mod, :modes, 0) do
      case state.adapter_mod.modes() do
        [] -> nil
        modes -> %{"availableModes" => modes, "currentModeId" => current_mode_id(modes)}
      end
    end
  end

  defp adapter_config_options(state) do
    if function_exported?(state.adapter_mod, :config_options, 0) do
      state.adapter_mod.config_options()
    else
      []
    end
  end

  defp current_mode_id([%{"id" => id} | _]), do: id
  defp current_mode_id([%{id: id} | _]), do: id
  defp current_mode_id(_), do: nil

  defp maybe_put_non_empty(map, _key, nil), do: map
  defp maybe_put_non_empty(map, _key, []), do: map
  defp maybe_put_non_empty(map, key, value), do: Map.put(map, key, value)

  defp maybe_post_connect(%{adapter_mod: adapter_mod, adapter_state: adapter_state} = state) do
    if function_exported?(adapter_mod, :post_connect, 1) do
      case adapter_mod.post_connect(adapter_state) do
        {:ok, data, new_adapter_state} ->
          _ = write_to_port(state, data)
          %{state | adapter_state: new_adapter_state}

        {:ok, new_adapter_state} ->
          %{state | adapter_state: new_adapter_state}
      end
    else
      state
    end
  end

  defp adapter_name(mod) do
    mod
    |> Module.split()
    |> List.last()
    |> String.downcase()
  end

  # Synthesize responses for ACP methods that adapted agents don't handle natively.
  # The Client sends these as normal JSON-RPC requests and expects matching responses.

  defp handle_outbound(%{"method" => "authenticate", "id" => id} = msg, _json, _from, state) do
    # Delegate to adapter. If it writes to the native process, the adapter is
    # responsible for producing the eventual ACP response from the native
    # response. A plain `:skip` is not a successful authentication.
    msg
    |> translate_outbound_message(state)
    |> handle_authenticate_translation_result(msg, id)
  end

  defp handle_outbound(%{"method" => "logout", "id" => id} = msg, _json, _from, state) do
    if ensure_capability(state, :logout) do
      synthesize_after_translate(msg, id, state, fn _state -> %{} end)
    else
      {:reply, :ok, reject_unsupported_method(state, id, "logout")}
    end
  end

  defp handle_outbound(%{"method" => "initialize", "id" => id} = msg, _json, _from, state) do
    case state.adapter_mod.translate_outbound(msg, state.adapter_state) do
      {:ok, :skip, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        state = synthesize_init_response(state, id)
        {:reply, :ok, state}

      {:ok, data, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        state = synthesize_init_response(state, id)
        _ = write_to_port(state, data)
        {:reply, :ok, state}

      {:reply, _result, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        state = synthesize_init_response(state, id)
        {:reply, :ok, state}
    end
  end

  defp handle_outbound(%{"method" => "session/new", "id" => id} = msg, _json, _from, state) do
    case state.adapter_mod.translate_outbound(msg, state.adapter_state) do
      {:ok, :skip, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        session_id = "session_#{System.unique_integer([:positive])}"
        state = synthesize_result(state, id, session_result(state, session_id))
        {:reply, :ok, state}

      {:ok, data, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        _ = write_to_port(state, data)
        {:reply, :ok, state}

      {:reply, result, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}

        state =
          synthesize_result(state, id, Map.merge(session_state_result(state), result || %{}))

        {:reply, :ok, state}
    end
  end

  defp handle_outbound(%{"method" => "session/load", "id" => id} = msg, _json, _from, state) do
    if ensure_capability(state, :load_session) do
      case state.adapter_mod.translate_outbound(msg, state.adapter_state) do
        {:ok, :skip, new_adapter_state} ->
          state = %{state | adapter_state: new_adapter_state}
          state = synthesize_result(state, id, session_state_result(state))
          {:reply, :ok, state}

        {:ok, data, new_adapter_state} ->
          state = %{state | adapter_state: new_adapter_state}
          _ = write_to_port(state, data)
          {:reply, :ok, state}

        {:reply, result, new_adapter_state} ->
          state = %{state | adapter_state: new_adapter_state}

          state =
            synthesize_result(state, id, Map.merge(session_state_result(state), result || %{}))

          {:reply, :ok, state}

        {:messages_and_reply, messages, result, new_adapter_state} ->
          state = %{state | adapter_state: new_adapter_state}
          state = push_messages(state, Enum.map(messages, &Jason.encode!/1))

          state =
            synthesize_result(state, id, Map.merge(session_state_result(state), result || %{}))

          {:reply, :ok, state}

        {:error, reason, new_adapter_state} ->
          state = %{state | adapter_state: new_adapter_state}
          state = synthesize_error(state, id, -32_603, to_string(reason))
          {:reply, :ok, state}
      end
    else
      {:reply, :ok, reject_unsupported_method(state, id, "session/load")}
    end
  end

  defp handle_outbound(%{"method" => "session/resume", "id" => id} = msg, _json, _from, state) do
    if ensure_capability(state, :session_resume) do
      case state.adapter_mod.translate_outbound(msg, state.adapter_state) do
        {:ok, :skip, new_adapter_state} ->
          state = %{state | adapter_state: new_adapter_state}
          state = synthesize_result(state, id, session_state_result(state))
          {:reply, :ok, state}

        {:ok, data, new_adapter_state} ->
          state = %{state | adapter_state: new_adapter_state}
          _ = write_to_port(state, data)
          {:reply, :ok, state}

        {:reply, result, new_adapter_state} ->
          state = %{state | adapter_state: new_adapter_state}

          state =
            synthesize_result(state, id, Map.merge(session_state_result(state), result || %{}))

          {:reply, :ok, state}

        {:messages_and_reply, messages, result, new_adapter_state} ->
          state = %{state | adapter_state: new_adapter_state}
          state = push_messages(state, Enum.map(messages, &Jason.encode!/1))

          state =
            synthesize_result(state, id, Map.merge(session_state_result(state), result || %{}))

          {:reply, :ok, state}

        {:error, reason, new_adapter_state} ->
          state = %{state | adapter_state: new_adapter_state}
          state = synthesize_error(state, id, -32_603, to_string(reason))
          {:reply, :ok, state}
      end
    else
      {:reply, :ok, reject_unsupported_method(state, id, "session/resume")}
    end
  end

  defp handle_outbound(%{"method" => "session/fork", "id" => id} = msg, _json, _from, state) do
    cond do
      not ensure_capability(state, :session_fork) ->
        {:reply, :ok, reject_unsupported_method(state, id, "session/fork")}

      function_exported?(state.adapter_mod, :fork_session, 2) ->
        handle_adapter_fork_callback(msg, id, state)

      true ->
        msg
        |> translate_outbound_message(state)
        |> handle_fork_translation(id)
    end
  end

  defp handle_outbound(%{"method" => "session/close", "id" => id} = msg, _json, _from, state) do
    if ensure_capability(state, :session_close) do
      synthesize_after_translate(msg, id, state, fn _state -> %{} end)
    else
      {:reply, :ok, reject_unsupported_method(state, id, "session/close")}
    end
  end

  defp handle_outbound(%{"method" => "session/delete", "id" => id} = msg, _json, _from, state) do
    if ensure_capability(state, :session_delete) do
      synthesize_after_translate(msg, id, state, fn _state -> %{} end)
    else
      {:reply, :ok, reject_unsupported_method(state, id, "session/delete")}
    end
  end

  defp handle_outbound(%{"method" => "session/list", "id" => id} = msg, _json, _from, state) do
    cond do
      not ensure_capability(state, :session_list) ->
        {:reply, :ok, reject_unsupported_method(state, id, "session/list")}

      function_exported?(state.adapter_mod, :list_sessions, 2) ->
        params = Map.get(msg, "params", %{})

        case state.adapter_mod.list_sessions(params, state.adapter_state) do
          {:ok, sessions, new_adapter_state} ->
            state = %{state | adapter_state: new_adapter_state}
            state = synthesize_result(state, id, %{"sessions" => sessions})
            {:reply, :ok, state}

          {:error, reason, new_adapter_state} ->
            state = %{state | adapter_state: new_adapter_state}
            state = synthesize_error(state, id, -32_603, to_string(reason))
            {:reply, :ok, state}
        end

      true ->
        # Let translate_outbound handle it (may send to native agent or skip)
        case state.adapter_mod.translate_outbound(msg, state.adapter_state) do
          {:ok, :skip, new_adapter_state} ->
            state = %{state | adapter_state: new_adapter_state}
            state = synthesize_result(state, id, %{"sessions" => []})
            {:reply, :ok, state}

          {:ok, data, new_adapter_state} ->
            state = %{state | adapter_state: new_adapter_state}
            _ = write_to_port(state, data)
            {:reply, :ok, state}

          {:reply, result, new_adapter_state} ->
            state = %{state | adapter_state: new_adapter_state}
            state = synthesize_result(state, id, result || %{"sessions" => []})
            {:reply, :ok, state}
        end
    end
  end

  defp handle_outbound(
         %{"method" => "session/set_mode", "id" => id} = msg,
         _json,
         _from,
         state
       ) do
    # Delegate to adapter — it may translate to a native command or handle in state
    synthesize_after_translate(msg, id, state, fn _state -> %{} end)
  end

  defp handle_outbound(
         %{"method" => "session/set_model", "id" => id} = msg,
         _json,
         _from,
         state
       ) do
    synthesize_after_translate(msg, id, state, fn _state -> %{} end)
  end

  defp handle_outbound(
         %{"method" => "session/set_config_option", "id" => id} = msg,
         _json,
         _from,
         state
       ) do
    case state.adapter_mod.translate_outbound(msg, state.adapter_state) do
      {:ok, :skip, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        state = synthesize_result(state, id, config_options_result(state))
        {:reply, :ok, state}

      {:ok, data, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        _ = write_to_port(state, data)
        state = synthesize_result(state, id, config_options_result(state))
        {:reply, :ok, state}

      {:reply, result, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        state = synthesize_result(state, id, result || config_options_result(state))
        {:reply, :ok, state}

      {:reply_and_write, result, data, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        _ = write_to_port(state, data)
        state = synthesize_result(state, id, result || config_options_result(state))
        {:reply, :ok, state}

      {:error, reason, new_adapter_state} ->
        # JSON-RPC -32602 = Invalid params. A config value outside the
        # adapter's enum (e.g. Pi's thinking_level) is invalid params from
        # the client's perspective.
        state = %{state | adapter_state: new_adapter_state}
        state = synthesize_error(state, id, -32_602, to_string(reason))
        {:reply, :ok, state}
    end
  end

  defp handle_outbound(msg, _json, _from, state) do
    msg
    |> translate_outbound_message(state)
    |> handle_translated_outbound(msg)
  end

  defp handle_authenticate_translation_result({:ok, :skip, state}, _msg, id) do
    if adapter_auth_methods(state) == [] do
      {:reply, :ok, reject_unsupported_method(state, id, "authenticate")}
    else
      {:reply, :ok, synthesize_error(state, id, -32_602, "Unsupported authenticate request")}
    end
  end

  defp handle_authenticate_translation_result({:ok, _sent, state}, _msg, _id),
    do: {:reply, :ok, state}

  defp handle_authenticate_translation_result({:reply, result, state}, _msg, id),
    do: {:reply, :ok, synthesize_result(state, id, result || %{})}

  defp handle_authenticate_translation_result({:messages, messages, state}, _msg, id) do
    state = push_messages(state, Enum.map(messages, &Jason.encode!/1))
    {:reply, :ok, synthesize_result(state, id, %{})}
  end

  defp handle_authenticate_translation_result(
         {:messages_and_reply, messages, result, state},
         _msg,
         id
       ) do
    state = push_messages(state, Enum.map(messages, &Jason.encode!/1))
    {:reply, :ok, synthesize_result(state, id, result || %{})}
  end

  defp handle_authenticate_translation_result(
         {:reply_and_write, result, _delivery, state},
         _msg,
         id
       ),
       do: {:reply, :ok, synthesize_result(state, id, result || %{})}

  defp handle_authenticate_translation_result(
         {:messages_and_write, messages, _delivery, state},
         _msg,
         id
       ) do
    state = push_messages(state, Enum.map(messages, &Jason.encode!/1))
    {:reply, :ok, synthesize_result(state, id, %{})}
  end

  defp handle_authenticate_translation_result({:error, reason, state}, msg, _id),
    do: reply_translation_error(msg, reason, state)

  defp handle_adapter_fork_callback(msg, id, state) do
    params = Map.get(msg, "params", %{})

    case state.adapter_mod.fork_session(params, state.adapter_state) do
      {:ok, result, adapter_state} ->
        state = %{state | adapter_state: adapter_state}
        {:reply, :ok, synthesize_session_lifecycle_result(state, id, result)}

      {:error, reason, adapter_state} ->
        state = %{state | adapter_state: adapter_state}
        {:reply, :ok, synthesize_error(state, id, -32_603, to_string(reason))}
    end
  end

  defp handle_fork_translation({:ok, :skip, state}, id),
    do: {:reply, :ok, synthesize_result(state, id, session_state_result(state))}

  defp handle_fork_translation({:ok, _delivery, state}, _id), do: {:reply, :ok, state}

  defp handle_fork_translation({:messages, messages, state}, id) do
    state = push_messages(state, Enum.map(messages, &Jason.encode!/1))
    {:reply, :ok, synthesize_result(state, id, session_state_result(state))}
  end

  defp handle_fork_translation({:reply, result, state}, id),
    do: {:reply, :ok, synthesize_session_lifecycle_result(state, id, result)}

  defp handle_fork_translation({:messages_and_reply, messages, result, state}, id) do
    state = push_messages(state, Enum.map(messages, &Jason.encode!/1))
    {:reply, :ok, synthesize_session_lifecycle_result(state, id, result)}
  end

  defp handle_fork_translation({:messages_and_write, messages, _delivery, state}, _id) do
    state = push_messages(state, Enum.map(messages, &Jason.encode!/1))
    {:reply, :ok, state}
  end

  defp handle_fork_translation({:error, reason, state}, id),
    do: {:reply, :ok, synthesize_error(state, id, -32_603, to_string(reason))}

  defp synthesize_session_lifecycle_result(state, id, result) do
    synthesize_result(state, id, Map.merge(session_state_result(state), result || %{}))
  end

  defp handle_translated_outbound({:ok, _delivery, state}, _msg), do: {:reply, :ok, state}

  defp handle_translated_outbound({:messages, messages, state}, _msg) do
    state = push_messages(state, Enum.map(messages, &Jason.encode!/1))
    {:reply, :ok, state}
  end

  defp handle_translated_outbound({:reply, result, state}, msg) do
    synthesize_translated_reply(msg, result, state)
  end

  defp handle_translated_outbound({:messages_and_reply, messages, result, state}, msg) do
    state = push_messages(state, Enum.map(messages, &Jason.encode!/1))
    synthesize_translated_reply(msg, result, state)
  end

  defp handle_translated_outbound({:reply_and_write, result, _delivery, state}, msg) do
    synthesize_translated_reply(msg, result, state)
  end

  defp handle_translated_outbound({:messages_and_write, messages, _delivery, state}, _msg) do
    state = push_messages(state, Enum.map(messages, &Jason.encode!/1))
    {:reply, :ok, state}
  end

  defp handle_translated_outbound({:error, reason, state}, msg) do
    reply_translation_error(msg, reason, state)
  end

  defp handle_translated_outbound({:one_shot, cmd_fn, adapter_state}, _msg) do
    start_one_shot_task(cmd_fn)
    {:reply, :ok, adapter_state}
  end

  defp synthesize_translated_reply(%{"id" => id}, result, state) do
    {:reply, :ok, synthesize_result(state, id, result || %{})}
  end

  defp synthesize_translated_reply(_msg, _result, state), do: {:reply, :ok, state}

  defp start_one_shot_task(cmd_fn) do
    bridge_pid = self()

    Task.start(fn ->
      case cmd_fn.() do
        {:ok, messages} ->
          send(bridge_pid, {:one_shot_result, messages})

        {:error, _reason} ->
          send(bridge_pid, {:one_shot_result, []})
      end
    end)
  end

  defp synthesize_after_translate(msg, id, state, result_fun) do
    case translate_outbound_message(msg, state) do
      {:ok, _delivery, state} ->
        {:reply, :ok, synthesize_result(state, id, result_fun.(state))}

      {:reply, result, state} ->
        {:reply, :ok, synthesize_result(state, id, result || result_fun.(state))}

      {:messages, messages, state} ->
        state = push_messages(state, Enum.map(messages, &Jason.encode!/1))
        {:reply, :ok, synthesize_result(state, id, result_fun.(state))}

      {:messages_and_reply, messages, result, state} ->
        state = push_messages(state, Enum.map(messages, &Jason.encode!/1))
        {:reply, :ok, synthesize_result(state, id, result || result_fun.(state))}

      {:reply_and_write, result, _delivery, state} ->
        {:reply, :ok, synthesize_result(state, id, result || result_fun.(state))}

      {:messages_and_write, messages, _delivery, state} ->
        state = push_messages(state, Enum.map(messages, &Jason.encode!/1))
        {:reply, :ok, synthesize_result(state, id, result_fun.(state))}

      {:error, reason, state} ->
        reply_translation_error(msg, reason, state)
    end
  end

  defp reply_translation_error(%{"id" => id}, reason, state) do
    {:reply, :ok, synthesize_error(state, id, -32_603, to_string(reason))}
  end

  defp reply_translation_error(_msg, reason, state) do
    {:reply, {:error, reason}, state}
  end

  defp translate_outbound_message(msg, state) do
    msg
    |> state.adapter_mod.translate_outbound(state.adapter_state)
    |> normalize_translated_outbound(state)
  end

  defp normalize_translated_outbound({:ok, :skip, adapter_state}, state),
    do: {:ok, :skip, %{state | adapter_state: adapter_state}}

  defp normalize_translated_outbound({:ok, data, adapter_state}, state) do
    state = %{state | adapter_state: adapter_state}
    write_translation_to_port(state, {:ok, :sent}, data)
  end

  defp normalize_translated_outbound({:reply, result, adapter_state}, state),
    do: {:reply, result, %{state | adapter_state: adapter_state}}

  defp normalize_translated_outbound({:messages, messages, adapter_state}, state),
    do: {:messages, messages, %{state | adapter_state: adapter_state}}

  defp normalize_translated_outbound(
         {:messages_and_reply, messages, result, adapter_state},
         state
       ),
       do: {:messages_and_reply, messages, result, %{state | adapter_state: adapter_state}}

  defp normalize_translated_outbound({:reply_and_write, result, data, adapter_state}, state) do
    state = %{state | adapter_state: adapter_state}
    write_translation_to_port(state, {:reply_and_write, result, :sent}, data)
  end

  defp normalize_translated_outbound(
         {:messages_and_write, messages, data, adapter_state},
         state
       ) do
    state = %{state | adapter_state: adapter_state}
    write_translation_to_port(state, {:messages_and_write, messages, :sent}, data)
  end

  defp normalize_translated_outbound({:error, reason, adapter_state}, state),
    do: {:error, reason, %{state | adapter_state: adapter_state}}

  defp normalize_translated_outbound({:one_shot, cmd_fn, adapter_state}, state),
    do: {:one_shot, cmd_fn, %{state | adapter_state: adapter_state}}

  defp write_translation_to_port(state, success, data) do
    case write_to_port(state, data) do
      :ok -> translated_write_success(success, state)
      {:error, reason} -> {:error, reason, state}
    end
  end

  defp translated_write_success({:ok, :sent}, state), do: {:ok, :sent, state}

  defp translated_write_success({:reply_and_write, result, :sent}, state),
    do: {:reply_and_write, result, :sent, state}

  defp translated_write_success({:messages_and_write, messages, :sent}, state),
    do: {:messages_and_write, messages, :sent, state}

  defp write_to_port(%{port: nil}, _data), do: {:error, :no_port}

  defp write_to_port(%{port: port}, data) do
    Port.command(port, data)
    :ok
  catch
    :error, reason -> {:error, reason}
  end

  defp process_port_data(state, data) do
    buffer = state.buffer <> data
    {lines, remaining} = split_lines(buffer)
    state = %{state | buffer: remaining}

    Enum.reduce(lines, state, fn line, acc ->
      case acc.adapter_mod.translate_inbound(line, acc.adapter_state) do
        {:messages, messages, new_adapter_state} ->
          acc = %{acc | adapter_state: new_adapter_state}
          push_messages(acc, Enum.map(messages, &Jason.encode!/1))

        {:messages_and_write, messages, write_data, new_adapter_state} ->
          acc = %{acc | adapter_state: new_adapter_state}
          acc = push_messages(acc, Enum.map(messages, &Jason.encode!/1))
          _ = write_to_port(acc, write_data)
          acc

        {:skip_and_write, write_data, new_adapter_state} ->
          acc = %{acc | adapter_state: new_adapter_state}
          _ = write_to_port(acc, write_data)
          acc

        {:partial, new_adapter_state} ->
          %{acc | adapter_state: new_adapter_state}

        {:skip, new_adapter_state} ->
          %{acc | adapter_state: new_adapter_state}
      end
    end)
  end

  defp flush_buffer(%{buffer: ""} = state), do: state

  defp flush_buffer(%{buffer: buffer} = state) do
    state = %{state | buffer: ""}

    case state.adapter_mod.translate_inbound(buffer, state.adapter_state) do
      {:messages, messages, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        push_messages(state, Enum.map(messages, &Jason.encode!/1))

      _ ->
        state
    end
  end

  defp split_lines(buffer) do
    lines = String.split(buffer, "\n")

    case List.pop_at(lines, -1) do
      {"", rest} -> {rest, ""}
      {last, rest} -> {rest, last}
    end
  end

  defp push_message(state, message) do
    case :queue.out(state.waiters) do
      {{:value, waiter}, rest} ->
        GenServer.reply(waiter, {:ok, message})
        %{state | waiters: rest}

      {:empty, _} ->
        %{state | outbox: :queue.in(message, state.outbox)}
    end
  end

  defp push_messages(state, messages) do
    Enum.reduce(messages, state, &push_message(&2, &1))
  end

  defp reply_error_to_waiters(state, reason) do
    state.waiters
    |> :queue.to_list()
    |> Enum.each(&GenServer.reply(&1, {:error, reason}))

    %{state | waiters: :queue.new()}
  end

  defp do_close(%{port: nil} = state), do: %{state | status: :closed}

  defp do_close(%{port: port} = state) do
    try do
      Port.close(port)
    catch
      :error, _ -> :ok
    end

    state = reply_error_to_waiters(state, :closed)
    %{state | port: nil, status: :closed}
  end

  @session_vars_to_clear ~w(
    CLAUDE_CODE_ENTRYPOINT CLAUDE_SESSION_ID CLAUDE_CONFIG_DIR
    CLAUDECODE CODEX_API_KEY OPENAI_API_KEY ANTHROPIC_API_KEY GEMINI_API_KEY
    GOOGLE_API_KEY PI_API_KEY
  )

  defp safe_env(opts, adapter_mod) do
    cleared = Enum.map(@session_vars_to_clear, &{to_charlist(&1), false})
    explicit_env = adapter_env(opts, adapter_mod)
    [{~c"TERM", ~c"dumb"} | cleared] ++ explicit_env
  end

  defp adapter_env(opts, adapter_mod) do
    adapter_default_env =
      if function_exported?(adapter_mod, :env, 1) do
        adapter_mod.env(opts)
      else
        []
      end

    adapter_default_env
    |> normalize_env()
    |> Map.merge(opts |> Keyword.get(:env, []) |> normalize_env())
    |> normalize_env()
    |> maybe_put_api_key(Keyword.get(opts, :api_key))
    |> Enum.map(fn {name, value} -> {to_charlist(name), to_charlist(value)} end)
  end

  defp normalize_env(env) when is_map(env) do
    NameValue.map(env)
  end

  defp normalize_env(env) when is_list(env) do
    NameValue.map(env)
  end

  defp normalize_env(_env), do: %{}

  defp maybe_put_api_key(env, nil), do: env
  defp maybe_put_api_key(env, api_key), do: Map.put(env, "PI_API_KEY", to_string(api_key))
end
