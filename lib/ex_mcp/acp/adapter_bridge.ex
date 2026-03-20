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

  require Logger

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
        case open_port(cmd, args, adapter_opts) do
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

  defp open_port(cmd, args, opts) do
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
        env: safe_env()
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
    response = %{
      "jsonrpc" => "2.0",
      "result" => result,
      "id" => request_id
    }

    push_message(state, Jason.encode!(response))
  end

  defp synthesize_init_response(state, request_id) do
    caps =
      if function_exported?(state.adapter_mod, :capabilities, 0) do
        state.adapter_mod.capabilities()
      else
        %{}
      end

    # Add modes if adapter declares them
    caps =
      if function_exported?(state.adapter_mod, :modes, 0) do
        case state.adapter_mod.modes() do
          [] -> caps
          modes -> Map.put(caps, "modes", modes)
        end
      else
        caps
      end

    # Add config options if adapter declares them
    caps =
      if function_exported?(state.adapter_mod, :config_options, 0) do
        case state.adapter_mod.config_options() do
          [] -> caps
          opts -> Map.put(caps, "configOptions", opts)
        end
      else
        caps
      end

    # Add session listing capability
    caps =
      if function_exported?(state.adapter_mod, :list_sessions, 1) do
        Map.put(caps, "sessionCapabilities", %{"list" => %{}})
      else
        caps
      end

    init_result = %{
      "jsonrpc" => "2.0",
      "result" => %{
        "agentInfo" => %{
          "name" => adapter_name(state.adapter_mod),
          "version" => "1.0.0"
        },
        "agentCapabilities" => caps,
        "protocolVersion" => 1
      },
      "id" => request_id
    }

    push_message(state, Jason.encode!(init_result))
  end

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
    # Delegate to adapter — it may have native auth support or handle internally
    case state.adapter_mod.translate_outbound(msg, state.adapter_state) do
      {:ok, :skip, new_adapter_state} ->
        # No native auth — synthesize OK (agent handles auth externally)
        state = %{state | adapter_state: new_adapter_state}
        state = synthesize_result(state, id, %{"authenticated" => true})
        {:reply, :ok, state}

      {:ok, data, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        _ = write_to_port(state, data)
        state = synthesize_result(state, id, %{"authenticated" => true})
        {:reply, :ok, state}
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
    end
  end

  defp handle_outbound(%{"method" => "session/new", "id" => id} = msg, _json, _from, state) do
    case state.adapter_mod.translate_outbound(msg, state.adapter_state) do
      {:ok, :skip, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        session_id = "session_#{System.unique_integer([:positive])}"
        state = synthesize_result(state, id, %{"sessionId" => session_id})
        {:reply, :ok, state}

      {:ok, data, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        _ = write_to_port(state, data)
        {:reply, :ok, state}
    end
  end

  defp handle_outbound(%{"method" => "session/load", "id" => id} = msg, _json, _from, state) do
    case state.adapter_mod.translate_outbound(msg, state.adapter_state) do
      {:ok, :skip, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        state = synthesize_result(state, id, nil)
        {:reply, :ok, state}

      {:ok, data, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        _ = write_to_port(state, data)
        {:reply, :ok, state}
    end
  end

  defp handle_outbound(%{"method" => "session/list", "id" => id} = msg, _json, _from, state) do
    # Try adapter's list_sessions callback, fall back to translate_outbound
    if function_exported?(state.adapter_mod, :list_sessions, 1) do
      case state.adapter_mod.list_sessions(state.adapter_state) do
        {:ok, sessions, new_adapter_state} ->
          state = %{state | adapter_state: new_adapter_state}
          state = synthesize_result(state, id, %{"sessions" => sessions})
          {:reply, :ok, state}
      end
    else
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
    case state.adapter_mod.translate_outbound(msg, state.adapter_state) do
      {:ok, :skip, new_adapter_state} ->
        # Adapter handled it internally (e.g., state update) — synthesize OK
        state = %{state | adapter_state: new_adapter_state}
        state = synthesize_result(state, id, %{})
        {:reply, :ok, state}

      {:ok, data, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        _ = write_to_port(state, data)
        # Synthesize result since native agent won't send ACP response
        state = synthesize_result(state, id, %{})
        {:reply, :ok, state}
    end
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
        state = synthesize_result(state, id, %{})
        {:reply, :ok, state}

      {:ok, data, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}
        _ = write_to_port(state, data)
        state = synthesize_result(state, id, %{})
        {:reply, :ok, state}
    end
  end

  defp handle_outbound(msg, _json, _from, state) do
    case state.adapter_mod.translate_outbound(msg, state.adapter_state) do
      {:ok, :skip, new_adapter_state} ->
        {:reply, :ok, %{state | adapter_state: new_adapter_state}}

      {:ok, data, new_adapter_state} ->
        state = %{state | adapter_state: new_adapter_state}

        case write_to_port(state, data) do
          :ok ->
            {:reply, :ok, state}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end

      {:one_shot, cmd_fn, new_adapter_state} ->
        # One-shot adapters run a subprocess and collect output
        state = %{state | adapter_state: new_adapter_state}
        bridge_pid = self()

        Task.start(fn ->
          case cmd_fn.() do
            {:ok, messages} ->
              send(bridge_pid, {:one_shot_result, messages})

            {:error, _reason} ->
              send(bridge_pid, {:one_shot_result, []})
          end
        end)

        {:reply, :ok, state}
    end
  end

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
    CLAUDECODE
  )

  defp safe_env do
    cleared = Enum.map(@session_vars_to_clear, &{to_charlist(&1), false})
    [{~c"TERM", ~c"dumb"} | cleared]
  end
end
