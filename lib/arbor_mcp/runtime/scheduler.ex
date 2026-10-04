defmodule Arbor.MCP.Server.Runtime.Scheduler do
  @moduledoc false

  use GenServer

  alias Arbor.MCP.Server.Runtime.{Admission, CallbackContext, Failure, Lifecycle, ShutdownGuard}

  def start_link(opts) do
    config = Keyword.fetch!(opts, :config)

    with {:ok, pid} <- GenServer.start_link(__MODULE__, opts, timeout: config.init_timeout_ms) do
      :ok = ShutdownGuard.watch(Keyword.fetch!(opts, :table), pid)
      {:ok, pid}
    end
  end

  def child_spec(opts) do
    config = Keyword.fetch!(opts, :config)

    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      shutdown: config.shutdown_timeout_ms
    }
  end

  @impl true
  def init(opts) do
    Process.flag(:trap_exit, true)
    config = Keyword.fetch!(opts, :config)
    table = Keyword.fetch!(opts, :table)
    :ok = ShutdownGuard.watch(table, self())

    with {:ok, handler_state} <- config.handler.init(config.handler_args),
         {:ok, generation} <- Admission.activate(table, self(), config) do
      [{:callback_tasks, task_supervisor}] = :ets.lookup(table, :callback_tasks)

      {:ok,
       %{
         table: table,
         config: config,
         handler_state: handler_state,
         generation: generation,
         task_supervisor: task_supervisor,
         queue: :queue.new(),
         work: %{},
         tasks: %{},
         owners: %{}
       }}
    else
      {:error, reason} -> {:stop, {:handler_init_failed, reason}}
      _invalid -> {:stop, :invalid_handler_init}
    end
  end

  @impl true
  def handle_info({:submit, generation, token, request, opts}, state) do
    if generation == state.generation do
      case Admission.bind(state.table, token) do
        {:ok, reservation} ->
          owner_ref = Process.monitor(reservation.owner)
          remaining = max(0, reservation.deadline - now())
          deadline_timer = Process.send_after(self(), {:deadline, generation, token}, remaining)

          work = %{
            reservation: reservation,
            request: request,
            opts: opts,
            owner_ref: owner_ref,
            deadline_timer: deadline_timer,
            kill_timer: nil,
            task: nil,
            terminal: false
          }

          state = %{
            state
            | work: Map.put(state.work, token, work),
              owners: Map.put(state.owners, owner_ref, token),
              queue: :queue.in(token, state.queue)
          }

          {:noreply, start_available(state)}

        {:error, _reason} ->
          {:noreply, state}
      end
    else
      {:noreply, state}
    end
  end

  def handle_info({ref, proposal}, state) when is_reference(ref) do
    case Map.get(state.tasks, ref) do
      nil -> {:noreply, state}
      token -> {:noreply, complete(state, token, proposal)}
    end
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason}, state) do
    cond do
      Map.has_key?(state.tasks, ref) ->
        token = Map.fetch!(state.tasks, ref)
        state = if state.work[token].terminal, do: state, else: fail(state, token, :handler_crash)
        {:noreply, state |> remove_work(token) |> start_available()}

      Map.has_key?(state.owners, ref) ->
        token = Map.fetch!(state.owners, ref)
        {:noreply, cancel_work(state, token, :owner_down)}

      true ->
        {:noreply, state}
    end
  end

  def handle_info({:deadline, generation, token}, state) do
    if generation == state.generation,
      do: {:noreply, cancel_work(state, token, :handler_timeout)},
      else: {:noreply, state}
  end

  def handle_info({:kill, generation, token}, state) do
    if generation == state.generation do
      case Map.get(state.work, token) do
        %{task: %Task{pid: pid}, terminal: true} -> Process.exit(pid, :kill)
        _ -> :ok
      end
    end

    {:noreply, state}
  end

  def handle_info(_message, state), do: {:noreply, state}

  @impl true
  def handle_call({:cancel, generation, key}, _from, state) do
    if generation == state.generation do
      case Admission.find(state.table, key) do
        %{token: token} = reservation ->
          if Map.has_key?(state.work, token) do
            {:reply, :ok, cancel_work(state, token, :request_cancelled)}
          else
            Admission.terminal(
              state.table,
              token,
              Failure.result(reservation, :request_cancelled)
            )

            Admission.release(state.table, token)
            {:reply, :ok, state}
          end

        _ ->
          {:reply, :ok, state}
      end
    else
      {:reply, {:error, :runtime_unavailable}, state}
    end
  end

  def handle_call(:stats, _from, state) do
    {:reply,
     %{
       active: map_size(state.tasks),
       queued: :queue.len(state.queue),
       generation: state.generation
     }, state}
  end

  @impl true
  def terminate(reason, state) do
    for {token, work} <- state.work do
      :ets.insert(state.table, {{:cancelled, token}, true})
      if work.task, do: Process.exit(work.task.pid, :shutdown)
    end

    Admission.close(
      state.table,
      if(ShutdownGuard.closing?(state.table) or reason == :normal,
        do: :runtime_stopped,
        else: :runtime_restarted
      )
    )

    if function_exported?(state.config.handler, :terminate, 2) do
      state.config.handler.terminate(reason, state.handler_state)
    end

    :ok
  end

  defp start_available(state) do
    if map_size(state.tasks) < state.config.max_concurrency do
      case :queue.out(state.queue) do
        {:empty, _queue} ->
          state

        {{:value, token}, queue} ->
          state = %{state | queue: queue}
          work = Map.fetch!(state.work, token)

          case start_failure(state, work) do
            nil -> state |> start_task(token, work) |> start_available()
            reason -> state |> fail(token, reason) |> remove_work(token) |> start_available()
          end
      end
    else
      state
    end
  end

  defp start_task(state, token, work) do
    snapshot = state.handler_state
    config = state.config

    invocation = %{
      table: state.table,
      token: token,
      generation: state.generation,
      deadline: work.reservation.deadline,
      scope: work.reservation.scope,
      runtime: Keyword.fetch!(work.opts, :runtime),
      owner: work.reservation.owner
    }

    task =
      Task.Supervisor.async_nolink(
        state.task_supervisor,
        fn ->
          invoke(invocation, work, config, snapshot)
        end,
        shutdown: config.cancel_grace_ms
      )

    %{
      state
      | tasks: Map.put(state.tasks, task.ref, token),
        work: Map.put(state.work, token, %{work | task: task})
    }
  rescue
    _exception -> state |> fail(token, :handler_start_failed) |> remove_work(token)
  catch
    :exit, _reason -> state |> fail(token, :handler_start_failed) |> remove_work(token)
  end

  defp complete(state, token, proposal) do
    work = Map.fetch!(state.work, token)

    context = %{
      terminal: work.terminal,
      deadline: work.reservation.deadline,
      request_id: work.reservation.request_id,
      execution: state.config.execution,
      state: state.handler_state
    }

    decision =
      if ShutdownGuard.closing?(state.table),
        do: {:cancel, :runtime_stopped},
        else: Lifecycle.complete(context, proposal, now())

    case decision do
      :ignore ->
        state

      {:cancel, reason} ->
        cancel_work(state, token, reason)

      {:fail, reason} ->
        fail(state, token, reason)

      {:commit, result, next_state} ->
        state |> Map.put(:handler_state, next_state) |> mark_terminal(token, result)
    end
  end

  defp cancel_work(state, token, reason) do
    case Map.get(state.work, token) do
      nil ->
        state

      %{terminal: true} ->
        state

      %{task: nil} ->
        state |> fail(token, reason) |> remove_work(token) |> start_available()

      work ->
        :ets.insert(state.table, {{:cancelled, token}, true})
        send(work.task.pid, {:arbor_mcp_cancelled, token, reason})

        timer =
          Process.send_after(
            self(),
            {:kill, state.generation, token},
            state.config.cancel_grace_ms
          )

        state = put_in(state.work[token].kill_timer, timer)
        fail(state, token, reason)
    end
  end

  defp fail(state, token, reason) do
    work = Map.fetch!(state.work, token)
    mark_terminal(state, token, Failure.result(work.reservation, reason))
  end

  defp mark_terminal(state, token, result) do
    work = Map.fetch!(state.work, token)
    if work.deadline_timer, do: Process.cancel_timer(work.deadline_timer)
    Admission.terminal(state.table, token, result)
    put_in(state.work[token].terminal, true)
  end

  defp remove_work(state, token) do
    case Map.pop(state.work, token) do
      {nil, _work} ->
        state

      {work, remaining} ->
        Process.demonitor(work.owner_ref, [:flush])
        if work.deadline_timer, do: Process.cancel_timer(work.deadline_timer)
        if work.kill_timer, do: Process.cancel_timer(work.kill_timer)
        if work.task, do: Process.demonitor(work.task.ref, [:flush])
        Admission.release(state.table, token)
        tasks = if work.task, do: Map.delete(state.tasks, work.task.ref), else: state.tasks
        queue = :queue.filter(&(&1 != token), state.queue)

        %{
          state
          | work: remaining,
            tasks: tasks,
            owners: Map.delete(state.owners, work.owner_ref),
            queue: queue
        }
    end
  end

  defp now, do: System.monotonic_time(:millisecond)

  defp invoke(invocation, work, config, snapshot) do
    :ok = ShutdownGuard.watch(invocation.table, self())

    CallbackContext.with_context(invocation, fn ->
      dispatch_opts =
        Keyword.merge(config.dispatch_opts, Keyword.get(work.opts, :dispatch_opts, []))

      dispatcher = config.dispatcher
      dispatcher.dispatch(work.request, config.handler, snapshot, dispatch_opts)
    end)
  rescue
    _exception -> {:runtime_failure, :handler_crash}
  catch
    _kind, _reason -> {:runtime_failure, :handler_crash}
  end

  defp start_failure(state, work) do
    cond do
      ShutdownGuard.closing?(state.table) -> :runtime_stopped
      not Process.alive?(work.reservation.owner) -> :owner_down
      now() >= work.reservation.deadline -> :handler_timeout
      true -> nil
    end
  end
end
