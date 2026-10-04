defmodule Arbor.MCP.Server.Runtime.ShutdownGuard do
  @moduledoc false

  # This runtime-owned peer intentionally survives its root supervisor. Every
  # PID it may kill is explicitly registered as an owned child/task. It never
  # follows process links, and exits after the root and owned descendants do.
  use GenServer

  @cleanup_grace_ms 50

  def start(supervisor, table, config) do
    GenServer.start(__MODULE__, {supervisor, table, config})
  end

  def watch(table, pid, type \\ :worker) do
    [{:shutdown_guard, guard}] = :ets.lookup(table, :shutdown_guard)

    with :ok <- GenServer.call(guard, {:watch, pid}) do
      watch_children(table, pid, type)
    end
  rescue
    ArgumentError -> {:error, :runtime_unavailable}
  catch
    :exit, _reason -> {:error, :runtime_unavailable}
  end

  def stop(table, reason) do
    [{:shutdown_guard, guard}] = :ets.lookup(table, :shutdown_guard)
    GenServer.call(guard, {:stop, reason}, :infinity)
  rescue
    ArgumentError -> {:error, :runtime_unavailable}
  catch
    :exit, _reason -> {:error, :runtime_unavailable}
  end

  def closing?(table) do
    :ets.member(table, :closing)
  rescue
    ArgumentError -> true
  end

  def owned_spec(child, table) do
    spec = Supervisor.child_spec(child, [])

    Map.put(
      spec,
      :start,
      {__MODULE__, :start_owned, [spec.start, table, Map.get(spec, :type, :worker)]}
    )
  end

  def start_owned({module, function, args}, table, type) do
    case apply(module, function, args) do
      {:ok, pid} = result ->
        :ok = watch(table, pid, type)
        result

      {:ok, pid, _extra} = result ->
        :ok = watch(table, pid, type)
        result

      result ->
        result
    end
  end

  @impl true
  def init({supervisor, table, config}) do
    {:ok,
     %{
       root: supervisor,
       root_monitor: Process.monitor(supervisor),
       root_down: false,
       table: table,
       budget: config.shutdown_timeout_ms,
       children: %{},
       monitors: %{},
       waiters: [],
       phase: :running,
       timer: nil,
       stopper: nil
     }}
  end

  @impl true
  def handle_call({:watch, pid}, _from, %{phase: :running} = state) do
    {:reply, :ok, watch_pid(state, pid)}
  end

  def handle_call({:watch, pid}, _from, state) do
    Process.exit(pid, :kill)
    {:reply, {:error, :runtime_stopped}, watch_pid(state, pid)}
  end

  def handle_call({:stop, reason}, from, state) do
    state = begin_shutdown(state)
    state = %{state | waiters: [from | state.waiters]}

    if state.stopper do
      {:noreply, state}
    else
      root = state.root
      stopper = spawn(fn -> Supervisor.stop(root, reason, :infinity) end)
      {:noreply, %{watch_pid(state, stopper) | stopper: stopper}}
    end
  end

  @impl true
  def handle_info({:DOWN, monitor, :process, _pid, _reason}, %{root_monitor: monitor} = state) do
    state = %{state | root_down: true, phase: :cleanup}
    force_children(state)
    finish_or_wait(state)
  end

  def handle_info({:DOWN, monitor, :process, _pid, _reason}, state) do
    case Map.pop(state.monitors, monitor) do
      {nil, _monitors} ->
        {:noreply, state}

      {pid, monitors} ->
        state = %{state | monitors: monitors, children: Map.delete(state.children, pid)}
        finish_or_wait(state)
    end
  end

  def handle_info(:shutdown_deadline, state) do
    force_children(state)
    Process.send_after(self(), :force_root, @cleanup_grace_ms)
    {:noreply, %{state | phase: :cleanup}}
  end

  def handle_info(:force_root, state) do
    if not state.root_down, do: Process.exit(state.root, :kill)
    force_children(state)
    finish_or_wait(state)
  end

  defp begin_shutdown(%{phase: :running} = state) do
    close_route(state.table)
    timer = Process.send_after(self(), :shutdown_deadline, state.budget)
    %{state | phase: :stopping, timer: timer}
  end

  defp begin_shutdown(state), do: state

  defp watch_children(_table, _pid, :worker), do: :ok

  # Walk child specifications in the registering process, never in the guard:
  # an unresponsive owned supervisor must not delay the overall stop timer.
  # Dynamically created descendants must register themselves through watch/3.
  defp watch_children(table, pid, :supervisor) do
    Enum.reduce_while(Supervisor.which_children(pid), :ok, fn
      {_id, child, type, _modules}, :ok when is_pid(child) ->
        case watch(table, child, type) do
          :ok -> {:cont, :ok}
          error -> {:halt, error}
        end

      _child, :ok ->
        {:cont, :ok}
    end)
  end

  defp watch_pid(state, pid) do
    if Map.has_key?(state.children, pid) do
      state
    else
      monitor = Process.monitor(pid)

      %{
        state
        | children: Map.put(state.children, pid, monitor),
          monitors: Map.put(state.monitors, monitor, pid)
      }
    end
  end

  defp force_children(state) do
    for {pid, _monitor} <- state.children, pid != state.stopper do
      Process.exit(pid, :kill)
    end
  end

  defp finish_or_wait(%{root_down: true, children: children} = state)
       when map_size(children) == 0 do
    if state.timer, do: Process.cancel_timer(state.timer)
    for waiter <- state.waiters, do: GenServer.reply(waiter, :ok)
    {:stop, :normal, state}
  end

  defp finish_or_wait(state), do: {:noreply, state}

  defp close_route(table) do
    :ets.insert(table, [{:closing, System.monotonic_time(:millisecond)}, {:route, :closed}])
  rescue
    ArgumentError -> :ok
  end
end
