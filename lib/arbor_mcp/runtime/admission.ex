defmodule Arbor.MCP.Server.Runtime.Admission do
  @moduledoc false

  use GenServer

  alias Arbor.MCP.Server.Runtime.{Failure, Ref, ShutdownGuard}

  def start_link(opts) do
    with {:ok, pid} <- GenServer.start_link(__MODULE__, opts) do
      :ok = ShutdownGuard.watch(Keyword.fetch!(opts, :table), pid)
      {:ok, pid}
    end
  end

  # The bounded ETS slot is claimed before sending even the small confirmation
  # message. No request payload is ever placed in this owner's mailbox.
  @spec reserve(Ref.t(), map(), keyword()) :: {:ok, map(), map()} | {:error, atom()}
  def reserve(runtime, request, opts) do
    table = Ref.table(runtime)

    with {:ok, route} <- route(table),
         {:ok, bytes} <- request_size(request, opts, route.config),
         {:ok, reservation} <- claim_slot(runtime, route, request, bytes, opts) do
      try do
        case GenServer.call(route.admission, {:confirm, reservation}, 5_000) do
          {:ok, confirmed} ->
            {:ok, route, confirmed}

          {:error, _reason} = error ->
            release_slot(table, reservation)
            error
        end
      catch
        :exit, _reason ->
          # Confirmation may have succeeded before the caller timed out. Keep
          # the slot until the admission owner has released its ledger record;
          # otherwise another producer could reuse count capacity too soon.
          GenServer.cast(route.admission, {:abandon, reservation})
          {:error, :runtime_unavailable}
      end
    end
  end

  def route(table) do
    case :ets.lookup(table, :route) do
      [{:route, %{scheduler: scheduler} = route}] when is_pid(scheduler) ->
        if Process.alive?(scheduler), do: {:ok, route}, else: {:error, :runtime_unavailable}

      _ ->
        {:error, :runtime_unavailable}
    end
  rescue
    ArgumentError -> {:error, :runtime_unavailable}
  end

  def activate(table, scheduler, config) do
    [{:admission, admission}] = :ets.lookup(table, :admission)
    GenServer.call(admission, {:activate, scheduler, config})
  end

  def bind(table, token), do: call(table, {:bind, token})
  def find(table, key), do: call(table, {:find, key})
  def terminal(table, token, result), do: call(table, {:terminal, token, result})
  def release(table, token), do: call(table, {:release, token})
  def close(table, reason), do: call(table, {:close, reason})
  def stats(table), do: call(table, :stats)

  def key(scope, request_id, direction), do: {scope, direction, request_id}

  @impl true
  def init(opts) do
    table = Keyword.fetch!(opts, :table)
    supervisor = Keyword.fetch!(opts, :supervisor)
    :ok = ShutdownGuard.watch(table, self())

    for {{:reservation, token}, %{terminal: false} = reservation} <- :ets.tab2list(table) do
      deliver_reply(
        reservation.reply_to,
        token,
        Failure.result(reservation, :runtime_restarted)
      )
    end

    for object <- :ets.tab2list(table), elem(object, 0) not in [:shutdown_guard, :closing] do
      :ets.delete_object(table, object)
    end

    :ets.insert(table, [{:admission, self()}, {:route, :closed}])
    Process.send_after(self(), :reap_unconfirmed, 100)

    {:ok,
     %{
       table: table,
       supervisor: supervisor,
       reservations: %{},
       monitors: %{},
       bytes: 0,
       scheduler_ref: nil,
       generation: nil,
       config: nil
     }}
  end

  @impl true
  def handle_call(:reference, _from, state) do
    {:reply, {:ok, Ref.new(state.supervisor, state.table)}, state}
  end

  def handle_call({:activate, scheduler, config}, _from, state) do
    state = drain(state, :runtime_restarted)
    if state.scheduler_ref, do: Process.demonitor(state.scheduler_ref, [:flush])
    generation = make_ref()
    monitor = Process.monitor(scheduler)

    route = %{
      admission: self(),
      scheduler: scheduler,
      generation: generation,
      config: config
    }

    :ets.insert(state.table, {:route, route})

    {:reply, {:ok, generation},
     %{state | scheduler_ref: monitor, generation: generation, config: config}}
  end

  def handle_call({:confirm, reservation}, _from, state) do
    cond do
      ShutdownGuard.closing?(state.table) ->
        {:reply, {:error, :runtime_stopped}, state}

      reservation.generation != state.generation ->
        {:reply, {:error, :runtime_unavailable}, state}

      not slot_owned?(state.table, reservation) ->
        {:reply, {:error, :admission_lost}, state}

      not Process.alive?(reservation.producer) ->
        release_slot(state.table, reservation)
        {:reply, {:error, :owner_down}, state}

      not Process.alive?(reservation.owner) ->
        release_slot(state.table, reservation)
        {:reply, {:error, :owner_down}, state}

      state.bytes + reservation.bytes > state.config.max_pending_bytes ->
        release_slot(state.table, reservation)
        {:reply, {:error, :server_busy}, state}

      Enum.any?(state.reservations, fn {_token, existing} -> existing.key == reservation.key end) ->
        release_slot(state.table, reservation)
        {:reply, {:error, :duplicate_request_id}, state}

      true ->
        monitor = Process.monitor(reservation.producer)

        timer =
          Process.send_after(
            self(),
            {:unbound_timeout, reservation.token},
            max(0, reservation.deadline - System.monotonic_time(:millisecond))
          )

        reservation =
          Map.merge(reservation, %{monitor: monitor, timer: timer, bound: false, terminal: false})

        state = %{
          state
          | reservations: Map.put(state.reservations, reservation.token, reservation),
            monitors: Map.put(state.monitors, monitor, reservation.token),
            bytes: state.bytes + reservation.bytes
        }

        :ets.insert(state.table, {{:request, reservation.key}, reservation.token})
        :ets.insert(state.table, {{:reservation, reservation.token}, reservation})

        :telemetry.execute(
          [:arbor_mcp, :server, :request, :admitted],
          %{count: 1, request_bytes: reservation.bytes},
          %{runtime: state.supervisor}
        )

        {:reply, {:ok, reservation}, state}
    end
  end

  def handle_call({:bind, token}, _from, state) do
    case Map.get(state.reservations, token) do
      %{bound: false, terminal: false} = reservation ->
        if Process.alive?(reservation.owner) do
          Process.demonitor(reservation.monitor, [:flush])
          Process.cancel_timer(reservation.timer)
          bound = %{reservation | bound: true, monitor: nil, timer: nil}
          reservations = Map.put(state.reservations, token, bound)
          monitors = Map.delete(state.monitors, reservation.monitor)
          :ets.insert(state.table, {{:reservation, token}, bound})
          {:reply, {:ok, bound}, %{state | reservations: reservations, monitors: monitors}}
        else
          state = deliver_terminal(state, token, Failure.result(reservation, :owner_down))
          {:reply, {:error, :owner_down}, release_reservation(state, token)}
        end

      _ ->
        {:reply, {:error, :admission_lost}, state}
    end
  end

  def handle_call({:find, key}, _from, state) do
    result =
      Enum.find_value(state.reservations, fn {_token, entry} -> if entry.key == key, do: entry end)

    {:reply, result, state}
  end

  def handle_call({:terminal, token, result}, _from, state) do
    {:reply, :ok, deliver_terminal(state, token, result)}
  end

  def handle_call({:release, token}, _from, state),
    do: {:reply, :ok, release_reservation(state, token)}

  def handle_call({:close, reason}, _from, state) do
    :ets.insert(state.table, {:route, :closed})
    {:reply, :ok, %{drain(state, reason) | generation: nil}}
  end

  def handle_call(:stats, _from, state) do
    slots = :ets.select_count(state.table, [{{{:slot, :_}, :_, :_}, [], [true]}])

    {:reply,
     %{reserved: slots, pending_bytes: state.bytes, confirmed: map_size(state.reservations)},
     state}
  end

  @impl true
  def handle_cast({:abandon, reservation}, state) do
    release_slot(state.table, reservation)
    {:noreply, release_reservation(state, reservation.token)}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _pid, _reason}, %{scheduler_ref: ref} = state) do
    :ets.insert(state.table, {:route, :closed})
    {:noreply, %{drain(state, :runtime_restarted) | scheduler_ref: nil, generation: nil}}
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason}, state) do
    case Map.get(state.monitors, ref) do
      nil ->
        {:noreply, state}

      token ->
        state =
          deliver_terminal(
            state,
            token,
            Failure.result(state.reservations[token], :producer_down)
          )

        {:noreply, release_reservation(state, token)}
    end
  end

  def handle_info({:unbound_timeout, token}, state) do
    case Map.get(state.reservations, token) do
      %{bound: false} ->
        state =
          deliver_terminal(
            state,
            token,
            Failure.result(state.reservations[token], :handler_timeout)
          )

        {:noreply, release_reservation(state, token)}

      _ ->
        {:noreply, state}
    end
  end

  def handle_info(:reap_unconfirmed, state) do
    # A producer may be killed between insert_new and confirm. Reap those
    # bounded slot records without charging bytes or allocating a monitor.
    for {{:slot, _slot}, token, producer} = object <- :ets.tab2list(state.table),
        not Map.has_key?(state.reservations, token),
        not Process.alive?(producer) do
      :ets.delete_object(state.table, object)
    end

    for {{:cancel_control, _key}, producer} = object <- :ets.tab2list(state.table),
        not Process.alive?(producer) do
      :ets.delete_object(state.table, object)
    end

    Process.send_after(self(), :reap_unconfirmed, 100)
    {:noreply, state}
  end

  defp request_size(request, opts, config) do
    request_bytes = :erlang.external_size(request)
    context_bytes = :erlang.external_size(Keyword.get(opts, :dispatch_opts, []))

    if request_bytes <= config.max_request_bytes do
      {:ok, request_bytes + context_bytes}
    else
      {:error, :request_too_large}
    end
  end

  defp claim_slot(runtime, route, request, bytes, opts) do
    token = make_ref()
    producer = self()
    owner = Keyword.get(opts, :owner, producer)
    target = Keyword.get(opts, :reply_to, producer)
    timeout = Keyword.get(opts, :timeout, route.config.request_timeout_ms)
    scope = Keyword.get(opts, :scope, {:connection, owner})
    direction = Keyword.get(opts, :direction, :inbound)
    request_id = Map.get(request, "id")
    key = key(scope, request_id || {:notification, token}, direction)

    cond do
      not local_pid?(owner) ->
        {:error, :invalid_owner}

      not local_reply_target?(target) ->
        {:error, :invalid_reply_target}

      not is_integer(timeout) or timeout <= 0 ->
        {:error, :invalid_timeout}

      :erlang.external_size(key) > 4_096 ->
        {:error, :invalid_scope}

      true ->
        capacity = route.config.max_concurrency + route.config.max_queue

        slot =
          Enum.find(1..capacity, fn index ->
            :ets.insert_new(Ref.table(runtime), {{:slot, index}, token, producer})
          end)

        if slot do
          now = System.monotonic_time(:millisecond)

          {:ok,
           %{
             token: token,
             slot: slot,
             producer: producer,
             owner: owner,
             reply_to: target,
             bytes: bytes,
             request_id: request_id,
             key: key,
             scope: scope,
             generation: route.generation,
             timeout: min(timeout, route.config.request_timeout_ms),
             deadline: now + min(timeout, route.config.request_timeout_ms)
           }}
        else
          {:error, :server_busy}
        end
    end
  rescue
    ArgumentError -> {:error, :runtime_unavailable}
  end

  defp slot_owned?(table, reservation) do
    :ets.lookup(table, {:slot, reservation.slot}) == [
      {{:slot, reservation.slot}, reservation.token, reservation.producer}
    ]
  end

  defp release_slot(table, reservation) do
    :ets.delete_object(
      table,
      {{:slot, reservation.slot}, reservation.token, reservation.producer}
    )
  rescue
    ArgumentError -> :ok
  end

  defp deliver_terminal(state, token, result) do
    case Map.get(state.reservations, token) do
      %{terminal: false} = reservation ->
        :ets.insert(state.table, {{:reservation, token}, %{reservation | terminal: true}})

        :telemetry.execute(
          [:arbor_mcp, :server, :request, :completed],
          %{
            count: 1,
            duration_ms:
              max(
                0,
                System.monotonic_time(:millisecond) - (reservation.deadline - reservation.timeout)
              )
          },
          %{runtime: state.supervisor, outcome: outcome_class(result)}
        )

        deliver_reply(reservation.reply_to, token, result)
        reservations = Map.put(state.reservations, token, %{reservation | terminal: true})
        %{state | reservations: reservations}

      _ ->
        state
    end
  end

  defp deliver_reply(target, token, result) do
    send(target, {:arbor_mcp_runtime, token, result})
    :ok
  rescue
    # Erlang cannot distinguish an ordinary reference from a process alias
    # during admission. Delivery is best effort and must not interrupt the
    # terminal ledger update or a restart's outstanding-reservation cleanup.
    ArgumentError -> :ok
  end

  defp release_reservation(state, token) do
    case Map.pop(state.reservations, token) do
      {nil, _reservations} ->
        state

      {reservation, reservations} ->
        if reservation.monitor, do: Process.demonitor(reservation.monitor, [:flush])
        if reservation.timer, do: Process.cancel_timer(reservation.timer)
        release_slot(state.table, reservation)
        :ets.delete_object(state.table, {{:request, reservation.key}, token})
        :ets.delete(state.table, {:reservation, token})
        :ets.delete(state.table, {:cancelled, token})

        %{
          state
          | reservations: reservations,
            monitors: Map.delete(state.monitors, reservation.monitor),
            bytes: state.bytes - reservation.bytes
        }
    end
  end

  defp drain(state, reason) do
    state =
      Enum.reduce(Map.keys(state.reservations), state, fn token, acc ->
        acc
        |> deliver_terminal(token, Failure.result(acc.reservations[token], reason))
        |> release_reservation(token)
      end)

    :ets.select_delete(state.table, [{{{:slot, :_}, :_, :_}, [], [true]}])
    :ets.select_delete(state.table, [{{{:cancel_control, :_}, :_}, [], [true]}])
    state
  end

  defp outcome_class({:ok, %{"error" => _error}}), do: :application_error
  defp outcome_class({:ok, _response}), do: :ok
  defp outcome_class({:error, %{"error" => %{"data" => %{"type" => type}}}}), do: type
  defp outcome_class({:error, reason}) when is_atom(reason), do: reason
  defp outcome_class(:notification), do: :notification

  defp local_pid?(pid), do: is_pid(pid) and node(pid) == node()

  defp local_reply_target?(target) do
    (is_pid(target) or is_reference(target)) and node(target) == node()
  end

  defp call(table, message) do
    case :ets.lookup(table, :admission) do
      [{:admission, owner}] -> GenServer.call(owner, message, 5_000)
      _ -> {:error, :runtime_unavailable}
    end
  rescue
    ArgumentError -> {:error, :runtime_unavailable}
  catch
    :exit, _reason -> {:error, :runtime_unavailable}
  end
end
