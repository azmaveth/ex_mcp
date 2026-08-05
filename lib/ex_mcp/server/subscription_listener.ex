defmodule ExMCP.Server.SubscriptionListener do
  @moduledoc false

  use GenServer

  @subscription_id_key "io.modelcontextprotocol/subscriptionId"

  defstruct [
    :registry,
    :token,
    :subscription_id,
    :transport_ref,
    :owner_monitor,
    :filter,
    :principal_id,
    :tenant_id,
    :publication_authorizer,
    :max_queue,
    :in_flight_kind,
    active?: false,
    closing?: false,
    queue: []
  ]

  @type notification_kind :: :acknowledged | :notification | :complete

  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts), do: GenServer.start_link(__MODULE__, opts)

  def child_spec(opts) do
    %{
      id: {__MODULE__, Keyword.fetch!(opts, :token)},
      start: {__MODULE__, :start_link, [opts]},
      restart: :temporary
    }
  end

  @spec activate(pid()) :: :ok
  def activate(listener), do: GenServer.cast(listener, :activate)

  @spec enqueue(pid(), String.t(), map()) :: :ok | :coalesced | {:closed, atom()}
  def enqueue(listener, method, params) do
    GenServer.call(listener, {:enqueue, method, params})
  catch
    :exit, _reason -> {:closed, :listener_unavailable}
  end

  @spec delivered(pid()) :: :ok
  def delivered(listener), do: GenServer.cast(listener, :delivered)

  @spec cancel(pid()) :: :ok
  def cancel(listener), do: GenServer.cast(listener, :cancel)

  @spec close(pid(), atom()) :: :ok
  def close(listener, reason \\ :server_closed), do: GenServer.cast(listener, {:close, reason})

  @impl true
  def init(opts) do
    transport_ref = Keyword.fetch!(opts, :transport_ref)
    lifetime = Keyword.fetch!(opts, :max_lifetime_ms)
    Process.send_after(self(), :expire, lifetime)

    state = %__MODULE__{
      registry: Keyword.fetch!(opts, :registry),
      token: Keyword.fetch!(opts, :token),
      subscription_id: Keyword.fetch!(opts, :subscription_id),
      transport_ref: transport_ref,
      owner_monitor: Process.monitor(transport_ref),
      filter: Keyword.fetch!(opts, :filter),
      principal_id: Keyword.get(opts, :principal_id),
      tenant_id: Keyword.get(opts, :tenant_id),
      publication_authorizer: Keyword.get(opts, :publication_authorizer),
      max_queue: Keyword.fetch!(opts, :max_queue)
    }

    {:ok, state}
  end

  @impl true
  def handle_cast(:activate, %{active?: false} = state) do
    message = acknowledgment(state.subscription_id, state.filter)
    {:noreply, deliver(:acknowledged, message, %{state | active?: true})}
  end

  def handle_cast(:activate, state), do: {:noreply, state}

  def handle_cast(:delivered, %{in_flight_kind: :complete} = state) do
    {:stop, :normal, %{state | in_flight_kind: nil}}
  end

  def handle_cast(:delivered, state) do
    {:noreply, deliver_next(%{state | in_flight_kind: nil})}
  end

  def handle_cast(:cancel, state), do: {:stop, :normal, state}

  def handle_cast({:close, reason}, state) do
    {:noreply, initiate_close(state, reason)}
  end

  @impl true
  def handle_call({:enqueue, _method, _params}, _from, %{closing?: true} = state) do
    {:reply, {:closed, :closing}, state}
  end

  def handle_call({:enqueue, method, params}, _from, state) do
    if publication_authorized?(state, method, params) do
      message = notification(state.subscription_id, method, params)
      key = coalescing_key(method, params)

      case enqueue_message(state, {:notification, message, key}) do
        {:ok, new_state} -> {:reply, :ok, deliver_next(new_state)}
        {:coalesced, new_state} -> {:reply, :coalesced, new_state}
        {:overflow, new_state} -> {:reply, {:closed, :slow_consumer}, new_state}
      end
    else
      {:reply, {:closed, :authorization_revoked}, initiate_close(state, :authorization_revoked)}
    end
  end

  @impl true
  def handle_info(:expire, state) do
    {:noreply, initiate_close(state, :maximum_lifetime)}
  end

  def handle_info({:DOWN, ref, :process, owner, _reason}, state)
      when ref == state.owner_monitor and owner == state.transport_ref do
    {:stop, :normal, state}
  end

  @impl true
  def terminate(reason, state) do
    send(
      state.registry,
      {:subscription_listener_closed, self(), state.token, state.transport_ref, reason}
    )

    :ok
  end

  defp enqueue_message(%{active?: false} = state, item) do
    if length(state.queue) < state.max_queue do
      {:ok, %{state | queue: state.queue ++ [item]}}
    else
      emit_queue_pressure(:closed)
      {:overflow, initiate_close(state, :slow_consumer)}
    end
  end

  defp enqueue_message(%{in_flight_kind: nil, queue: []} = state, {kind, message, _key}) do
    {:ok, deliver(kind, message, state)}
  end

  defp enqueue_message(state, {_kind, _message, key} = item) do
    case replace_coalesced(state.queue, key, item) do
      {:ok, queue} ->
        {:coalesced, %{state | queue: queue}}

      :not_found when length(state.queue) < state.max_queue ->
        {:ok, %{state | queue: state.queue ++ [item]}}

      :not_found ->
        emit_queue_pressure(:closed)
        {:overflow, initiate_close(state, :slow_consumer)}
    end
  end

  defp replace_coalesced(_queue, nil, _item), do: :not_found

  defp replace_coalesced(queue, key, item) do
    if Enum.any?(queue, fn {_kind, _message, queued_key} -> queued_key == key end) do
      emit_queue_pressure(:coalesced)

      {:ok,
       Enum.map(queue, fn
         {_kind, _message, ^key} -> item
         queued -> queued
       end)}
    else
      :not_found
    end
  end

  defp deliver_next(%{active?: false} = state), do: state

  defp deliver_next(%{in_flight_kind: nil, queue: [{kind, message, _key} | rest]} = state) do
    deliver(kind, message, %{state | queue: rest})
  end

  defp deliver_next(state), do: state

  defp deliver(kind, message, state) do
    send(
      state.transport_ref,
      {:ex_mcp_subscription_message, self(), kind, message}
    )

    %{state | in_flight_kind: kind}
  end

  defp initiate_close(%{closing?: true} = state, _reason), do: state

  defp initiate_close(state, reason) do
    emit_closed(reason)
    message = completion(state.subscription_id)
    state = %{state | closing?: true, queue: [{:complete, message, nil}]}
    deliver_next(state)
  end

  defp publication_authorized?(%{publication_authorizer: nil}, _method, _params), do: true

  defp publication_authorized?(state, method, params) do
    context = %{
      filter: state.filter,
      principal_id: state.principal_id,
      tenant_id: state.tenant_id,
      subscription_id: state.subscription_id
    }

    case state.publication_authorizer.(method, params, context) do
      true -> true
      :ok -> true
      {:ok, true} -> true
      _other -> false
    end
  rescue
    _error -> false
  catch
    _kind, _value -> false
  end

  defp acknowledgment(subscription_id, filter) do
    %{
      "jsonrpc" => "2.0",
      "method" => "notifications/subscriptions/acknowledged",
      "params" => %{
        "_meta" => %{@subscription_id_key => subscription_id},
        "notifications" => filter
      }
    }
  end

  defp notification(subscription_id, method, params) do
    meta =
      params
      |> Map.get("_meta", %{})
      |> Map.put(@subscription_id_key, subscription_id)

    %{
      "jsonrpc" => "2.0",
      "method" => method,
      "params" => Map.put(params, "_meta", meta)
    }
  end

  defp completion(subscription_id) do
    %{
      "jsonrpc" => "2.0",
      "id" => subscription_id,
      "result" => %{
        "resultType" => "complete",
        "_meta" => %{@subscription_id_key => subscription_id}
      }
    }
  end

  defp coalescing_key("notifications/tools/list_changed", _params), do: :tools_list_changed

  defp coalescing_key("notifications/prompts/list_changed", _params),
    do: :prompts_list_changed

  defp coalescing_key("notifications/resources/list_changed", _params),
    do: :resources_list_changed

  defp coalescing_key("notifications/resources/updated", %{"uri" => uri}),
    do: {:resource_updated, uri}

  defp coalescing_key("notifications/tasks", %{"taskId" => task_id}),
    do: {:task, task_id}

  defp coalescing_key(_method, _params), do: nil

  defp emit_closed(reason) do
    :telemetry.execute(
      [:ex_mcp, :server, :subscription, :closed],
      %{count: 1},
      %{reason: reason}
    )
  end

  defp emit_queue_pressure(action) do
    :telemetry.execute(
      [:ex_mcp, :server, :subscription, :queue_pressure],
      %{count: 1},
      %{action: action}
    )
  end
end
