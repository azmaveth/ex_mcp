defmodule ExMCP.Server.Subscriptions do
  @moduledoc """
  Registry and publication coordinator for MCP 2026-07-28 subscriptions.

  Registrations contain no credential material. Each listener is isolated in a
  monitored process with an acknowledgment-first, bounded queue. The default
  adapter is node-local; clustered deployments can configure another adapter.
  """

  use GenServer

  alias ExMCP.Server.SubscriptionListener
  alias ExMCP.Server.Subscriptions.{Entry, ETS}
  alias ExMCP.Tasks
  alias ExMCP.Tasks.Extension, as: TasksExtension

  @filter_keys [
    "toolsListChanged",
    "promptsListChanged",
    "resourcesListChanged",
    "resourceSubscriptions",
    "taskIds"
  ]
  @default_supported Map.new(@filter_keys, &{&1, true})

  defstruct [
    :adapter,
    :adapter_state,
    :listener_supervisor,
    :filter_authorizer,
    :publication_authorizer,
    :max_global,
    :max_per_principal,
    :max_per_tenant,
    :max_queue,
    :max_lifetime_ms,
    :max_filter_uris,
    :max_filter_task_ids,
    :max_filter_bytes,
    :supported_notifications,
    monitors: %{}
  ]

  @type registry :: GenServer.server()

  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    case Keyword.get(opts, :name, __MODULE__) do
      nil -> GenServer.start_link(__MODULE__, opts)
      name -> GenServer.start_link(__MODULE__, opts, name: name)
    end
  end

  @spec listen(ExMCP.Types.request_id(), map(), pid(), keyword()) ::
          {:ok, Entry.t()} | {:error, term()}
  def listen(subscription_id, requested_filter, transport_ref, opts \\ []) do
    registry = Keyword.get(opts, :registry, __MODULE__)
    GenServer.call(registry, {:listen, subscription_id, requested_filter, transport_ref, opts})
  end

  @spec cancel(pid(), ExMCP.Types.request_id(), keyword()) :: :ok
  def cancel(transport_ref, subscription_id, opts \\ []) do
    registry = Keyword.get(opts, :registry, __MODULE__)
    GenServer.call(registry, {:cancel, transport_ref, subscription_id})
  end

  @spec close(pid(), ExMCP.Types.request_id(), atom(), keyword()) :: :ok | {:error, :not_found}
  def close(transport_ref, subscription_id, reason \\ :server_closed, opts \\ []) do
    registry = Keyword.get(opts, :registry, __MODULE__)
    GenServer.call(registry, {:close, transport_ref, subscription_id, reason})
  end

  @spec remove_transport(pid(), keyword()) :: :ok
  def remove_transport(transport_ref, opts \\ []) do
    registry = Keyword.get(opts, :registry, __MODULE__)
    GenServer.call(registry, {:remove_transport, transport_ref})
  catch
    :exit, _reason -> :ok
  end

  @spec publish(String.t(), map(), keyword()) :: %{
          subscribers: non_neg_integer(),
          enqueued: non_neg_integer(),
          coalesced: non_neg_integer(),
          closed: non_neg_integer()
        }
  def publish(method, params \\ %{}, opts \\ []) do
    registry = Keyword.get(opts, :registry, __MODULE__)
    transport_ref = Keyword.get(opts, :transport_ref)
    GenServer.call(registry, {:publish, method, params, transport_ref})
  end

  @doc false
  @spec publish_async(String.t(), map(), keyword()) :: :ok
  def publish_async(method, params \\ %{}, opts \\ []) do
    registry = Keyword.get(opts, :registry, __MODULE__)
    transport_ref = Keyword.get(opts, :transport_ref)
    GenServer.cast(registry, {:publish, method, params, transport_ref})
  catch
    :exit, _reason -> :ok
  end

  @spec entries(keyword()) :: [Entry.t()]
  def entries(opts \\ []) do
    registry = Keyword.get(opts, :registry, __MODULE__)
    GenServer.call(registry, :entries)
  end

  @spec delivered(pid()) :: :ok
  def delivered(listener), do: SubscriptionListener.delivered(listener)

  @doc false
  @spec runtime_options(keyword()) :: keyword()
  def runtime_options(opts) do
    opts
    |> Keyword.take([
      :subscription_registry,
      :authorize_subscription_filter,
      :authorize_subscription_publication,
      :subscription_max_queue,
      :subscription_max_lifetime_ms,
      :task_store_opts,
      :client_capabilities
    ])
    |> Enum.map(fn
      {:subscription_registry, value} -> {:registry, value}
      {:authorize_subscription_filter, value} -> {:authorize_filter, value}
      {:authorize_subscription_publication, value} -> {:authorize_publication, value}
      {:subscription_max_queue, value} -> {:max_queue, value}
      {:subscription_max_lifetime_ms, value} -> {:max_lifetime_ms, value}
      {:task_store_opts, value} -> {:task_store_opts, value}
      {:client_capabilities, value} -> {:client_capabilities, value}
    end)
    |> Keyword.put(:principal_id, Keyword.get(opts, :principal_id))
    |> Keyword.put(:tenant_id, Keyword.get(opts, :tenant_id))
    |> Keyword.put(:audience, Keyword.get(opts, :audience, Keyword.get(opts, :endpoint)))
  end

  @doc false
  @spec runtime_options(keyword(), module() | term()) :: keyword()
  def runtime_options(opts, handler) when is_list(opts) do
    opts
    |> put_handler_task_store(handler)
    |> runtime_options()
  end

  defp put_handler_task_store(opts, handler) when is_atom(handler) do
    if Code.ensure_loaded?(handler) and
         function_exported?(handler, :__task_store_enabled__, 0) and
         handler.__task_store_enabled__() do
      Keyword.put(opts, :task_store_opts, handler.__task_store_options__())
    else
      opts
    end
  end

  defp put_handler_task_store(opts, _handler), do: opts

  @impl true
  def init(opts) do
    {adapter, adapter_opts} = adapter_spec(Keyword.get(opts, :adapter, ETS))

    with {:ok, adapter_state} <- adapter.init(adapter_opts),
         {:ok, limits} <- validate_limits(opts),
         :ok <- validate_filter_authorizer(Keyword.get(opts, :authorize_filter)),
         :ok <- validate_publication_authorizer(Keyword.get(opts, :authorize_publication)) do
      {:ok,
       struct!(__MODULE__,
         adapter: adapter,
         adapter_state: adapter_state,
         listener_supervisor: Keyword.get(opts, :listener_supervisor, ExMCP.DynamicSupervisor),
         filter_authorizer: Keyword.get(opts, :authorize_filter),
         publication_authorizer: Keyword.get(opts, :authorize_publication),
         supported_notifications: Keyword.get(opts, :supported_notifications, @default_supported),
         max_global: limits.max_global,
         max_per_principal: limits.max_per_principal,
         max_per_tenant: limits.max_per_tenant,
         max_queue: limits.max_queue,
         max_lifetime_ms: limits.max_lifetime_ms,
         max_filter_uris: limits.max_filter_uris,
         max_filter_task_ids: limits.max_filter_task_ids,
         max_filter_bytes: limits.max_filter_bytes
       )}
    end
  end

  @impl true
  def handle_call({:listen, subscription_id, requested, transport_ref, opts}, _from, state) do
    {entries, state} = all_entries(state)

    with :ok <- validate_subscription_id(subscription_id),
         :ok <- validate_transport_ref(transport_ref),
         :ok <- validate_identity(opts),
         :ok <-
           validate_filter_authorizer(
             Keyword.get(opts, :authorize_filter, state.filter_authorizer)
           ),
         :ok <-
           validate_publication_authorizer(
             Keyword.get(opts, :authorize_publication, state.publication_authorizer)
           ),
         {:ok, requested} <- normalize_filter(requested, state),
         :ok <- validate_task_capability(requested, opts),
         supported = honour_supported(requested, state.supported_notifications),
         {:ok, task_authorized} <- authorize_task_filter(supported, transport_ref, opts, state),
         {:ok, honoured} <- authorize_filter(task_authorized, transport_ref, opts, state),
         :ok <- ensure_not_registered(entries, transport_ref, subscription_id),
         :ok <- enforce_limits(entries, transport_ref, opts, state),
         {:ok, entry, state} <-
           start_listener(subscription_id, honoured, transport_ref, opts, state) do
      {:reply, {:ok, entry}, state}
    else
      {:error, reason, state} -> {:reply, {:error, reason}, state}
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:cancel, transport_ref, subscription_id}, _from, state) do
    {entries, state} = all_entries(state)

    case find_entry(entries, transport_ref, subscription_id) do
      nil ->
        {:reply, :ok, state}

      entry ->
        SubscriptionListener.cancel(entry.listener_pid)
        {:reply, :ok, delete_entry(entry.token, state)}
    end
  end

  def handle_call({:close, transport_ref, subscription_id, reason}, _from, state) do
    {entries, state} = all_entries(state)

    case find_entry(entries, transport_ref, subscription_id) do
      nil ->
        {:reply, {:error, :not_found}, state}

      entry ->
        SubscriptionListener.close(entry.listener_pid, reason)
        {:reply, :ok, state}
    end
  end

  def handle_call({:remove_transport, transport_ref}, _from, state) do
    {entries, state} = all_entries(state)

    state =
      entries
      |> Enum.filter(&(&1.transport_ref == transport_ref))
      |> Enum.reduce(state, fn entry, acc ->
        SubscriptionListener.cancel(entry.listener_pid)
        delete_entry(entry.token, acc)
      end)

    {:reply, :ok, state}
  end

  def handle_call({:publish, method, params, transport_ref}, _from, state) do
    {result, state} = publish_to_matching(method, params, transport_ref, state)
    {:reply, result, broadcast(method, params, transport_ref, state)}
  end

  def handle_call(:entries, _from, state) do
    {entries, state} = all_entries(state)
    {:reply, entries, state}
  end

  @impl true
  def handle_cast({:publish, method, params, transport_ref}, state) do
    {_result, state} = publish_to_matching(method, params, transport_ref, state)
    {:noreply, broadcast(method, params, transport_ref, state)}
  end

  defp publish_to_matching(method, params, transport_ref, state) do
    {entries, state} = all_entries(state)

    matching =
      Enum.filter(entries, fn entry ->
        (is_nil(transport_ref) or entry.transport_ref == transport_ref) and
          filter_matches?(entry.filter, method, params)
      end)

    result =
      Enum.reduce(
        matching,
        %{subscribers: length(matching), enqueued: 0, coalesced: 0, closed: 0},
        fn entry, counts ->
          case SubscriptionListener.enqueue(entry.listener_pid, method, params) do
            :ok -> Map.update!(counts, :enqueued, &(&1 + 1))
            :coalesced -> Map.update!(counts, :coalesced, &(&1 + 1))
            {:closed, _reason} -> Map.update!(counts, :closed, &(&1 + 1))
          end
        end
      )

    {result, state}
  end

  @impl true
  def handle_info(
        {:subscription_listener_closed, listener, token, _transport_ref, _reason},
        state
      ) do
    {:noreply, remove_monitor(listener, token, state)}
  end

  def handle_info({:DOWN, ref, :process, listener, _reason}, state) do
    case Map.pop(state.monitors, ref) do
      {nil, _monitors} ->
        {:noreply, state}

      {{^listener, token}, monitors} ->
        {:noreply, delete_entry(token, %{state | monitors: monitors})}
    end
  end

  def handle_info(message, state) do
    if function_exported?(state.adapter, :handle_info, 2) do
      case state.adapter.handle_info(message, state.adapter_state) do
        {:publish, method, params, transport_ref, adapter_state} ->
          {_result, state} =
            publish_to_matching(
              method,
              params,
              transport_ref,
              %{state | adapter_state: adapter_state}
            )

          {:noreply, state}

        {:noreply, adapter_state} ->
          {:noreply, %{state | adapter_state: adapter_state}}

        :unhandled ->
          {:noreply, state}
      end
    else
      {:noreply, state}
    end
  end

  defp start_listener(subscription_id, filter, transport_ref, opts, state) do
    token = random_token()

    max_lifetime_ms =
      min(option(opts, :max_lifetime_ms, state.max_lifetime_ms), state.max_lifetime_ms)

    max_queue = min(option(opts, :max_queue, state.max_queue), state.max_queue)
    expires_at = System.system_time(:millisecond) + max_lifetime_ms

    listener_opts = [
      registry: self(),
      token: token,
      subscription_id: subscription_id,
      transport_ref: transport_ref,
      filter: filter,
      principal_id: Keyword.get(opts, :principal_id),
      tenant_id: Keyword.get(opts, :tenant_id),
      publication_authorizer:
        Keyword.get(opts, :authorize_publication, state.publication_authorizer),
      max_queue: max_queue,
      max_lifetime_ms: max_lifetime_ms
    ]

    case DynamicSupervisor.start_child(
           state.listener_supervisor,
           {SubscriptionListener, listener_opts}
         ) do
      {:ok, listener} ->
        entry = %Entry{
          token: token,
          subscription_id: subscription_id,
          listener_pid: listener,
          transport_ref: transport_ref,
          filter: filter,
          principal_id: Keyword.get(opts, :principal_id),
          tenant_id: Keyword.get(opts, :tenant_id),
          expires_at: expires_at
        }

        register_listener(listener, entry, state)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp register_listener(listener, entry, state) do
    case put_entry(entry, state) do
      {:ok, state} ->
        ref = Process.monitor(listener)
        SubscriptionListener.activate(listener)

        {:ok, entry, %{state | monitors: Map.put(state.monitors, ref, {listener, entry.token})}}

      {:error, reason, state} ->
        _result = DynamicSupervisor.terminate_child(state.listener_supervisor, listener)
        {:error, reason, state}
    end
  end

  defp authorize_filter(requested, transport_ref, opts, state) do
    authorizer = Keyword.get(opts, :authorize_filter, state.filter_authorizer)
    context = identity_context(transport_ref, opts)

    result =
      case authorizer do
        nil -> {:ok, requested}
        callback when is_function(callback, 2) -> callback.(requested, context)
        _invalid -> {:error, :invalid_filter_authorizer}
      end

    with {:ok, authorised} <- normalize_authorization_result(result),
         {:ok, authorised} <- normalize_filter(authorised, state),
         true <- filter_subset?(authorised, requested) do
      {:ok, authorised}
    else
      false -> {:error, :authorizer_broadened_filter}
      {:error, reason} -> {:error, reason}
    end
  rescue
    _error -> {:error, :filter_authorization_failed}
  catch
    _kind, _value -> {:error, :filter_authorization_failed}
  end

  defp validate_task_capability(filter, opts) do
    if Map.has_key?(filter, "taskIds") and
         not TasksExtension.declared?(Keyword.get(opts, :client_capabilities, %{})) do
      {:error,
       ExMCP.Error.missing_required_client_capability(TasksExtension.required_capabilities())}
    else
      :ok
    end
  end

  defp authorize_task_filter(filter, transport_ref, opts, state) do
    case Map.fetch(filter, "taskIds") do
      :error ->
        {:ok, filter}

      {:ok, task_ids} ->
        authorize_task_ids(filter, task_ids, transport_ref, opts, state)
    end
  end

  defp authorize_task_ids(filter, task_ids, transport_ref, opts, state) do
    cond do
      Keyword.has_key?(opts, :task_store_opts) ->
        task_opts = task_authorization_options(transport_ref, opts)

        authorized =
          Enum.filter(task_ids, fn task_id ->
            match?({:ok, _task}, Tasks.get(task_id, task_opts))
          end)

        {:ok, put_nonempty_ids(filter, "taskIds", authorized)}

      not is_nil(Keyword.get(opts, :authorize_filter, state.filter_authorizer)) ->
        {:ok, filter}

      true ->
        {:error, :task_subscription_authorizer_required}
    end
  end

  defp task_authorization_options(transport_ref, opts) do
    owner = %{
      principal_id: Keyword.get(opts, :principal_id),
      tenant_id: Keyword.get(opts, :tenant_id),
      audience: Keyword.get(opts, :audience)
    }

    opts
    |> Keyword.fetch!(:task_store_opts)
    |> Keyword.put(:owner, owner)
    |> Keyword.put(:transport_ref, transport_ref)
  end

  defp put_nonempty_ids(filter, key, []), do: Map.delete(filter, key)
  defp put_nonempty_ids(filter, key, ids), do: Map.put(filter, key, ids)

  defp normalize_authorization_result({:ok, filter}) when is_map(filter), do: {:ok, filter}
  defp normalize_authorization_result(true), do: {:error, :filter_authorizer_must_return_filter}
  defp normalize_authorization_result(false), do: {:error, :subscription_not_authorized}
  defp normalize_authorization_result({:error, reason}), do: {:error, reason}
  defp normalize_authorization_result(_other), do: {:error, :invalid_filter_authorizer_result}

  defp normalize_filter(filter, state) when is_map(filter) do
    string_filter = Map.new(filter, fn {key, value} -> {to_string(key), value} end)

    with true <- Enum.all?(Map.keys(string_filter), &(&1 in @filter_keys)),
         {:ok, normalized} <- normalize_filter_fields(string_filter),
         :ok <- validate_filter_size(normalized, state) do
      {:ok, normalized}
    else
      false -> {:error, :unknown_subscription_filter}
      {:error, reason} -> {:error, reason}
    end
  end

  defp normalize_filter(_filter, _state), do: {:error, :subscription_filter_required}

  defp normalize_filter_fields(filter) do
    Enum.reduce_while(filter, {:ok, %{}}, fn
      {key, true}, {:ok, acc} when key not in ["resourceSubscriptions", "taskIds"] ->
        {:cont, {:ok, Map.put(acc, key, true)}}

      {key, false}, {:ok, acc} when key not in ["resourceSubscriptions", "taskIds"] ->
        {:cont, {:ok, acc}}

      {"resourceSubscriptions", uris}, {:ok, acc} when is_list(uris) ->
        if Enum.all?(uris, &(is_binary(&1) and byte_size(&1) > 0)) do
          {:cont, {:ok, Map.put(acc, "resourceSubscriptions", Enum.uniq(uris))}}
        else
          {:halt, {:error, :invalid_resource_subscription}}
        end

      {"taskIds", task_ids}, {:ok, acc} when is_list(task_ids) ->
        if Enum.all?(task_ids, &(is_binary(&1) and byte_size(&1) > 0)) do
          {:cont, {:ok, Map.put(acc, "taskIds", Enum.uniq(task_ids))}}
        else
          {:halt, {:error, :invalid_task_subscription}}
        end

      {_key, _value}, _acc ->
        {:halt, {:error, :invalid_subscription_filter}}
    end)
  end

  defp validate_filter_size(filter, state) do
    uris = Map.get(filter, "resourceSubscriptions", [])
    task_ids = Map.get(filter, "taskIds", [])

    cond do
      length(uris) > state.max_filter_uris ->
        {:error, :subscription_filter_uri_limit}

      length(task_ids) > state.max_filter_task_ids ->
        {:error, :subscription_filter_task_id_limit}

      byte_size(Jason.encode!(filter)) > state.max_filter_bytes ->
        {:error, :subscription_filter_too_large}

      true ->
        :ok
    end
  end

  defp honour_supported(filter, supported) do
    Map.new(filter, fn {key, value} -> {key, value} end)
    |> Enum.reduce(%{}, fn {key, value}, acc ->
      if supported?(supported, key), do: Map.put(acc, key, value), else: acc
    end)
  end

  defp supported?(supported, key) when is_map(supported) do
    Map.get(supported, key, Map.get(supported, String.to_atom(key), false)) == true
  rescue
    ArgumentError -> Map.get(supported, key, false) == true
  end

  defp supported?(_supported, _key), do: false

  defp filter_subset?(authorised, requested) do
    Enum.all?(authorised, fn
      {"resourceSubscriptions", uris} ->
        requested_uris = Map.get(requested, "resourceSubscriptions", [])
        Enum.all?(uris, &(&1 in requested_uris))

      {"taskIds", task_ids} ->
        requested_task_ids = Map.get(requested, "taskIds", [])
        Enum.all?(task_ids, &(&1 in requested_task_ids))

      {key, true} ->
        Map.get(requested, key) == true
    end)
  end

  defp filter_matches?(filter, "notifications/tools/list_changed", _params),
    do: Map.get(filter, "toolsListChanged") == true

  defp filter_matches?(filter, "notifications/prompts/list_changed", _params),
    do: Map.get(filter, "promptsListChanged") == true

  defp filter_matches?(filter, "notifications/resources/list_changed", _params),
    do: Map.get(filter, "resourcesListChanged") == true

  defp filter_matches?(filter, "notifications/resources/updated", %{"uri" => uri}) do
    uri in Map.get(filter, "resourceSubscriptions", [])
  end

  defp filter_matches?(filter, "notifications/tasks", %{"taskId" => task_id}) do
    task_id in Map.get(filter, "taskIds", [])
  end

  defp filter_matches?(_filter, _method, _params), do: false

  defp enforce_limits(entries, transport_ref, opts, state) do
    principal = Keyword.get(opts, :principal_id)
    tenant = Keyword.get(opts, :tenant_id)
    principal_scope = principal || {:transport, transport_ref}
    tenant_scope = tenant || {:transport, transport_ref}

    cond do
      length(entries) >= state.max_global ->
        {:error, {:subscription_limit_exceeded, :global}}

      Enum.count(entries, &(identity_scope(&1.principal_id, &1.transport_ref) == principal_scope)) >=
          state.max_per_principal ->
        {:error, {:subscription_limit_exceeded, :principal}}

      Enum.count(entries, &(identity_scope(&1.tenant_id, &1.transport_ref) == tenant_scope)) >=
          state.max_per_tenant ->
        {:error, {:subscription_limit_exceeded, :tenant}}

      true ->
        :ok
    end
  end

  defp identity_scope(nil, transport_ref), do: {:transport, transport_ref}
  defp identity_scope(identity, _transport_ref), do: identity

  defp ensure_not_registered(entries, transport_ref, subscription_id) do
    if find_entry(entries, transport_ref, subscription_id),
      do: {:error, :subscription_already_registered},
      else: :ok
  end

  defp find_entry(entries, transport_ref, subscription_id) do
    Enum.find(entries, fn entry ->
      entry.transport_ref == transport_ref and entry.subscription_id == subscription_id
    end)
  end

  defp validate_subscription_id(id) when is_binary(id) and byte_size(id) > 0, do: :ok
  defp validate_subscription_id(id) when is_integer(id), do: :ok
  defp validate_subscription_id(_id), do: {:error, :invalid_subscription_id}

  defp validate_transport_ref(pid) when is_pid(pid), do: :ok
  defp validate_transport_ref(_other), do: {:error, :invalid_subscription_transport}

  defp validate_identity(opts) do
    if valid_identity?(Keyword.get(opts, :principal_id)) and
         valid_identity?(Keyword.get(opts, :tenant_id)) do
      :ok
    else
      {:error, :invalid_subscription_identity}
    end
  end

  defp valid_identity?(nil), do: true
  defp valid_identity?(identity), do: is_binary(identity) and byte_size(identity) > 0

  defp validate_filter_authorizer(nil), do: :ok
  defp validate_filter_authorizer(callback) when is_function(callback, 2), do: :ok
  defp validate_filter_authorizer(_invalid), do: {:error, :invalid_filter_authorizer}

  defp validate_publication_authorizer(nil), do: :ok
  defp validate_publication_authorizer(callback) when is_function(callback, 3), do: :ok

  defp validate_publication_authorizer(_invalid),
    do: {:error, :invalid_publication_authorizer}

  defp identity_context(transport_ref, opts) do
    %{
      principal_id: Keyword.get(opts, :principal_id),
      tenant_id: Keyword.get(opts, :tenant_id),
      audience: Keyword.get(opts, :audience),
      transport_ref: transport_ref
    }
  end

  defp validate_limits(opts) do
    limits = %{
      max_global: Keyword.get(opts, :max_global, 1_000),
      max_per_principal: Keyword.get(opts, :max_per_principal, 100),
      max_per_tenant: Keyword.get(opts, :max_per_tenant, 500),
      max_queue: Keyword.get(opts, :max_queue, 100),
      max_lifetime_ms: Keyword.get(opts, :max_lifetime_ms, 3_600_000),
      max_filter_uris: Keyword.get(opts, :max_filter_uris, 256),
      max_filter_task_ids: Keyword.get(opts, :max_filter_task_ids, 256),
      max_filter_bytes: Keyword.get(opts, :max_filter_bytes, 65_536)
    }

    if Enum.all?(limits, fn {_key, value} -> is_integer(value) and value > 0 end),
      do: {:ok, limits},
      else: {:error, :invalid_subscription_limits}
  end

  defp adapter_spec({adapter, opts}) when is_atom(adapter) and is_list(opts), do: {adapter, opts}
  defp adapter_spec(adapter) when is_atom(adapter), do: {adapter, []}

  defp all_entries(state) do
    {entries, adapter_state} = state.adapter.all(state.adapter_state)
    {entries, %{state | adapter_state: adapter_state}}
  end

  defp put_entry(entry, state) do
    case state.adapter.put(entry, state.adapter_state) do
      {:ok, adapter_state} -> {:ok, %{state | adapter_state: adapter_state}}
      {:error, reason, adapter_state} -> {:error, reason, %{state | adapter_state: adapter_state}}
    end
  end

  defp delete_entry(token, state) do
    {:ok, adapter_state} = state.adapter.delete(token, state.adapter_state)
    %{state | adapter_state: adapter_state}
  end

  defp broadcast(method, params, transport_ref, state) do
    if function_exported?(state.adapter, :broadcast, 4) do
      case state.adapter.broadcast(method, params, transport_ref, state.adapter_state) do
        {:ok, adapter_state} ->
          %{state | adapter_state: adapter_state}

        {:error, reason, adapter_state} ->
          :telemetry.execute(
            [:ex_mcp, :server, :subscription, :fanout],
            %{count: 1},
            %{result: :error, reason: inspect(reason)}
          )

          %{state | adapter_state: adapter_state}
      end
    else
      state
    end
  end

  defp remove_monitor(listener, token, state) do
    {ref, _value} =
      Enum.find(state.monitors, fn {_ref, {pid, _token}} -> pid == listener end) || {nil, nil}

    if ref, do: Process.demonitor(ref, [:flush])
    monitors = if ref, do: Map.delete(state.monitors, ref), else: state.monitors
    delete_entry(token, %{state | monitors: monitors})
  end

  defp option(opts, key, default) do
    case Keyword.get(opts, key, default) do
      value when is_integer(value) and value > 0 -> value
      _invalid -> default
    end
  end

  defp random_token do
    16
    |> :crypto.strong_rand_bytes()
    |> Base.url_encode64(padding: false)
  end
end
