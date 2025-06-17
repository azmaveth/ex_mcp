defmodule ExMCP.Transport.Beam.LoadBalancer do
  @moduledoc """
  Load balancer for distributing client connections across MCP service instances.

  Provides multiple load balancing strategies optimized for BEAM transport:
  - Round-robin: Cycles through available services
  - Least-connections: Routes to service with fewest active connections
  - Weighted: Distributes based on service weights
  - Health-aware: Excludes unhealthy services from selection

  ## Example Usage

      # Start load balancer with round-robin strategy
      {:ok, balancer} = LoadBalancer.start_link(%{
        cluster: cluster_pid,
        strategy: :round_robin,
        health_aware: true
      })

      # Get a service instance
      {:ok, service} = LoadBalancer.get_service(balancer, "calculator")

      # Update service connection count for least-connections strategy
      LoadBalancer.update_connections(balancer, service_id, 5)
  """

  use GenServer
  require Logger

  alias ExMCP.Transport.Beam.Cluster

  defstruct [
    :cluster,
    :strategy,
    :health_aware,
    :connection_tracking,
    :round_robin_state,
    :weights_cache,
    :stats
  ]

  @type strategy :: :round_robin | :least_connections | :weighted | :random

  @type config :: %{
          cluster: GenServer.server(),
          strategy: strategy(),
          health_aware: boolean(),
          connection_tracking: boolean(),
          exclude_circuit_broken: boolean()
        }

  @default_config %{
    strategy: :round_robin,
    health_aware: true,
    connection_tracking: false,
    exclude_circuit_broken: true
  }

  @doc """
  Starts a load balancer with the given configuration.
  """
  @spec start_link(config()) :: GenServer.on_start()
  def start_link(config) do
    GenServer.start_link(__MODULE__, config)
  end

  @doc """
  Stops the load balancer.
  """
  @spec stop(GenServer.server()) :: :ok
  def stop(balancer) do
    GenServer.stop(balancer, :normal)
  end

  @doc """
  Gets a service instance using the configured load balancing strategy.
  """
  @spec get_service(GenServer.server(), String.t(), map()) ::
          {:ok, map()} | {:error, term()}
  def get_service(balancer, service_name, opts \\ %{}) do
    GenServer.call(balancer, {:get_service, service_name, opts})
  end

  @doc """
  Updates connection count for a service (used by least-connections strategy).
  """
  @spec update_connections(GenServer.server(), String.t(), non_neg_integer()) :: :ok
  def update_connections(balancer, service_id, connection_count) do
    GenServer.cast(balancer, {:update_connections, service_id, connection_count})
  end

  @doc """
  Records that a connection to a service was established.
  """
  @spec record_connection(GenServer.server(), String.t()) :: :ok
  def record_connection(balancer, service_id) do
    GenServer.cast(balancer, {:record_connection, service_id})
  end

  @doc """
  Records that a connection to a service was closed.
  """
  @spec record_disconnection(GenServer.server(), String.t()) :: :ok
  def record_disconnection(balancer, service_id) do
    GenServer.cast(balancer, {:record_disconnection, service_id})
  end

  @doc """
  Gets load balancer statistics.
  """
  @spec get_stats(GenServer.server()) :: {:ok, map()}
  def get_stats(balancer) do
    GenServer.call(balancer, :get_stats)
  end

  @doc """
  Updates the load balancing strategy.
  """
  @spec update_strategy(GenServer.server(), strategy()) :: :ok
  def update_strategy(balancer, strategy) do
    GenServer.call(balancer, {:update_strategy, strategy})
  end

  # GenServer callbacks

  @impl true
  def init(config) do
    full_config = Map.merge(@default_config, config)

    state = %__MODULE__{
      cluster: Map.fetch!(full_config, :cluster),
      strategy: full_config.strategy,
      health_aware: full_config.health_aware,
      connection_tracking: full_config.connection_tracking,
      round_robin_state: %{},
      weights_cache: %{},
      stats: init_stats()
    }

    Logger.debug("Load balancer started with #{full_config.strategy} strategy")

    {:ok, state}
  end

  @impl true
  def handle_call({:get_service, service_name, opts}, _from, state) do
    filters = build_filters(service_name, opts, state)

    case Cluster.discover_services(state.cluster, filters) do
      {:ok, [_ | _] = services} ->
        case select_service(services, state) do
          {:ok, selected_service, updated_state} ->
            update_stats(updated_state, :selections, 1)
            {:reply, {:ok, selected_service}, updated_state}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end

      {:ok, []} ->
        {:reply, {:error, :no_services_available}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:update_strategy, strategy}, _from, state) do
    # Reset strategy-specific state when changing strategies
    updated_state = %{state | strategy: strategy, round_robin_state: %{}, weights_cache: %{}}

    Logger.info("Load balancer strategy changed to #{strategy}")
    {:reply, :ok, updated_state}
  end

  def handle_call(:get_stats, _from, state) do
    current_stats =
      Map.merge(state.stats, %{
        strategy: state.strategy,
        health_aware: state.health_aware,
        connection_tracking: state.connection_tracking,
        uptime: System.system_time(:millisecond) - state.stats.started_at
      })

    {:reply, {:ok, current_stats}, state}
  end

  @impl true
  def handle_cast({:update_connections, service_id, connection_count}, state) do
    if state.connection_tracking do
      # This would update connection tracking state
      # For now, we'll just log it
      Logger.debug("Service #{service_id} now has #{connection_count} connections")
    end

    {:noreply, state}
  end

  def handle_cast({:record_connection, service_id}, state) do
    update_stats(state, :connections_established, 1)
    Logger.debug("Recorded connection to service #{service_id}")
    {:noreply, state}
  end

  def handle_cast({:record_disconnection, service_id}, state) do
    update_stats(state, :connections_closed, 1)
    Logger.debug("Recorded disconnection from service #{service_id}")
    {:noreply, state}
  end

  @impl true
  def handle_info(msg, state) do
    Logger.debug("Unexpected message in LoadBalancer: #{inspect(msg)}")
    {:noreply, state}
  end

  # Private helper functions

  defp build_filters(service_name, opts, state) do
    base_filters = %{name: service_name}

    base_filters
    |> maybe_add_health_filter(state.health_aware)
    |> maybe_add_circuit_breaker_filter(state)
    |> Map.merge(opts)
  end

  defp maybe_add_health_filter(filters, true) do
    Map.put(filters, :healthy_only, true)
  end

  defp maybe_add_health_filter(filters, false), do: filters

  defp maybe_add_circuit_breaker_filter(filters, _state) do
    Map.put(filters, :exclude_circuit_broken, true)
  end

  defp select_service(services, state) do
    case state.strategy do
      :round_robin ->
        select_round_robin(services, state)

      :least_connections ->
        select_least_connections(services, state)

      :weighted ->
        select_weighted(services, state)

      :random ->
        select_random(services, state)

      _ ->
        {:error, :unknown_strategy}
    end
  end

  defp select_round_robin(services, state) do
    service_name = hd(services).name

    # Sort services by a stable identifier (instance number if available, otherwise node name)
    sorted_services =
      Enum.sort_by(services, fn service ->
        case Map.get(service.metadata || %{}, :instance) do
          nil -> to_string(service.node)
          instance -> instance
        end
      end)

    current_index = Map.get(state.round_robin_state, service_name, 0)

    selected_service = Enum.at(sorted_services, current_index)
    next_index = rem(current_index + 1, length(sorted_services))

    updated_rr_state = Map.put(state.round_robin_state, service_name, next_index)
    updated_state = %{state | round_robin_state: updated_rr_state}

    {:ok, selected_service, updated_state}
  end

  defp select_least_connections(services, state) do
    # Select service with fewest connections
    selected_service =
      Enum.min_by(services, fn service ->
        Map.get(service.metadata || %{}, :connections, 0)
      end)

    {:ok, selected_service, state}
  end

  defp select_weighted(services, state) do
    # Build weight distribution if not cached
    service_name = hd(services).name

    weights =
      case Map.get(state.weights_cache, service_name) do
        nil ->
          build_weight_distribution(services)

        cached ->
          cached
      end

    # Select based on weights
    selected_service = select_by_weight(services, weights)

    # Cache the weights for future use
    updated_cache = Map.put(state.weights_cache, service_name, weights)
    updated_state = %{state | weights_cache: updated_cache}

    {:ok, selected_service, updated_state}
  end

  defp select_random(services, state) do
    selected_service = Enum.random(services)
    {:ok, selected_service, state}
  end

  defp build_weight_distribution(services) do
    total_weight =
      Enum.reduce(services, 0, fn service, acc ->
        weight = Map.get(service.metadata || %{}, :weight, 1)
        acc + weight
      end)

    # Build cumulative distribution
    {_, distribution} =
      Enum.reduce(services, {0, []}, fn service, {running_sum, dist} ->
        weight = Map.get(service.metadata || %{}, :weight, 1)
        new_sum = running_sum + weight
        probability = new_sum / total_weight
        {new_sum, [{service, probability} | dist]}
      end)

    Enum.reverse(distribution)
  end

  # Fallback
  defp select_by_weight(services, []), do: hd(services)

  defp select_by_weight(services, weights) do
    rand = :rand.uniform()

    case Enum.find(weights, fn {_service, probability} -> rand <= probability end) do
      {service, _} -> service
      # Fallback
      nil -> hd(services)
    end
  end

  defp init_stats do
    %{
      started_at: System.system_time(:millisecond),
      selections: 0,
      connections_established: 0,
      connections_closed: 0
    }
  end

  defp update_stats(state, metric, increment) do
    current_value = Map.get(state.stats, metric, 0)
    updated_stats = Map.put(state.stats, metric, current_value + increment)
    %{state | stats: updated_stats}
  end
end
