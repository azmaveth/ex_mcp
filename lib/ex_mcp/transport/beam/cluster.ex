defmodule ExMCP.Transport.Beam.Cluster do
  @moduledoc """
  Native BEAM clustering support for distributed MCP servers.

  Provides service discovery, load balancing, and fault tolerance for MCP servers
  running across multiple Erlang nodes. Leverages BEAM's distributed computing
  capabilities for seamless clustering.

  ## Features

  - **Service Discovery**: Automatic registration and discovery of MCP servers
  - **Load Balancing**: Distribute client connections across server instances
  - **Health Monitoring**: Monitor service health and remove failed instances
  - **Fault Tolerance**: Handle node failures and network partitions
  - **Dynamic Membership**: Add/remove nodes from cluster at runtime
  - **Circuit Breakers**: Protect against cascading failures

  ## Architecture

  The cluster uses a distributed registry pattern with the following components:

  - **Service Registry**: Tracks available MCP services across nodes
  - **Health Monitor**: Monitors service health and availability
  - **Load Balancer**: Routes client requests to appropriate servers
  - **Partition Detector**: Detects and handles network partitions
  - **Membership Manager**: Manages cluster node membership

  ## Example Usage

      # Start a cluster coordinator
      {:ok, cluster} = Cluster.start_link(%{
        node_name: :mcp_cluster,
        discovery_strategy: :distributed_registry,
        health_check_interval: 5000
      })
      
      # Register an MCP service
      service_info = %{
        name: "calculator",
        version: "1.0.0", 
        capabilities: ["tools"],
        node: node(),
        pid: server_pid
      }
      
      {:ok, service_id} = Cluster.register_service(cluster, service_info)
      
      # Discover available services
      {:ok, services} = Cluster.discover_services(cluster, %{name: "calculator"})
      
      # Get a service instance with load balancing
      {:ok, service} = Cluster.get_service(cluster, "calculator", strategy: :round_robin)
  """

  use GenServer
  require Logger

  alias ExMCP.Transport.Beam.{ServiceRegistry, HealthMonitor, PartitionDetector}

  defstruct [
    :node_name,
    :discovery_strategy,
    :registry_pid,
    :health_monitor_pid,
    :partition_detector_pid,
    :cluster_nodes,
    :config,
    :stats
  ]

  @type service_info :: %{
          name: String.t(),
          version: String.t() | nil,
          capabilities: [String.t()] | nil,
          node: atom(),
          pid: pid(),
          metadata: map() | nil,
          health_check: map() | nil,
          circuit_breaker: map() | nil
        }

  @type cluster_config :: %{
          node_name: atom() | nil,
          discovery_strategy: :local_registry | :distributed_registry | :dns | :consul,
          health_check_enabled: boolean(),
          health_check_interval: non_neg_integer(),
          service_timeout: non_neg_integer(),
          node_monitoring: boolean(),
          failure_detection_timeout: non_neg_integer(),
          partition_detection: boolean(),
          merge_strategy: :last_writer_wins | :first_writer_wins | :manual,
          cluster_management: boolean(),
          failover_enabled: boolean()
        }

  @default_config %{
    node_name: nil,
    discovery_strategy: :local_registry,
    health_check_enabled: true,
    health_check_interval: 5000,
    service_timeout: 15000,
    node_monitoring: false,
    failure_detection_timeout: 10000,
    partition_detection: false,
    merge_strategy: :last_writer_wins,
    cluster_management: false,
    failover_enabled: false
  }

  @doc """
  Starts a cluster coordinator with the given configuration.
  """
  @spec start_link(cluster_config()) :: GenServer.on_start()
  def start_link(config \\ %{}) do
    GenServer.start_link(__MODULE__, config)
  end

  @doc """
  Stops the cluster coordinator.
  """
  @spec stop(GenServer.server()) :: :ok
  def stop(cluster) do
    GenServer.stop(cluster, :normal)
  end

  @doc """
  Registers an MCP service in the cluster.

  Returns a unique service ID that can be used for updates and removal.
  """
  @spec register_service(GenServer.server(), service_info()) ::
          {:ok, String.t()} | {:error, term()}
  def register_service(cluster, service_info) do
    GenServer.call(cluster, {:register_service, service_info})
  end

  @doc """
  Updates an existing service registration.
  """
  @spec update_service(GenServer.server(), String.t(), service_info()) ::
          :ok | {:error, term()}
  def update_service(cluster, service_id, service_info) do
    GenServer.call(cluster, {:update_service, service_id, service_info})
  end

  @doc """
  Removes a service from the cluster registry.
  """
  @spec unregister_service(GenServer.server(), String.t()) :: :ok
  def unregister_service(cluster, service_id) do
    GenServer.call(cluster, {:unregister_service, service_id})
  end

  @doc """
  Discovers services matching the given criteria.

  ## Filter Options

  - `:name` - Service name to match
  - `:version` - Service version to match
  - `:capabilities` - Required capabilities (list)
  - `:node` - Specific node to search
  - `:exclude_circuit_broken` - Exclude services with open circuit breakers
  - `:healthy_only` - Only return healthy services
  """
  @spec discover_services(GenServer.server(), map()) ::
          {:ok, [service_info()]} | {:error, term()}
  def discover_services(cluster, filters \\ %{}) do
    GenServer.call(cluster, {:discover_services, filters})
  end

  @doc """
  Gets a specific service by ID.
  """
  @spec get_service(GenServer.server(), String.t()) ::
          {:ok, service_info()} | {:error, :not_found}
  def get_service(cluster, service_id) do
    GenServer.call(cluster, {:get_service, service_id})
  end

  @doc """
  Records a failure for a service (used by circuit breakers).
  """
  @spec record_failure(GenServer.server(), String.t()) :: :ok
  def record_failure(cluster, service_id) do
    GenServer.cast(cluster, {:record_failure, service_id})
  end

  @doc """
  Records a success for a service (used by circuit breakers).
  """
  @spec record_success(GenServer.server(), String.t()) :: :ok
  def record_success(cluster, service_id) do
    GenServer.cast(cluster, {:record_success, service_id})
  end

  @doc """
  Lists all nodes in the cluster.
  """
  @spec list_nodes(GenServer.server()) :: {:ok, [map()]}
  def list_nodes(cluster) do
    GenServer.call(cluster, :list_nodes)
  end

  @doc """
  Adds a node to the cluster.
  """
  @spec add_node(GenServer.server(), atom(), map()) :: :ok | {:error, term()}
  def add_node(cluster, node_name, node_info \\ %{}) do
    GenServer.call(cluster, {:add_node, node_name, node_info})
  end

  @doc """
  Removes a node from the cluster.
  """
  @spec remove_node(GenServer.server(), atom()) :: :ok
  def remove_node(cluster, node_name) do
    GenServer.call(cluster, {:remove_node, node_name})
  end

  @doc """
  Gets cluster statistics and health information.
  """
  @spec get_stats(GenServer.server()) :: {:ok, map()}
  def get_stats(cluster) do
    GenServer.call(cluster, :get_stats)
  end

  # Testing and simulation functions

  @doc """
  Simulates a node failure for testing purposes.
  """
  @spec simulate_node_failure(GenServer.server(), atom()) :: :ok
  def simulate_node_failure(cluster, node_name) do
    GenServer.cast(cluster, {:simulate_node_failure, node_name})
  end

  @doc """
  Simulates a network partition for testing.
  """
  @spec simulate_partition(GenServer.server(), [atom()], [atom()]) :: :ok
  def simulate_partition(cluster, partition_a, partition_b) do
    GenServer.cast(cluster, {:simulate_partition, partition_a, partition_b})
  end

  @doc """
  Heals a simulated network partition.
  """
  @spec heal_partition(GenServer.server()) :: :ok
  def heal_partition(cluster) do
    GenServer.cast(cluster, :heal_partition)
  end

  # GenServer callbacks

  @impl true
  def init(config) do
    full_config = Map.merge(@default_config, config)

    # Start service registry
    {:ok, registry_pid} =
      ServiceRegistry.start_link(%{
        strategy: full_config.discovery_strategy,
        node_name: full_config.node_name
      })

    # Start health monitor if enabled
    health_monitor_pid =
      if full_config.health_check_enabled do
        {:ok, pid} =
          HealthMonitor.start_link(%{
            registry: registry_pid,
            check_interval: full_config.health_check_interval,
            service_timeout: full_config.service_timeout,
            max_failures: Map.get(full_config, :max_failures, 3),
            failure_window: Map.get(full_config, :failure_window, 30000),
            methods: Map.get(full_config, :methods, [:process, :ping])
          })

        pid
      else
        nil
      end

    # Start partition detector if enabled
    partition_detector_pid =
      if full_config.partition_detection do
        {:ok, pid} =
          PartitionDetector.start_link(%{
            cluster: self(),
            registry: registry_pid,
            merge_strategy: full_config.merge_strategy
          })

        pid
      else
        nil
      end

    state = %__MODULE__{
      node_name: full_config.node_name || node(),
      discovery_strategy: full_config.discovery_strategy,
      registry_pid: registry_pid,
      health_monitor_pid: health_monitor_pid,
      partition_detector_pid: partition_detector_pid,
      cluster_nodes: %{},
      config: full_config,
      stats: init_stats()
    }

    Logger.info("BEAM cluster started with strategy #{full_config.discovery_strategy}")

    {:ok, state}
  end

  @impl true
  def handle_call({:register_service, service_info}, _from, state) do
    case ServiceRegistry.register(state.registry_pid, service_info) do
      {:ok, service_id} ->
        # Add service to health monitoring if health monitor is enabled
        if state.health_monitor_pid do
          alias ExMCP.Transport.Beam.HealthMonitor
          health_config = Map.get(service_info, :health_check, %{})
          HealthMonitor.monitor_service(state.health_monitor_pid, service_id, health_config)
        end

        update_stats(state, :services_registered, 1)
        {:reply, {:ok, service_id}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:update_service, service_id, service_info}, _from, state) do
    case ServiceRegistry.update(state.registry_pid, service_id, service_info) do
      :ok ->
        {:reply, :ok, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:unregister_service, service_id}, _from, state) do
    ServiceRegistry.unregister(state.registry_pid, service_id)

    # Remove service from health monitoring if health monitor is enabled
    if state.health_monitor_pid do
      alias ExMCP.Transport.Beam.HealthMonitor
      HealthMonitor.unmonitor_service(state.health_monitor_pid, service_id)
    end

    update_stats(state, :services_unregistered, 1)
    {:reply, :ok, state}
  end

  def handle_call({:discover_services, filters}, _from, state) do
    case ServiceRegistry.discover(state.registry_pid, filters) do
      {:ok, services} ->
        update_stats(state, :discovery_requests, 1)
        {:reply, {:ok, services}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:get_service, service_id}, _from, state) do
    case ServiceRegistry.get(state.registry_pid, service_id) do
      {:ok, service} ->
        {:reply, {:ok, service}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(:list_nodes, _from, state) do
    nodes = Map.values(state.cluster_nodes)
    {:reply, {:ok, nodes}, state}
  end

  def handle_call({:add_node, node_name, node_info}, _from, state) do
    node_data =
      Map.merge(node_info, %{
        name: node_name,
        added_at: System.system_time(:millisecond),
        status: :active
      })

    updated_nodes = Map.put(state.cluster_nodes, node_name, node_data)
    updated_state = %{state | cluster_nodes: updated_nodes}

    Logger.info("Added node #{node_name} to cluster")
    {:reply, :ok, updated_state}
  end

  def handle_call({:remove_node, node_name}, _from, state) do
    updated_nodes = Map.delete(state.cluster_nodes, node_name)
    updated_state = %{state | cluster_nodes: updated_nodes}

    # Also remove all services from this node
    ServiceRegistry.remove_node_services(state.registry_pid, node_name)

    Logger.info("Removed node #{node_name} from cluster")
    {:reply, :ok, updated_state}
  end

  def handle_call(:get_stats, _from, state) do
    registry_stats = ServiceRegistry.get_stats(state.registry_pid)

    stats =
      Map.merge(state.stats, %{
        node_count: map_size(state.cluster_nodes),
        registry_stats: registry_stats,
        uptime: System.system_time(:millisecond) - state.stats.started_at
      })

    {:reply, {:ok, stats}, state}
  end

  @impl true
  def handle_cast({:record_failure, service_id}, state) do
    ServiceRegistry.record_failure(state.registry_pid, service_id)
    update_stats(state, :failures_recorded, 1)
    {:noreply, state}
  end

  def handle_cast({:record_success, service_id}, state) do
    ServiceRegistry.record_success(state.registry_pid, service_id)
    {:noreply, state}
  end

  def handle_cast({:simulate_node_failure, node_name}, state) do
    # Remove all services from the failing node
    ServiceRegistry.remove_node_services(state.registry_pid, node_name)

    # Update node status
    if Map.has_key?(state.cluster_nodes, node_name) do
      updated_node = Map.put(state.cluster_nodes[node_name], :status, :failed)
      updated_nodes = Map.put(state.cluster_nodes, node_name, updated_node)
      updated_state = %{state | cluster_nodes: updated_nodes}

      Logger.warning("Simulated failure of node #{node_name}")
      {:noreply, updated_state}
    else
      {:noreply, state}
    end
  end

  def handle_cast({:simulate_partition, partition_a, partition_b}, state) do
    if state.partition_detector_pid do
      PartitionDetector.simulate_partition(state.partition_detector_pid, partition_a, partition_b)
    end

    Logger.warning(
      "Simulated network partition: #{inspect(partition_a)} | #{inspect(partition_b)}"
    )

    {:noreply, state}
  end

  def handle_cast(:heal_partition, state) do
    if state.partition_detector_pid do
      PartitionDetector.heal_partition(state.partition_detector_pid)
    end

    Logger.info("Healed network partition")
    {:noreply, state}
  end

  @impl true
  def handle_info({:service_health_check_failed, service_id}, state) do
    Logger.warning("Service #{service_id} failed health check, removing from registry")
    ServiceRegistry.unregister(state.registry_pid, service_id)
    update_stats(state, :health_check_failures, 1)
    {:noreply, state}
  end

  def handle_info({:partition_detected, partitions}, state) do
    Logger.warning("Network partition detected: #{inspect(partitions)}")
    update_stats(state, :partitions_detected, 1)
    {:noreply, state}
  end

  def handle_info({:partition_healed, merged_services}, state) do
    Logger.info("Network partition healed, merged #{length(merged_services)} services")
    {:noreply, state}
  end

  def handle_info(msg, state) do
    Logger.debug("Unexpected message in Cluster: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def terminate(reason, state) do
    Logger.info("BEAM cluster terminating: #{inspect(reason)}")

    # Clean up child processes
    if state.health_monitor_pid do
      HealthMonitor.stop(state.health_monitor_pid)
    end

    if state.partition_detector_pid do
      PartitionDetector.stop(state.partition_detector_pid)
    end

    ServiceRegistry.stop(state.registry_pid)

    :ok
  end

  # Private helper functions

  defp init_stats do
    %{
      started_at: System.system_time(:millisecond),
      services_registered: 0,
      services_unregistered: 0,
      discovery_requests: 0,
      failures_recorded: 0,
      health_check_failures: 0,
      partitions_detected: 0
    }
  end

  defp update_stats(state, metric, increment) do
    current_value = Map.get(state.stats, metric, 0)
    updated_stats = Map.put(state.stats, metric, current_value + increment)
    %{state | stats: updated_stats}
  end
end
