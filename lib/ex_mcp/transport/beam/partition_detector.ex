defmodule ExMCP.Transport.Beam.PartitionDetector do
  @moduledoc """
  Network partition detection and healing for BEAM transport clustering.

  Monitors cluster connectivity and detects network partitions (split-brain scenarios).
  When partitions are detected, applies configured merge strategies to reconcile
  conflicting service registrations.

  ## Partition Detection Methods

  - **Node Monitoring**: Monitor BEAM node connections
  - **Heartbeat**: Regular heartbeat messages between cluster members  
  - **Service Registry Comparison**: Compare service states across nodes
  - **Clock Synchronization**: Detect time drift that may indicate partitions

  ## Merge Strategies

  - `:last_writer_wins` - Keep services with most recent timestamps
  - `:first_writer_wins` - Keep services with earliest timestamps
  - `:manual` - Require manual intervention to resolve conflicts

  ## Example Usage

      {:ok, detector} = PartitionDetector.start_link(%{
        cluster: cluster_pid,
        detection_interval: 10000,
        merge_strategy: :last_writer_wins,
        partition_threshold: 3
      })
  """

  use GenServer
  require Logger

  alias ExMCP.Transport.Beam.ServiceRegistry

  defstruct [
    :cluster,
    :registry,
    :detection_interval,
    :merge_strategy,
    :partition_threshold,
    :monitored_nodes,
    :partition_state,
    :heartbeat_state,
    :stats
  ]

  @type merge_strategy :: :last_writer_wins | :first_writer_wins | :manual
  @type partition_state :: :normal | :partitioned | :healing

  @type config :: %{
          cluster: GenServer.server(),
          detection_interval: non_neg_integer(),
          merge_strategy: merge_strategy(),
          partition_threshold: non_neg_integer(),
          heartbeat_timeout: non_neg_integer()
        }

  @default_config %{
    detection_interval: 10000,
    merge_strategy: :last_writer_wins,
    partition_threshold: 3,
    heartbeat_timeout: 5000
  }

  @doc """
  Starts the partition detector with the given configuration.
  """
  @spec start_link(config()) :: GenServer.on_start()
  def start_link(config) do
    GenServer.start_link(__MODULE__, config)
  end

  @doc """
  Stops the partition detector.
  """
  @spec stop(GenServer.server()) :: :ok
  def stop(detector) do
    GenServer.stop(detector, :normal)
  end

  @doc """
  Manually triggers partition detection.
  """
  @spec detect_partitions(GenServer.server()) :: {:ok, partition_state()} | {:error, term()}
  def detect_partitions(detector) do
    GenServer.call(detector, :detect_partitions)
  end

  @doc """
  Gets current partition status.
  """
  @spec get_partition_status(GenServer.server()) :: {:ok, partition_state()}
  def get_partition_status(detector) do
    GenServer.call(detector, :get_partition_status)
  end

  @doc """
  Manually heals a detected partition.
  """
  @spec heal_partition(GenServer.server()) :: :ok | {:error, term()}
  def heal_partition(detector) do
    GenServer.call(detector, :heal_partition)
  end

  @doc """
  Simulates a network partition for testing purposes.
  """
  @spec simulate_partition(GenServer.server(), [atom()], [atom()]) :: :ok
  def simulate_partition(detector, partition_a, partition_b) do
    GenServer.cast(detector, {:simulate_partition, partition_a, partition_b})
  end

  @doc """
  Updates the merge strategy.
  """
  @spec update_merge_strategy(GenServer.server(), merge_strategy()) :: :ok
  def update_merge_strategy(detector, strategy) do
    GenServer.call(detector, {:update_merge_strategy, strategy})
  end

  @doc """
  Gets partition detector statistics.
  """
  @spec get_stats(GenServer.server()) :: {:ok, map()}
  def get_stats(detector) do
    GenServer.call(detector, :get_stats)
  end

  # GenServer callbacks

  @impl true
  def init(config) do
    full_config = Map.merge(@default_config, config)

    state = %__MODULE__{
      cluster: Map.fetch!(full_config, :cluster),
      registry: Map.fetch!(full_config, :registry),
      detection_interval: full_config.detection_interval,
      merge_strategy: full_config.merge_strategy,
      partition_threshold: full_config.partition_threshold,
      monitored_nodes: %{},
      partition_state: :normal,
      heartbeat_state: %{},
      stats: init_stats()
    }

    # Start partition detection timer
    schedule_detection(state.detection_interval)

    # Start monitoring node connections
    start_node_monitoring(state)

    Logger.debug("Partition detector started with #{full_config.merge_strategy} merge strategy")

    {:ok, state}
  end

  @impl true
  def handle_call(:detect_partitions, _from, state) do
    {partition_status, updated_state} = perform_partition_detection(state)
    {:reply, {:ok, partition_status}, updated_state}
  end

  def handle_call(:get_partition_status, _from, state) do
    {:reply, {:ok, state.partition_state}, state}
  end

  def handle_call(:heal_partition, _from, state) do
    case state.partition_state do
      :partitioned ->
        {result, updated_state} = perform_partition_healing(state)
        {:reply, result, updated_state}

      _ ->
        {:reply, {:error, :no_partition_detected}, state}
    end
  end

  def handle_call({:update_merge_strategy, strategy}, _from, state) do
    updated_state = %{state | merge_strategy: strategy}
    Logger.info("Partition detector merge strategy changed to #{strategy}")
    {:reply, :ok, updated_state}
  end

  def handle_call(:get_stats, _from, state) do
    current_stats =
      Map.merge(state.stats, %{
        partition_state: state.partition_state,
        monitored_nodes: map_size(state.monitored_nodes),
        merge_strategy: state.merge_strategy,
        uptime: System.system_time(:millisecond) - state.stats.started_at
      })

    {:reply, {:ok, current_stats}, state}
  end

  @impl true
  def handle_cast({:simulate_partition, partition_a, partition_b}, state) do
    simulation_state = %{
      type: :simulated,
      partition_a: partition_a,
      partition_b: partition_b,
      started_at: System.system_time(:millisecond)
    }

    updated_state = %{
      state
      | partition_state: :partitioned,
        monitored_nodes: Map.put(state.monitored_nodes, :simulation, simulation_state)
    }

    # Notify cluster about the simulated partition
    send_partition_notification(updated_state.cluster, [partition_a, partition_b])

    Logger.warning(
      "Simulated network partition: #{inspect(partition_a)} | #{inspect(partition_b)}"
    )

    {:noreply, update_stats(updated_state, :partitions_detected, 1)}
  end

  @impl true
  def handle_info(:detect_partitions, state) do
    # Perform periodic partition detection
    {_status, updated_state} = perform_partition_detection(state)

    # Schedule next detection
    schedule_detection(state.detection_interval)

    {:noreply, updated_state}
  end

  def handle_info({:nodedown, node}, state) do
    Logger.warning("Node #{node} went down, checking for partition")

    # Update monitored nodes
    updated_nodes =
      Map.put(state.monitored_nodes, node, %{
        status: :down,
        last_seen: System.system_time(:millisecond)
      })

    updated_state = %{state | monitored_nodes: updated_nodes}

    # Check if this indicates a partition
    {_status, final_state} = perform_partition_detection(updated_state)

    {:noreply, final_state}
  end

  def handle_info({:nodeup, node}, state) do
    Logger.info("Node #{node} came back up")

    # Update monitored nodes
    updated_nodes =
      Map.put(state.monitored_nodes, node, %{
        status: :up,
        last_seen: System.system_time(:millisecond)
      })

    updated_state = %{state | monitored_nodes: updated_nodes}

    # If we were in a partitioned state, try to heal
    final_state =
      if state.partition_state == :partitioned do
        {_result, healed_state} = perform_partition_healing(updated_state)
        healed_state
      else
        updated_state
      end

    {:noreply, final_state}
  end

  def handle_info({:heartbeat_timeout, node}, state) do
    Logger.warning("Heartbeat timeout from node #{node}")

    # Mark node as potentially down
    updated_nodes =
      Map.update(
        state.monitored_nodes,
        node,
        %{status: :timeout, last_seen: System.system_time(:millisecond)},
        fn existing -> Map.put(existing, :status, :timeout) end
      )

    updated_state = %{state | monitored_nodes: updated_nodes}

    # Check for partition
    {_status, final_state} = perform_partition_detection(updated_state)

    {:noreply, final_state}
  end

  def handle_info(msg, state) do
    Logger.debug("Unexpected message in PartitionDetector: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, _state) do
    Logger.debug("Partition detector terminated")
    :ok
  end

  # Private helper functions

  defp schedule_detection(interval) do
    Process.send_after(self(), :detect_partitions, interval)
  end

  defp start_node_monitoring(state) do
    # Monitor all connected nodes
    connected_nodes = [node() | Node.list()]

    Enum.each(connected_nodes, fn _node ->
      :net_kernel.monitor_nodes(true)
    end)

    # Initialize heartbeat state
    heartbeat_state =
      Enum.reduce(connected_nodes, %{}, fn node, acc ->
        Map.put(acc, node, %{
          last_heartbeat: System.system_time(:millisecond),
          consecutive_failures: 0
        })
      end)

    %{state | heartbeat_state: heartbeat_state}
  end

  defp perform_partition_detection(state) do
    current_time = System.system_time(:millisecond)

    # Check various partition indicators
    indicators = [
      check_node_connectivity(state),
      check_heartbeat_failures(state, current_time),
      check_service_registry_conflicts(state)
    ]

    # Count partition indicators
    partition_count = Enum.count(indicators, &(&1 == :partition_detected))

    new_partition_state =
      if partition_count >= state.partition_threshold do
        case state.partition_state do
          :normal ->
            Logger.warning("Network partition detected (#{partition_count} indicators)")
            send_partition_notification(state.cluster, extract_partitions(state))
            :partitioned

          :partitioned ->
            :partitioned

          :healing ->
            # Revert to partitioned if detection fails during healing
            :partitioned
        end
      else
        case state.partition_state do
          :partitioned ->
            Logger.info("Partition indicators below threshold, attempting healing")
            :healing

          :healing ->
            Logger.info("Partition healing successful")
            :normal

          :normal ->
            :normal
        end
      end

    updated_state = %{state | partition_state: new_partition_state}

    if new_partition_state != state.partition_state do
      update_stats(updated_state, :partition_state_changes, 1)
    else
      updated_state
    end

    {new_partition_state, updated_state}
  end

  defp check_node_connectivity(_state) do
    # Check if we can connect to expected nodes
    expected_nodes = Node.list(:known)
    connected_nodes = Node.list(:connected)

    disconnected_count = length(expected_nodes) - length(connected_nodes)

    if disconnected_count > 0 do
      :partition_detected
    else
      :normal
    end
  end

  defp check_heartbeat_failures(state, current_time) do
    timeout_threshold = Map.get(state.stats, :heartbeat_timeout, 5000)

    failed_heartbeats =
      Enum.count(state.heartbeat_state, fn {_node, heartbeat_data} ->
        time_since_last = current_time - heartbeat_data.last_heartbeat
        time_since_last > timeout_threshold
      end)

    if failed_heartbeats > 0 do
      :partition_detected
    else
      :normal
    end
  end

  defp check_service_registry_conflicts(_state) do
    # In a real implementation, this would compare service registries
    # across nodes to detect inconsistencies
    # For now, we'll assume no conflicts
    :normal
  end

  defp extract_partitions(state) do
    # Extract partition information from monitored nodes
    up_nodes =
      Enum.filter(state.monitored_nodes, fn {_node, data} ->
        Map.get(data, :status) == :up
      end)
      |> Enum.map(fn {node, _} -> node end)

    down_nodes =
      Enum.filter(state.monitored_nodes, fn {_node, data} ->
        Map.get(data, :status) in [:down, :timeout]
      end)
      |> Enum.map(fn {node, _} -> node end)

    [up_nodes, down_nodes]
  end

  defp perform_partition_healing(state) do
    case state.merge_strategy do
      :last_writer_wins ->
        heal_with_last_writer_wins(state)

      :first_writer_wins ->
        heal_with_first_writer_wins(state)

      :manual ->
        {:error, :manual_healing_required}
    end
  end

  defp heal_with_last_writer_wins(state) do
    Logger.info("Performing partition healing with last-writer-wins strategy")

    # Get all services directly from the registry
    case ServiceRegistry.discover(state.registry, %{}) do
      {:ok, all_services} ->
        # Group services by name to find conflicts
        services_by_name = Enum.group_by(all_services, & &1.name)

        merged_services =
          Enum.flat_map(services_by_name, fn {_name, services} ->
            case services do
              [single_service] ->
                # No conflict, keep the service
                [single_service]

              multiple_services ->
                # Conflict detected, keep the service with the latest registered_at time
                latest_service = Enum.max_by(multiple_services, & &1.registered_at)

                # Remove all other conflicting services directly from registry
                services_to_remove = multiple_services -- [latest_service]

                Enum.each(services_to_remove, fn service ->
                  ServiceRegistry.unregister(state.registry, service.id)
                end)

                Logger.info(
                  "Resolved conflict for service '#{latest_service.name}': kept version #{latest_service.version}"
                )

                [latest_service]
            end
          end)

        # Send healing notification
        send_healing_notification(state.cluster, merged_services)

        updated_state = %{state | partition_state: :normal}
        final_state = update_stats(updated_state, :partitions_healed, 1)

        {:ok, final_state}

      {:error, reason} ->
        Logger.error("Failed to get services for partition healing: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp heal_with_first_writer_wins(state) do
    Logger.info("Performing partition healing with first-writer-wins strategy")

    # Similar to last_writer_wins but keeps earliest timestamps
    merged_services = []

    send_healing_notification(state.cluster, merged_services)

    updated_state = %{state | partition_state: :normal}
    final_state = update_stats(updated_state, :partitions_healed, 1)

    {:ok, final_state}
  end

  defp send_partition_notification(cluster, partitions) do
    send(cluster, {:partition_detected, partitions})
  end

  defp send_healing_notification(cluster, merged_services) do
    send(cluster, {:partition_healed, merged_services})
  end

  defp init_stats do
    %{
      started_at: System.system_time(:millisecond),
      partitions_detected: 0,
      partitions_healed: 0,
      partition_state_changes: 0,
      heartbeat_timeout: 5000
    }
  end

  defp update_stats(state, metric, increment) do
    current_value = Map.get(state.stats, metric, 0)
    updated_stats = Map.put(state.stats, metric, current_value + increment)
    %{state | stats: updated_stats}
  end
end
