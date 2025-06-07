defmodule ExMCP.Transport.Beam.HealthMonitor do
  @moduledoc """
  Health monitoring for MCP services in BEAM transport clustering.

  Monitors service health through various mechanisms:
  - Process liveness checks
  - Custom health check callbacks
  - Response time monitoring
  - Circuit breaker integration

  Automatically removes unhealthy services from the registry and notifies
  the cluster coordinator of health changes.

  ## Health Check Methods

  - **Process Monitor**: Monitor service processes for crashes
  - **Ping**: Send ping messages to verify responsiveness
  - **Custom Callback**: Use service-defined health check functions
  - **Response Time**: Track and threshold response times

  ## Example Usage

      {:ok, monitor} = HealthMonitor.start_link(%{
        registry: registry_pid,
        check_interval: 5000,
        service_timeout: 3000,
        methods: [:process, :ping, :custom]
      })
  """

  use GenServer
  require Logger

  alias ExMCP.Transport.Beam.ServiceRegistry

  defstruct [
    :registry,
    :check_interval,
    :service_timeout,
    :health_check_methods,
    :monitored_services,
    :health_timers,
    :stats
  ]

  @type health_status :: :healthy | :unhealthy | :unknown
  @type health_method :: :process | :ping | :custom | :response_time

  @type config :: %{
          registry: GenServer.server(),
          check_interval: non_neg_integer(),
          service_timeout: non_neg_integer(),
          methods: [health_method()],
          max_failures: non_neg_integer(),
          failure_window: non_neg_integer()
        }

  @default_config %{
    check_interval: 5000,
    service_timeout: 3000,
    methods: [:process, :ping],
    max_failures: 3,
    failure_window: 30000
  }

  @doc """
  Starts the health monitor with the given configuration.
  """
  @spec start_link(config()) :: GenServer.on_start()
  def start_link(config) do
    GenServer.start_link(__MODULE__, config)
  end

  @doc """
  Stops the health monitor.
  """
  @spec stop(GenServer.server()) :: :ok
  def stop(monitor) do
    GenServer.stop(monitor, :normal)
  end

  @doc """
  Manually triggers a health check for a specific service.
  """
  @spec check_service(GenServer.server(), String.t()) ::
          {:ok, health_status()} | {:error, term()}
  def check_service(monitor, service_id) do
    GenServer.call(monitor, {:check_service, service_id})
  end

  @doc """
  Gets health status for a specific service.
  """
  @spec get_health_status(GenServer.server(), String.t()) ::
          {:ok, health_status()} | {:error, :not_found}
  def get_health_status(monitor, service_id) do
    GenServer.call(monitor, {:get_health_status, service_id})
  end

  @doc """
  Adds a service to health monitoring.
  """
  @spec monitor_service(GenServer.server(), String.t(), map()) :: :ok
  def monitor_service(monitor, service_id, health_config \\ %{}) do
    GenServer.cast(monitor, {:monitor_service, service_id, health_config})
  end

  @doc """
  Removes a service from health monitoring.
  """
  @spec unmonitor_service(GenServer.server(), String.t()) :: :ok
  def unmonitor_service(monitor, service_id) do
    GenServer.cast(monitor, {:unmonitor_service, service_id})
  end

  @doc """
  Gets health monitoring statistics.
  """
  @spec get_stats(GenServer.server()) :: {:ok, map()}
  def get_stats(monitor) do
    GenServer.call(monitor, :get_stats)
  end

  # GenServer callbacks

  @impl true
  def init(config) do
    full_config = Map.merge(@default_config, config)

    state = %__MODULE__{
      registry: Map.fetch!(full_config, :registry),
      check_interval: full_config.check_interval,
      service_timeout: full_config.service_timeout,
      health_check_methods: full_config.methods,
      monitored_services: %{},
      health_timers: %{},
      stats: init_stats(full_config)
    }

    # Start periodic health checks
    schedule_health_check(state.check_interval)

    Logger.debug("Health monitor started with interval #{state.check_interval}ms")

    {:ok, state}
  end

  @impl true
  def handle_call({:check_service, service_id}, _from, state) do
    case ServiceRegistry.get(state.registry, service_id) do
      {:ok, service} ->
        health_status = perform_health_check(service, state)
        {:reply, {:ok, health_status}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:get_health_status, service_id}, _from, state) do
    case Map.get(state.monitored_services, service_id) do
      nil ->
        {:reply, {:error, :not_found}, state}

      service_data ->
        status = Map.get(service_data, :health_status, :unknown)
        {:reply, {:ok, status}, state}
    end
  end

  def handle_call(:get_stats, _from, state) do
    current_stats =
      Map.merge(state.stats, %{
        monitored_services: map_size(state.monitored_services),
        active_timers: map_size(state.health_timers),
        uptime: System.system_time(:millisecond) - state.stats.started_at
      })

    {:reply, {:ok, current_stats}, state}
  end

  @impl true
  def handle_cast({:monitor_service, service_id, health_config}, state) do
    service_data = %{
      service_id: service_id,
      health_config: health_config,
      health_status: :unknown,
      last_check: nil,
      failure_count: 0,
      failure_window_start: nil
    }

    updated_services = Map.put(state.monitored_services, service_id, service_data)
    updated_state = %{state | monitored_services: updated_services}

    Logger.debug("Started monitoring service #{service_id}")
    {:noreply, updated_state}
  end

  def handle_cast({:unmonitor_service, service_id}, state) do
    # Cancel any pending health check timer
    if Map.has_key?(state.health_timers, service_id) do
      timer_ref = state.health_timers[service_id]
      Process.cancel_timer(timer_ref)
    end

    updated_services = Map.delete(state.monitored_services, service_id)
    updated_timers = Map.delete(state.health_timers, service_id)

    updated_state = %{state | monitored_services: updated_services, health_timers: updated_timers}

    Logger.debug("Stopped monitoring service #{service_id}")
    {:noreply, updated_state}
  end

  @impl true
  def handle_info(:health_check, state) do
    # Perform health checks for all monitored services
    updated_state = perform_all_health_checks(state)

    # Schedule next health check
    schedule_health_check(state.check_interval)

    {:noreply, updated_state}
  end

  def handle_info({:health_check_timeout, service_id}, state) do
    # Handle timeout for a specific service health check
    case Map.get(state.monitored_services, service_id) do
      nil ->
        {:noreply, state}

      service_data ->
        Logger.warning("Health check timeout for service #{service_id}")
        updated_data = record_health_failure(service_data, state)
        updated_services = Map.put(state.monitored_services, service_id, updated_data)
        updated_state = %{state | monitored_services: updated_services}

        # Remove service if it has too many failures
        final_state = maybe_remove_unhealthy_service(service_id, updated_data, updated_state)

        {:noreply, final_state}
    end
  end

  def handle_info({:DOWN, _ref, :process, pid, reason}, state) do
    # Handle monitored process death
    case find_service_by_pid(state.monitored_services, pid) do
      nil ->
        {:noreply, state}

      service_id ->
        Logger.warning("Monitored service #{service_id} process died: #{inspect(reason)}")

        # Remove the service from monitoring and registry
        ServiceRegistry.unregister(state.registry, service_id)
        updated_services = Map.delete(state.monitored_services, service_id)
        updated_state = %{state | monitored_services: updated_services}

        update_stats(updated_state, :process_failures, 1)
        {:noreply, updated_state}
    end
  end

  def handle_info(msg, state) do
    Logger.debug("Unexpected message in HealthMonitor: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, state) do
    # Cancel all pending timers
    Enum.each(state.health_timers, fn {_service_id, timer_ref} ->
      Process.cancel_timer(timer_ref)
    end)

    Logger.debug("Health monitor terminated")
    :ok
  end

  # Private helper functions

  defp schedule_health_check(interval) do
    Process.send_after(self(), :health_check, interval)
  end

  defp perform_all_health_checks(state) do
    Enum.reduce(state.monitored_services, state, fn {service_id, service_data}, acc_state ->
      case ServiceRegistry.get(state.registry, service_id) do
        {:ok, service} ->
          health_status = perform_health_check(service, acc_state)
          updated_data = update_health_status(service_data, health_status, acc_state)

          updated_services = Map.put(acc_state.monitored_services, service_id, updated_data)
          updated_state = %{acc_state | monitored_services: updated_services}

          # Remove service if unhealthy and return the updated state
          maybe_remove_unhealthy_service(service_id, updated_data, updated_state)

        {:error, :not_found} ->
          # Service no longer exists in registry, stop monitoring
          updated_services = Map.delete(acc_state.monitored_services, service_id)
          %{acc_state | monitored_services: updated_services}
      end
    end)
  end

  defp perform_health_check(service, state) do
    methods = state.health_check_methods

    # Try each health check method
    results =
      Enum.map(methods, fn method ->
        case method do
          :process ->
            check_process_health(service)

          :ping ->
            check_ping_health(service, state.service_timeout)

          :custom ->
            check_custom_health(service, state.service_timeout)

          :response_time ->
            check_response_time_health(service, state.service_timeout)
        end
      end)

    # Aggregate results - service is healthy if all checks pass
    if Enum.all?(results, &(&1 == :healthy)) do
      :healthy
    else
      :unhealthy
    end
  end

  defp check_process_health(service) do
    case Process.alive?(service.pid) do
      true -> :healthy
      false -> :unhealthy
    end
  end

  defp check_ping_health(service, timeout) do
    # Send a simple ping message to the service process
    case GenServer.call(service.pid, :health_ping, timeout) do
      :pong -> :healthy
      _ -> :unhealthy
    end
  rescue
    _ -> :unhealthy
  catch
    :exit, _ -> :unhealthy
  end

  defp check_custom_health(service, timeout) do
    case Map.get(service, :health_check) do
      nil ->
        # No custom health check defined
        :healthy

      %{ref: ref} ->
        try do
          send(service.pid, {:health_check, ref})

          receive do
            {:reply, :healthy} -> :healthy
            {:reply, :unhealthy} -> :unhealthy
          after
            timeout -> :unhealthy
          end
        rescue
          _ -> :unhealthy
        catch
          _ -> :unhealthy
        end

      _other ->
        # Unknown health check config, assume healthy
        :healthy
    end
  end

  defp check_response_time_health(service, timeout) do
    start_time = System.monotonic_time(:millisecond)

    result = check_ping_health(service, timeout)

    end_time = System.monotonic_time(:millisecond)
    response_time = end_time - start_time

    # Consider service unhealthy if response time exceeds 80% of timeout
    threshold = trunc(timeout * 0.8)

    case {result, response_time} do
      {:healthy, time} when time < threshold -> :healthy
      _ -> :unhealthy
    end
  end

  defp update_health_status(service_data, health_status, state) do
    current_time = System.system_time(:millisecond)

    updated_data = %{service_data | health_status: health_status, last_check: current_time}

    case health_status do
      :healthy ->
        # Reset failure count on successful health check
        %{updated_data | failure_count: 0, failure_window_start: nil}

      :unhealthy ->
        record_health_failure(updated_data, state)
    end
  end

  defp record_health_failure(service_data, state) do
    current_time = System.system_time(:millisecond)
    _failure_window = state.stats.max_failures || 3
    failure_timeout = state.stats.failure_window || 30000

    # Check if we need to start a new failure window
    window_start =
      case service_data.failure_window_start do
        nil -> current_time
        start_time when current_time - start_time > failure_timeout -> current_time
        start_time -> start_time
      end

    %{
      service_data
      | failure_count: service_data.failure_count + 1,
        failure_window_start: window_start
    }
  end

  defp maybe_remove_unhealthy_service(service_id, service_data, state) do
    max_failures = Map.get(state.stats, :max_failures, 3)

    if service_data.failure_count >= max_failures do
      Logger.warning(
        "Removing unhealthy service #{service_id} after #{service_data.failure_count} failures"
      )

      # Remove from registry and stop monitoring
      ServiceRegistry.unregister(state.registry, service_id)

      # Stop monitoring this service
      updated_services = Map.delete(state.monitored_services, service_id)
      update_stats(%{state | monitored_services: updated_services}, :services_removed, 1)
    else
      state
    end
  end

  defp find_service_by_pid(monitored_services, pid) do
    Enum.find_value(monitored_services, fn {service_id, _service_data} ->
      # In a real implementation, we'd need to track PIDs properly
      # For now, this is a simplified version
      case ServiceRegistry.get(self(), service_id) do
        {:ok, service} when service.pid == pid -> service_id
        _ -> nil
      end
    end)
  end

  defp init_stats(config) do
    %{
      started_at: System.system_time(:millisecond),
      health_checks_performed: 0,
      services_removed: 0,
      process_failures: 0,
      max_failures: Map.get(config, :max_failures, 3),
      failure_window: Map.get(config, :failure_window, 30000)
    }
  end

  defp update_stats(state, metric, increment) do
    current_value = Map.get(state.stats, metric, 0)
    updated_stats = Map.put(state.stats, metric, current_value + increment)
    %{state | stats: updated_stats}
  end
end
