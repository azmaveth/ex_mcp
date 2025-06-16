defmodule ExMCP.Transport.Beam.Observability do
  @moduledoc """
  Observability module for BEAM transport providing metrics collection,
  health monitoring, distributed tracing, and alerting capabilities.

  This module implements comprehensive observability features for the enhanced 
  BEAM transport to enable monitoring, debugging, and performance analysis.

  ## Features

  - Metrics collection and aggregation
  - Health monitoring and status checks
  - Distributed tracing correlation
  - Real-time alerting and notifications
  - Performance analytics
  - Resource usage tracking
  """

  use GenServer
  require Logger

  # 1 hour
  @default_metrics_retention_ms 3_600_000
  # 30 minutes
  @default_trace_retention_ms 1_800_000
  # 5 seconds
  @default_health_check_interval_ms 5_000

  defstruct [
    :metrics,
    :traces,
    :health_status,
    :alert_handlers,
    :performance_counters,
    start_time: nil,
    last_health_check: nil
  ]

  @doc """
  Starts the observability manager.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Gets current metrics.
  """
  @spec get_metrics() :: {:ok, map()}
  def get_metrics do
    case GenServer.whereis(__MODULE__) do
      nil -> {:ok, default_metrics()}
      _pid -> GenServer.call(__MODULE__, :get_metrics)
    end
  end

  @doc """
  Resets all metrics to zero.
  """
  @spec reset_metrics() :: :ok
  def reset_metrics do
    case GenServer.whereis(__MODULE__) do
      nil -> :ok
      _pid -> GenServer.call(__MODULE__, :reset_metrics)
    end
  end

  @doc """
  Performs a health check.
  """
  @spec health_check() :: {:ok, :healthy | :unhealthy}
  def health_check do
    case GenServer.whereis(__MODULE__) do
      nil -> {:ok, :healthy}
      _pid -> GenServer.call(__MODULE__, :health_check)
    end
  end

  @doc """
  Performs a health check on a specific server.
  """
  @spec health_check(term()) :: {:ok, :healthy | :unhealthy}
  def health_check(_server) do
    # For now, delegate to the general health check
    health_check()
  end

  @doc """
  Gets traces for a specific trace ID.
  """
  @spec get_traces(String.t()) :: {:ok, list()}
  def get_traces(trace_id) do
    case GenServer.whereis(__MODULE__) do
      nil -> {:ok, []}
      _pid -> GenServer.call(__MODULE__, {:get_traces, trace_id})
    end
  end

  @doc """
  Gets all active traces.
  """
  @spec get_active_traces() :: {:ok, list()}
  def get_active_traces do
    case GenServer.whereis(__MODULE__) do
      nil -> {:ok, []}
      _pid -> GenServer.call(__MODULE__, :get_active_traces)
    end
  end

  @doc """
  Sets an alert handler function.
  """
  @spec set_alert_handler(function()) :: :ok
  def set_alert_handler(handler_fn) when is_function(handler_fn) do
    case GenServer.whereis(__MODULE__) do
      nil -> :ok
      _pid -> GenServer.call(__MODULE__, {:set_alert_handler, handler_fn})
    end
  end

  @doc """
  Records a metric value.
  """
  @spec record_metric(atom(), number()) :: :ok
  def record_metric(metric_name, value) do
    case GenServer.whereis(__MODULE__) do
      nil -> :ok
      _pid -> GenServer.cast(__MODULE__, {:record_metric, metric_name, value})
    end
  end

  @doc """
  Records a message sent event.
  """
  @spec record_message_sent(non_neg_integer()) :: :ok
  def record_message_sent(byte_size) do
    record_metric(:bytes_sent, byte_size)
    record_metric(:total_messages, 1)
  end

  @doc """
  Records a message received event.
  """
  @spec record_message_received(non_neg_integer(), non_neg_integer()) :: :ok
  def record_message_received(byte_size, _latency_ms) do
    record_metric(:bytes_received, byte_size)
    record_metric(:total_messages, 1)
  end

  @doc """
  Records an error event.
  """
  @spec record_error(atom()) :: :ok
  def record_error(_error_type) do
    record_metric(:failed_requests, 1)
    # Simplified error rate tracking
    record_metric(:error_rate, 0.1)
  end

  @doc """
  Gets comprehensive statistics including derived metrics.
  """
  @spec get_comprehensive_stats() :: {:ok, map()}
  def get_comprehensive_stats do
    {:ok, metrics} = get_metrics()

    # Add additional derived stats
    enhanced_metrics =
      Map.merge(metrics, %{
        requests_per_second: calculate_rps(metrics),
        error_percentage: calculate_error_percentage(metrics),
        average_message_size: calculate_avg_message_size(metrics)
      })

    {:ok, enhanced_metrics}
  end

  @doc """
  Records a trace span.
  """
  @spec record_trace(String.t(), map()) :: :ok
  def record_trace(trace_id, span_data) do
    case GenServer.whereis(__MODULE__) do
      nil -> :ok
      _pid -> GenServer.cast(__MODULE__, {:record_trace, trace_id, span_data})
    end
  end

  @doc """
  Triggers an alert.
  """
  @spec trigger_alert(map()) :: :ok
  def trigger_alert(alert_data) do
    case GenServer.whereis(__MODULE__) do
      nil -> :ok
      _pid -> GenServer.cast(__MODULE__, {:trigger_alert, alert_data})
    end
  end

  # GenServer callbacks

  @impl true
  def init(_opts) do
    state = %__MODULE__{
      metrics: default_metrics(),
      traces: %{},
      health_status: :healthy,
      alert_handlers: [],
      performance_counters: %{},
      start_time: System.monotonic_time(:millisecond),
      last_health_check: System.monotonic_time(:millisecond)
    }

    # Schedule periodic health checks
    schedule_health_check()

    # Schedule cleanup
    schedule_cleanup()

    Logger.info("Observability service started")
    {:ok, state}
  end

  @impl true
  def handle_call(:get_metrics, _from, state) do
    # Calculate derived metrics
    uptime_ms = System.monotonic_time(:millisecond) - state.start_time

    # Calculate average latency
    avg_latency =
      if state.metrics.successful_requests > 0 do
        state.metrics.total_latency / state.metrics.successful_requests
      else
        0.0
      end

    enhanced_metrics =
      Map.merge(state.metrics, %{
        uptime_ms: uptime_ms,
        uptime_hours: uptime_ms / (1000 * 60 * 60),
        active_traces: map_size(state.traces),
        last_health_check: state.last_health_check,
        health_status: state.health_status,
        average_latency: avg_latency
      })

    {:reply, {:ok, enhanced_metrics}, state}
  end

  def handle_call(:reset_metrics, _from, state) do
    new_state = %{state | metrics: default_metrics()}
    Logger.info("Metrics reset")
    {:reply, :ok, new_state}
  end

  def handle_call(:health_check, _from, state) do
    # Perform comprehensive health check
    health_status = perform_health_check(state)

    new_state = %{
      state
      | health_status: health_status,
        last_health_check: System.monotonic_time(:millisecond)
    }

    {:reply, {:ok, health_status}, new_state}
  end

  def handle_call({:get_traces, trace_id}, _from, state) do
    traces = Map.get(state.traces, trace_id, [])
    {:reply, {:ok, traces}, state}
  end

  def handle_call(:get_active_traces, _from, state) do
    all_traces =
      state.traces
      |> Enum.flat_map(fn {trace_id, spans} ->
        Enum.map(spans, &Map.put(&1, :trace_id, trace_id))
      end)

    {:reply, {:ok, all_traces}, state}
  end

  def handle_call({:set_alert_handler, handler_fn}, _from, state) do
    new_handlers = [handler_fn | state.alert_handlers]
    new_state = %{state | alert_handlers: new_handlers}
    Logger.info("Alert handler added")
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_cast({:record_metric, metric_name, value}, state) do
    updated_metrics =
      case metric_name do
        :average_latency ->
          # For average latency, accumulate into total_latency
          current_total = Map.get(state.metrics, :total_latency, 0.0)
          Map.put(state.metrics, :total_latency, current_total + value)

        _ ->
          case Map.get(state.metrics, metric_name) do
            nil ->
              Map.put(state.metrics, metric_name, value)

            existing_value when is_number(existing_value) ->
              Map.put(state.metrics, metric_name, existing_value + value)

            _ ->
              Map.put(state.metrics, metric_name, value)
          end
      end

    {:noreply, %{state | metrics: updated_metrics}}
  end

  def handle_cast({:record_trace, trace_id, span_data}, state) do
    span_with_timestamp = Map.put(span_data, :timestamp, System.system_time(:millisecond))

    updated_traces =
      Map.update(state.traces, trace_id, [span_with_timestamp], fn existing_spans ->
        [span_with_timestamp | existing_spans]
      end)

    {:noreply, %{state | traces: updated_traces}}
  end

  def handle_cast({:trigger_alert, alert_data}, state) do
    # Send alert to all registered handlers
    Enum.each(state.alert_handlers, fn handler ->
      try do
        handler.(alert_data)
      rescue
        error ->
          Logger.error("Alert handler failed: #{inspect(error)}")
      end
    end)

    # Log the alert
    Logger.warning("Alert triggered: #{inspect(alert_data)}")

    {:noreply, state}
  end

  @impl true
  def handle_info(:health_check, state) do
    health_status = perform_health_check(state)

    # Check for health status changes and trigger alerts
    if health_status != state.health_status do
      alert_data = %{
        type: :health_status_change,
        old_status: state.health_status,
        new_status: health_status,
        timestamp: System.system_time(:millisecond)
      }

      GenServer.cast(self(), {:trigger_alert, alert_data})
    end

    new_state = %{
      state
      | health_status: health_status,
        last_health_check: System.monotonic_time(:millisecond)
    }

    schedule_health_check()
    {:noreply, new_state}
  end

  def handle_info(:cleanup, state) do
    # Clean up old traces and metrics
    now = System.system_time(:millisecond)
    cutoff = now - @default_trace_retention_ms

    cleaned_traces =
      state.traces
      |> Enum.into(%{}, fn {trace_id, spans} ->
        recent_spans =
          Enum.filter(spans, fn span ->
            Map.get(span, :timestamp, 0) > cutoff
          end)

        if Enum.empty?(recent_spans) do
          nil
        else
          {trace_id, recent_spans}
        end
      end)
      |> Enum.reject(&is_nil/1)
      |> Enum.into(%{})

    schedule_cleanup()
    {:noreply, %{state | traces: cleaned_traces}}
  end

  def handle_info(msg, state) do
    Logger.debug("Unexpected message in Observability: #{inspect(msg)}")
    {:noreply, state}
  end

  # Private helper functions

  defp default_metrics do
    %{
      total_messages: 0,
      successful_requests: 0,
      failed_requests: 0,
      average_latency: 0.0,
      # Track total latency to calculate average
      total_latency: 0.0,
      memory_usage: :erlang.memory(:total),
      process_count: length(Process.list()),
      connection_count: 0,
      bytes_sent: 0,
      bytes_received: 0,
      error_rate: 0.0
    }
  end

  defp perform_health_check(state) do
    checks = [
      check_memory_usage(),
      check_process_count(),
      check_message_queues(),
      check_error_rates(state.metrics)
    ]

    if Enum.all?(checks, &(&1 == :ok)) do
      :healthy
    else
      :unhealthy
    end
  end

  defp check_memory_usage do
    total_memory = :erlang.memory(:total)
    # Consider unhealthy if using more than 1GB
    if total_memory > 1_000_000_000 do
      :unhealthy
    else
      :ok
    end
  end

  defp check_process_count do
    process_count = length(Process.list())
    # Consider unhealthy if more than 10,000 processes
    if process_count > 10_000 do
      :unhealthy
    else
      :ok
    end
  end

  defp check_message_queues do
    # Check for processes with large message queues
    large_queues =
      Process.list()
      |> Enum.count(fn pid ->
        case Process.info(pid, :message_queue_len) do
          {:message_queue_len, len} when len > 1000 -> true
          _ -> false
        end
      end)

    if large_queues > 10 do
      :unhealthy
    else
      :ok
    end
  end

  defp check_error_rates(metrics) do
    error_rate = Map.get(metrics, :error_rate, 0.0)
    # More than 10% error rate
    if error_rate > 0.1 do
      :unhealthy
    else
      :ok
    end
  end

  defp schedule_health_check do
    Process.send_after(self(), :health_check, @default_health_check_interval_ms)
  end

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup, @default_metrics_retention_ms)
  end

  # Helper functions for comprehensive stats calculation
  defp calculate_rps(metrics) do
    uptime_seconds = Map.get(metrics, :uptime_hours, 0) * 3600

    if uptime_seconds > 0 do
      Map.get(metrics, :successful_requests, 0) / uptime_seconds
    else
      0.0
    end
  end

  defp calculate_error_percentage(metrics) do
    total = Map.get(metrics, :total_messages, 0)
    failed = Map.get(metrics, :failed_requests, 0)

    if total > 0 do
      failed / total * 100
    else
      0.0
    end
  end

  defp calculate_avg_message_size(metrics) do
    total_bytes = Map.get(metrics, :bytes_sent, 0) + Map.get(metrics, :bytes_received, 0)
    total_messages = Map.get(metrics, :total_messages, 0)

    if total_messages > 0 do
      total_bytes / total_messages
    else
      0.0
    end
  end
end
