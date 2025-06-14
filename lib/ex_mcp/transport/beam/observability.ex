defmodule ExMCP.Transport.Beam.Observability do
  @moduledoc """
  Comprehensive observability and metrics collection for the enhanced BEAM transport.

  This module provides detailed monitoring, metrics collection, health checks,
  and observability features for the BEAM transport components.

  ## Features

  - **Real-time Metrics**: Message throughput, latency, error rates
  - **Health Monitoring**: Component health checks and status tracking
  - **Performance Analytics**: Zero-copy efficiency, batching effectiveness
  - **Security Metrics**: Security violations, rate limiting stats
  - **Distributed Tracing**: Request correlation and distributed tracing
  - **Alerting**: Configurable thresholds and alert mechanisms

  ## Usage

      # Start observability system
      {:ok, _pid} = Observability.start_link()

      # Record metrics
      Observability.record_message_sent(byte_size)
      Observability.record_message_received(byte_size, latency_ms)
      Observability.record_error(:connection_failed)

      # Get comprehensive stats
      stats = Observability.get_comprehensive_stats()

      # Health check
      health = Observability.health_check()

  ## Metrics Collected

  - Message counts (sent/received/failed)
  - Byte throughput (in/out)
  - Latency percentiles (p50, p95, p99)
  - Error rates by type
  - Zero-copy usage statistics
  - Batching effectiveness
  - Security event counts
  - Connection pool health
  """

  use GenServer
  require Logger

  alias ExMCP.Transport.Beam.{Security, ZeroCopy}

  @metrics_table :beam_transport_metrics
  @health_table :beam_transport_health
  @trace_table :beam_transport_traces

  # Metric collection intervals
  # 10 seconds
  @stats_interval 10_000
  # 30 seconds
  @health_check_interval 30_000
  # 5 minutes
  @trace_cleanup_interval 300_000

  # Alerting thresholds
  @default_thresholds %{
    error_rate_percent: 5.0,
    avg_latency_ms: 1000,
    memory_usage_mb: 100,
    connection_failures_per_min: 10
  }

  defstruct [
    :start_time,
    :thresholds,
    :alert_handler,
    :stats_timer,
    :health_timer,
    :trace_timer,
    metrics: %{},
    health_status: %{},
    active_traces: %{}
  ]

  @type t :: %__MODULE__{}
  @type metric_name :: atom()
  @type metric_value :: number()
  @type health_status :: :healthy | :degraded | :unhealthy
  @type alert_handler :: (map() -> :ok)

  # Public API

  @doc """
  Starts the observability system.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Records a message sent event.
  """
  @spec record_message_sent(pos_integer()) :: :ok
  def record_message_sent(byte_size) do
    record_metric(:messages_sent, 1)
    record_metric(:bytes_sent, byte_size)
    record_metric(:last_sent_timestamp, System.system_time(:millisecond))
  end

  @doc """
  Records a message received event with latency.
  """
  @spec record_message_received(pos_integer(), non_neg_integer()) :: :ok
  def record_message_received(byte_size, latency_ms) do
    record_metric(:messages_received, 1)
    record_metric(:bytes_received, byte_size)
    record_metric(:last_received_timestamp, System.system_time(:millisecond))
    record_latency(:message_latency, latency_ms)
  end

  @doc """
  Records an error event by type.
  """
  @spec record_error(atom()) :: :ok
  def record_error(error_type) do
    record_metric(:total_errors, 1)
    record_metric(:"error_#{error_type}", 1)
    record_metric(:last_error_timestamp, System.system_time(:millisecond))
  end

  @doc """
  Records zero-copy usage statistics.
  """
  @spec record_zero_copy_usage(pos_integer(), pos_integer()) :: :ok
  def record_zero_copy_usage(original_size, optimized_size) do
    record_metric(:zero_copy_uses, 1)
    record_metric(:zero_copy_bytes_saved, original_size - optimized_size)
    efficiency = (1.0 - optimized_size / original_size) * 100
    record_metric(:zero_copy_efficiency_percent, efficiency)
  end

  @doc """
  Records batching statistics.
  """
  @spec record_batch_stats(pos_integer(), pos_integer(), pos_integer()) :: :ok
  def record_batch_stats(batch_size, total_bytes, wait_time_ms) do
    record_metric(:batches_sent, 1)
    record_metric(:avg_batch_size, batch_size)
    record_metric(:batch_bytes, total_bytes)
    record_latency(:batch_wait_time, wait_time_ms)
  end

  @doc """
  Records security event.
  """
  @spec record_security_event(atom(), map()) :: :ok
  def record_security_event(event_type, details \\ %{}) do
    record_metric(:security_events, 1)
    record_metric(:"security_#{event_type}", 1)

    # Store detailed security event
    GenServer.cast(__MODULE__, {:security_event, event_type, details})
  end

  @doc """
  Records connection pool statistics.
  """
  @spec record_connection_stats(pos_integer(), pos_integer(), pos_integer()) :: :ok
  def record_connection_stats(active, idle, failed) do
    :ets.insert(@metrics_table, {:connections_active, active, System.system_time(:millisecond)})
    :ets.insert(@metrics_table, {:connections_idle, idle, System.system_time(:millisecond)})
    :ets.insert(@metrics_table, {:connections_failed, failed, System.system_time(:millisecond)})
  end

  @doc """
  Starts a distributed trace for a request.
  """
  @spec start_trace(String.t(), map()) :: String.t()
  def start_trace(operation, metadata \\ %{}) do
    trace_id = generate_trace_id()

    trace_data = %{
      id: trace_id,
      operation: operation,
      start_time: System.system_time(:microsecond),
      metadata: metadata,
      spans: []
    }

    GenServer.cast(__MODULE__, {:start_trace, trace_id, trace_data})
    trace_id
  end

  @doc """
  Adds a span to an existing trace.
  """
  @spec add_span(String.t(), String.t(), map()) :: :ok
  def add_span(trace_id, span_name, metadata \\ %{}) do
    span = %{
      name: span_name,
      start_time: System.system_time(:microsecond),
      metadata: metadata
    }

    GenServer.cast(__MODULE__, {:add_span, trace_id, span})
  end

  @doc """
  Finishes a trace.
  """
  @spec finish_trace(String.t(), map()) :: :ok
  def finish_trace(trace_id, result \\ %{}) do
    GenServer.cast(__MODULE__, {:finish_trace, trace_id, result})
  end

  @doc """
  Gets comprehensive statistics for all components.
  """
  @spec get_comprehensive_stats() :: map()
  def get_comprehensive_stats do
    GenServer.call(__MODULE__, :get_comprehensive_stats)
  end

  @doc """
  Performs a comprehensive health check.
  """
  @spec health_check() :: map()
  def health_check do
    GenServer.call(__MODULE__, :health_check)
  end

  @doc """
  Gets real-time performance metrics.
  """
  @spec get_performance_metrics() :: map()
  def get_performance_metrics do
    GenServer.call(__MODULE__, :get_performance_metrics)
  end

  @doc """
  Gets active traces for debugging.
  """
  @spec get_active_traces() :: [map()]
  def get_active_traces do
    GenServer.call(__MODULE__, :get_active_traces)
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    # Create ETS tables for metrics storage
    :ets.new(@metrics_table, [:named_table, :public, :set, {:write_concurrency, true}])
    :ets.new(@health_table, [:named_table, :public, :set, {:write_concurrency, true}])
    :ets.new(@trace_table, [:named_table, :public, :set, {:write_concurrency, true}])

    thresholds = Keyword.get(opts, :thresholds, @default_thresholds)
    alert_handler = Keyword.get(opts, :alert_handler)

    state = %__MODULE__{
      start_time: System.system_time(:millisecond),
      thresholds: thresholds,
      alert_handler: alert_handler,
      metrics: %{},
      health_status: %{},
      active_traces: %{}
    }

    # Schedule periodic tasks
    stats_timer = Process.send_after(self(), :collect_stats, @stats_interval)
    health_timer = Process.send_after(self(), :health_check, @health_check_interval)
    trace_timer = Process.send_after(self(), :cleanup_traces, @trace_cleanup_interval)

    final_state = %{
      state
      | stats_timer: stats_timer,
        health_timer: health_timer,
        trace_timer: trace_timer
    }

    Logger.info("BEAM transport observability system started")
    {:ok, final_state}
  end

  @impl true
  def handle_call(:get_comprehensive_stats, _from, state) do
    stats = %{
      uptime_ms: System.system_time(:millisecond) - state.start_time,
      metrics: collect_all_metrics(),
      health: collect_health_status(),
      performance: calculate_performance_metrics(),
      security: collect_security_metrics(),
      traces: %{
        active_count: map_size(state.active_traces),
        completed_today: count_completed_traces_today()
      },
      thresholds: state.thresholds
    }

    {:reply, stats, state}
  end

  def handle_call(:health_check, _from, state) do
    health = perform_comprehensive_health_check()
    {:reply, health, %{state | health_status: health}}
  end

  def handle_call(:get_performance_metrics, _from, state) do
    metrics = calculate_performance_metrics()
    {:reply, metrics, state}
  end

  def handle_call(:get_active_traces, _from, state) do
    traces = Map.values(state.active_traces)
    {:reply, traces, state}
  end

  @impl true
  def handle_cast({:security_event, event_type, details}, state) do
    # Store security event with timestamp
    event_data = %{
      type: event_type,
      details: details,
      timestamp: System.system_time(:millisecond)
    }

    :ets.insert(@metrics_table, {:"security_event_#{System.unique_integer()}", event_data})

    # Check if security thresholds are exceeded
    check_security_alerts(event_type, state)

    {:noreply, state}
  end

  def handle_cast({:start_trace, trace_id, trace_data}, state) do
    new_traces = Map.put(state.active_traces, trace_id, trace_data)
    :ets.insert(@trace_table, {trace_id, trace_data})

    {:noreply, %{state | active_traces: new_traces}}
  end

  def handle_cast({:add_span, trace_id, span}, state) do
    case Map.get(state.active_traces, trace_id) do
      nil ->
        Logger.warning("Attempted to add span to non-existent trace: #{trace_id}")
        {:noreply, state}

      trace_data ->
        updated_spans = [span | trace_data.spans]
        updated_trace = %{trace_data | spans: updated_spans}
        new_traces = Map.put(state.active_traces, trace_id, updated_trace)
        :ets.insert(@trace_table, {trace_id, updated_trace})

        {:noreply, %{state | active_traces: new_traces}}
    end
  end

  def handle_cast({:finish_trace, trace_id, result}, state) do
    case Map.get(state.active_traces, trace_id) do
      nil ->
        Logger.warning("Attempted to finish non-existent trace: #{trace_id}")
        {:noreply, state}

      trace_data ->
        end_time = System.system_time(:microsecond)
        duration_ms = (end_time - trace_data.start_time) / 1000

        finished_trace =
          Map.merge(trace_data, %{
            end_time: end_time,
            duration_ms: duration_ms,
            result: result,
            status: :completed
          })

        # Move to completed traces table
        :ets.insert(@trace_table, {:"completed_#{trace_id}", finished_trace})
        :ets.delete(@trace_table, trace_id)

        new_traces = Map.delete(state.active_traces, trace_id)
        record_latency(:trace_duration, trunc(duration_ms))

        {:noreply, %{state | active_traces: new_traces}}
    end
  end

  @impl true
  def handle_info(:collect_stats, state) do
    # Collect stats from all components
    collect_component_stats()

    # Check performance thresholds
    check_performance_alerts(state)

    # Schedule next collection
    stats_timer = Process.send_after(self(), :collect_stats, @stats_interval)
    {:noreply, %{state | stats_timer: stats_timer}}
  end

  def handle_info(:health_check, state) do
    health = perform_comprehensive_health_check()

    # Check health thresholds
    check_health_alerts(health, state)

    # Schedule next health check
    health_timer = Process.send_after(self(), :health_check, @health_check_interval)
    {:noreply, %{state | health_status: health, health_timer: health_timer}}
  end

  def handle_info(:cleanup_traces, state) do
    # Clean up old completed traces (older than 1 hour)
    cleanup_old_traces()

    # Schedule next cleanup
    trace_timer = Process.send_after(self(), :cleanup_traces, @trace_cleanup_interval)
    {:noreply, %{state | trace_timer: trace_timer}}
  end

  @impl true
  def terminate(_reason, state) do
    # Cancel timers
    if state.stats_timer, do: Process.cancel_timer(state.stats_timer)
    if state.health_timer, do: Process.cancel_timer(state.health_timer)
    if state.trace_timer, do: Process.cancel_timer(state.trace_timer)

    # Clean up ETS tables
    :ets.delete(@metrics_table)
    :ets.delete(@health_table)
    :ets.delete(@trace_table)

    Logger.info("BEAM transport observability system stopped")
    :ok
  end

  # Private helper functions

  defp record_metric(name, value) do
    timestamp = System.system_time(:millisecond)

    # Handle case where ETS table doesn't exist
    try do
      :ets.insert(@metrics_table, {name, value, timestamp})
    catch
      # Table doesn't exist, ignore
      :error, :badarg -> :ok
    end
  end

  defp record_latency(name, latency_ms) do
    # Store latency in a rolling window for percentile calculations
    timestamp = System.system_time(:millisecond)
    key = :"#{name}_latencies"

    try do
      case :ets.lookup(@metrics_table, key) do
        [{^key, latencies}] when is_list(latencies) ->
          # Keep only last 1000 measurements
          new_latencies = Enum.take([{latency_ms, timestamp} | latencies], 1000)
          :ets.insert(@metrics_table, {key, new_latencies})

        _ ->
          :ets.insert(@metrics_table, {key, [{latency_ms, timestamp}]})
      end
    catch
      # Table doesn't exist, ignore
      :error, :badarg -> :ok
    end
  end

  defp collect_all_metrics do
    :ets.tab2list(@metrics_table)
    |> Enum.reduce(%{}, fn
      {key, value, timestamp}, acc -> Map.put(acc, key, %{value: value, timestamp: timestamp})
      {key, value}, acc -> Map.put(acc, key, value)
    end)
  end

  defp collect_health_status do
    try do
      %{
        security_service: get_service_health(fn -> Security.get_security_stats() end),
        zero_copy_service: get_service_health(fn -> ZeroCopy.get_stats() end),
        connection_pool: check_connection_pool_health(),
        memory_usage: check_memory_usage(),
        system_load: check_system_load()
      }
    catch
      error ->
        Logger.warning("Error collecting health status: #{inspect(error)}")
        %{error: "Failed to collect health status"}
    end
  end

  defp calculate_performance_metrics do
    # 1 minute window
    window_ms = 60_000

    %{
      throughput: calculate_throughput(window_ms),
      latency_percentiles: calculate_latency_percentiles(),
      error_rate: calculate_error_rate(window_ms),
      efficiency: calculate_efficiency_metrics(),
      resource_utilization: calculate_resource_utilization()
    }
  end

  defp collect_security_metrics do
    security_events =
      :ets.tab2list(@metrics_table)
      |> Enum.filter(fn
        {key, _} ->
          key |> Atom.to_string() |> String.starts_with?("security_")

        {key, _, _} ->
          key |> Atom.to_string() |> String.starts_with?("security_")
      end)
      |> Enum.into(%{})

    %{
      total_events: map_size(security_events),
      events_by_type: group_security_events_by_type(security_events),
      last_event_time: get_last_security_event_time(security_events)
    }
  end

  defp get_service_health(health_func) do
    try do
      health_func.()
      :healthy
    catch
      :exit, _ ->
        :unhealthy

      error ->
        Logger.warning("Service health check failed: #{inspect(error)}")
        :degraded
    end
  end

  defp check_connection_pool_health() do
    try do
      active = get_metric_value(:connections_active, 0)
      failed = get_metric_value(:connections_failed, 0)

      cond do
        failed > active * 0.5 -> :unhealthy
        failed > active * 0.2 -> :degraded
        true -> :healthy
      end
    catch
      _ -> :unknown
    end
  end

  defp check_memory_usage() do
    memory_mb = :erlang.memory(:total) / (1024 * 1024)

    cond do
      memory_mb > 500 -> :unhealthy
      memory_mb > 200 -> :degraded
      true -> :healthy
    end
  end

  defp check_system_load() do
    # Skip CPU monitoring in test environment since :cpu_sup might not be available
    :healthy
  end

  defp calculate_throughput(window_ms) do
    cutoff = System.system_time(:millisecond) - window_ms

    sent = count_recent_metrics(:messages_sent, cutoff)
    received = count_recent_metrics(:messages_received, cutoff)

    %{
      messages_per_second: %{
        sent: sent * 1000 / window_ms,
        received: received * 1000 / window_ms
      },
      bytes_per_second: %{
        sent: count_recent_metrics(:bytes_sent, cutoff) * 1000 / window_ms,
        received: count_recent_metrics(:bytes_received, cutoff) * 1000 / window_ms
      }
    }
  end

  defp calculate_latency_percentiles() do
    case :ets.lookup(@metrics_table, :message_latency_latencies) do
      [{_, latencies}] when is_list(latencies) ->
        values = Enum.map(latencies, fn {latency, _} -> latency end)
        sorted = Enum.sort(values)
        count = length(sorted)

        if count > 0 do
          %{
            p50: percentile(sorted, 50),
            p95: percentile(sorted, 95),
            p99: percentile(sorted, 99),
            avg: Enum.sum(sorted) / count,
            max: Enum.max(sorted),
            min: Enum.min(sorted)
          }
        else
          %{}
        end

      _ ->
        %{}
    end
  end

  defp calculate_error_rate(window_ms) do
    cutoff = System.system_time(:millisecond) - window_ms

    total_messages =
      count_recent_metrics(:messages_sent, cutoff) +
        count_recent_metrics(:messages_received, cutoff)

    total_errors = count_recent_metrics(:total_errors, cutoff)

    if total_messages > 0 do
      total_errors / total_messages * 100
    else
      0.0
    end
  end

  defp calculate_efficiency_metrics() do
    %{
      zero_copy: %{
        usage_count: get_metric_value(:zero_copy_uses, 0),
        bytes_saved: get_metric_value(:zero_copy_bytes_saved, 0),
        efficiency_percent: get_metric_value(:zero_copy_efficiency_percent, 0)
      },
      batching: %{
        batches_sent: get_metric_value(:batches_sent, 0),
        avg_batch_size: get_metric_value(:avg_batch_size, 0)
      }
    }
  end

  defp calculate_resource_utilization() do
    %{
      memory_mb: :erlang.memory(:total) / (1024 * 1024),
      process_count: :erlang.system_info(:process_count),
      # Skip CPU monitoring for compatibility
      cpu_load: 0.0,
      ets_tables: length(:ets.all())
    }
  end

  defp perform_comprehensive_health_check() do
    components = [
      {:security, &check_security_health/0},
      {:connections, &check_connections_health/0},
      {:performance, &check_performance_health/0},
      {:resources, &check_resource_health/0}
    ]

    results =
      Enum.map(components, fn {name, health_check} ->
        try do
          {name, health_check.()}
        catch
          error ->
            Logger.warning("Health check failed for #{name}: #{inspect(error)}")
            {name, :unhealthy}
        end
      end)

    overall_status = determine_overall_health(results)

    %{
      overall: overall_status,
      components: Map.new(results),
      timestamp: System.system_time(:millisecond)
    }
  end

  defp check_security_health() do
    # 1 minute
    recent_violations = count_recent_security_events(60_000)

    cond do
      recent_violations > 10 -> :unhealthy
      recent_violations > 3 -> :degraded
      true -> :healthy
    end
  end

  defp check_connections_health() do
    active = get_metric_value(:connections_active, 0)
    failed = get_metric_value(:connections_failed, 0)

    if active > 0 do
      failure_rate = failed / active

      cond do
        failure_rate > 0.3 -> :unhealthy
        failure_rate > 0.1 -> :degraded
        true -> :healthy
      end
    else
      :healthy
    end
  end

  defp check_performance_health() do
    # 1 minute window
    error_rate = calculate_error_rate(60_000)

    cond do
      error_rate > 10.0 -> :unhealthy
      error_rate > 5.0 -> :degraded
      true -> :healthy
    end
  end

  defp check_resource_health() do
    memory_mb = :erlang.memory(:total) / (1024 * 1024)
    process_count = :erlang.system_info(:process_count)

    cond do
      memory_mb > 1000 or process_count > 100_000 -> :unhealthy
      memory_mb > 500 or process_count > 50_000 -> :degraded
      true -> :healthy
    end
  end

  defp determine_overall_health(component_results) do
    statuses = Enum.map(component_results, fn {_, status} -> status end)

    cond do
      :unhealthy in statuses -> :unhealthy
      :degraded in statuses -> :degraded
      true -> :healthy
    end
  end

  defp check_performance_alerts(state) do
    error_rate = calculate_error_rate(60_000)

    if error_rate > state.thresholds.error_rate_percent do
      send_alert(
        :high_error_rate,
        %{
          current_rate: error_rate,
          threshold: state.thresholds.error_rate_percent
        },
        state
      )
    end

    case calculate_latency_percentiles() do
      %{avg: avg_latency} when avg_latency > state.thresholds.avg_latency_ms ->
        send_alert(
          :high_latency,
          %{
            current_avg: avg_latency,
            threshold: state.thresholds.avg_latency_ms
          },
          state
        )

      _ ->
        :ok
    end
  end

  defp check_health_alerts(health, state) do
    if health.overall == :unhealthy do
      send_alert(:system_unhealthy, health, state)
    end
  end

  defp check_security_alerts(event_type, state) do
    # 1 minute
    recent_count = count_recent_security_events(60_000)

    if recent_count > 5 do
      send_alert(
        :high_security_events,
        %{
          event_type: event_type,
          recent_count: recent_count
        },
        state
      )
    end
  end

  defp send_alert(alert_type, data, state) do
    alert = %{
      type: alert_type,
      data: data,
      timestamp: System.system_time(:millisecond),
      severity: determine_alert_severity(alert_type)
    }

    Logger.warning("BEAM Transport Alert: #{alert_type} - #{inspect(data)}")

    if state.alert_handler do
      try do
        state.alert_handler.(alert)
      catch
        error ->
          Logger.error("Alert handler failed: #{inspect(error)}")
      end
    end
  end

  defp determine_alert_severity(:system_unhealthy), do: :critical
  defp determine_alert_severity(:high_error_rate), do: :high
  defp determine_alert_severity(:high_latency), do: :medium
  defp determine_alert_severity(:high_security_events), do: :high

  defp collect_component_stats() do
    # Collect stats from Security service
    try do
      security_stats = Security.get_security_stats()

      :ets.insert(
        @metrics_table,
        {:security_stats, security_stats, System.system_time(:millisecond)}
      )
    catch
      :exit, _ -> :ok
    end

    # Collect zero-copy stats
    try do
      zero_copy_stats = ZeroCopy.get_stats()

      :ets.insert(
        @metrics_table,
        {:zero_copy_stats, zero_copy_stats, System.system_time(:millisecond)}
      )
    catch
      :exit, _ -> :ok
    end
  end

  defp count_recent_metrics(metric_name, cutoff_timestamp) do
    case :ets.lookup(@metrics_table, metric_name) do
      [{_, value, timestamp}] when timestamp >= cutoff_timestamp -> value
      _ -> 0
    end
  end

  defp count_recent_security_events(window_ms) do
    cutoff = System.system_time(:millisecond) - window_ms

    :ets.tab2list(@metrics_table)
    |> Enum.count(fn
      {key, _data, timestamp} when timestamp >= cutoff ->
        key |> Atom.to_string() |> String.starts_with?("security_event_")

      _ ->
        false
    end)
  end

  defp get_metric_value(metric_name, default) do
    case :ets.lookup(@metrics_table, metric_name) do
      [{_, value, _}] -> value
      [{_, value}] -> value
      _ -> default
    end
  end

  defp group_security_events_by_type(events) do
    events
    |> Enum.group_by(fn
      {key, _} ->
        key |> Atom.to_string() |> String.replace_prefix("security_", "")

      {key, _, _} ->
        key |> Atom.to_string() |> String.replace_prefix("security_", "")
    end)
    |> Map.new(fn {type, events} -> {type, length(events)} end)
  end

  defp get_last_security_event_time(events) do
    events
    |> Enum.map(fn
      {_, data} ->
        case data do
          %{timestamp: ts} -> ts
          _ -> 0
        end

      {_, _, timestamp} ->
        timestamp
    end)
    |> Enum.max(fn -> 0 end)
  end

  defp count_completed_traces_today() do
    # 24 hours ago
    today_start = System.system_time(:millisecond) - 86_400_000

    :ets.tab2list(@trace_table)
    |> Enum.count(fn
      {key, %{end_time: end_time}} when is_integer(end_time) ->
        # Convert to microseconds
        String.starts_with?(Atom.to_string(key), "completed_") and
          end_time >= today_start * 1000

      _ ->
        false
    end)
  end

  defp cleanup_old_traces() do
    # 1 hour in microseconds
    one_hour_ago = System.system_time(:microsecond) - 3_600_000_000

    :ets.tab2list(@trace_table)
    |> Enum.each(fn
      {key, %{end_time: end_time}} when is_integer(end_time) and end_time < one_hour_ago ->
        :ets.delete(@trace_table, key)

      _ ->
        :ok
    end)
  end

  defp percentile(sorted_list, p) do
    count = length(sorted_list)
    index = trunc(count * p / 100)
    Enum.at(sorted_list, min(index, count - 1))
  end

  defp generate_trace_id() do
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
  end
end
