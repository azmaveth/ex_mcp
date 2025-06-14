defmodule ExMCP.Transport.Beam.ObservabilityTest do
  @moduledoc """
  Tests for the BEAM transport observability and metrics system.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Transport.Beam.Observability

  describe "observability service lifecycle" do
    test "starts and stops correctly" do
      {:ok, pid} = Observability.start_link()
      assert Process.alive?(pid)

      GenServer.stop(pid)
      refute Process.alive?(pid)
    end

    test "accepts custom thresholds and alert handler" do
      alert_handler = fn alert ->
        send(self(), {:alert, alert})
        :ok
      end

      custom_thresholds = %{
        error_rate_percent: 2.0,
        avg_latency_ms: 500
      }

      {:ok, pid} =
        Observability.start_link(
          thresholds: custom_thresholds,
          alert_handler: alert_handler
        )

      # Get comprehensive stats to verify configuration
      stats = Observability.get_comprehensive_stats()
      assert stats.thresholds.error_rate_percent == 2.0
      assert stats.thresholds.avg_latency_ms == 500

      GenServer.stop(pid)
    end
  end

  describe "metric recording" do
    setup do
      {:ok, pid} = Observability.start_link()

      on_exit(fn ->
        if Process.alive?(pid) do
          GenServer.stop(pid)
        end
      end)

      %{pid: pid}
    end

    test "records message sent metrics" do
      Observability.record_message_sent(1024)

      stats = Observability.get_comprehensive_stats()

      assert stats.metrics[:messages_sent][:value] == 1
      assert stats.metrics[:bytes_sent][:value] == 1024
      assert is_integer(stats.metrics[:last_sent_timestamp][:value])
    end

    test "records message received metrics with latency" do
      Observability.record_message_received(512, 50)

      stats = Observability.get_comprehensive_stats()

      assert stats.metrics[:messages_received][:value] == 1
      assert stats.metrics[:bytes_received][:value] == 512
      assert is_integer(stats.metrics[:last_received_timestamp][:value])

      # Check that latency was recorded
      assert is_list(stats.metrics[:message_latency_latencies])
    end

    test "records error events by type" do
      Observability.record_error(:connection_failed)
      Observability.record_error(:timeout)
      Observability.record_error(:connection_failed)

      stats = Observability.get_comprehensive_stats()

      assert stats.metrics[:total_errors][:value] == 3
      assert stats.metrics[:error_connection_failed][:value] == 2
      assert stats.metrics[:error_timeout][:value] == 1
      assert is_integer(stats.metrics[:last_error_timestamp][:value])
    end

    test "records zero-copy usage statistics" do
      Observability.record_zero_copy_usage(10000, 6000)

      stats = Observability.get_comprehensive_stats()

      assert stats.metrics[:zero_copy_uses][:value] == 1
      assert stats.metrics[:zero_copy_bytes_saved][:value] == 4000
      assert stats.metrics[:zero_copy_efficiency_percent][:value] == 40.0
    end

    test "records batching statistics" do
      Observability.record_batch_stats(25, 51200, 10)

      stats = Observability.get_comprehensive_stats()

      assert stats.metrics[:batches_sent][:value] == 1
      assert stats.metrics[:avg_batch_size][:value] == 25
      assert stats.metrics[:batch_bytes][:value] == 51200
      assert is_list(stats.metrics[:batch_wait_time_latencies])
    end

    test "records security events" do
      Observability.record_security_event(:rate_limit_exceeded, %{client_ip: "127.0.0.1"})
      Observability.record_security_event(:invalid_auth, %{token: "xxx"})

      stats = Observability.get_comprehensive_stats()

      assert stats.metrics[:security_events][:value] == 2
      assert stats.metrics[:security_rate_limit_exceeded][:value] == 1
      assert stats.metrics[:security_invalid_auth][:value] == 1
    end

    test "records connection pool statistics" do
      Observability.record_connection_stats(5, 3, 1)

      stats = Observability.get_comprehensive_stats()

      assert stats.metrics[:connections_active][:value] == 5
      assert stats.metrics[:connections_idle][:value] == 3
      assert stats.metrics[:connections_failed][:value] == 1
    end
  end

  describe "distributed tracing" do
    setup do
      {:ok, pid} = Observability.start_link()

      on_exit(fn ->
        if Process.alive?(pid) do
          GenServer.stop(pid)
        end
      end)

      %{pid: pid}
    end

    test "creates and manages traces" do
      trace_id = Observability.start_trace("test_operation", %{user_id: "123"})

      assert is_binary(trace_id)
      # 16 bytes encoded as hex
      assert byte_size(trace_id) == 32

      # Verify trace is active
      active_traces = Observability.get_active_traces()
      assert length(active_traces) == 1

      trace = hd(active_traces)
      assert trace.id == trace_id
      assert trace.operation == "test_operation"
      assert trace.metadata.user_id == "123"
      assert is_integer(trace.start_time)
    end

    test "adds spans to traces" do
      trace_id = Observability.start_trace("complex_operation")

      Observability.add_span(trace_id, "database_query", %{table: "users"})
      Observability.add_span(trace_id, "api_call", %{endpoint: "/api/data"})

      active_traces = Observability.get_active_traces()
      trace = Enum.find(active_traces, &(&1.id == trace_id))

      assert length(trace.spans) == 2

      span_names = Enum.map(trace.spans, & &1.name)
      assert "database_query" in span_names
      assert "api_call" in span_names
    end

    test "finishes traces and calculates duration" do
      trace_id = Observability.start_trace("timed_operation")

      # Simulate some work
      Process.sleep(10)

      Observability.finish_trace(trace_id, %{status: "success", records: 42})

      # Trace should no longer be active
      active_traces = Observability.get_active_traces()
      assert Enum.find(active_traces, &(&1.id == trace_id)) == nil

      # Should be recorded in stats
      stats = Observability.get_comprehensive_stats()
      assert stats.traces.completed_today >= 1
    end

    test "handles adding spans to non-existent traces gracefully" do
      # This should not crash
      Observability.add_span("non-existent-trace", "some_span")

      # Should not affect active traces
      active_traces = Observability.get_active_traces()
      assert Enum.empty?(active_traces)
    end
  end

  describe "performance metrics calculation" do
    setup do
      {:ok, pid} = Observability.start_link()

      on_exit(fn ->
        if Process.alive?(pid) do
          GenServer.stop(pid)
        end
      end)

      %{pid: pid}
    end

    test "calculates throughput metrics" do
      # Record some traffic
      Observability.record_message_sent(1024)
      Observability.record_message_sent(2048)
      Observability.record_message_received(512, 25)

      metrics = Observability.get_performance_metrics()

      assert is_map(metrics.throughput)
      assert is_map(metrics.throughput.messages_per_second)
      assert is_map(metrics.throughput.bytes_per_second)

      assert metrics.throughput.messages_per_second.sent >= 0
      assert metrics.throughput.messages_per_second.received >= 0
      assert metrics.throughput.bytes_per_second.sent >= 0
      assert metrics.throughput.bytes_per_second.received >= 0
    end

    test "calculates latency percentiles" do
      # Record various latencies
      latencies = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

      Enum.each(latencies, fn latency ->
        Observability.record_message_received(100, latency)
      end)

      metrics = Observability.get_performance_metrics()
      percentiles = metrics.latency_percentiles

      assert is_map(percentiles)
      # Should be around median
      assert percentiles.p50 >= 50
      # Should be high
      assert percentiles.p95 >= 90
      # Should be very high
      assert percentiles.p99 >= 90
      # Average of our test data
      assert percentiles.avg == 55.0
      assert percentiles.min == 10
      assert percentiles.max == 100
    end

    test "calculates error rate" do
      # Record some messages and errors
      Observability.record_message_sent(1000)
      Observability.record_message_received(800, 25)

      Observability.record_error(:network_error)
      Observability.record_error(:timeout)

      metrics = Observability.get_performance_metrics()

      # Error rate should be > 0 since we recorded errors
      assert metrics.error_rate > 0
      # Should be a percentage
      assert metrics.error_rate <= 100
    end

    test "calculates efficiency metrics" do
      # Record zero-copy and batching usage
      Observability.record_zero_copy_usage(20000, 12000)
      Observability.record_batch_stats(30, 15000, 5)

      metrics = Observability.get_performance_metrics()
      efficiency = metrics.efficiency

      assert efficiency.zero_copy.usage_count == 1
      assert efficiency.zero_copy.bytes_saved == 8000
      assert efficiency.zero_copy.efficiency_percent == 40.0

      assert efficiency.batching.batches_sent == 1
      assert efficiency.batching.avg_batch_size == 30
    end

    test "calculates resource utilization" do
      metrics = Observability.get_performance_metrics()
      utilization = metrics.resource_utilization

      assert is_float(utilization.memory_mb)
      assert utilization.memory_mb > 0

      assert is_integer(utilization.process_count)
      assert utilization.process_count > 0

      assert is_float(utilization.cpu_load)
      assert utilization.cpu_load >= 0

      assert is_integer(utilization.ets_tables)
      assert utilization.ets_tables > 0
    end
  end

  describe "health monitoring" do
    setup do
      {:ok, pid} = Observability.start_link()

      on_exit(fn ->
        if Process.alive?(pid) do
          GenServer.stop(pid)
        end
      end)

      %{pid: pid}
    end

    test "performs comprehensive health check" do
      health = Observability.health_check()

      assert is_map(health)
      assert health.overall in [:healthy, :degraded, :unhealthy]
      assert is_map(health.components)
      assert is_integer(health.timestamp)

      # Check that all expected components are monitored
      expected_components = [:security, :connections, :performance, :resources]

      for component <- expected_components do
        assert Map.has_key?(health.components, component)
        assert health.components[component] in [:healthy, :degraded, :unhealthy]
      end
    end

    test "returns healthy status under normal conditions" do
      health = Observability.health_check()

      # Under test conditions, most components should be healthy
      assert health.overall in [:healthy, :degraded]
    end

    test "detects performance issues" do
      # Simulate high error rate
      Enum.each(1..20, fn _ ->
        Observability.record_error(:critical_error)
      end)

      # Record minimal successful messages
      Observability.record_message_sent(100)

      health = Observability.health_check()

      # Performance component should detect the high error rate
      assert health.components.performance in [:degraded, :unhealthy]
    end
  end

  describe "alerting system" do
    test "triggers alerts when thresholds are exceeded" do
      # Set up alert handler to capture alerts
      test_pid = self()

      alert_handler = fn alert ->
        send(test_pid, {:alert_received, alert})
        :ok
      end

      # Start with low thresholds to trigger alerts easily
      {:ok, pid} =
        Observability.start_link(
          thresholds: %{
            # Very low threshold
            error_rate_percent: 1.0,
            # Very low threshold
            avg_latency_ms: 10
          },
          alert_handler: alert_handler
        )

      # Generate high error rate
      Observability.record_message_sent(100)
      Observability.record_error(:test_error)
      Observability.record_error(:test_error)

      # Generate high latency
      # 500ms latency
      Observability.record_message_received(100, 500)

      # Trigger performance check (this is normally done periodically)
      send(pid, :collect_stats)

      # Give some time for async processing
      Process.sleep(100)

      # Should receive alerts
      assert_receive {:alert_received, %{type: :high_error_rate}}, 1000

      GenServer.stop(pid)
    end

    test "security alerts are triggered for high event rates" do
      test_pid = self()

      alert_handler = fn alert ->
        send(test_pid, {:alert_received, alert})
        :ok
      end

      {:ok, pid} = Observability.start_link(alert_handler: alert_handler)

      # Generate many security events quickly (more than threshold of 5)
      Enum.each(1..8, fn _ ->
        Observability.record_security_event(:rate_limit_exceeded)
        # Small delay to ensure events are properly processed
        Process.sleep(1)
      end)

      # Wait for alert processing
      Process.sleep(200)

      # Should receive security alert
      assert_receive {:alert_received, %{type: :high_security_events}}, 2000

      GenServer.stop(pid)
    end
  end

  describe "comprehensive statistics" do
    setup do
      {:ok, pid} = Observability.start_link()

      on_exit(fn ->
        if Process.alive?(pid) do
          GenServer.stop(pid)
        end
      end)

      %{pid: pid}
    end

    test "provides complete system overview" do
      # Small delay to ensure uptime is measurable
      Process.sleep(1)

      # Generate some activity
      Observability.record_message_sent(1024)
      Observability.record_message_received(512, 25)
      Observability.record_error(:network_error)
      Observability.record_zero_copy_usage(5000, 3000)
      Observability.record_batch_stats(10, 2048, 5)
      Observability.record_security_event(:auth_failed)

      trace_id = Observability.start_trace("test_operation")
      Observability.finish_trace(trace_id)

      stats = Observability.get_comprehensive_stats()

      # Verify all major sections are present
      assert is_integer(stats.uptime_ms)
      assert stats.uptime_ms > 0

      assert is_map(stats.metrics)
      assert is_map(stats.health)
      assert is_map(stats.performance)
      assert is_map(stats.security)
      assert is_map(stats.traces)
      assert is_map(stats.thresholds)

      # Verify metrics contain our recorded data
      assert stats.metrics[:messages_sent][:value] == 1
      assert stats.metrics[:bytes_sent][:value] == 1024
      assert stats.metrics[:total_errors][:value] == 1
      assert stats.metrics[:zero_copy_uses][:value] == 1
      assert stats.metrics[:batches_sent][:value] == 1
      assert stats.metrics[:security_events][:value] == 1

      # Verify traces section
      assert stats.traces.completed_today >= 1
      assert is_integer(stats.traces.active_count)

      # Verify security section (may have events from concurrent tests)
      assert stats.security.total_events >= 1
      assert is_map(stats.security.events_by_type)
    end

    test "tracks system uptime correctly" do
      start_time = System.system_time(:millisecond)

      # Wait a bit
      Process.sleep(50)

      stats = Observability.get_comprehensive_stats()

      assert stats.uptime_ms >= 50
      assert stats.uptime_ms <= System.system_time(:millisecond) - start_time + 100
    end
  end
end
