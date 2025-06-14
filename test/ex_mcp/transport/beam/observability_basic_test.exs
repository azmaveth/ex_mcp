defmodule ExMCP.Transport.Beam.ObservabilityBasicTest do
  @moduledoc """
  Basic tests for the BEAM transport observability system.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Transport.Beam.Observability

  test "starts and stops correctly" do
    {:ok, pid} = Observability.start_link()
    assert Process.alive?(pid)

    GenServer.stop(pid)
    refute Process.alive?(pid)
  end

  test "records basic metrics without crashing" do
    {:ok, pid} = Observability.start_link()

    # These should not crash
    Observability.record_message_sent(1024)
    Observability.record_message_received(512, 25)
    Observability.record_error(:test_error)

    # Give some time for async processing
    Process.sleep(10)

    # Should be able to get stats without crashing
    stats = Observability.get_comprehensive_stats()
    assert is_map(stats)
    assert is_integer(stats.uptime_ms)

    GenServer.stop(pid)
  end

  test "basic tracing functionality" do
    {:ok, pid} = Observability.start_link()

    trace_id = Observability.start_trace("test_operation")
    assert is_binary(trace_id)

    active_traces = Observability.get_active_traces()
    assert length(active_traces) == 1

    Observability.finish_trace(trace_id)

    # Should no longer be active
    active_traces = Observability.get_active_traces()
    assert Enum.empty?(active_traces)

    GenServer.stop(pid)
  end

  test "health check returns expected structure" do
    {:ok, pid} = Observability.start_link()

    health = Observability.health_check()

    assert is_map(health)
    assert health.overall in [:healthy, :degraded, :unhealthy]
    assert is_map(health.components)
    assert is_integer(health.timestamp)

    GenServer.stop(pid)
  end
end
