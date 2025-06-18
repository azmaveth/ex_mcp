#!/usr/bin/env elixir

# Example: Reliability-Enhanced MCP Client
# 
# This example demonstrates how to use ExMCP's reliability features:
# - Circuit breaker for fault tolerance
# - Retry logic with exponential backoff
# - Health monitoring
#
# Usage: elixir reliable_client.exs

Mix.install([
  {:ex_mcp, path: Path.expand("../../..", __DIR__)}
])

defmodule ReliableClientExample do
  @moduledoc """
  Example of using ExMCP with reliability features.
  """
  
  alias ExMCP.Reliability
  alias ExMCP.Reliability.{CircuitBreaker, HealthCheck, Retry}
  
  def run do
    IO.puts("🛡️  ExMCP Reliability Example\n")
    
    # Start the reliability supervisor
    {:ok, _sup} = Reliability.Supervisor.start_link(name: MyApp.Reliability)
    
    demo_circuit_breaker()
    IO.puts("\n" <> String.duplicate("-", 50) <> "\n")
    
    demo_retry_logic()
    IO.puts("\n" <> String.duplicate("-", 50) <> "\n")
    
    demo_health_checks()
    IO.puts("\n" <> String.duplicate("-", 50) <> "\n")
    
    demo_integrated_client()
  end
  
  defp demo_circuit_breaker do
    IO.puts("1️⃣  Circuit Breaker Demo")
    IO.puts("   Protecting against cascading failures\n")
    
    # Create a circuit breaker
    {:ok, breaker} = CircuitBreaker.start_link(
      name: :demo_breaker,
      failure_threshold: 3,
      reset_timeout: 2000
    )
    
    # Simulate a flaky service
    flaky_service = fn ->
      if :rand.uniform() > 0.7 do
        {:ok, "Success!"}
      else
        {:error, "Service unavailable"}
      end
    end
    
    # Make several calls
    for i <- 1..10 do
      result = CircuitBreaker.call(breaker, flaky_service)
      
      case result do
        {:ok, msg} ->
          IO.puts("   ✅ Call #{i}: #{msg}")
          
        {:error, :circuit_open} ->
          IO.puts("   🚫 Call #{i}: Circuit open (failing fast)")
          
        {:error, reason} ->
          IO.puts("   ❌ Call #{i}: Failed - #{reason}")
      end
      
      Process.sleep(300)
    end
    
    # Check final stats
    stats = CircuitBreaker.get_stats(breaker)
    IO.puts("\n   📊 Circuit Breaker Stats:")
    IO.puts("      Total calls: #{stats.total_calls}")
    IO.puts("      Successful: #{stats.successful_calls}")
    IO.puts("      Failed: #{stats.failed_calls}")
    IO.puts("      Rejected: #{stats.rejected_calls}")
  end
  
  defp demo_retry_logic do
    IO.puts("2️⃣  Retry Logic Demo")
    IO.puts("   Handling transient failures with exponential backoff\n")
    
    # Counter to track attempts
    {:ok, attempt_counter} = Agent.start_link(fn -> 0 end)
    
    # Simulate a service that fails initially but succeeds after 3 attempts
    unstable_service = fn ->
      attempt = Agent.get_and_update(attempt_counter, &{&1 + 1, &1 + 1})
      
      if attempt < 3 do
        IO.puts("   ⏳ Attempt #{attempt}: Failing...")
        {:error, :temporary_failure}
      else
        IO.puts("   ✨ Attempt #{attempt}: Success!")
        {:ok, "Data retrieved successfully"}
      end
    end
    
    # Execute with retry
    IO.puts("   Starting operation with retry logic...")
    start_time = System.monotonic_time(:millisecond)
    
    result = Retry.with_retry(
      unstable_service,
      max_attempts: 5,
      initial_delay: 200,
      backoff_factor: 2,
      on_retry: fn attempt, error ->
        IO.puts("   🔄 Retrying after #{inspect(error)}... (attempt #{attempt})")
      end
    )
    
    duration = System.monotonic_time(:millisecond) - start_time
    
    case result do
      {:ok, data} ->
        IO.puts("\n   ✅ Final result: #{data}")
        IO.puts("   ⏱️  Total time: #{duration}ms")
        
      {:error, reason} ->
        IO.puts("\n   ❌ Failed after all retries: #{inspect(reason)}")
    end
  end
  
  defp demo_health_checks do
    IO.puts("3️⃣  Health Check Demo")
    IO.puts("   Monitoring service health proactively\n")
    
    # Create a mock service that alternates between healthy and unhealthy
    {:ok, service} = Agent.start_link(fn -> %{healthy: true, calls: 0} end)
    
    check_fn = fn target ->
      state = Agent.get_and_update(target, fn state ->
        new_state = %{
          healthy: rem(state.calls, 4) != 3,  # Fail every 4th call
          calls: state.calls + 1
        }
        {new_state, new_state}
      end)
      
      if state.healthy do
        {:ok, %{response_time: :rand.uniform(50), status: "OK"}}
      else
        {:error, :service_error}
      end
    end
    
    # Start health checker
    {:ok, health_checker} = HealthCheck.start_link(
      name: :demo_health,
      target: service,
      check_fn: check_fn,
      check_interval: 500,
      failure_threshold: 2,
      recovery_threshold: 2,
      on_status_change: fn old, new ->
        IO.puts("   🔔 Health status changed: #{old} → #{new}")
      end
    )
    
    # Subscribe to health events
    HealthCheck.subscribe(health_checker)
    
    IO.puts("   Monitoring health for 5 seconds...")
    
    # Monitor for a while
    for i <- 1..10 do
      Process.sleep(500)
      
      {status, info} = HealthCheck.get_status(health_checker)
      
      status_icon = case status do
        :healthy -> "💚"
        :unhealthy -> "💔"
        :unknown -> "❓"
      end
      
      IO.puts("   #{status_icon} Check #{i}: #{status} " <>
              "(failures: #{info.consecutive_failures}, " <>
              "successes: #{info.consecutive_successes})")
    end
    
    # Show history
    history = HealthCheck.get_history(health_checker, 5)
    IO.puts("\n   📜 Recent History:")
    for {check, i} <- Enum.with_index(history, 1) do
      IO.puts("      #{i}. #{check.status} (#{check.duration_ms}ms)")
    end
  end
  
  defp demo_integrated_client do
    IO.puts("4️⃣  Integrated Reliable Client Demo")
    IO.puts("   Complete reliability stack for MCP client\n")
    
    # Note: This would normally connect to a real MCP server
    # For demo purposes, we'll show the configuration
    
    config = [
      transport: [type: :stdio, command: "echo-server"],
      circuit_breaker: [
        failure_threshold: 5,
        success_threshold: 2,
        reset_timeout: 30_000
      ],
      retry: [
        max_attempts: 3,
        initial_delay: 500,
        backoff_factor: 2
      ],
      health_check: [
        check_interval: 60_000,
        failure_threshold: 3,
        recovery_threshold: 2
      ]
    ]
    
    IO.puts("   Configuration for reliable client:")
    IO.puts("   " <> inspect(config, pretty: true, width: 60))
    
    IO.puts("\n   With this configuration:")
    IO.puts("   • Circuit breaker prevents cascading failures")
    IO.puts("   • Automatic retry handles transient errors")
    IO.puts("   • Health checks detect issues proactively")
    IO.puts("   • All features work together seamlessly")
    
    # Example usage (commented out as it needs a real server)
    # {:ok, client} = Reliability.Supervisor.create_reliable_client(config)
    # {:ok, result} = GenServer.call(client, {:call_tool, "example", %{}})
  end
end

# Run the example
ReliableClientExample.run()