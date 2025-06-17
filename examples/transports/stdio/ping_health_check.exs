# Ping and Health Check Example
#
# This example demonstrates how to use the MCP ping functionality
# for connection health monitoring and verification.
#
# Run with: mix run examples/ping_health_check.exs

alias ExMCP.{Client, Server}
alias ExMCP.Server.Handler
alias ExMCP.Client.Handler, as: ClientHandler

defmodule HealthMonitorHandler do
  @behaviour Handler

  @impl true
  def init(_args) do
    {:ok, %{
      start_time: System.system_time(:second),
      request_count: 0,
      last_ping: nil
    }}
  end

  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      name: "health-monitor-server",
      version: "1.0.0",
      capabilities: %{
        tools: %{}
      }
    }, state}
  end

  @impl true
  def handle_list_tools(_params, state) do
    _uptime = System.system_time(:second) - state.start_time
    
    tools = [
      %{
        name: "get_server_stats",
        description: "Get server statistics including uptime",
        input_schema: %{
          type: "object",
          properties: %{}
        }
      }
    ]
    
    {:ok, tools, %{state | request_count: state.request_count + 1}}
  end

  @impl true
  def handle_call_tool("get_server_stats", _params, state) do
    uptime = System.system_time(:second) - state.start_time
    
    stats = %{
      uptime_seconds: uptime,
      request_count: state.request_count + 1,
      last_ping: state.last_ping || "never"
    }
    
    result = [%{
      type: "text",
      text: "Server Stats:\n" <>
            "- Uptime: #{uptime} seconds\n" <>
            "- Requests handled: #{stats.request_count}\n" <>
            "- Last ping: #{stats.last_ping}"
    }]
    
    {:ok, result, %{state | request_count: state.request_count + 1}}
  end

  @impl true
  def terminate(_reason, _state) do
    :ok
  end
end

defmodule HealthCheckClientHandler do
  @behaviour ClientHandler

  @impl true
  def init(args) do
    {:ok, Map.merge(%{ping_count: 0}, args)}
  end

  @impl true
  def handle_ping(state) do
    new_count = state.ping_count + 1
    IO.puts("📥 Received ping ##{new_count} from server")
    {:ok, %{}, %{state | ping_count: new_count}}
  end

  @impl true
  def handle_list_roots(state) do
    {:ok, [], state}
  end

  @impl true
  def handle_create_message(_params, state) do
    {:ok, %{content: %{type: "text", text: "pong"}}, state}
  end

  @impl true
  def terminate(_reason, _state) do
    :ok
  end
end

IO.puts("🏓 MCP Ping and Health Check Example")
IO.puts("=" <> String.duplicate("=", 36))

# Start the server
IO.puts("\n1. Starting MCP server with health monitoring...")
{:ok, server} = Server.start_link(
  transport: :stdio,
  name: :health_server,
  handler: HealthMonitorHandler
)

# Start the client with handler for bidirectional pings
{:ok, client} = Client.start_link(
  transport: :stdio,
  server: :health_server,
  handler: HealthCheckClientHandler,
  handler_state: %{}
)

# Wait for initialization
Process.sleep(100)
IO.puts("✓ Server and client initialized")

# Example 1: Basic client-to-server ping
IO.puts("\n2. Basic Client-to-Server Ping")
IO.puts("   Sending ping to verify connection...")

case Client.ping(client) do
  {:ok, %{}} ->
    IO.puts("   ✓ Ping successful - connection is healthy")
  {:error, reason} ->
    IO.puts("   ✗ Ping failed: #{inspect(reason)}")
end

# Example 2: Multiple pings with timing
IO.puts("\n3. Multiple Pings with Response Time Measurement")
IO.puts("   Sending 5 pings with timing...")

for i <- 1..5 do
  start_time = System.monotonic_time(:microsecond)
  
  result = Client.ping(client)
  
  end_time = System.monotonic_time(:microsecond)
  elapsed = end_time - start_time
  
  case result do
    {:ok, %{}} ->
      IO.puts("   → Ping #{i}: ✓ (#{elapsed} μs)")
    {:error, reason} ->
      IO.puts("   → Ping #{i}: ✗ #{inspect(reason)}")
  end
  
  Process.sleep(200)
end

# Example 3: Server-to-client ping
IO.puts("\n4. Server-to-Client Ping (Bidirectional)")
IO.puts("   Server will ping client...")

case Server.ping(server) do
  {:ok, %{}} ->
    IO.puts("   ✓ Server successfully pinged client")
  {:error, error} ->
    IO.puts("   ✗ Server ping failed: #{inspect(error)}")
end

# Example 4: Concurrent bidirectional pings
IO.puts("\n5. Concurrent Bidirectional Pings")
IO.puts("   Both sides pinging simultaneously...")

task1 = Task.async(fn ->
  for _i <- 1..3 do
    Client.ping(client)
    Process.sleep(100)
  end
end)

task2 = Task.async(fn ->
  for _i <- 1..3 do
    Server.ping(server)
    Process.sleep(150)
  end
end)

Task.await(task1)
Task.await(task2)
IO.puts("   ✓ Concurrent pings completed")

# Example 5: Health check pattern
IO.puts("\n6. Health Check Pattern")
IO.puts("   Implementing periodic health checks...")

defmodule HealthChecker do
  def start_monitoring(client, interval_ms, duration_ms) do
    IO.puts("   Starting health monitor (#{interval_ms}ms interval)...")
    
    end_time = System.monotonic_time(:millisecond) + duration_ms
    monitor_loop(client, interval_ms, end_time, 0, 0)
  end
  
  defp monitor_loop(client, interval_ms, end_time, success_count, fail_count) do
    current_time = System.monotonic_time(:millisecond)
    
    if current_time >= end_time do
      {success_count, fail_count}
    else
      case Client.ping(client, 500) do
        {:ok, %{}} ->
          IO.write(".")
          monitor_loop(client, interval_ms, end_time, success_count + 1, fail_count)
        {:error, _} ->
          IO.write("X")
          monitor_loop(client, interval_ms, end_time, success_count, fail_count + 1)
      end
      
      Process.sleep(interval_ms)
    end
  end
end

{successes, failures} = HealthChecker.start_monitoring(client, 250, 3000)
IO.puts("\n   Health check results: #{successes} successful, #{failures} failed")

# Example 6: Timeout handling
IO.puts("\n7. Timeout Handling")
IO.puts("   Testing ping with different timeouts...")

timeouts = [50, 100, 500, 1000]

for timeout <- timeouts do
  case Client.ping(client, timeout) do
    {:ok, %{}} ->
      IO.puts("   → #{timeout}ms timeout: ✓ Success")
    {:error, :timeout} ->
      IO.puts("   → #{timeout}ms timeout: ⏱ Timed out")
    {:error, reason} ->
      IO.puts("   → #{timeout}ms timeout: ✗ Error: #{inspect(reason)}")
  end
end

# Example 7: Using ping to detect stale connections
IO.puts("\n8. Stale Connection Detection")
IO.puts("   Simulating connection staleness check...")

# Get initial server stats
{:ok, initial_stats} = Client.call_tool(client, "get_server_stats", %{})
IO.puts("   Initial stats: #{inspect(initial_stats)}")

# Function to check if connection is alive
check_connection = fn ->
  case Client.ping(client, 1000) do
    {:ok, %{}} -> :alive
    {:error, _} -> :dead
  end
end

IO.puts("   Connection status: #{check_connection.()}")

# Example 8: Ping-based reconnection logic
IO.puts("\n9. Ping-Based Reconnection Pattern")
IO.puts("   Demonstrating reconnection logic...")

defmodule ConnectionManager do
  def ensure_connected(client, max_retries \\ 3) do
    ensure_connected_loop(client, max_retries, 0)
  end
  
  defp ensure_connected_loop(_client, max_retries, attempt) when attempt >= max_retries do
    {:error, :max_retries_exceeded}
  end
  
  defp ensure_connected_loop(client, max_retries, attempt) do
    case Client.ping(client, 1000) do
      {:ok, %{}} ->
        {:ok, :connected}
      {:error, _reason} ->
        IO.puts("   → Connection check failed, attempt #{attempt + 1}/#{max_retries}")
        Process.sleep(500 * (attempt + 1))  # Exponential backoff
        ensure_connected_loop(client, max_retries, attempt + 1)
    end
  end
end

case ConnectionManager.ensure_connected(client) do
  {:ok, :connected} ->
    IO.puts("   ✓ Connection verified")
  {:error, :max_retries_exceeded} ->
    IO.puts("   ✗ Could not verify connection after retries")
end

# Example 9: Performance baseline
IO.puts("\n10. Ping Performance Baseline")
IO.puts("    Measuring ping latency statistics...")

ping_times = for _i <- 1..20 do
  start = System.monotonic_time(:microsecond)
  Client.ping(client)
  System.monotonic_time(:microsecond) - start
end

avg_time = Enum.sum(ping_times) / length(ping_times)
min_time = Enum.min(ping_times)
max_time = Enum.max(ping_times)

IO.puts("    Ping statistics (20 samples):")
IO.puts("    → Average: #{Float.round(avg_time, 2)} μs")
IO.puts("    → Min: #{min_time} μs")
IO.puts("    → Max: #{max_time} μs")

# Get final server stats
{:ok, final_stats} = Client.call_tool(client, "get_server_stats", %{})
IO.puts("\n11. Final Server Statistics")
IO.puts("    #{inspect(final_stats)}")

# Cleanup
IO.puts("\n12. Cleanup")
GenServer.stop(client)
GenServer.stop(server)
IO.puts("    ✓ Client and server stopped")

IO.puts("\n✅ Ping Example Complete!")
IO.puts("\nKey Concepts Demonstrated:")
IO.puts("• Basic client-to-server pings")
IO.puts("• Bidirectional ping support")
IO.puts("• Response time measurement")
IO.puts("• Health check patterns")
IO.puts("• Timeout handling")
IO.puts("• Connection verification")
IO.puts("• Performance monitoring")
IO.puts("• Reconnection strategies")
IO.puts("• MCP specification compliance")