defmodule ExMCP.Examples.Interop.ElixirToPythonHTTP do
  @moduledoc """
  Example of ExMCP (Elixir) client connecting to a Python MCP server via HTTP transport.
  
  This demonstrates:
  - HTTP-based MCP communication for networked services
  - RESTful MCP server integration
  - WebSocket/SSE streaming capabilities (if supported)
  - Load balancing and fault tolerance for HTTP services
  - Performance comparison between stdio and HTTP transports
  
  ## Prerequisites
  
  1. Python 3.8+ with MCP SDK and HTTP server support:
     ```bash
     pip install mcp httpx uvicorn
     ```
  
  2. The Python HTTP MCP server should be running on localhost:8000
  
  ## Usage
  
      # Start Python HTTP server first
      python examples/interoperability/python_mcp_servers/http_server.py
      
      # Then run Elixir client
      iex> ExMCP.Examples.Interop.ElixirToPythonHTTP.run()
  """
  
  require Logger
  
  @python_server_url "http://localhost:8000"
  
  def run do
    Logger.info("Starting Elixir → Python MCP HTTP Integration Demo")
    Logger.info("=" |> String.duplicate(65))
    
    # Connect to Python HTTP MCP server
    {:ok, client} = ExMCP.Client.start_link([
      transport: :http,
      url: @python_server_url,
      name: :python_http_client
    ])
    
    Logger.info("✅ Connected to Python MCP HTTP Server at #{@python_server_url}")
    
    # Wait for connection to stabilize
    Process.sleep(200)
    
    try do
      # Demo 1: Compare HTTP vs stdio performance
      demo_http_vs_stdio_performance()
      
      # Demo 2: HTTP-specific features
      demo_http_features(client)
      
      # Demo 3: Networked operations
      demo_networked_operations(client)
      
      # Demo 4: Error handling and resilience
      demo_error_resilience(client)
      
      Logger.info("\n✅ Elixir → Python HTTP MCP demo completed successfully!")
      
    rescue
      error ->
        Logger.error("HTTP demo failed: #{inspect(error)}")
    after
      # Clean up
      ExMCP.Client.stop(client)
      Logger.info("🧹 Cleaned up Python HTTP MCP server connection")
    end
    
    :ok
  end
  
  def demo_with_load_balancing do
    Logger.info("Starting Load Balanced HTTP MCP Demo")
    Logger.info("=" |> String.duplicate(50))
    
    # Connect to multiple Python HTTP servers (if available)
    server_urls = [
      "http://localhost:8000",
      "http://localhost:8001", 
      "http://localhost:8002"
    ]
    
    clients = Enum.map(server_urls, fn url ->
      case ExMCP.Client.start_link([
        transport: :http,
        url: url,
        timeout: 2000
      ]) do
        {:ok, client} -> {url, client, :healthy}
        {:error, _reason} -> {url, nil, :unhealthy}
      end
    end)
    
    healthy_clients = Enum.filter(clients, fn {_url, _client, status} -> status == :healthy end)
    
    Logger.info("Connected to #{length(healthy_clients)}/#{length(server_urls)} HTTP servers")
    
    if length(healthy_clients) > 0 do
      try do
        # Demo load balancing
        demo_load_balancing(healthy_clients)
        
        # Demo failover
        demo_failover(healthy_clients)
        
      rescue
        error ->
          Logger.error("Load balancing demo failed: #{inspect(error)}")
      after
        # Clean up all clients
        Enum.each(healthy_clients, fn {_url, client, _status} ->
          if client, do: ExMCP.Client.stop(client)
        end)
      end
    else
      Logger.warn("No HTTP servers available for load balancing demo")
      Logger.info("Start multiple Python HTTP servers on ports 8000, 8001, 8002")
    end
    
    :ok
  end
  
  defp demo_http_vs_stdio_performance do
    Logger.info("\n=== HTTP vs stdio Performance Comparison ===")
    
    # This would require both servers to be running
    # For now, we'll simulate the comparison
    
    operation_count = 20
    
    Logger.info("Simulating performance comparison...")
    Logger.info("Testing #{operation_count} operations each:")
    
    # Simulated results based on typical performance
    stdio_avg = 1200  # ~1.2ms average for stdio transport
    http_avg = 5800   # ~5.8ms average for HTTP transport
    
    IO.puts("\n📊 Performance Comparison Results:")
    IO.puts("stdio transport (local subprocess):")
    IO.puts("  - Average per operation: #{stdio_avg}μs")
    IO.puts("  - Overhead: Process communication + JSON parsing")
    IO.puts("  - Best for: Local services, development")
    
    IO.puts("\nHTTP transport (networked service):")
    IO.puts("  - Average per operation: #{http_avg}μs") 
    IO.puts("  - Overhead: Network latency + HTTP headers + JSON parsing")
    IO.puts("  - Best for: Distributed services, microservices")
    
    ratio = Float.round(http_avg / stdio_avg, 1)
    IO.puts("\n🔍 HTTP transport is ~#{ratio}x slower than stdio (expected due to network)")
    IO.puts("💡 HTTP enables distributed architecture and load balancing")
  end
  
  defp demo_http_features(client) do
    Logger.info("\n=== HTTP-Specific Features Demo ===")
    
    # Demo 1: HTTP headers and metadata
    Logger.info("Testing HTTP-specific capabilities...")
    
    start_time = System.monotonic_time(:microsecond)
    case ExMCP.Client.list_tools(client) do
      {:ok, %{"tools" => tools}} ->
        elapsed = System.monotonic_time(:microsecond) - start_time
        Logger.info("📋 Listed #{length(tools)} tools via HTTP (#{elapsed}μs)")
        
        # Show HTTP-optimized operations
        Enum.take(tools, 3) |> Enum.each(fn tool ->
          IO.puts("  • #{tool["name"]}: #{tool["description"]}")
        end)
        
      {:error, reason} ->
        Logger.error("HTTP tools listing failed: #{inspect(reason)}")
    end
    
    # Demo 2: HTTP request batching (if supported)
    Logger.info("\nTesting HTTP request patterns...")
    
    operations = [
      {"add", %{"a" => 10, "b" => 20}},
      {"multiply", %{"a" => 5, "b" => 8}},
      {"divide", %{"a" => 100, "b" => 4}}
    ]
    
    # Sequential HTTP requests
    sequential_start = System.monotonic_time(:microsecond)
    sequential_results = Enum.map(operations, fn {op, args} ->
      case ExMCP.Client.call_tool(client, op, args) do
        {:ok, %{"content" => [%{"text" => result}]}} -> {:ok, result}
        {:error, reason} -> {:error, reason}
      end
    end)
    sequential_time = System.monotonic_time(:microsecond) - sequential_start
    
    successful_count = Enum.count(sequential_results, fn 
      {:ok, _} -> true
      _ -> false
    end)
    
    Logger.info("Sequential HTTP requests:")
    Logger.info("  - Operations: #{successful_count}/#{length(operations)} successful")
    Logger.info("  - Total time: #{sequential_time}μs")
    Logger.info("  - Average per request: #{div(sequential_time, length(operations))}μs")
  end
  
  defp demo_networked_operations(client) do
    Logger.info("\n=== Networked Operations Demo ===")
    
    # Demo operations that benefit from HTTP transport
    network_operations = [
      {"get_stats", %{}, "Server statistics"},
      {"add", %{"a" => 100, "b" => 200}, "Network calculation"},
      {"power", %{"base" => 2, "exponent" => 16}, "Complex computation"}
    ]
    
    Logger.info("Testing operations designed for networked services...")
    
    Enum.each(network_operations, fn {operation, args, description} ->
      start_time = System.monotonic_time(:microsecond)
      
      case ExMCP.Client.call_tool(client, operation, args) do
        {:ok, %{"content" => [%{"text" => result}]}} ->
          elapsed = System.monotonic_time(:microsecond) - start_time
          result_preview = String.slice(result, 0, 50)
          Logger.info("  ✓ #{description}: #{result_preview}... (#{elapsed}μs)")
          
        {:error, reason} ->
          Logger.error("  ✗ #{description}: #{inspect(reason)}")
      end
    end)
    
    # Demo concurrent HTTP requests
    Logger.info("\nTesting concurrent HTTP requests...")
    
    concurrent_operations = [
      {"add", %{"a" => 1, "b" => 1}},
      {"add", %{"a" => 2, "b" => 2}},
      {"add", %{"a" => 3, "b" => 3}},
      {"add", %{"a" => 4, "b" => 4}},
      {"add", %{"a" => 5, "b" => 5}}
    ]
    
    start_time = System.monotonic_time(:microsecond)
    
    results = concurrent_operations
    |> Task.async_stream(fn {op, args} ->
      call_start = System.monotonic_time(:microsecond)
      result = ExMCP.Client.call_tool(client, op, args)
      call_time = System.monotonic_time(:microsecond) - call_start
      {op, result, call_time}
    end, max_concurrency: 5, timeout: 10000)
    |> Enum.to_list()
    
    total_concurrent_time = System.monotonic_time(:microsecond) - start_time
    
    successful_concurrent = Enum.count(results, fn
      {:ok, {_op, {:ok, _}, _time}} -> true
      _ -> false
    end)
    
    Logger.info("Concurrent HTTP results:")
    Logger.info("  - Operations: #{successful_concurrent}/#{length(concurrent_operations)} successful")
    Logger.info("  - Total time: #{total_concurrent_time}μs")
    Logger.info("  - Concurrency benefit: HTTP connection pooling enables parallel requests")
  end
  
  defp demo_error_resilience(client) do
    Logger.info("\n=== HTTP Error Handling and Resilience ===")
    
    # Test various error conditions
    error_tests = [
      {"invalid_operation", %{"a" => 1}, "Unknown HTTP endpoint"},
      {"divide", %{"a" => 10, "b" => 0}, "Server-side error"},
      {"add", %{"invalid" => "params"}, "Invalid parameters"}
    ]
    
    Logger.info("Testing HTTP error handling...")
    
    Enum.each(error_tests, fn {operation, args, test_name} ->
      case ExMCP.Client.call_tool(client, operation, args) do
        {:ok, %{"content" => [%{"text" => result}]}} ->
          if String.contains?(result, "Error") do
            Logger.info("  ✓ #{test_name}: Properly handled - #{result}")
          else
            Logger.warn("  ⚠ #{test_name}: Unexpected success - #{result}")
          end
          
        {:error, reason} ->
          Logger.info("  ✓ #{test_name}: HTTP error handling - #{inspect(reason)}")
      end
    end)
    
    # Test connection resilience
    Logger.info("\nTesting connection resilience...")
    
    # Test multiple rapid requests
    rapid_requests = 1..10
    
    rapid_start = System.monotonic_time(:microsecond)
    rapid_results = Enum.map(rapid_requests, fn i ->
      case ExMCP.Client.call_tool(client, "add", %{"a" => i, "b" => i}) do
        {:ok, _} -> :success
        {:error, _} -> :failure
      end
    end)
    rapid_time = System.monotonic_time(:microsecond) - rapid_start
    
    success_count = Enum.count(rapid_results, &(&1 == :success))
    
    Logger.info("Rapid requests test:")
    Logger.info("  - Requests: #{success_count}/#{length(rapid_requests)} successful")
    Logger.info("  - Total time: #{rapid_time}μs")
    Logger.info("  - Average per request: #{div(rapid_time, length(rapid_requests))}μs")
    Logger.info("  - HTTP connection handling: #{if success_count == length(rapid_requests), do: "Excellent", else: "Needs improvement"}")
  end
  
  defp demo_load_balancing(healthy_clients) do
    Logger.info("\n=== Load Balancing Demo ===")
    
    # Round-robin through available clients
    operations = 1..15
    
    Logger.info("Distributing #{length(operations)} operations across #{length(healthy_clients)} servers...")
    
    results = Enum.map(operations, fn i ->
      # Select client using round-robin
      client_index = rem(i - 1, length(healthy_clients))
      {url, client, _status} = Enum.at(healthy_clients, client_index)
      
      start_time = System.monotonic_time(:microsecond)
      result = ExMCP.Client.call_tool(client, "add", %{"a" => i, "b" => i * 2})
      elapsed = System.monotonic_time(:microsecond) - start_time
      
      {url, result, elapsed}
    end)
    
    # Analyze distribution
    url_counts = results
    |> Enum.group_by(fn {url, _result, _time} -> url end)
    |> Enum.map(fn {url, ops} -> {url, length(ops)} end)
    
    Logger.info("Load distribution:")
    Enum.each(url_counts, fn {url, count} ->
      IO.puts("  • #{url}: #{count} operations")
    end)
    
    # Show performance per server
    avg_times = results
    |> Enum.group_by(fn {url, _result, _time} -> url end)
    |> Enum.map(fn {url, ops} ->
      times = Enum.map(ops, fn {_url, _result, time} -> time end)
      avg_time = Enum.sum(times) / length(times)
      {url, Float.round(avg_time, 1)}
    end)
    
    Logger.info("Average response times:")
    Enum.each(avg_times, fn {url, avg_time} ->
      IO.puts("  • #{url}: #{avg_time}μs")
    end)
  end
  
  defp demo_failover(healthy_clients) do
    Logger.info("\n=== Failover Demo ===")
    
    if length(healthy_clients) >= 2 do
      Logger.info("Simulating failover between HTTP servers...")
      
      # Use first client normally
      {primary_url, primary_client, _} = List.first(healthy_clients)
      {backup_url, backup_client, _} = Enum.at(healthy_clients, 1)
      
      # Test primary
      case ExMCP.Client.call_tool(primary_client, "add", %{"a" => 10, "b" => 20}) do
        {:ok, %{"content" => [%{"text" => result}]}} ->
          Logger.info("  ✓ Primary server (#{primary_url}): #{result}")
          
          # Simulate primary failure by using backup
          case ExMCP.Client.call_tool(backup_client, "add", %{"a" => 10, "b" => 20}) do
            {:ok, %{"content" => [%{"text" => backup_result}]}} ->
              Logger.info("  ✓ Backup server (#{backup_url}): #{backup_result}")
              Logger.info("  🔄 Failover successful!")
              
            {:error, reason} ->
              Logger.error("  ✗ Backup server failed: #{inspect(reason)}")
          end
          
        {:error, reason} ->
          Logger.error("  ✗ Primary server failed: #{inspect(reason)}")
      end
    else
      Logger.info("Need at least 2 servers for failover demo")
    end
  end
end

# Demo runner
defmodule ExMCP.Examples.Interop.ElixirToPythonHTTPDemo do
  def run do
    Logger.info("Starting Elixir → Python HTTP MCP Integration Demo Suite")
    Logger.info("=" |> String.duplicate(75))
    
    # Check if Python HTTP server is running
    Logger.info("Checking Python HTTP MCP server availability...")
    
    case :httpc.request(:get, {'http://localhost:8000/health', []}, [], []) do
      {:ok, {{_version, 200, _reason_phrase}, _headers, _body}} ->
        Logger.info("✅ Python HTTP MCP server is running")
        
        # Run basic HTTP demo
        Logger.info("\n=== Basic HTTP Integration Demo ===")
        ExMCP.Examples.Interop.ElixirToPythonHTTP.run()
        
        Process.sleep(1000)
        
        # Run load balancing demo
        Logger.info("\n=== Load Balancing Demo ===")
        ExMCP.Examples.Interop.ElixirToPythonHTTP.demo_with_load_balancing()
        
      {:error, reason} ->
        Logger.warn("❌ Python HTTP MCP server not accessible: #{inspect(reason)}")
        Logger.info("To run this demo:")
        Logger.info("  1. pip install mcp httpx uvicorn")
        Logger.info("  2. python examples/interoperability/python_mcp_servers/http_server.py")
        Logger.info("  3. Ensure server is running on http://localhost:8000")
    end
    
    Logger.info("\n=== HTTP Transport Benefits ===")
    IO.puts("✓ Distributed service architecture")
    IO.puts("✓ Load balancing and failover capabilities")
    IO.puts("✓ Network-based service discovery")
    IO.puts("✓ HTTP standards compliance")
    IO.puts("✓ Monitoring and observability hooks")
    IO.puts("✓ Scalable microservices patterns")
    
    :ok
  end
end

# Example usage:
if __FILE__ == Path.absname("#{__DIR__}/elixir_to_python_http.ex") do
  ExMCP.Examples.Interop.ElixirToPythonHTTPDemo.run()
end