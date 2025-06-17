defmodule ExMCP.Examples.Interop.ElixirToPythonStdio do
  @moduledoc """
  Example of ExMCP (Elixir) client connecting to a Python MCP server via stdio transport.
  
  This demonstrates:
  - Cross-language MCP communication
  - stdio transport usage
  - Performance comparison between native and interop calls
  - Error handling across language boundaries
  - JSON serialization compatibility
  
  ## Prerequisites
  
  1. Python 3.8+ with MCP SDK:
     ```bash
     pip install mcp
     ```
  
  2. The Python calculator server should be available at:
     `examples/interoperability/python_mcp_servers/calculator_server.py`
  
  ## Usage
  
      iex> ExMCP.Examples.Interop.ElixirToPythonStdio.run()
      
  Or run the comprehensive demo:
  
      iex> ExMCP.Examples.Interop.ElixirToPythonStdio.demo_with_performance()
  """
  
  require Logger
  
  @python_server_path "examples/interoperability/python_mcp_servers/calculator_server.py"
  
  def run do
    Logger.info("Starting Elixir → Python MCP Integration Demo")
    Logger.info("=" |> String.duplicate(60))
    
    # Start Python MCP server as subprocess via stdio transport
    {:ok, client} = ExMCP.Client.start_link([
      transport: :stdio,
      command: ["python3", @python_server_path],
      name: :python_calculator_client
    ])
    
    Logger.info("✅ Connected to Python MCP Calculator Server via stdio")
    
    # Wait for connection to stabilize
    Process.sleep(100)
    
    try do
      # Demo 1: List available tools from Python server
      demo_list_tools(client)
      
      # Demo 2: Basic calculations
      demo_basic_calculations(client)
      
      # Demo 3: Error handling
      demo_error_handling(client)
      
      # Demo 4: History and stats
      demo_history_and_stats(client)
      
      Logger.info("\n✅ Elixir → Python MCP demo completed successfully!")
      
    rescue
      error ->
        Logger.error("Demo failed: #{inspect(error)}")
    after
      # Clean up
      ExMCP.Client.stop(client)
      Logger.info("🧹 Cleaned up Python MCP server connection")
    end
    
    :ok
  end
  
  def demo_with_performance do
    Logger.info("Starting Comprehensive Elixir → Python MCP Demo with Performance Analysis")
    Logger.info("=" |> String.duplicate(80))
    
    # Start both Python server and Native Elixir service for comparison
    {:ok, python_client} = ExMCP.Client.start_link([
      transport: :stdio,
      command: ["python3", @python_server_path],
      name: :python_calc_client
    ])
    
    {:ok, _elixir_service} = Examples.Native.CalculatorService.start_link([])
    
    Process.sleep(200)  # Allow connections to stabilize
    
    try do
      # Performance comparison
      Logger.info("\n=== Performance Comparison: Native vs Python Interop ===")
      compare_performance(python_client)
      
      # Comprehensive feature testing
      Logger.info("\n=== Comprehensive Feature Testing ===")
      test_all_python_features(python_client)
      
      # Parallel operations test
      Logger.info("\n=== Parallel Operations Test ===")
      test_parallel_operations(python_client)
      
    rescue
      error ->
        Logger.error("Performance demo failed: #{inspect(error)}")
    after
      ExMCP.Client.stop(python_client)
      Logger.info("🧹 Performance demo cleanup completed")
    end
    
    :ok
  end
  
  defp demo_list_tools(client) do
    Logger.info("\n=== Available Tools from Python Server ===")
    
    start_time = System.monotonic_time(:microsecond)
    case ExMCP.Client.list_tools(client) do
      {:ok, %{"tools" => tools}} ->
        elapsed = System.monotonic_time(:microsecond) - start_time
        Logger.info("📋 Found #{length(tools)} tools (#{elapsed}μs):")
        
        Enum.each(tools, fn tool ->
          IO.puts("  • #{tool["name"]}: #{tool["description"]}")
          if tool["inputSchema"]["properties"] do
            params = tool["inputSchema"]["properties"] |> Map.keys() |> Enum.join(", ")
            IO.puts("    Parameters: #{params}")
          end
        end)
        
      {:error, reason} ->
        Logger.error("Failed to list tools: #{inspect(reason)}")
    end
  end
  
  defp demo_basic_calculations(client) do
    Logger.info("\n=== Basic Calculations via Python Server ===")
    
    calculations = [
      {"add", %{"a" => 15, "b" => 27}},
      {"subtract", %{"a" => 100, "b" => 37}}, 
      {"multiply", %{"a" => 8, "b" => 9}},
      {"divide", %{"a" => 144, "b" => 12}},
      {"power", %{"base" => 2, "exponent" => 10}}
    ]
    
    total_time = 0
    
    Enum.each(calculations, fn {operation, args} ->
      start_time = System.monotonic_time(:microsecond)
      
      case ExMCP.Client.call_tool(client, operation, args) do
        {:ok, %{"content" => [%{"text" => result}]}} ->
          elapsed = System.monotonic_time(:microsecond) - start_time
          total_time = total_time + elapsed
          IO.puts("  ✓ #{result} (#{elapsed}μs)")
          
        {:error, reason} ->
          Logger.error("  ✗ #{operation} failed: #{inspect(reason)}")
      end
    end)
    
    avg_time = div(total_time, length(calculations))
    Logger.info("Average operation time: #{avg_time}μs")
  end
  
  defp demo_error_handling(client) do
    Logger.info("\n=== Error Handling Tests ===")
    
    error_cases = [
      {"divide", %{"a" => 10, "b" => 0}, "Division by zero"},
      {"unknown_operation", %{"a" => 1}, "Unknown tool"},
      {"add", %{"a" => "not_a_number", "b" => 5}, "Invalid argument type"}
    ]
    
    Enum.each(error_cases, fn {operation, args, expected_error} ->
      case ExMCP.Client.call_tool(client, operation, args) do
        {:ok, %{"content" => [%{"text" => result}]}} ->
          if String.contains?(result, "Error") do
            IO.puts("  ✓ #{expected_error}: #{result}")
          else
            IO.puts("  ⚠ Expected error but got: #{result}")
          end
          
        {:error, reason} ->
          IO.puts("  ✓ #{expected_error}: #{inspect(reason)}")
      end
    end)
  end
  
  defp demo_history_and_stats(client) do
    Logger.info("\n=== History and Statistics ===")
    
    # Get calculation history
    case ExMCP.Client.call_tool(client, "get_history", %{"limit" => 5}) do
      {:ok, %{"content" => [%{"text" => history}]}} ->
        IO.puts("Recent calculations:")
        IO.puts(history)
        
      {:error, reason} ->
        Logger.error("Failed to get history: #{inspect(reason)}")
    end
    
    # Get server statistics  
    case ExMCP.Client.call_tool(client, "get_stats", %{}) do
      {:ok, %{"content" => [%{"text" => stats}]}} ->
        IO.puts("\nServer statistics:")
        IO.puts(stats)
        
      {:error, reason} ->
        Logger.error("Failed to get stats: #{inspect(reason)}")
    end
  end
  
  defp compare_performance(python_client) do
    # Test same operation on both Python and Native Elixir
    operation_count = 50
    args = %{"a" => 123, "b" => 456}
    
    # Test Python server performance
    Logger.info("Testing Python MCP server performance (#{operation_count} operations)...")
    
    python_start = System.monotonic_time(:microsecond)
    python_results = Enum.map(1..operation_count, fn _i ->
      start_time = System.monotonic_time(:microsecond)
      case ExMCP.Client.call_tool(python_client, "add", args) do
        {:ok, _result} ->
          System.monotonic_time(:microsecond) - start_time
        {:error, _} -> 
          nil
      end
    end)
    python_total = System.monotonic_time(:microsecond) - python_start
    
    python_times = Enum.filter(python_results, & &1)
    python_avg = if length(python_times) > 0, do: Enum.sum(python_times) / length(python_times), else: 0
    
    # Test Native Elixir service performance
    Logger.info("Testing Native Elixir service performance (#{operation_count} operations)...")
    
    native_start = System.monotonic_time(:microsecond)
    native_results = Enum.map(1..operation_count, fn _i ->
      start_time = System.monotonic_time(:microsecond)
      case ExMCP.Native.call(:calculator_service, "tools/call", %{
        "name" => "add",
        "arguments" => args
      }) do
        {:ok, _result} ->
          System.monotonic_time(:microsecond) - start_time
        {:error, _} ->
          nil
      end
    end)
    native_total = System.monotonic_time(:microsecond) - native_start
    
    native_times = Enum.filter(native_results, & &1)
    native_avg = if length(native_times) > 0, do: Enum.sum(native_times) / length(native_times), else: 0
    
    # Performance summary
    Logger.info("\n📊 Performance Comparison Results:")
    IO.puts("Python MCP Server (stdio transport):")
    IO.puts("  - Total time: #{python_total}μs")
    IO.puts("  - Average per operation: #{Float.round(python_avg, 1)}μs")
    IO.puts("  - Successful operations: #{length(python_times)}/#{operation_count}")
    
    IO.puts("\nNative Elixir Service (Native Service Dispatcher):")
    IO.puts("  - Total time: #{native_total}μs") 
    IO.puts("  - Average per operation: #{Float.round(native_avg, 1)}μs")
    IO.puts("  - Successful operations: #{length(native_times)}/#{operation_count}")
    
    if native_avg > 0 and python_avg > 0 do
      speedup = python_avg / native_avg
      IO.puts("\n🚀 Native Service Dispatcher is #{Float.round(speedup, 1)}x faster than Python interop")
      IO.puts("💡 Use Native for performance-critical Elixir-to-Elixir communication")
      IO.puts("💡 Use stdio/HTTP transports for cross-language interoperability")
    end
  end
  
  defp test_all_python_features(client) do
    # Test all available tools from Python server
    {:ok, %{"tools" => tools}} = ExMCP.Client.list_tools(client)
    
    test_cases = [
      {"add", %{"a" => 42, "b" => 58}},
      {"subtract", %{"a" => 1000, "b" => 1}},
      {"multiply", %{"a" => 7, "b" => 13}},
      {"divide", %{"a" => 81, "b" => 9}},
      {"power", %{"base" => 3, "exponent" => 4}},
      {"get_history", %{"limit" => 3}},
      {"get_stats", %{}}
    ]
    
    Logger.info("Testing all #{length(tools)} Python server tools:")
    
    Enum.each(test_cases, fn {tool_name, args} ->
      case ExMCP.Client.call_tool(client, tool_name, args) do
        {:ok, %{"content" => [%{"text" => result}]}} ->
          result_preview = String.slice(result, 0, 60)
          IO.puts("  ✓ #{tool_name}: #{result_preview}#{if String.length(result) > 60, do: "...", else: ""}")
          
        {:error, reason} ->
          IO.puts("  ✗ #{tool_name}: #{inspect(reason)}")
      end
    end)
  end
  
  defp test_parallel_operations(client) do
    # Test parallel operations to Python server
    operations = [
      {"add", %{"a" => 10, "b" => 20}},
      {"multiply", %{"a" => 5, "b" => 6}},
      {"power", %{"base" => 2, "exponent" => 8}},
      {"divide", %{"a" => 100, "b" => 4}},
      {"subtract", %{"a" => 50, "b" => 15}}
    ]
    
    Logger.info("Testing parallel operations to Python server...")
    
    start_time = System.monotonic_time(:microsecond)
    
    results = operations
    |> Task.async_stream(fn {operation, args} ->
      call_start = System.monotonic_time(:microsecond)
      result = ExMCP.Client.call_tool(client, operation, args)
      call_time = System.monotonic_time(:microsecond) - call_start
      {operation, result, call_time}
    end, max_concurrency: 5, timeout: 10000)
    |> Enum.to_list()
    
    total_parallel_time = System.monotonic_time(:microsecond) - start_time
    
    Logger.info("Parallel operation results:")
    Enum.each(results, fn 
      {:ok, {operation, {:ok, %{"content" => [%{"text" => text}]}}, call_time}} ->
        result_preview = String.slice(text, 0, 40)
        IO.puts("  ✓ #{operation}: #{result_preview}... (#{call_time}μs)")
        
      {:ok, {operation, {:error, reason}, call_time}} ->
        IO.puts("  ✗ #{operation}: #{inspect(reason)} (#{call_time}μs)")
        
      {:exit, reason} ->
        IO.puts("  ✗ Task failed: #{inspect(reason)}")
    end)
    
    successful_count = Enum.count(results, fn 
      {:ok, {_op, {:ok, _}, _time}} -> true
      _ -> false
    end)
    
    IO.puts("\nParallel execution summary:")
    IO.puts("  - Total time: #{total_parallel_time}μs")
    IO.puts("  - Successful operations: #{successful_count}/#{length(operations)}")
    IO.puts("  - Average per operation: #{div(total_parallel_time, length(operations))}μs")
    
    # Compare with estimated sequential time
    individual_times = Enum.map(results, fn
      {:ok, {_op, {:ok, _}, call_time}} -> call_time
      _ -> 2000  # Estimate for failed operations
    end)
    
    estimated_sequential = Enum.sum(individual_times)
    parallel_benefit = if total_parallel_time > 0, do: Float.round(estimated_sequential / total_parallel_time, 1), else: 1
    
    IO.puts("  - Parallel benefit: ~#{parallel_benefit}x faster than sequential")
    IO.puts("💡 Python MCP servers can handle concurrent requests efficiently")
  end
end

# Demo runner
defmodule ExMCP.Examples.Interop.ElixirToPythonDemo do
  def run do
    Logger.info("Starting Elixir → Python MCP Integration Demo Suite")
    Logger.info("=" |> String.duplicate(70))
    
    # Check if Python server file exists
    python_server_path = "examples/interoperability/python_mcp_servers/calculator_server.py"
    
    if File.exists?(python_server_path) do
      Logger.info("✅ Found Python calculator server")
      
      # Run basic demo
      Logger.info("\n=== Basic Integration Demo ===")
      ExMCP.Examples.Interop.ElixirToPythonStdio.run()
      
      Process.sleep(1000)
      
      # Run performance demo
      Logger.info("\n=== Performance Analysis Demo ===")
      ExMCP.Examples.Interop.ElixirToPythonStdio.demo_with_performance()
      
    else
      Logger.error("❌ Python calculator server not found at: #{python_server_path}")
      Logger.info("Please ensure the Python MCP server is available:")
      Logger.info("  1. pip install mcp")
      Logger.info("  2. Check that #{python_server_path} exists")
    end
    
    Logger.info("\n=== Integration Benefits ===")
    IO.puts("✓ Cross-language MCP communication via stdio transport")
    IO.puts("✓ JSON serialization handled automatically by ExMCP")
    IO.puts("✓ Error handling across language boundaries")
    IO.puts("✓ Subprocess management and cleanup")
    IO.puts("✓ Performance monitoring for interop vs native calls")
    
    :ok
  end
end

# Example usage:
if __FILE__ == Path.absname("#{__DIR__}/elixir_to_python_stdio.ex") do
  ExMCP.Examples.Interop.ElixirToPythonDemo.run()
end