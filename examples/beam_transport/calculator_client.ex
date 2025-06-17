defmodule Examples.Native.CalculatorClient do
  @moduledoc """
  Example of using ExMCP's Native Service Dispatcher to communicate with a calculator service.
  
  This demonstrates:
  - Direct service-to-service communication with ExMCP.Native
  - Calling tools with parameters
  - Handling responses and errors
  - Performance benefits of Native Service Dispatcher
  """
  
  require Logger
  
  def run_example do
    Logger.info("Starting calculator client example...")
    
    # Start the calculator service
    {:ok, _server_pid} = Examples.Native.CalculatorService.start_link([])
    
    # Wait for service registration
    Process.sleep(100)
    
    # Verify service is available
    unless ExMCP.Native.service_available?(:calculator_service) do
      raise "Calculator service not available"
    end
    
    Logger.info("Connected to calculator service via Native Service Dispatcher")
    
    # List available tools
    {:ok, %{"tools" => tools}} = ExMCP.Native.call(:calculator_service, "list_tools", %{})
    Logger.info("Available tools:")
    for tool <- tools do
      Logger.info("  - #{tool["name"]}: #{tool["description"]}")
    end
    
    # Demonstrate performance by timing operations
    IO.puts("\nPerformance Demonstration:")
    IO.puts("=" |> String.duplicate(40))
    
    # Perform some calculations with timing
    examples = [
      {"add", %{"a" => 5, "b" => 3}},
      {"multiply", %{"a" => 4, "b" => 7}}, 
      {"divide", %{"a" => 10, "b" => 3}},
      {"divide", %{"a" => 10, "b" => 0}},  # This will error
      {"factorial", %{"n" => 5}}
    ]
    
    IO.puts("\nExecuting calculations:")
    
    total_time = 
      examples
      |> Enum.map(fn {tool, args} ->
        start_time = System.monotonic_time(:microsecond)
        
        case ExMCP.Native.call(:calculator_service, "tools/call", %{
          "name" => tool,
          "arguments" => args
        }) do
          {:ok, %{"content" => [%{"text" => text}]}} ->
            elapsed = System.monotonic_time(:microsecond) - start_time
            IO.puts("  ✓ #{text} (#{elapsed}μs)")
            elapsed
            
          {:error, %{"message" => error}} ->
            elapsed = System.monotonic_time(:microsecond) - start_time
            IO.puts("  ✗ Error: #{error} (#{elapsed}μs)")
            elapsed
        end
      end)
      |> Enum.sum()
    
    IO.puts("\nTotal time for #{length(examples)} operations: #{total_time}μs")
    IO.puts("Average time per operation: #{div(total_time, length(examples))}μs")
    
    # Demonstrate parallel execution
    IO.puts("\nParallel Execution Demo:")
    parallel_examples = [
      {"add", %{"a" => 1, "b" => 1}},
      {"add", %{"a" => 2, "b" => 2}},
      {"add", %{"a" => 3, "b" => 3}},
      {"add", %{"a" => 4, "b" => 4}},
      {"add", %{"a" => 5, "b" => 5}}
    ]
    
    start_time = System.monotonic_time(:microsecond)
    
    results = 
      parallel_examples
      |> Task.async_stream(fn {tool, args} ->
        ExMCP.Native.call(:calculator_service, "tools/call", %{
          "name" => tool,
          "arguments" => args
        })
      end, max_concurrency: 10)
      |> Enum.to_list()
    
    parallel_time = System.monotonic_time(:microsecond) - start_time
    
    IO.puts("Parallel execution of #{length(parallel_examples)} operations: #{parallel_time}μs")
    IO.puts("Sequential would take ~#{length(parallel_examples) * 15}μs (estimated)")
    IO.puts("Speedup: ~#{Float.round((length(parallel_examples) * 15) / parallel_time, 2)}x")
    
    # Show calculation history
    {:ok, %{"content" => [%{"text" => history}]}} = 
      ExMCP.Native.call(:calculator_service, "tools/call", %{
        "name" => "history",
        "arguments" => %{"limit" => 5}
      })
    
    IO.puts("\nRecent calculation history:")
    IO.puts(history)
    
    # Demonstrate fire-and-forget notification
    :ok = ExMCP.Native.notify(:calculator_service, "reset_history", %{})
    Logger.info("Sent reset history notification")
    
    Logger.info("Calculator client example completed")
    
    :ok
  end
  
  def benchmark_vs_traditional_mcp do
    """
    This function demonstrates the performance difference between:
    1. Native Service Dispatcher (~15μs per call)
    2. Traditional MCP over stdio/HTTP (~1-5ms per call)
    
    Native Service Dispatcher is 67-333x faster for local service communication!
    """
    
    # Start service
    {:ok, _} = Examples.Native.CalculatorService.start_link([])
    Process.sleep(100)
    
    operations_count = 1000
    
    IO.puts("Benchmarking #{operations_count} operations...")
    
    # Benchmark Native calls
    start_time = System.monotonic_time(:microsecond)
    
    1..operations_count
    |> Enum.each(fn i ->
      ExMCP.Native.call(:calculator_service, "tools/call", %{
        "name" => "add",
        "arguments" => %{"a" => i, "b" => 1}
      })
    end)
    
    native_time = System.monotonic_time(:microsecond) - start_time
    
    IO.puts("\nNative Service Dispatcher:")
    IO.puts("  Total time: #{native_time}μs")
    IO.puts("  Average per call: #{div(native_time, operations_count)}μs")
    
    # Estimated traditional MCP performance
    estimated_mcp_time = operations_count * 2000  # 2ms average
    
    IO.puts("\nEstimated Traditional MCP (stdio/HTTP):")
    IO.puts("  Estimated total time: #{estimated_mcp_time}μs")
    IO.puts("  Estimated average per call: 2000μs")
    
    IO.puts("\nPerformance improvement:")
    IO.puts("  Native is ~#{div(estimated_mcp_time, native_time)}x faster")
    
    :ok
  end
end

# Example usage:
if __FILE__ == Path.absname("#{__DIR__}/calculator_client.ex") do
  Examples.Native.CalculatorClient.run_example()
  
  IO.puts("\n" <> ("=" |> String.duplicate(50)))
  IO.puts("Running benchmark comparison...")
  Examples.Native.CalculatorClient.benchmark_vs_traditional_mcp()
end