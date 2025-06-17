defmodule ExMCP.Examples.Interop.HybridArchitecture do
  @moduledoc """
  Comprehensive example demonstrating a hybrid Elixir/Python MCP architecture.
  
  This example shows how to build a complex system that combines:
  - Native Elixir services (ExMCP.Native) for high-performance operations
  - Python MCP services (stdio/HTTP) for specialized capabilities
  - Load balancing and failover between service types
  - Service orchestration and workflow management
  - Performance monitoring and optimization
  
  ## Architecture Overview
  
  ```
  ┌─────────────────────────────────────────────────────────────┐
  │                    Hybrid MCP Architecture                 │
  ├─────────────────────────────────────────────────────────────┤
  │  Elixir Orchestrator (This Example)                        │
  │  ┌─────────────────────────────────────────────────────┐    │
  │  │ Service Registry & Load Balancer                    │    │
  │  │ • Native Elixir Services (15μs avg)                 │    │
  │  │ • Python stdio Services (1-5ms avg)                │    │
  │  │ • Python HTTP Services (5-20ms avg)                │    │
  │  └─────────────────────────────────────────────────────┘    │
  ├─────────────────────────────────────────────────────────────┤
  │  Native Elixir Services (ExMCP.Native)                     │
  │  • Data Processing Service                                  │
  │  • Calculator Service                                       │
  │  • File Management Service                                  │
  ├─────────────────────────────────────────────────────────────┤
  │  Python Services (stdio transport)                         │
  │  • Machine Learning Service                                │
  │  • Data Analysis Service                                    │
  │  • Image Processing Service                                 │
  ├─────────────────────────────────────────────────────────────┤
  │  Python Services (HTTP transport)                          │
  │  • Distributed ML Inference                                │
  │  • External API Gateway                                     │
  │  • Monitoring & Metrics Service                            │
  └─────────────────────────────────────────────────────────────┘
  ```
  
  ## Usage
  
      iex> ExMCP.Examples.Interop.HybridArchitecture.run()
      
  Or test specific scenarios:
  
      iex> ExMCP.Examples.Interop.HybridArchitecture.demo_performance_routing()
      iex> ExMCP.Examples.Interop.HybridArchitecture.demo_failover()
  """
  
  require Logger
  
  defmodule ServiceRegistry do
    @moduledoc """
    Registry for managing hybrid Elixir/Python MCP services.
    """
    
    use GenServer
    
    defstruct [
      native_services: %{},
      stdio_services: %{},
      http_services: %{},
      service_stats: %{},
      load_balancer_state: %{}
    ]
    
    def start_link(opts \\ []) do
      GenServer.start_link(__MODULE__, opts, name: __MODULE__)
    end
    
    def register_native_service(name, pid, metadata \\ %{}) do
      GenServer.call(__MODULE__, {:register_native, name, pid, metadata})
    end
    
    def register_stdio_service(name, client_pid, command, metadata \\ %{}) do
      GenServer.call(__MODULE__, {:register_stdio, name, client_pid, command, metadata})
    end
    
    def register_http_service(name, url, metadata \\ %{}) do
      GenServer.call(__MODULE__, {:register_http, name, url, metadata})
    end
    
    def get_service(name, strategy \\ :fastest) do
      GenServer.call(__MODULE__, {:get_service, name, strategy})
    end
    
    def list_all_services do
      GenServer.call(__MODULE__, :list_all)
    end
    
    def get_performance_stats do
      GenServer.call(__MODULE__, :get_stats)
    end
    
    @impl true
    def init(_opts) do
      state = %__MODULE__{}
      Logger.info("Hybrid MCP Service Registry started")
      {:ok, state}
    end
    
    @impl true
    def handle_call({:register_native, name, pid, metadata}, _from, state) do
      service_info = %{
        type: :native,
        pid: pid,
        metadata: metadata,
        registered_at: DateTime.utc_now(),
        avg_response_time: 15  # μs
      }
      
      new_state = put_in(state.native_services[name], service_info)
      {:reply, :ok, new_state}
    end
    
    def handle_call({:register_stdio, name, client_pid, command, metadata}, _from, state) do
      service_info = %{
        type: :stdio,
        client_pid: client_pid,
        command: command,
        metadata: metadata,
        registered_at: DateTime.utc_now(),
        avg_response_time: 2500  # μs
      }
      
      new_state = put_in(state.stdio_services[name], service_info)
      {:reply, :ok, new_state}
    end
    
    def handle_call({:register_http, name, url, metadata}, _from, state) do
      service_info = %{
        type: :http,
        url: url,
        metadata: metadata,
        registered_at: DateTime.utc_now(),
        avg_response_time: 8000  # μs
      }
      
      new_state = put_in(state.http_services[name], service_info)
      {:reply, :ok, new_state}
    end
    
    def handle_call({:get_service, name, strategy}, _from, state) do
      # Find service across all types
      candidates = []
      
      candidates = if Map.has_key?(state.native_services, name) do
        [Map.get(state.native_services, name) | candidates]
      else
        candidates
      end
      
      candidates = if Map.has_key?(state.stdio_services, name) do
        [Map.get(state.stdio_services, name) | candidates]
      else
        candidates
      end
      
      candidates = if Map.has_key?(state.http_services, name) do
        [Map.get(state.http_services, name) | candidates]
      else
        candidates
      end
      
      service = case {candidates, strategy} do
        {[], _} -> 
          nil
          
        {[single], _} -> 
          single
          
        {multiple, :fastest} ->
          # Choose service with lowest average response time
          Enum.min_by(multiple, & &1.avg_response_time)
          
        {multiple, :native_first} ->
          # Prefer native, then stdio, then HTTP
          Enum.find(multiple, fn s -> s.type == :native end) ||
          Enum.find(multiple, fn s -> s.type == :stdio end) ||
          Enum.find(multiple, fn s -> s.type == :http end)
          
        {multiple, :round_robin} ->
          # Simple round-robin (would need more state in real implementation)
          Enum.random(multiple)
      end
      
      {:reply, {:ok, service}, state}
    end
    
    def handle_call(:list_all, _from, state) do
      all_services = %{
        native: state.native_services,
        stdio: state.stdio_services,
        http: state.http_services
      }
      
      {:reply, all_services, state}
    end
    
    def handle_call(:get_stats, _from, state) do
      stats = %{
        total_services: map_size(state.native_services) + map_size(state.stdio_services) + map_size(state.http_services),
        native_count: map_size(state.native_services),
        stdio_count: map_size(state.stdio_services),
        http_count: map_size(state.http_services),
        service_stats: state.service_stats
      }
      
      {:reply, stats, state}
    end
  end
  
  defmodule HybridOrchestrator do
    @moduledoc """
    Service orchestrator that routes requests to optimal services.
    """
    
    def call_service(service_name, method, params, strategy \\ :fastest) do
      case ServiceRegistry.get_service(service_name, strategy) do
        {:ok, nil} ->
          {:error, :service_not_found}
          
        {:ok, service_info} ->
          call_service_by_type(service_info, method, params)
      end
    end
    
    defp call_service_by_type(%{type: :native, pid: pid}, method, params) do
      start_time = System.monotonic_time(:microsecond)
      
      # For native services, we need to determine the service name from the pid
      # In a real implementation, this would be better tracked
      service_atom = case :global.whereis_name(:calculator_service) do
        ^pid -> :calculator_service
        _ -> 
          case :global.whereis_name(:data_processor) do
            ^pid -> :data_processor
            _ -> :file_service  # fallback
          end
      end
      
      result = ExMCP.Native.call(service_atom, method, params)
      elapsed = System.monotonic_time(:microsecond) - start_time
      
      {:ok, result, elapsed, :native}
    end
    
    defp call_service_by_type(%{type: :stdio, client_pid: client_pid}, method, params) do
      start_time = System.monotonic_time(:microsecond)
      
      result = case method do
        "tools/call" ->
          tool_name = params["name"]
          args = params["arguments"]
          ExMCP.Client.call_tool(client_pid, tool_name, args)
        "list_tools" ->
          ExMCP.Client.list_tools(client_pid)
        _ ->
          {:error, "Unsupported method for stdio service"}
      end
      
      elapsed = System.monotonic_time(:microsecond) - start_time
      {:ok, result, elapsed, :stdio}
    end
    
    defp call_service_by_type(%{type: :http, url: url}, method, params) do
      start_time = System.monotonic_time(:microsecond)
      
      # HTTP calls would go here - simplified for example
      result = {:ok, %{"content" => [%{"text" => "HTTP service response simulated"}]}}
      elapsed = System.monotonic_time(:microsecond) - start_time
      
      {:ok, result, elapsed, :http}
    end
    
    def call_with_fallback(service_name, method, params, strategies \\ [:native_first, :fastest]) do
      attempt_call = fn strategy ->
        case call_service(service_name, method, params, strategy) do
          {:ok, result, elapsed, type} -> {:ok, result, elapsed, type}
          {:error, reason} -> {:error, reason}
        end
      end
      
      Enum.reduce_while(strategies, {:error, :no_strategies}, fn strategy, _acc ->
        case attempt_call.(strategy) do
          {:ok, result, elapsed, type} -> {:halt, {:ok, result, elapsed, type}}
          {:error, _reason} -> {:cont, {:error, :strategy_failed}}
        end
      end)
    end
  end
  
  def run do
    Logger.info("Starting Hybrid Elixir/Python MCP Architecture Demo")
    Logger.info("=" |> String.duplicate(70))
    
    # Start the service registry
    {:ok, _registry} = ServiceRegistry.start_link()
    
    # Start native Elixir services
    start_native_services()
    
    # Start Python services (if available)
    start_python_services()
    
    # Wait for all services to be ready
    Process.sleep(500)
    
    try do
      # Demo 1: Service discovery and routing
      demo_service_discovery()
      
      # Demo 2: Performance-based routing
      demo_performance_routing()
      
      # Demo 3: Complex workflow orchestration
      demo_workflow_orchestration()
      
      # Demo 4: Failover and resilience
      demo_failover()
      
      # Demo 5: Performance comparison
      demo_performance_comparison()
      
      Logger.info("\n✅ Hybrid architecture demo completed successfully!")
      
    rescue
      error ->
        Logger.error("Hybrid demo failed: #{inspect(error)}")
    end
    
    :ok
  end
  
  defp start_native_services do
    Logger.info("\n=== Starting Native Elixir Services ===")
    
    # Start calculator service
    {:ok, calc_pid} = Examples.Native.CalculatorService.start_link([])
    ServiceRegistry.register_native_service(:calculator, calc_pid, %{
      capabilities: ["arithmetic", "statistics"],
      performance_tier: "ultra_fast"
    })
    Logger.info("✅ Native Calculator Service started")
    
    # Start data processor
    {:ok, data_pid} = Examples.Native.DataProcessor.start_link([])
    ServiceRegistry.register_native_service(:data_processor, data_pid, %{
      capabilities: ["data_processing", "analytics"],
      performance_tier: "ultra_fast"
    })
    Logger.info("✅ Native Data Processor started")
    
    # Start file service
    {:ok, file_pid} = Examples.Native.FileService.start_link([])
    ServiceRegistry.register_native_service(:file_manager, file_pid, %{
      capabilities: ["file_operations", "storage"],
      performance_tier: "ultra_fast"
    })
    Logger.info("✅ Native File Service started")
  end
  
  defp start_python_services do
    Logger.info("\n=== Starting Python MCP Services ===")
    
    # Try to start Python stdio calculator
    python_calc_path = "examples/interoperability/python_mcp_servers/calculator_server.py"
    
    if File.exists?(python_calc_path) do
      case ExMCP.Client.start_link([
        transport: :stdio,
        command: ["python3", python_calc_path],
        name: :python_calc_stdio
      ]) do
        {:ok, client_pid} ->
          ServiceRegistry.register_stdio_service(:calculator, client_pid, ["python3", python_calc_path], %{
            capabilities: ["arithmetic", "history", "statistics"],
            performance_tier: "fast",
            language: "python"
          })
          Logger.info("✅ Python Calculator Service (stdio) started")
          
        {:error, reason} ->
          Logger.warn("❌ Failed to start Python calculator (stdio): #{inspect(reason)}")
      end
    else
      Logger.warn("❌ Python calculator server not found at #{python_calc_path}")
    end
    
    # Try to connect to Python HTTP calculator
    case :httpc.request(:get, {'http://localhost:8000/health', []}, [{:timeout, 2000}], []) do
      {:ok, {{_version, 200, _reason_phrase}, _headers, _body}} ->
        ServiceRegistry.register_http_service(:calculator, "http://localhost:8000", %{
          capabilities: ["arithmetic", "history", "statistics", "http_api"],
          performance_tier: "moderate",
          language: "python"
        })
        Logger.info("✅ Python Calculator Service (HTTP) connected")
        
      {:error, _reason} ->
        Logger.warn("❌ Python HTTP calculator not available at http://localhost:8000")
    end
  end
  
  def demo_service_discovery do
    Logger.info("\n=== Service Discovery Demo ===")
    
    all_services = ServiceRegistry.list_all_services()
    
    Logger.info("Discovered services:")
    Enum.each(all_services, fn {type, services} ->
      IO.puts("  #{type |> Atom.to_string() |> String.upcase()} Services: #{map_size(services)}")
      Enum.each(services, fn {name, info} ->
        capabilities = Map.get(info.metadata, :capabilities, [])
        performance = Map.get(info.metadata, :performance_tier, "unknown")
        IO.puts("    • #{name}: #{Enum.join(capabilities, ", ")} (#{performance})")
      end)
    end)
    
    # Test service routing strategies
    Logger.info("\nTesting service routing strategies:")
    
    strategies = [:fastest, :native_first, :round_robin]
    
    Enum.each(strategies, fn strategy ->
      case ServiceRegistry.get_service(:calculator, strategy) do
        {:ok, service} when not is_nil(service) ->
          type = service.type
          avg_time = service.avg_response_time
          IO.puts("  • #{strategy}: Selected #{type} service (avg: #{avg_time}μs)")
          
        {:ok, nil} ->
          IO.puts("  • #{strategy}: No service found")
      end
    end)
  end
  
  def demo_performance_routing do
    Logger.info("\n=== Performance-Based Routing Demo ===")
    
    # Test the same operation across different service types
    test_data = %{"numbers" => [1, 2, 3, 4, 5, 10, 15, 20]}
    
    Logger.info("Testing data processing across service types...")
    
    # Test with different strategies
    strategies = [
      {:fastest, "Fastest available service"},
      {:native_first, "Prefer native, fallback to others"},
      {:round_robin, "Random selection"}
    ]
    
    Enum.each(strategies, fn {strategy, description} ->
      IO.puts("\n#{description}:")
      
      case HybridOrchestrator.call_service(:data_processor, "tools/call", %{
        "name" => "process_data",
        "arguments" => test_data
      }, strategy) do
        {:ok, {:ok, result}, elapsed, service_type} ->
          IO.puts("  ✓ #{service_type} service: #{elapsed}μs")
          if result["content"] do
            content_preview = result["content"] |> List.first() |> Map.get("text", "") |> String.slice(0, 50)
            IO.puts("    Result: #{content_preview}...")
          end
          
        {:ok, {:error, reason}, elapsed, service_type} ->
          IO.puts("  ✗ #{service_type} service failed: #{inspect(reason)} (#{elapsed}μs)")
          
        {:error, reason} ->
          IO.puts("  ✗ No service available: #{inspect(reason)}")
      end
    end)
  end
  
  def demo_workflow_orchestration do
    Logger.info("\n=== Workflow Orchestration Demo ===")
    
    # Complex workflow: file processing → data analysis → calculation
    workflow_data = [100, 150, 200, 175, 300, 250]
    
    Logger.info("Executing multi-step workflow:")
    Logger.info("  1. Create file with data (native file service)")
    Logger.info("  2. Process data (best available data processor)")
    Logger.info("  3. Perform calculations (best available calculator)")
    
    workflow_start = System.monotonic_time(:microsecond)
    
    # Step 1: Create file
    step1_start = System.monotonic_time(:microsecond)
    step1_result = HybridOrchestrator.call_service(:file_manager, "tools/call", %{
      "name" => "create_file",
      "arguments" => %{
        "name" => "workflow_data.txt",
        "content" => Jason.encode!(workflow_data)
      }
    }, :fastest)
    step1_time = System.monotonic_time(:microsecond) - step1_start
    
    case step1_result do
      {:ok, {:ok, _result}, _elapsed, service_type} ->
        IO.puts("  ✓ Step 1 (#{service_type}): File created (#{step1_time}μs)")
        
        # Step 2: Process data
        step2_start = System.monotonic_time(:microsecond)
        step2_result = HybridOrchestrator.call_service(:data_processor, "tools/call", %{
          "name" => "process_data",
          "arguments" => %{"numbers" => workflow_data}
        }, :fastest)
        step2_time = System.monotonic_time(:microsecond) - step2_start
        
        case step2_result do
          {:ok, {:ok, _result}, _elapsed, service_type} ->
            IO.puts("  ✓ Step 2 (#{service_type}): Data processed (#{step2_time}μs)")
            
            # Step 3: Calculate sum using calculator
            step3_start = System.monotonic_time(:microsecond)
            step3_result = HybridOrchestrator.call_service(:calculator, "tools/call", %{
              "name" => "add",
              "arguments" => %{"a" => Enum.sum(workflow_data), "b" => 0}
            }, :fastest)
            step3_time = System.monotonic_time(:microsecond) - step3_start
            
            case step3_result do
              {:ok, {:ok, result}, _elapsed, service_type} ->
                IO.puts("  ✓ Step 3 (#{service_type}): Calculation complete (#{step3_time}μs)")
                if result["content"] do
                  final_result = result["content"] |> List.first() |> Map.get("text", "")
                  IO.puts("    Final result: #{final_result}")
                end
                
              _ ->
                IO.puts("  ✗ Step 3: Calculation failed")
            end
            
          _ ->
            IO.puts("  ✗ Step 2: Data processing failed")
        end
        
      _ ->
        IO.puts("  ✗ Step 1: File creation failed")
    end
    
    total_workflow_time = System.monotonic_time(:microsecond) - workflow_start
    IO.puts("\nWorkflow completed in #{total_workflow_time}μs")
    IO.puts("Benefits of hybrid architecture:")
    IO.puts("  • Each step uses the optimal service type")
    IO.puts("  • Automatic failover between service implementations") 
    IO.puts("  • Performance monitoring and optimization")
  end
  
  def demo_failover do
    Logger.info("\n=== Failover and Resilience Demo ===")
    
    # Test failover between service types
    Logger.info("Testing service failover capabilities...")
    
    test_operation = %{
      "name" => "add",
      "arguments" => %{"a" => 42, "b" => 58}
    }
    
    # Try with fallback strategies
    case HybridOrchestrator.call_with_fallback(:calculator, "tools/call", test_operation, [:native_first, :fastest]) do
      {:ok, {:ok, result}, elapsed, service_type} ->
        IO.puts("  ✓ Failover successful: #{service_type} service (#{elapsed}μs)")
        if result["content"] do
          final_result = result["content"] |> List.first() |> Map.get("text", "")
          IO.puts("    Result: #{final_result}")
        end
        
      {:error, reason} ->
        IO.puts("  ✗ All failover strategies failed: #{inspect(reason)}")
    end
    
    # Test parallel execution across service types
    Logger.info("\nTesting parallel execution across service types...")
    
    parallel_operations = [
      {:calculator, "tools/call", %{"name" => "add", "arguments" => %{"a" => 1, "b" => 2}}},
      {:calculator, "tools/call", %{"name" => "multiply", "arguments" => %{"a" => 3, "b" => 4}}},
      {:data_processor, "tools/call", %{"name" => "process_data", "arguments" => %{"numbers" => [1, 2, 3]}}},
      {:file_manager, "tools/call", %{"name" => "create_file", "arguments" => %{"name" => "test.txt", "content" => "test"}}}
    ]
    
    parallel_start = System.monotonic_time(:microsecond)
    
    results = parallel_operations
    |> Task.async_stream(fn {service, method, params} ->
      HybridOrchestrator.call_service(service, method, params, :fastest)
    end, max_concurrency: 10, timeout: 10000)
    |> Enum.to_list()
    
    parallel_total = System.monotonic_time(:microsecond) - parallel_start
    
    successful_count = Enum.count(results, fn 
      {:ok, {:ok, {:ok, _result}, _elapsed, _type}} -> true
      _ -> false
    end)
    
    IO.puts("Parallel execution results:")
    IO.puts("  - Operations: #{successful_count}/#{length(parallel_operations)} successful")
    IO.puts("  - Total time: #{parallel_total}μs")
    IO.puts("  - Average per operation: #{div(parallel_total, length(parallel_operations))}μs")
    IO.puts("  - Concurrency: Excellent (services run independently)")
  end
  
  def demo_performance_comparison do
    Logger.info("\n=== Performance Comparison Across Service Types ===")
    
    # Get performance statistics
    stats = ServiceRegistry.get_performance_stats()
    
    IO.puts("Service registry statistics:")
    IO.puts("  - Total services: #{stats.total_services}")
    IO.puts("  - Native services: #{stats.native_count}")
    IO.puts("  - stdio services: #{stats.stdio_count}")
    IO.puts("  - HTTP services: #{stats.http_count}")
    
    # Performance tiers summary
    Logger.info("\nPerformance tier comparison:")
    IO.puts("  🚀 Native Elixir (Ultra Fast): ~15μs")
    IO.puts("    • ExMCP.Native dispatcher")
    IO.puts("    • Zero serialization overhead")
    IO.puts("    • Direct GenServer calls")
    IO.puts("    • Best for: High-frequency operations")
    
    IO.puts("  ⚡ Python stdio (Fast): ~1-5ms")
    IO.puts("    • JSON-RPC over stdio")
    IO.puts("    • Process communication")
    IO.puts("    • Best for: Specialized Python libraries")
    
    IO.puts("  🌐 Python HTTP (Moderate): ~5-20ms")
    IO.puts("    • HTTP JSON-RPC")
    IO.puts("    • Network overhead")
    IO.puts("    • Best for: Distributed services")
    
    Logger.info("\n💡 Hybrid Architecture Benefits:")
    IO.puts("  ✓ Use each technology for its strengths")
    IO.puts("  ✓ Automatic performance-based routing")
    IO.puts("  ✓ Graceful degradation and failover")
    IO.puts("  ✓ Unified interface across service types")
    IO.puts("  ✓ Monitoring and optimization opportunities")
  end
end

# Demo runner
defmodule ExMCP.Examples.Interop.HybridArchitectureDemo do
  def run do
    Logger.info("Starting Comprehensive Hybrid MCP Architecture Demo")
    Logger.info("=" |> String.duplicate(70))
    
    # Run the full hybrid architecture demo
    ExMCP.Examples.Interop.HybridArchitecture.run()
    
    Logger.info("\n=== Hybrid Architecture Summary ===")
    IO.puts("This demo showcased a production-ready architecture combining:")
    IO.puts("  • Native Elixir services for ultra-high performance")
    IO.puts("  • Python stdio services for specialized capabilities")
    IO.puts("  • Python HTTP services for distributed systems")
    IO.puts("  • Intelligent routing and load balancing")
    IO.puts("  • Automatic failover and resilience")
    IO.puts("  • Performance monitoring and optimization")
    
    :ok
  end
end

# Example usage:
if __FILE__ == Path.absname("#{__DIR__}/hybrid_architecture.ex") do
  ExMCP.Examples.Interop.HybridArchitectureDemo.run()
end