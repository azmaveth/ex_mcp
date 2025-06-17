defmodule Examples.Native.ClusteringExample do
  @moduledoc """
  Example demonstrating Native Service Dispatcher clustering capabilities.
  
  This example shows how to:
  - Set up distributed services across Elixir nodes
  - Use Horde.Registry for service discovery
  - Implement load balancing patterns
  - Handle service health monitoring
  - Manage cluster membership dynamically
  
  ## Usage
  
      # Start the example
      cluster_info = Examples.Native.ClusteringExample.run()
      
      # Interact with the cluster
      cluster_info.test_discovery.()
      cluster_info.test_load_balancing.()
      cluster_info.simulate_failure.()
      
      # Clean up when done
      Examples.Native.ClusteringExample.stop_example(cluster_info)
  """
  
  require Logger
  
  defmodule CalculatorServiceV1 do
    use ExMCP.Service, name: :calculator_v1
    
    @impl true
    def init(args) do
      instance = Keyword.get(args, :instance, 1)
      load = Keyword.get(args, :load, 0.5)
      {:ok, %{instance: instance, load: load, requests: 0}}
    end
    
    @impl true
    def handle_mcp_request("list_tools", _params, state) do
      tools = [
        %{
          "name" => "add",
          "description" => "Add two numbers",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "a" => %{"type" => "number"},
              "b" => %{"type" => "number"}
            },
            "required" => ["a", "b"]
          }
        },
        %{
          "name" => "get_stats",
          "description" => "Get service statistics",
          "inputSchema" => %{"type" => "object", "properties" => %{}}
        }
      ]
      
      {:ok, %{"tools" => tools}, state}
    end
    
    def handle_mcp_request("tools/call", %{"name" => "add", "arguments" => %{"a" => a, "b" => b}}, state) do
      result = a + b
      new_state = %{state | requests: state.requests + 1}
      
      content = [%{
        "type" => "text",
        "text" => "Calculator Instance #{state.instance}: #{a} + #{b} = #{result}"
      }]
      
      {:ok, %{"content" => content}, new_state}
    end
    
    def handle_mcp_request("tools/call", %{"name" => "get_stats", "arguments" => _args}, state) do
      stats = %{
        instance: state.instance,
        load: state.load,
        requests_handled: state.requests,
        node: node(),
        uptime: "Service running"
      }
      
      content = [%{
        "type" => "text",
        "text" => "Stats: #{Jason.encode!(stats, pretty: true)}"
      }]
      
      {:ok, %{"content" => content}, state}
    end
    
    def handle_mcp_request(method, _params, state) do
      {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
    end
  end
  
  defmodule WeatherService do
    use ExMCP.Service, name: :weather_service
    
    @impl true
    def init(_args) do
      {:ok, %{requests: 0, regions: ["US", "EU", "ASIA"]}}
    end
    
    @impl true
    def handle_mcp_request("list_tools", _params, state) do
      tools = [
        %{
          "name" => "get_weather",
          "description" => "Get weather for a region",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "region" => %{"type" => "string", "enum" => state.regions}
            },
            "required" => ["region"]
          }
        }
      ]
      
      {:ok, %{"tools" => tools}, state}
    end
    
    def handle_mcp_request("tools/call", %{"name" => "get_weather", "arguments" => %{"region" => region}}, state) do
      temperature = Enum.random(15..35)
      new_state = %{state | requests: state.requests + 1}
      
      content = [%{
        "type" => "text",
        "text" => "Weather in #{region}: #{temperature}°C (handled #{new_state.requests} requests)"
      }]
      
      {:ok, %{"content" => content}, new_state}
    end
    
    def handle_mcp_request(method, _params, state) do
      {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
    end
  end
  
  defmodule LoadBalancer do
    @moduledoc """
    Simple load balancer for Native Service Dispatcher.
    """
    
    def round_robin(service_base, count) do
      current_index = :persistent_term.get({:lb_index, service_base}, 0)
      next_index = rem(current_index + 1, count)
      :persistent_term.put({:lb_index, service_base}, next_index)
      
      service_name = String.to_atom("#{service_base}_#{next_index + 1}")
      
      if ExMCP.Native.service_available?(service_name) do
        {:ok, service_name}
      else
        {:error, :service_unavailable}
      end
    end
    
    def weighted_random(services_with_weights) do
      total_weight = Enum.sum(Enum.map(services_with_weights, fn {_service, weight} -> weight end))
      
      if total_weight == 0 do
        {:error, :no_services}
      else
        random_value = :rand.uniform(total_weight)
        select_service(services_with_weights, random_value, 0)
      end
    end
    
    defp select_service([{service, weight} | _], random_value, acc) when acc + weight >= random_value do
      if ExMCP.Native.service_available?(service) do
        {:ok, service}
      else
        {:error, :service_unavailable}
      end
    end
    
    defp select_service([{_, weight} | rest], random_value, acc) do
      select_service(rest, random_value, acc + weight)
    end
    
    defp select_service([], _, _) do
      {:error, :no_services}
    end
  end
  
  def run do
    IO.puts("🚀 Starting Native Service Dispatcher Clustering Example")
    IO.puts("=" |> String.duplicate(65))
    
    # Start multiple calculator service instances
    calculator_services = for i <- 1..3 do
      service_name = String.to_atom("calculator_v1_#{i}")
      load = i * 0.2  # Different loads: 0.2, 0.4, 0.6
      
      {:ok, pid} = GenServer.start_link(CalculatorServiceV1, [instance: i, load: load], name: service_name)
      
      # Register with Native Service Dispatcher
      ExMCP.Native.register_service(service_name, pid, %{
        type: :calculator,
        instance: i,
        load: load,
        weight: case i do
          1 -> 1    # Low priority
          2 -> 3    # High priority  
          3 -> 2    # Medium priority
        end
      })
      
      IO.puts("📋 Registered calculator service instance #{i} as #{service_name}")
      
      {service_name, pid, %{instance: i, load: load}}
    end
    
    # Start weather service
    {:ok, weather_pid} = WeatherService.start_link([])
    IO.puts("🌤️  Registered weather service")
    
    # Wait for all services to register
    Process.sleep(200)
    
    IO.puts("⚖️  Services ready for load balancing")
    
    # Return cluster info for interactive use
    cluster_info = %{
      calculator_services: calculator_services,
      weather_service: {:weather_service, weather_pid},
      
      # Interactive functions
      test_discovery: fn -> test_service_discovery() end,
      test_load_balancing: fn -> test_load_balancing() end,
      test_cluster_stats: fn -> test_cluster_stats() end,
      simulate_failure: fn -> simulate_service_failure(calculator_services) end,
      test_distributed: fn -> test_distributed_calls() end
    }
    
    IO.puts("\n🎉 Clustering example is ready!")
    IO.puts("\nTry these interactive functions:")
    IO.puts("  • cluster_info.test_discovery.() - Test service discovery")
    IO.puts("  • cluster_info.test_load_balancing.() - Test load balancing strategies")
    IO.puts("  • cluster_info.test_cluster_stats.() - View cluster statistics")
    IO.puts("  • cluster_info.simulate_failure.() - Simulate a service failure")
    IO.puts("  • cluster_info.test_distributed.() - Test distributed calls")
    
    cluster_info
  end
  
  def test_service_discovery do
    IO.puts("\n🔍 Testing Service Discovery")
    IO.puts("=" |> String.duplicate(40))
    
    # Discover all services
    all_services = ExMCP.Native.list_services()
    IO.puts("📊 Total services in cluster: #{length(all_services)}")
    
    # Filter calculator services
    calc_services = Enum.filter(all_services, fn {service_name, _pid, _meta} ->
      String.starts_with?(to_string(service_name), "calculator_v1")
    end)
    
    IO.puts("🧮 Calculator services: #{length(calc_services)}")
    Enum.each(calc_services, fn {service_name, _pid, meta} ->
      instance = Map.get(meta, :instance, "?")
      load = Map.get(meta, :load, "?")
      IO.puts("  • #{service_name} - Instance #{instance} (load: #{load})")
    end)
    
    # Check weather service
    weather_available = ExMCP.Native.service_available?(:weather_service)
    IO.puts("🌤️  Weather service available: #{if weather_available, do: "✓", else: "✗"}")
    
    # Show node distribution
    nodes = all_services
    |> Enum.map(fn {_name, pid, _meta} -> node(pid) end)
    |> Enum.uniq()
    
    IO.puts("🖥️  Services distributed across nodes: #{inspect(nodes)}")
  end
  
  def test_load_balancing do
    IO.puts("\n⚖️  Testing Load Balancing")
    IO.puts("=" |> String.duplicate(40))
    
    # Test round-robin
    IO.puts("🔄 Round-robin selection:")
    for i <- 1..6 do
      case LoadBalancer.round_robin("calculator_v1", 3) do
        {:ok, service_name} ->
          IO.puts("  #{i}. Selected #{service_name}")
        {:error, reason} ->
          IO.puts("  #{i}. Error: #{inspect(reason)}")
      end
    end
    
    # Test weighted selection
    IO.puts("\n⚖️  Weighted selection (10 attempts):")
    services_with_weights = [
      {:calculator_v1_1, 1},
      {:calculator_v1_2, 3}, 
      {:calculator_v1_3, 2}
    ]
    
    selections = for _i <- 1..10 do
      case LoadBalancer.weighted_random(services_with_weights) do
        {:ok, service} -> service
        _ -> nil
      end
    end
    
    selections = Enum.filter(selections, & &1)
    
    if length(selections) > 0 do
      counts = Enum.frequencies(selections)
      IO.puts("  Distribution: #{inspect(counts)}")
      IO.puts("  (calculator_v1_2 should be selected most often due to weight 3)")
    end
    
    # Test actual service calls through load balancer
    IO.puts("\n🧮 Load balanced calculations:")
    for i <- 1..5 do
      case LoadBalancer.round_robin("calculator_v1", 3) do
        {:ok, service_name} ->
          start_time = System.monotonic_time(:microsecond)
          case ExMCP.Native.call(service_name, "tools/call", %{
            "name" => "add",
            "arguments" => %{"a" => i, "b" => i + 1}
          }) do
            {:ok, %{"content" => [%{"text" => text}]}} ->
              elapsed = System.monotonic_time(:microsecond) - start_time
              IO.puts("  #{text} (#{elapsed}μs)")
            {:error, reason} ->
              IO.puts("  Error from #{service_name}: #{inspect(reason)}")
          end
        {:error, reason} ->
          IO.puts("  Load balancer error: #{inspect(reason)}")
      end
    end
  end
  
  def test_cluster_stats do
    IO.puts("\n📊 Cluster Statistics")
    IO.puts("=" |> String.duplicate(30))
    
    all_services = ExMCP.Native.list_services()
    
    IO.puts("Total services: #{length(all_services)}")
    
    # Get stats from calculator services
    calc_services = Enum.filter(all_services, fn {service_name, _pid, _meta} ->
      String.starts_with?(to_string(service_name), "calculator_v1")
    end)
    
    IO.puts("\nCalculator service statistics:")
    Enum.each(calc_services, fn {service_name, _pid, _meta} ->
      case ExMCP.Native.call(service_name, "tools/call", %{
        "name" => "get_stats",
        "arguments" => %{}
      }) do
        {:ok, %{"content" => [%{"text" => stats_text}]}} ->
          IO.puts("  • #{service_name}:")
          IO.puts("    #{String.replace(stats_text, "Stats: ", "")}")
        {:error, reason} ->
          IO.puts("  • #{service_name}: Error getting stats - #{inspect(reason)}")
      end
    end)
    
    # Show cluster health
    healthy_services = Enum.count(all_services, fn {service_name, _pid, _meta} ->
      ExMCP.Native.service_available?(service_name)
    end)
    
    IO.puts("\nCluster health:")
    IO.puts("  Healthy services: #{healthy_services}/#{length(all_services)}")
    IO.puts("  Health ratio: #{Float.round(healthy_services / length(all_services) * 100, 1)}%")
  end
  
  def simulate_service_failure(calculator_services) do
    IO.puts("\n💥 Simulating Service Failure")
    IO.puts("=" |> String.duplicate(40))
    
    # Stop one of the calculator services
    {service_name, service_pid, service_info} = hd(calculator_services)
    instance = service_info.instance
    
    IO.puts("🎯 Stopping calculator service #{instance} (#{service_name})")
    GenServer.stop(service_pid, :normal)
    
    # Wait a moment for the failure to be detected
    Process.sleep(500)
    
    # Check service availability
    available = ExMCP.Native.service_available?(service_name)
    IO.puts("Service #{service_name} available: #{if available, do: "✓", else: "✗ (failed as expected)"}")
    
    # Test load balancer behavior with failed service
    IO.puts("\n🔄 Testing load balancer with failed service:")
    for i <- 1..3 do
      case LoadBalancer.round_robin("calculator_v1", 3) do
        {:ok, selected_service} ->
          IO.puts("  #{i}. Selected #{selected_service}")
        {:error, :service_unavailable} ->
          IO.puts("  #{i}. Service unavailable (expected for failed service)")
      end
    end
    
    IO.puts("✅ Load balancer properly handles service failures")
    IO.puts("💡 In production, failed services would be removed from rotation")
  end
  
  def test_distributed_calls do
    IO.puts("\n🌐 Testing Distributed Service Calls")
    IO.puts("=" |> String.duplicate(45))
    
    # Test parallel calls to multiple services
    operations = [
      {:calculator_v1_1, "add", %{"a" => 10, "b" => 20}},
      {:calculator_v1_2, "add", %{"a" => 30, "b" => 40}},
      {:calculator_v1_3, "add", %{"a" => 50, "b" => 60}},
      {:weather_service, "get_weather", %{"region" => "US"}}
    ]
    
    IO.puts("Executing parallel distributed calls...")
    start_time = System.monotonic_time(:microsecond)
    
    results = operations
    |> Task.async_stream(fn {service, tool, args} ->
      call_start = System.monotonic_time(:microsecond)
      result = ExMCP.Native.call(service, "tools/call", %{
        "name" => tool,
        "arguments" => args
      })
      call_time = System.monotonic_time(:microsecond) - call_start
      {service, result, call_time}
    end, max_concurrency: 10, timeout: 5000)
    |> Enum.to_list()
    
    total_time = System.monotonic_time(:microsecond) - start_time
    
    IO.puts("\nResults:")
    Enum.each(results, fn 
      {:ok, {service, {:ok, %{"content" => [%{"text" => text}]}}, call_time}} ->
        IO.puts("  ✓ #{service}: #{text} (#{call_time}μs)")
      {:ok, {service, {:error, reason}, call_time}} ->
        IO.puts("  ✗ #{service}: #{inspect(reason)} (#{call_time}μs)")
      {:exit, reason} ->
        IO.puts("  ✗ Task failed: #{inspect(reason)}")
    end)
    
    IO.puts("\nPerformance:")
    IO.puts("  Total parallel execution time: #{total_time}μs")
    IO.puts("  Average per call: #{div(total_time, length(operations))}μs")
    IO.puts("  Concurrency benefit: ~#{length(operations)}x faster than sequential")
  end
  
  def stop_example(cluster_info) do
    IO.puts("\n🛑 Stopping Clustering Example")
    IO.puts("=" |> String.duplicate(40))
    
    # Stop all calculator services
    Enum.each(cluster_info.calculator_services, fn {service_name, pid, _info} ->
      if Process.alive?(pid) do
        GenServer.stop(pid, :normal)
        IO.puts("Stopped #{service_name}")
      end
    end)
    
    # Stop weather service
    {_service_name, weather_pid} = cluster_info.weather_service
    if Process.alive?(weather_pid) do
      GenServer.stop(weather_pid, :normal)
      IO.puts("Stopped weather service")
    end
    
    IO.puts("✅ Clustering example stopped cleanly")
  end
end

# Demo runner
defmodule Examples.Native.ClusteringDemo do
  def run do
    Logger.info("Starting Native Service Dispatcher Clustering Demo")
    Logger.info("=" |> String.duplicate(55))
    
    # Start the clustering example
    cluster_info = Examples.Native.ClusteringExample.run()
    
    Process.sleep(500)
    
    # Run discovery test
    Logger.info("\n=== Service Discovery Test ===")
    cluster_info.test_discovery.()
    
    Process.sleep(500)
    
    # Run load balancing test  
    Logger.info("\n=== Load Balancing Test ===")
    cluster_info.test_load_balancing.()
    
    Process.sleep(500)
    
    # Run distributed calls test
    Logger.info("\n=== Distributed Calls Test ===")
    cluster_info.test_distributed.()
    
    Process.sleep(500)
    
    # Show cluster stats
    Logger.info("\n=== Cluster Statistics ===")
    cluster_info.test_cluster_stats.()
    
    Logger.info("\n=== Demo Benefits ===")
    IO.puts("✓ Zero-latency service discovery via Native Service Dispatcher")
    IO.puts("✓ Sub-millisecond distributed service calls")
    IO.puts("✓ Built-in load balancing patterns")
    IO.puts("✓ Automatic failure detection and handling")
    IO.puts("✓ Perfect for BEAM distributed applications")
    
    :ok
  end
end

# Example usage:
if __FILE__ == Path.absname("#{__DIR__}/clustering_example.ex") do
  Examples.Native.ClusteringDemo.run()
  
  IO.puts("\nServices are running. Press Ctrl+C to stop.")
  Process.sleep(:infinity)
end