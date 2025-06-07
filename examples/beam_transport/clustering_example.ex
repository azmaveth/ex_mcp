defmodule ExMCP.Examples.BeamTransport.ClusteringExample do
  @moduledoc """
  Example demonstrating BEAM transport clustering capabilities.
  
  This example shows how to:
  - Set up a distributed cluster of MCP services
  - Register and discover services across the cluster
  - Use load balancing to distribute requests
  - Handle node failures and service health monitoring
  - Manage cluster membership dynamically
  
  ## Usage
  
      # Start the example
      cluster_info = ExMCP.Examples.BeamTransport.ClusteringExample.run()
      
      # Interact with the cluster
      cluster_info.test_discovery.()
      cluster_info.test_load_balancing.()
      cluster_info.simulate_failure.()
      
      # Clean up when done
      ExMCP.Examples.BeamTransport.ClusteringExample.stop_example(cluster_info)
  """
  
  alias ExMCP.Transport.Beam.{Cluster, LoadBalancer}
  alias ExMCP.{Server, Client}
  
  def run do
    IO.puts("🚀 Starting BEAM Clustering Example")
    IO.puts("=====================================")
    
    # Start the main cluster coordinator
    {:ok, cluster} = Cluster.start_link(%{
      node_name: :main_cluster,
      discovery_strategy: :local_registry,
      health_check_enabled: true,
      health_check_interval: 10000,  # 10 seconds
      cluster_management: true
    })
    
    IO.puts("✅ Cluster coordinator started")
    
    # Register multiple calculator services (simulating different nodes)
    calculator_services = for i <- 1..3 do
      # Create a long-running process to simulate a service
      service_pid = spawn(fn ->
        receive_loop("Calculator#{i}")
      end)
      
      service_info = %{
        name: "calculator",
        version: "1.#{i}.0",
        capabilities: ["tools", "resources"],
        node: :"calc_node_#{i}",
        pid: service_pid,
        metadata: %{
          instance: i,
          load: :rand.uniform(100) / 100,
          connections: :rand.uniform(10),
          weight: case i do
            1 -> 1    # Low priority
            2 -> 3    # High priority  
            3 -> 2    # Medium priority
          end
        }
      }
      
      {:ok, service_id} = Cluster.register_service(cluster, service_info)
      IO.puts("📋 Registered calculator service #{i} (ID: #{service_id})")
      
      {service_id, service_pid, service_info}
    end
    
    # Register a weather service
    weather_pid = spawn(fn -> receive_loop("WeatherService") end)
    weather_info = %{
      name: "weather",
      version: "2.0.0",
      capabilities: ["tools"],
      node: :weather_node,
      pid: weather_pid,
      metadata: %{temperature_unit: "celsius", regions: ["US", "EU"]}
    }
    
    {:ok, weather_id} = Cluster.register_service(cluster, weather_info)
    IO.puts("🌤️  Registered weather service (ID: #{weather_id})")
    
    # Set up load balancer for calculator services
    {:ok, lb_round_robin} = LoadBalancer.start_link(%{
      cluster: cluster,
      strategy: :round_robin,
      health_aware: true
    })
    
    {:ok, lb_weighted} = LoadBalancer.start_link(%{
      cluster: cluster,
      strategy: :weighted,
      health_aware: true
    })
    
    {:ok, lb_least_conn} = LoadBalancer.start_link(%{
      cluster: cluster,
      strategy: :least_connections,
      health_aware: true
    })
    
    IO.puts("⚖️  Load balancers started (round-robin, weighted, least-connections)")
    
    # Add some cluster nodes
    Cluster.add_node(cluster, :compute_node_1, %{role: :compute, cpu_cores: 8})
    Cluster.add_node(cluster, :storage_node_1, %{role: :storage, disk_gb: 1000})
    
    IO.puts("🖥️  Added cluster nodes")
    
    # Return cluster info for interactive use
    cluster_info = %{
      cluster: cluster,
      calculator_services: calculator_services,
      weather_service: {weather_id, weather_pid, weather_info},
      load_balancers: %{
        round_robin: lb_round_robin,
        weighted: lb_weighted,
        least_connections: lb_least_conn
      },
      
      # Interactive functions
      test_discovery: fn -> test_service_discovery(cluster) end,
      test_load_balancing: fn -> test_load_balancing(lb_round_robin, lb_weighted, lb_least_conn) end,
      test_cluster_stats: fn -> test_cluster_stats(cluster) end,
      simulate_failure: fn -> simulate_service_failure(cluster, calculator_services) end,
      add_test_node: fn -> add_test_node(cluster) end,
      remove_test_node: fn -> remove_test_node(cluster) end
    }
    
    IO.puts("\n🎉 Clustering example is ready!")
    IO.puts("\nTry these interactive functions:")
    IO.puts("  • cluster_info.test_discovery.() - Test service discovery")
    IO.puts("  • cluster_info.test_load_balancing.() - Test load balancing strategies")
    IO.puts("  • cluster_info.test_cluster_stats.() - View cluster statistics")
    IO.puts("  • cluster_info.simulate_failure.() - Simulate a service failure")
    IO.puts("  • cluster_info.add_test_node.() - Add a new cluster node")
    IO.puts("  • cluster_info.remove_test_node.() - Remove a cluster node")
    
    cluster_info
  end
  
  def test_service_discovery(cluster) do
    IO.puts("\n🔍 Testing Service Discovery")
    IO.puts("============================")
    
    # Discover all services
    {:ok, all_services} = Cluster.discover_services(cluster)
    IO.puts("📊 Total services in cluster: #{length(all_services)}")
    
    # Discover calculator services
    {:ok, calc_services} = Cluster.discover_services(cluster, %{name: "calculator"})
    IO.puts("🧮 Calculator services: #{length(calc_services)}")
    
    Enum.each(calc_services, fn service ->
      IO.puts("  • Instance #{service.metadata.instance} on #{service.node} (load: #{service.metadata.load})")
    end)
    
    # Discover weather services
    {:ok, weather_services} = Cluster.discover_services(cluster, %{name: "weather"})
    IO.puts("🌤️  Weather services: #{length(weather_services)}")
    
    # Discover services by capability
    {:ok, tool_services} = Cluster.discover_services(cluster, %{capabilities: ["tools"]})
    IO.puts("🔧 Services with 'tools' capability: #{length(tool_services)}")
    
    # Discover services by node
    {:ok, node_services} = Cluster.discover_services(cluster, %{node: :calc_node_2})
    IO.puts("🖥️  Services on calc_node_2: #{length(node_services)}")
  end
  
  def test_load_balancing(lb_round_robin, lb_weighted, lb_least_conn) do
    IO.puts("\n⚖️  Testing Load Balancing")
    IO.puts("==========================")
    
    # Test round-robin
    IO.puts("🔄 Round-robin selection:")
    for i <- 1..6 do
      case LoadBalancer.get_service(lb_round_robin, "calculator") do
        {:ok, service} ->
          IO.puts("  #{i}. Selected instance #{service.metadata.instance} on #{service.node}")
        {:error, reason} ->
          IO.puts("  #{i}. Error: #{inspect(reason)}")
      end
    end
    
    # Test weighted
    IO.puts("\n⚖️  Weighted selection (10 attempts):")
    selections = for _i <- 1..10 do
      case LoadBalancer.get_service(lb_weighted, "calculator") do
        {:ok, service} -> service.metadata.instance
        _ -> nil
      end
    end
    
    selections = Enum.filter(selections, & &1)
    
    if length(selections) > 0 do
      counts = Enum.frequencies(selections)
      IO.puts("  Distribution: #{inspect(counts)}")
      IO.puts("  (Instance 2 should be selected most often due to weight 3)")
    end
    
    # Test least connections
    IO.puts("\n🔗 Least connections selection:")
    case LoadBalancer.get_service(lb_least_conn, "calculator") do
      {:ok, service} ->
        IO.puts("  Selected instance #{service.metadata.instance} with #{service.metadata.connections} connections")
      {:error, reason} ->
        IO.puts("  Error: #{inspect(reason)}")
    end
  end
  
  def test_cluster_stats(cluster) do
    IO.puts("\n📊 Cluster Statistics")
    IO.puts("=====================")
    
    {:ok, stats} = Cluster.get_stats(cluster)
    
    IO.puts("Cluster uptime: #{div(stats.uptime, 1000)} seconds")
    IO.puts("Total nodes: #{stats.node_count}")
    IO.puts("Services registered: #{stats.services_registered}")
    IO.puts("Discovery requests: #{stats.discovery_requests}")
    
    if Map.has_key?(stats, :registry_stats) do
      registry_stats = stats.registry_stats
      IO.puts("Active services: #{registry_stats.total_services}")
      IO.puts("Active nodes with services: #{registry_stats.active_nodes}")
    end
    
    # List cluster nodes
    {:ok, nodes} = Cluster.list_nodes(cluster)
    IO.puts("\nCluster nodes:")
    Enum.each(nodes, fn node ->
      IO.puts("  • #{node.name} (#{Map.get(node, :role, "unknown role")})")
    end)
  end
  
  def simulate_service_failure(cluster, calculator_services) do
    IO.puts("\n💥 Simulating Service Failure")
    IO.puts("==============================")
    
    # Kill one of the calculator services
    {service_id, service_pid, service_info} = hd(calculator_services)
    instance = service_info.metadata.instance
    
    IO.puts("🎯 Killing calculator service #{instance} (PID: #{inspect(service_pid)})")
    Process.exit(service_pid, :kill)
    
    # Wait a moment for the failure to be detected
    Process.sleep(500)
    
    # Check services again
    {:ok, services} = Cluster.discover_services(cluster, %{name: "calculator"})
    IO.puts("📊 Remaining calculator services: #{length(services)}")
    
    if length(services) < 3 do
      IO.puts("✅ Service failure detected and service removed from cluster")
    else
      IO.puts("⏳ Service failure not yet detected (health checks may be slower)")
    end
    
    IO.puts("💡 In a real system, clients would automatically failover to remaining services")
  end
  
  def add_test_node(cluster) do
    node_name = :"test_node_#{:rand.uniform(1000)}"
    
    :ok = Cluster.add_node(cluster, node_name, %{
      role: :test,
      added_at: DateTime.utc_now(),
      capabilities: ["testing", "temporary"]
    })
    
    IO.puts("➕ Added test node: #{node_name}")
    
    {:ok, nodes} = Cluster.list_nodes(cluster)
    IO.puts("📊 Total cluster nodes: #{length(nodes)}")
  end
  
  def remove_test_node(cluster) do
    {:ok, nodes} = Cluster.list_nodes(cluster)
    
    test_nodes = Enum.filter(nodes, fn node ->
      String.starts_with?(to_string(node.name), "test_node_")
    end)
    
    case test_nodes do
      [] ->
        IO.puts("ℹ️  No test nodes to remove")
        
      [node | _] ->
        :ok = Cluster.remove_node(cluster, node.name)
        IO.puts("➖ Removed test node: #{node.name}")
        
        {:ok, remaining_nodes} = Cluster.list_nodes(cluster)
        IO.puts("📊 Total cluster nodes: #{length(remaining_nodes)}")
    end
  end
  
  def stop_example(cluster_info) do
    IO.puts("\n🛑 Stopping Clustering Example")
    IO.puts("===============================")
    
    # Stop all calculator service processes
    Enum.each(cluster_info.calculator_services, fn {_id, pid, _info} ->
      if Process.alive?(pid) do
        Process.exit(pid, :normal)
      end
    end)
    
    # Stop weather service
    {_weather_id, weather_pid, _weather_info} = cluster_info.weather_service
    if Process.alive?(weather_pid) do
      Process.exit(weather_pid, :normal)
    end
    
    # Stop load balancers
    Enum.each(cluster_info.load_balancers, fn {_name, balancer} ->
      LoadBalancer.stop(balancer)
    end)
    
    # Stop cluster
    Cluster.stop(cluster_info.cluster)
    
    IO.puts("✅ Clustering example stopped cleanly")
  end
  
  # Helper function to simulate a service process
  defp receive_loop(service_name) do
    receive do
      {:health_check, ref} ->
        send(self(), {:reply, :healthy})
        receive_loop(service_name)
        
      :health_ping ->
        :pong
        receive_loop(service_name)
        
      {:echo, message} ->
        {:ok, "#{service_name} echoed: #{message}"}
        receive_loop(service_name)
        
      other ->
        IO.puts("#{service_name} received: #{inspect(other)}")
        receive_loop(service_name)
    after
      30000 ->  # 30 second timeout
        receive_loop(service_name)
    end
  end
end