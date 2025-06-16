defmodule Examples.NativeBeam.DistributedExample do
  @moduledoc """
  Example of using Native BEAM transport for distributed MCP services across Elixir nodes.
  
  This demonstrates:
  - Starting services on different nodes
  - Cross-node service discovery
  - Transparent distributed communication
  - Node clustering and fault tolerance
  
  To run this example:
  
  1. Start the first node:
     ```
     iex --name node1@localhost --cookie democookie -S mix
     iex> Examples.NativeBeam.DistributedExample.start_server_node()
     ```
  
  2. Start the second node and connect:
     ```
     iex --name node2@localhost --cookie democookie -S mix
     iex> Examples.NativeBeam.DistributedExample.start_client_node()
     ```
  
  Or run a single-node demo:
     ```
     iex> Examples.NativeBeam.DistributedExample.demo_single_node()
     ```
  """
  
  require Logger
  
  def start_server_node do
    Logger.info("Starting server node with calculator service...")
    
    # Start the calculator service on this node
    {:ok, _server_pid} = Examples.NativeBeam.CalculatorServer.start_link([])
    
    # Wait for service registration
    Process.sleep(100)
    
    Logger.info("Server node ready!")
    Logger.info("Calculator service available at: #{Node.self()}")
    
    # List services on this node
    services = ExMCP.Transport.Native.list_services()
    Logger.info("Services on this node:")
    for {service_name, _pid, _meta} <- services do
      Logger.info("  - #{service_name}")
    end
    
    Logger.info("Waiting for client connections...")
    :ok
  end
  
  def start_client_node do
    Logger.info("Starting client node...")
    
    # Connect to the server node
    server_node = :"node1@localhost"
    
    case Node.connect(server_node) do
      true ->
        Logger.info("Connected to server node: #{server_node}")
        
        # Wait a moment for node sync
        Process.sleep(200)
        
        # Try to call the remote calculator service
        remote_service_id = {:calculator_server, server_node}
        
        Logger.info("Testing cross-node service call...")
        
        case ExMCP.Transport.Native.call(remote_service_id, "tools/call", %{
          "name" => "add",
          "arguments" => %{"a" => 15, "b" => 25}
        }) do
          {:ok, result} ->
            text = result["content"] |> List.first() |> Map.get("text")
            Logger.info("Remote calculation result: #{text}")
            
          {:error, reason} ->
            Logger.error("Remote call failed: #{inspect(reason)}")
        end
        
        # Test with multiple operations
        operations = [
          {"multiply", %{"a" => 6, "b" => 7}},
          {"divide", %{"a" => 100, "b" => 4}},
          {"factorial", %{"n" => 6}}
        ]
        
        Logger.info("\nPerforming multiple remote operations...")
        for {op, params} <- operations do
          case ExMCP.Transport.Native.call(remote_service_id, "tools/call", %{
            "name" => op,
            "arguments" => params
          }) do
            {:ok, result} ->
              text = result["content"] |> List.first() |> Map.get("text")
              Logger.info("#{op}: #{text}")
              
            {:error, reason} ->
              Logger.error("#{op} failed: #{inspect(reason)}")
          end
        end
        
        Logger.info("\nClient node demo completed!")
        
      false ->
        Logger.error("Failed to connect to server node: #{server_node}")
        Logger.info("Make sure the server node is running with the same cookie")
    end
  end
  
  def demo_single_node do
    Logger.info("Starting single-node distributed demo...")
    
    # Simulate distributed services on the same node
    # Start multiple services
    {:ok, calc_pid} = Examples.NativeBeam.CalculatorServer.start_link([])
    {:ok, file_pid} = Examples.NativeBeam.SupervisorExample.FileService.start_link([])
    {:ok, proc_pid} = Examples.NativeBeam.SupervisorExample.DataProcessor.start_link([])
    
    # Wait for all services to register
    Process.sleep(200)
    
    # Discover all services
    services = ExMCP.Transport.Native.list_services()
    Logger.info("Discovered services:")
    for {service_name, pid, meta} <- services do
      node_name = if node(pid) == node(), do: "local", else: inspect(node(pid))
      registered_at = meta[:registered_at] || "unknown"
      Logger.info("  - #{service_name} (#{node_name}) - registered: #{registered_at}")
    end
    
    # Test service availability
    Logger.info("\nTesting service availability:")
    service_names = [:calculator_server, :file_service, :data_processor]
    for service <- service_names do
      available = ExMCP.Transport.Native.service_available?(service)
      Logger.info("  - #{service}: #{if available, do: "✓ available", else: "✗ unavailable"}")
    end
    
    # Test inter-service communication
    Logger.info("\n--- Testing Inter-service Communication ---")
    
    # File service creates some files
    Logger.info("Creating files...")
    files_to_create = [
      {"data1.txt", "1,2,3,4,5"},
      {"data2.txt", "10,20,30"},
      {"config.json", "{\"setting\": \"value\"}"}
    ]
    
    for {filename, content} <- files_to_create do
      {:ok, _} = ExMCP.Transport.Native.call(:file_service, "tools/call", %{
        "name" => "create_file",
        "arguments" => %{"name" => filename, "content" => content}
      })
    end
    
    # List files
    {:ok, result} = ExMCP.Transport.Native.call(:file_service, "tools/call", %{
      "name" => "list_files",
      "arguments" => %{}
    })
    Logger.info("Files created: #{result["content"] |> List.first() |> Map.get("text")}")
    
    # Data processor processes some numbers using calculator
    Logger.info("\nProcessing data using multiple services...")
    {:ok, result} = ExMCP.Transport.Native.call(:data_processor, "tools/call", %{
      "name" => "process_data",
      "arguments" => %{"numbers" => [10, 20, 30, 40, 50]}
    })
    Logger.info("Processing result: #{result["content"] |> List.first() |> Map.get("text")}")
    
    # Get stats
    {:ok, result} = ExMCP.Transport.Native.call(:data_processor, "tools/call", %{
      "name" => "get_stats",
      "arguments" => %{}
    })
    Logger.info("Processing stats: #{result["content"] |> List.first() |> Map.get("text")}")
    
    # Test notifications (fire-and-forget)
    Logger.info("\n--- Testing Notifications ---")
    
    # Send notifications to services (they won't handle them, but this shows the API)
    :ok = ExMCP.Transport.Native.notify(:calculator_server, "resource_updated", %{
      "uri" => "file:///config.json",
      "type" => "modified"
    })
    
    :ok = ExMCP.Transport.Native.notify(:file_service, "system_event", %{
      "event" => "backup_completed",
      "timestamp" => DateTime.utc_now()
    })
    
    Logger.info("Notifications sent successfully")
    
    # Cleanup
    GenServer.stop(calc_pid)
    GenServer.stop(file_pid) 
    GenServer.stop(proc_pid)
    
    Logger.info("\nSingle-node distributed demo completed!")
  end
  
  def demo_cluster_communication do
    Logger.info("Testing cluster communication features...")
    
    # Show current node info
    Logger.info("Current node: #{Node.self()}")
    Logger.info("Connected nodes: #{inspect(Node.list())}")
    
    # Start a service on this node
    {:ok, calc_pid} = Examples.NativeBeam.CalculatorServer.start_link([])
    Process.sleep(100)
    
    # Test local service call
    Logger.info("\nTesting local service call...")
    {:ok, result} = ExMCP.Transport.Native.call(:calculator_server, "tools/call", %{
      "name" => "add",
      "arguments" => %{"a" => 100, "b" => 200}
    })
    Logger.info("Local result: #{result["content"] |> List.first() |> Map.get("text")}")
    
    # If there are other nodes, test remote calls
    case Node.list() do
      [] ->
        Logger.info("No remote nodes connected for cross-node testing")
        
      remote_nodes ->
        Logger.info("Testing cross-node calls to: #{inspect(remote_nodes)}")
        
        for remote_node <- remote_nodes do
          remote_service_id = {:calculator_server, remote_node}
          
          case ExMCP.Transport.Native.call(remote_service_id, "tools/call", %{
            "name" => "multiply",
            "arguments" => %{"a" => 7, "b" => 8}
          }) do
            {:ok, result} ->
              text = result["content"] |> List.first() |> Map.get("text")
              Logger.info("Remote result from #{remote_node}: #{text}")
              
            {:error, reason} ->
              Logger.info("Remote call to #{remote_node} failed: #{inspect(reason)}")
          end
        end
    end
    
    # Cleanup
    GenServer.stop(calc_pid)
    
    Logger.info("Cluster communication demo completed!")
  end
end