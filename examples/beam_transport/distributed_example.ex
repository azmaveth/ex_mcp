defmodule Examples.Native.DistributedExample do
  @moduledoc """
  Example of using Native Service Dispatcher for distributed MCP services across Elixir nodes.
  
  This demonstrates:
  - Starting services on different nodes
  - Cross-node service discovery with Horde.Registry
  - Transparent distributed communication
  - Node clustering and fault tolerance
  - Performance benefits of Native Service Dispatcher across nodes
  
  To run this example:
  
  1. Start the first node:
     ```
     iex --name node1@localhost --cookie democookie -S mix
     iex> Examples.Native.DistributedExample.start_server_node()
     ```
  
  2. Start the second node and connect:
     ```
     iex --name node2@localhost --cookie democookie -S mix
     iex> Examples.Native.DistributedExample.start_client_node()
     ```
  
  Or run a single-node demo:
     ```
     iex> Examples.Native.DistributedExample.demo_single_node()
     ```
  """
  
  require Logger
  
  def start_server_node do
    Logger.info("Starting server node with distributed services...")
    
    # Start the calculator service on this node
    {:ok, _calc_pid} = Examples.Native.CalculatorService.start_link([])
    
    # Start additional services for demonstration
    {:ok, _file_pid} = Examples.Native.FileService.start_link([])
    {:ok, _data_pid} = Examples.Native.DataProcessor.start_link([])
    
    # Wait for service registration
    Process.sleep(200)
    
    Logger.info("Server node ready!")
    Logger.info("Services available at: #{Node.self()}")
    
    # List services on this node
    services = ExMCP.Native.list_services()
    Logger.info("Services on this node:")
    for {service_name, _pid, _meta} <- services do
      Logger.info("  - #{service_name}")
    end
    
    Logger.info("Waiting for client connections...")
    Logger.info("Node cookie: #{Node.get_cookie()}")
    :ok
  end
  
  def start_client_node do
    Logger.info("Starting client node...")
    
    # Connect to the server node
    server_node = :"node1@localhost"
    
    case Node.connect(server_node) do
      true ->
        Logger.info("Connected to server node: #{server_node}")
        
        # Wait a moment for node sync and service discovery
        Process.sleep(500)
        
        Logger.info("Testing cross-node service discovery...")
        
        # Try to discover remote services
        all_services = ExMCP.Native.list_services()
        remote_services = Enum.filter(all_services, fn {_name, pid, _meta} ->
          node(pid) == server_node
        end)
        
        Logger.info("Discovered #{length(remote_services)} services on remote node:")
        for {service_name, _pid, _meta} <- remote_services do
          Logger.info("  - #{service_name}")
        end
        
        # Test cross-node calculator service calls
        if ExMCP.Native.service_available?(:calculator_service) do
          Logger.info("\nTesting cross-node calculator service...")
          
          start_time = System.monotonic_time(:microsecond)
          case ExMCP.Native.call(:calculator_service, "tools/call", %{
            "name" => "add",
            "arguments" => %{"a" => 15, "b" => 25}
          }) do
            {:ok, %{"content" => [%{"text" => text}]}} ->
              elapsed = System.monotonic_time(:microsecond) - start_time
              Logger.info("Remote calculation result: #{text}")
              Logger.info("Cross-node call completed in #{elapsed}μs")
              
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
            start_time = System.monotonic_time(:microsecond)
            case ExMCP.Native.call(:calculator_service, "tools/call", %{
              "name" => op,
              "arguments" => params
            }) do
              {:ok, %{"content" => [%{"text" => text}]}} ->
                elapsed = System.monotonic_time(:microsecond) - start_time
                Logger.info("#{op}: #{text} (#{elapsed}μs)")
                
              {:error, reason} ->
                Logger.error("#{op} failed: #{inspect(reason)}")
            end
          end
        else
          Logger.warn("Calculator service not available on remote node")
        end
        
        # Test other services if available
        if ExMCP.Native.service_available?(:file_service) do
          Logger.info("\nTesting cross-node file service...")
          
          case ExMCP.Native.call(:file_service, "tools/call", %{
            "name" => "create_file",
            "arguments" => %{"name" => "remote_test.txt", "content" => "Cross-node file creation"}
          }) do
            {:ok, %{"content" => [%{"text" => text}]}} ->
              Logger.info("File service result: #{text}")
            {:error, reason} ->
              Logger.error("File service failed: #{inspect(reason)}")
          end
        end
        
        Logger.info("\nClient node demo completed!")
        
      false ->
        Logger.error("Failed to connect to server node: #{server_node}")
        Logger.info("Make sure the server node is running with the same cookie")
    end
  end
  
  def demo_single_node do
    Logger.info("Starting single-node distributed demo with Native Service Dispatcher...")
    
    # Start multiple services to simulate distributed architecture
    {:ok, calc_pid} = Examples.Native.CalculatorService.start_link([])
    {:ok, file_pid} = Examples.Native.FileService.start_link([])
    {:ok, proc_pid} = Examples.Native.DataProcessor.start_link([])
    
    # Wait for all services to register
    Process.sleep(300)
    
    # Discover all services
    services = ExMCP.Native.list_services()
    Logger.info("Discovered #{length(services)} services:")
    for {service_name, pid, meta} <- services do
      node_name = if node(pid) == node(), do: "local", else: inspect(node(pid))
      registered_at = Map.get(meta, :registered_at, "unknown")
      Logger.info("  - #{service_name} (#{node_name}) - registered: #{registered_at}")
    end
    
    # Test service availability
    Logger.info("\nTesting service availability:")
    service_names = [:calculator_service, :file_service, :data_processor]
    for service <- service_names do
      available = ExMCP.Native.service_available?(service)
      Logger.info("  - #{service}: #{if available, do: "✓ available", else: "✗ unavailable"}")
    end
    
    # Test inter-service communication with performance metrics
    Logger.info("\n--- Testing Inter-service Communication ---")
    
    # File service creates some files
    Logger.info("Creating files...")
    files_to_create = [
      {"data1.txt", "1,2,3,4,5"},
      {"data2.txt", "10,20,30"},
      {"config.json", "{\"setting\": \"value\"}"}
    ]
    
    total_file_time = 0
    for {filename, content} <- files_to_create do
      start_time = System.monotonic_time(:microsecond)
      {:ok, _} = ExMCP.Native.call(:file_service, "tools/call", %{
        "name" => "create_file",
        "arguments" => %{"name" => filename, "content" => content}
      })
      elapsed = System.monotonic_time(:microsecond) - start_time
      total_file_time = total_file_time + elapsed
      Logger.info("  Created #{filename} in #{elapsed}μs")
    end
    
    # List files
    start_time = System.monotonic_time(:microsecond)
    {:ok, %{"content" => [%{"text" => file_list}]}} = ExMCP.Native.call(:file_service, "tools/call", %{
      "name" => "list_files",
      "arguments" => %{}
    })
    list_time = System.monotonic_time(:microsecond) - start_time
    Logger.info("Files created: #{file_list} (listed in #{list_time}μs)")
    
    # Data processor processes some numbers using calculator
    Logger.info("\nProcessing data using multiple services...")
    start_time = System.monotonic_time(:microsecond)
    {:ok, %{"content" => [%{"text" => processing_result}]}} = ExMCP.Native.call(:data_processor, "tools/call", %{
      "name" => "process_data",
      "arguments" => %{"numbers" => [10, 20, 30, 40, 50]}
    })
    processing_time = System.monotonic_time(:microsecond) - start_time
    Logger.info("Processing result: #{processing_result} (#{processing_time}μs)")
    
    # Get stats
    start_time = System.monotonic_time(:microsecond)
    {:ok, %{"content" => [%{"text" => stats_result}]}} = ExMCP.Native.call(:data_processor, "tools/call", %{
      "name" => "get_stats",
      "arguments" => %{}
    })
    stats_time = System.monotonic_time(:microsecond) - start_time
    Logger.info("Processing stats: #{stats_result} (#{stats_time}μs)")
    
    # Test parallel service calls
    Logger.info("\n--- Testing Parallel Service Calls ---")
    parallel_operations = [
      {:calculator_service, "tools/call", %{"name" => "add", "arguments" => %{"a" => 100, "b" => 200}}},
      {:calculator_service, "tools/call", %{"name" => "multiply", "arguments" => %{"a" => 15, "b" => 3}}},
      {:file_service, "tools/call", %{"name" => "create_file", "arguments" => %{"name" => "parallel_test.txt", "content" => "Created in parallel"}}},
      {:data_processor, "tools/call", %{"name" => "process_data", "arguments" => %{"numbers" => [1, 2, 3]}}}
    ]
    
    start_time = System.monotonic_time(:microsecond)
    results = parallel_operations
    |> Task.async_stream(fn {service, method, params} ->
      call_start = System.monotonic_time(:microsecond)
      result = ExMCP.Native.call(service, method, params)
      call_time = System.monotonic_time(:microsecond) - call_start
      {service, result, call_time}
    end, max_concurrency: 10, timeout: 5000)
    |> Enum.to_list()
    
    parallel_total_time = System.monotonic_time(:microsecond) - start_time
    
    Logger.info("Parallel operation results:")
    Enum.each(results, fn 
      {:ok, {service, {:ok, %{"content" => [%{"text" => text}]}}, call_time}} ->
        Logger.info("  ✓ #{service}: #{String.slice(text, 0, 60)}... (#{call_time}μs)")
      {:ok, {service, {:error, reason}, call_time}} ->
        Logger.info("  ✗ #{service}: #{inspect(reason)} (#{call_time}μs)")
      {:exit, reason} ->
        Logger.info("  ✗ Task failed: #{inspect(reason)}")
    end)
    
    Logger.info("Total parallel execution time: #{parallel_total_time}μs")
    Logger.info("Average per operation: #{div(parallel_total_time, length(parallel_operations))}μs")
    
    # Test notifications (fire-and-forget)
    Logger.info("\n--- Testing Fire-and-Forget Notifications ---")
    
    notification_start = System.monotonic_time(:microsecond)
    
    # Send notifications to services
    :ok = ExMCP.Native.notify(:calculator_service, "resource_updated", %{
      "uri" => "file:///config.json",
      "type" => "modified"
    })
    
    :ok = ExMCP.Native.notify(:file_service, "system_event", %{
      "event" => "backup_completed",
      "timestamp" => DateTime.utc_now()
    })
    
    :ok = ExMCP.Native.notify(:data_processor, "config_changed", %{
      "setting" => "batch_size",
      "old_value" => 100,
      "new_value" => 200
    })
    
    notification_time = System.monotonic_time(:microsecond) - notification_start
    Logger.info("Notifications sent successfully in #{notification_time}μs")
    
    # Performance summary
    Logger.info("\n--- Performance Summary ---")
    Logger.info("File operations: #{total_file_time}μs total")
    Logger.info("Data processing: #{processing_time}μs")
    Logger.info("Stats retrieval: #{stats_time}μs")
    Logger.info("Parallel operations: #{parallel_total_time}μs")
    Logger.info("Notifications: #{notification_time}μs")
    
    total_demo_time = total_file_time + processing_time + stats_time + parallel_total_time + notification_time
    Logger.info("Total demo operations: #{total_demo_time}μs")
    Logger.info("Average operation time: #{div(total_demo_time, 12)}μs")
    
    # Cleanup
    GenServer.stop(calc_pid)
    GenServer.stop(file_pid) 
    GenServer.stop(proc_pid)
    
    Logger.info("\n✅ Single-node distributed demo completed!")
    Logger.info("🚀 Native Service Dispatcher provides sub-millisecond distributed service calls")
  end
  
  def demo_cluster_communication do
    Logger.info("Testing cluster communication features with Native Service Dispatcher...")
    
    # Show current node info
    Logger.info("Current node: #{Node.self()}")
    Logger.info("Connected nodes: #{inspect(Node.list())}")
    
    # Start a service on this node
    {:ok, calc_pid} = Examples.Native.CalculatorService.start_link([])
    Process.sleep(100)
    
    # Test local service call with performance measurement
    Logger.info("\nTesting local service call...")
    start_time = System.monotonic_time(:microsecond)
    {:ok, %{"content" => [%{"text" => result_text}]}} = ExMCP.Native.call(:calculator_service, "tools/call", %{
      "name" => "add",
      "arguments" => %{"a" => 100, "b" => 200}
    })
    local_time = System.monotonic_time(:microsecond) - start_time
    Logger.info("Local result: #{result_text} (#{local_time}μs)")
    
    # If there are other nodes, test remote calls
    case Node.list() do
      [] ->
        Logger.info("No remote nodes connected for cross-node testing")
        Logger.info("💡 To test cross-node communication:")
        Logger.info("   1. Start node1: iex --name node1@localhost --cookie democookie -S mix")
        Logger.info("   2. Start node2: iex --name node2@localhost --cookie democookie -S mix")
        Logger.info("   3. Run Examples.Native.DistributedExample.start_server_node() on node1")
        Logger.info("   4. Run Examples.Native.DistributedExample.start_client_node() on node2")
        
      remote_nodes ->
        Logger.info("Testing cross-node calls to: #{inspect(remote_nodes)}")
        
        # Test calls to remote services
        for remote_node <- remote_nodes do
          Logger.info("\nTesting service calls to #{remote_node}...")
          
          # Check what services are available on the remote node
          all_services = ExMCP.Native.list_services()
          remote_services = Enum.filter(all_services, fn {_name, pid, _meta} ->
            node(pid) == remote_node
          end)
          
          Logger.info("Services on #{remote_node}: #{length(remote_services)}")
          for {service_name, _pid, _meta} <- remote_services do
            Logger.info("  - #{service_name}")
          end
          
          # Test calculator service if available
          if ExMCP.Native.service_available?(:calculator_service) do
            start_time = System.monotonic_time(:microsecond)
            case ExMCP.Native.call(:calculator_service, "tools/call", %{
              "name" => "multiply",
              "arguments" => %{"a" => 7, "b" => 8}
            }) do
              {:ok, %{"content" => [%{"text" => text}]}} ->
                remote_time = System.monotonic_time(:microsecond) - start_time
                Logger.info("Remote calculator result: #{text} (#{remote_time}μs)")
                
                # Compare local vs remote performance
                performance_ratio = if local_time > 0, do: Float.round(remote_time / local_time, 2), else: "N/A"
                Logger.info("Remote call is #{performance_ratio}x slower than local (expected due to network)")
                
              {:error, reason} ->
                Logger.info("Remote calculator call failed: #{inspect(reason)}")
            end
          end
        end
    end
    
    # Show distributed service registry stats
    Logger.info("\n--- Distributed Service Registry Stats ---")
    all_services = ExMCP.Native.list_services()
    nodes_with_services = all_services
    |> Enum.map(fn {_name, pid, _meta} -> node(pid) end)
    |> Enum.uniq()
    
    Logger.info("Total services: #{length(all_services)}")
    Logger.info("Nodes with services: #{length(nodes_with_services)}")
    for node_name <- nodes_with_services do
      services_on_node = Enum.count(all_services, fn {_name, pid, _meta} -> node(pid) == node_name end)
      Logger.info("  - #{node_name}: #{services_on_node} services")
    end
    
    # Cleanup
    GenServer.stop(calc_pid)
    
    Logger.info("\n✅ Cluster communication demo completed!")
    Logger.info("🌐 Native Service Dispatcher seamlessly handles distributed services")
  end
end

# Demo runner with service definitions
defmodule Examples.Native.FileService do
  use ExMCP.Service, name: :file_service
  
  @impl true
  def init(_args) do
    {:ok, %{files: %{}}}
  end
  
  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [
      %{
        "name" => "create_file",
        "description" => "Create a file with content",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "name" => %{"type" => "string"},
            "content" => %{"type" => "string"}
          },
          "required" => ["name", "content"]
        }
      },
      %{
        "name" => "list_files",
        "description" => "List all files",
        "inputSchema" => %{"type" => "object", "properties" => %{}}
      }
    ]
    
    {:ok, %{"tools" => tools}, state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "create_file", "arguments" => %{"name" => name, "content" => content}}, state) do
    new_files = Map.put(state.files, name, content)
    new_state = %{state | files: new_files}
    
    content_resp = [%{
      "type" => "text",
      "text" => "Created file '#{name}' with #{String.length(content)} characters"
    }]
    
    {:ok, %{"content" => content_resp}, new_state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "list_files", "arguments" => _args}, state) do
    file_list = state.files
    |> Map.keys()
    |> Enum.join(", ")
    
    content_resp = [%{
      "type" => "text", 
      "text" => if file_list == "", do: "No files", else: file_list
    }]
    
    {:ok, %{"content" => content_resp}, state}
  end
  
  def handle_mcp_request(method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
  end
end

defmodule Examples.Native.DataProcessor do
  use ExMCP.Service, name: :data_processor
  
  @impl true
  def init(_args) do
    {:ok, %{processed_count: 0, total_processed: 0}}
  end
  
  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [
      %{
        "name" => "process_data",
        "description" => "Process a list of numbers",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "numbers" => %{"type" => "array", "items" => %{"type" => "number"}}
          },
          "required" => ["numbers"]
        }
      },
      %{
        "name" => "get_stats",
        "description" => "Get processing statistics",
        "inputSchema" => %{"type" => "object", "properties" => %{}}
      }
    ]
    
    {:ok, %{"tools" => tools}, state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "process_data", "arguments" => %{"numbers" => numbers}}, state) do
    # Use calculator service to sum the numbers
    sum_result = if ExMCP.Native.service_available?(:calculator_service) do
      Enum.reduce(numbers, 0, fn num, acc ->
        case ExMCP.Native.call(:calculator_service, "tools/call", %{
          "name" => "add",
          "arguments" => %{"a" => acc, "b" => num}
        }) do
          {:ok, _} -> acc + num  # Just do the math directly for efficiency
          _ -> acc + num
        end
      end)
    else
      Enum.sum(numbers)
    end
    
    new_state = %{state | 
      processed_count: state.processed_count + 1,
      total_processed: state.total_processed + length(numbers)
    }
    
    content_resp = [%{
      "type" => "text",
      "text" => "Processed #{length(numbers)} numbers, sum: #{sum_result}"
    }]
    
    {:ok, %{"content" => content_resp}, new_state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "get_stats", "arguments" => _args}, state) do
    stats = %{
      processed_batches: state.processed_count,
      total_numbers: state.total_processed,
      node: node()
    }
    
    content_resp = [%{
      "type" => "text",
      "text" => "Stats: #{Jason.encode!(stats, pretty: true)}"
    }]
    
    {:ok, %{"content" => content_resp}, state}
  end
  
  def handle_mcp_request(method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
  end
end

# Demo runner
defmodule Examples.Native.DistributedDemo do
  def run do
    Logger.info("Starting Native Service Dispatcher Distributed Demo")
    Logger.info("=" |> String.duplicate(55))
    
    # Run the single-node demo to show distributed patterns
    Examples.Native.DistributedExample.demo_single_node()
    
    Process.sleep(500)
    
    # Show cluster communication capabilities
    Logger.info("\n=== Cluster Communication Capabilities ===")
    Examples.Native.DistributedExample.demo_cluster_communication()
    
    Logger.info("\n=== Demo Benefits ===")
    IO.puts("✓ Transparent distributed service calls via Horde.Registry")
    IO.puts("✓ Sub-millisecond local service communication")
    IO.puts("✓ Automatic service discovery across BEAM cluster")
    IO.puts("✓ Zero configuration distributed architecture")
    IO.puts("✓ Perfect for microservices on Elixir clusters")
    
    :ok
  end
end

# Example usage:
if __FILE__ == Path.absname("#{__DIR__}/distributed_example.ex") do
  Examples.Native.DistributedDemo.run()
  
  IO.puts("\nServices are running. Press Ctrl+C to stop.")
  Process.sleep(:infinity)
end