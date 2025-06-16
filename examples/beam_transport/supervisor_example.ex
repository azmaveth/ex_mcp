defmodule Examples.NativeBeam.SupervisorExample do
  @moduledoc """
  Example demonstrating OTP supervision with Native BEAM transport services.
  
  This shows:
  - Running multiple MCP services under supervision
  - Service discovery and availability
  - Fault tolerance and automatic restart
  - Inter-service communication
  """
  
  require Logger
  
  defmodule ServiceSupervisor do
    @moduledoc """
    Supervisor for multiple MCP services using Native BEAM transport.
    """
    use Supervisor
    
    def start_link(init_arg) do
      Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end
    
    def init(_init_arg) do
      children = [
        {Examples.NativeBeam.CalculatorServer, []},
        {Examples.NativeBeam.FileService, []},
        {Examples.NativeBeam.DataProcessor, []}
      ]
      
      Supervisor.init(children, strategy: :one_for_one)
    end
  end
  
  defmodule FileService do
    @moduledoc """
    Simple file service for demonstration.
    """
    use GenServer
    require Logger
    
    def start_link(args) do
      GenServer.start_link(__MODULE__, args, name: __MODULE__)
    end
    
    def init(_args) do
      Logger.info("File service starting...")
      :ok = ExMCP.Transport.Native.register_service(:file_service)
      Logger.info("File service registered as :file_service")
      {:ok, %{files: []}}
    end
    
    def handle_call({:mcp_request, %{"method" => "tools/list"}}, _from, state) do
      tools = [
        %{
          "name" => "list_files",
          "description" => "List available files",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{}
          }
        },
        %{
          "name" => "create_file",
          "description" => "Create a new file",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "name" => %{"type" => "string", "description" => "File name"},
              "content" => %{"type" => "string", "description" => "File content"}
            },
            "required" => ["name", "content"]
          }
        }
      ]
      
      {:reply, {:ok, %{"tools" => tools}}, state}
    end
    
    def handle_call({:mcp_request, %{"method" => "tools/call", "params" => %{"name" => "list_files"}}}, _from, state) do
      file_list = Enum.map(state.files, fn {name, _content} -> name end)
      content = [%{
        "type" => "text",
        "text" => "Files: #{Enum.join(file_list, ", ")}"
      }]
      
      {:reply, {:ok, %{"content" => content}}, state}
    end
    
    def handle_call({:mcp_request, %{"method" => "tools/call", "params" => %{"name" => "create_file", "arguments" => %{"name" => name, "content" => content}}}}, _from, state) do
      new_files = [{name, content} | state.files]
      new_state = %{state | files: new_files}
      
      response_content = [%{
        "type" => "text",
        "text" => "Created file: #{name}"
      }]
      
      {:reply, {:ok, %{"content" => response_content}}, new_state}
    end
  end
  
  defmodule DataProcessor do
    @moduledoc """
    Data processing service that can communicate with other services.
    """
    use GenServer
    require Logger
    
    def start_link(args) do
      GenServer.start_link(__MODULE__, args, name: __MODULE__)
    end
    
    def init(_args) do
      Logger.info("Data processor starting...")
      :ok = ExMCP.Transport.Native.register_service(:data_processor)
      Logger.info("Data processor registered as :data_processor")
      {:ok, %{processed_count: 0}}
    end
    
    def handle_call({:mcp_request, %{"method" => "tools/list"}}, _from, state) do
      tools = [
        %{
          "name" => "process_data",
          "description" => "Process data using other services",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "numbers" => %{"type" => "array", "description" => "Numbers to process"}
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
      
      {:reply, {:ok, %{"tools" => tools}}, state}
    end
    
    def handle_call({:mcp_request, %{"method" => "tools/call", "params" => %{"name" => "process_data", "arguments" => %{"numbers" => numbers}}}}, _from, state) do
      # Use the calculator service to sum the numbers
      results = for num <- numbers do
        case ExMCP.Transport.Native.call(:calculator_server, "tools/call", %{
          "name" => "add",
          "arguments" => %{"a" => num, "b" => 1}
        }) do
          {:ok, %{"content" => content}} ->
            # Extract result from content
            content |> List.first() |> Map.get("text")
          {:error, _} ->
            "Error processing #{num}"
        end
      end
      
      new_state = %{state | processed_count: state.processed_count + length(numbers)}
      
      content = [%{
        "type" => "text",
        "text" => "Processed #{length(numbers)} numbers: #{Enum.join(results, ", ")}"
      }]
      
      {:reply, {:ok, %{"content" => content}}, new_state}
    end
    
    def handle_call({:mcp_request, %{"method" => "tools/call", "params" => %{"name" => "get_stats"}}}, _from, state) do
      content = [%{
        "type" => "text",
        "text" => "Total items processed: #{state.processed_count}"
      }]
      
      {:reply, {:ok, %{"content" => content}}, state}
    end
  end
  
  def demo do
    Logger.info("Starting Native BEAM transport supervisor example...")
    
    # Start the service supervisor
    {:ok, supervisor_pid} = ServiceSupervisor.start_link([])
    
    # Wait for all services to register
    Process.sleep(200)
    
    # Discover available services
    services = ExMCP.Transport.Native.list_services()
    Logger.info("Discovered services:")
    for {service_name, _pid, _meta} <- services do
      Logger.info("  - #{service_name}")
    end
    
    # Test calculator service
    Logger.info("\n--- Testing Calculator Service ---")
    {:ok, result} = ExMCP.Transport.Native.call(:calculator_server, "tools/call", %{
      "name" => "add",
      "arguments" => %{"a" => 10, "b" => 5}
    })
    Logger.info("Calculator result: #{result["content"] |> List.first() |> Map.get("text")}")
    
    # Test file service
    Logger.info("\n--- Testing File Service ---")
    {:ok, _} = ExMCP.Transport.Native.call(:file_service, "tools/call", %{
      "name" => "create_file",
      "arguments" => %{"name" => "test.txt", "content" => "Hello World"}
    })
    
    {:ok, result} = ExMCP.Transport.Native.call(:file_service, "tools/call", %{
      "name" => "list_files",
      "arguments" => %{}
    })
    Logger.info("File service result: #{result["content"] |> List.first() |> Map.get("text")}")
    
    # Test data processor (inter-service communication)
    Logger.info("\n--- Testing Data Processor (Inter-service Communication) ---")
    {:ok, result} = ExMCP.Transport.Native.call(:data_processor, "tools/call", %{
      "name" => "process_data",
      "arguments" => %{"numbers" => [1, 2, 3, 4, 5]}
    })
    Logger.info("Data processor result: #{result["content"] |> List.first() |> Map.get("text")}")
    
    # Get processing stats
    {:ok, result} = ExMCP.Transport.Native.call(:data_processor, "tools/call", %{
      "name" => "get_stats",
      "arguments" => %{}
    })
    Logger.info("Processing stats: #{result["content"] |> List.first() |> Map.get("text")}")
    
    # Demonstrate fault tolerance by crashing a service
    Logger.info("\n--- Testing Fault Tolerance ---")
    [data_processor_pid] = Registry.lookup(ExMCP.Registry, :data_processor) |> Enum.map(fn {pid, _} -> pid end)
    Process.exit(data_processor_pid, :kill)
    
    Logger.info("Killed data processor, waiting for restart...")
    Process.sleep(100)
    
    # Service should be restarted automatically
    if ExMCP.Transport.Native.service_available?(:data_processor) do
      Logger.info("Data processor restarted successfully!")
      
      # Test that it still works
      {:ok, result} = ExMCP.Transport.Native.call(:data_processor, "tools/call", %{
        "name" => "get_stats",
        "arguments" => %{}
      })
      Logger.info("Stats after restart: #{result["content"] |> List.first() |> Map.get("text")}")
    else
      Logger.warning("Data processor not available after restart")
    end
    
    # Cleanup
    Supervisor.stop(supervisor_pid)
    
    Logger.info("\nSupervisor example completed!")
  end
end