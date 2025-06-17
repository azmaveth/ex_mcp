defmodule ExMCP.Examples.Interop.ElixirServerForPython do
  @moduledoc """
  ExMCP servers designed to be used by Python MCP clients.
  
  This demonstrates:
  - Elixir MCP servers accessible via stdio transport
  - JSON-RPC compatibility with Python MCP SDK
  - Rich tool schemas for Python client validation
  - Error handling that's compatible across languages
  - Resource and prompt capabilities for Python clients
  
  ## Usage
  
  Start the server for Python clients to connect:
  
      iex> ExMCP.Examples.Interop.ElixirServerForPython.run()
      
  Or start specific servers:
  
      iex> ExMCP.Examples.Interop.ElixirServerForPython.start_data_server()
      iex> ExMCP.Examples.Interop.ElixirServerForPython.start_file_server()
  
  ## Python Client Example
  
  ```python
  import asyncio
  from mcp import ClientSession, StdioServerParameters
  from mcp.client.stdio import stdio_client
  
  async def main():
      server_params = StdioServerParameters(
          command=["elixir", "-e", "ExMCP.Examples.Interop.ElixirServerForPython.start_cli_server()"]
      )
      
      async with stdio_client(server_params) as (read, write):
          async with ClientSession(read, write) as session:
              # List tools
              tools = await session.list_tools()
              print(f"Available tools: {len(tools.tools)}")
              
              # Call a tool
              result = await session.call_tool("process_data", {"numbers": [1, 2, 3, 4, 5]})
              print(f"Result: {result.content[0].text}")
  
  asyncio.run(main())
  ```
  """
  
  require Logger
  
  defmodule DataProcessingServer do
    @moduledoc """
    Elixir MCP server providing data processing capabilities for Python clients.
    """
    
    use ExMCP.Server.Handler
    
    defmodule State do
      defstruct [
        :datasets,
        :processing_count,
        :server_start_time
      ]
    end
    
    @impl true
    def init(_args) do
      state = %State{
        datasets: %{},
        processing_count: 0,
        server_start_time: DateTime.utc_now()
      }
      
      Logger.info("Elixir Data Processing Server started for Python clients")
      {:ok, state}
    end
    
    @impl true
    def handle_initialize(_params, state) do
      server_info = %{
        "name" => "elixir-data-processor",
        "version" => "1.0.0",
        "description" => "High-performance data processing server built with Elixir",
        "capabilities" => %{
          "tools" => %{},
          "resources" => %{},
          "prompts" => %{}
        }
      }
      
      {:ok, server_info, state}
    end
    
    @impl true
    def handle_list_tools(state) do
      tools = [
        %{
          "name" => "process_data",
          "description" => "Process numerical data with various statistical operations",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "numbers" => %{
                "type" => "array",
                "items" => %{"type" => "number"},
                "description" => "Array of numbers to process"
              },
              "operations" => %{
                "type" => "array",
                "items" => %{
                  "type" => "string",
                  "enum" => ["sum", "mean", "median", "std_dev", "min", "max"]
                },
                "default" => ["sum", "mean"],
                "description" => "Statistical operations to perform"
              }
            },
            "required" => ["numbers"]
          }
        },
        %{
          "name" => "create_dataset",
          "description" => "Create and store a named dataset",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "name" => %{
                "type" => "string",
                "description" => "Name for the dataset"
              },
              "data" => %{
                "type" => "array",
                "items" => %{"type" => "number"},
                "description" => "Dataset values"
              },
              "metadata" => %{
                "type" => "object",
                "properties" => %{
                  "description" => %{"type" => "string"},
                  "source" => %{"type" => "string"},
                  "created_by" => %{"type" => "string"}
                },
                "description" => "Optional metadata for the dataset"
              }
            },
            "required" => ["name", "data"]
          }
        },
        %{
          "name" => "get_dataset",
          "description" => "Retrieve a stored dataset",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "name" => %{
                "type" => "string",
                "description" => "Name of the dataset to retrieve"
              }
            },
            "required" => ["name"]
          }
        },
        %{
          "name" => "list_datasets",
          "description" => "List all stored datasets",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{}
          }
        },
        %{
          "name" => "compare_datasets",
          "description" => "Compare two datasets statistically",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "dataset1" => %{"type" => "string", "description" => "First dataset name"},
              "dataset2" => %{"type" => "string", "description" => "Second dataset name"}
            },
            "required" => ["dataset1", "dataset2"]
          }
        },
        %{
          "name" => "get_server_stats",
          "description" => "Get server performance and usage statistics",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{}
          }
        }
      ]
      
      {:ok, tools, state}
    end
    
    @impl true
    def handle_call_tool("process_data", params, state) do
      numbers = params["numbers"]
      operations = params["operations"] || ["sum", "mean"]
      
      results = %{}
      
      # Perform requested operations
      results = if "sum" in operations do
        Map.put(results, "sum", Enum.sum(numbers))
      else
        results
      end
      
      results = if "mean" in operations do
        mean = Enum.sum(numbers) / length(numbers)
        Map.put(results, "mean", Float.round(mean, 4))
      else
        results
      end
      
      results = if "median" in operations do
        sorted = Enum.sort(numbers)
        len = length(sorted)
        median = if rem(len, 2) == 0 do
          (Enum.at(sorted, div(len, 2) - 1) + Enum.at(sorted, div(len, 2))) / 2
        else
          Enum.at(sorted, div(len, 2))
        end
        Map.put(results, "median", median)
      else
        results
      end
      
      results = if "std_dev" in operations do
        mean = Enum.sum(numbers) / length(numbers)
        variance = Enum.sum(Enum.map(numbers, fn x -> (x - mean) * (x - mean) end)) / length(numbers)
        std_dev = :math.sqrt(variance)
        Map.put(results, "std_dev", Float.round(std_dev, 4))
      else
        results
      end
      
      results = if "min" in operations do
        Map.put(results, "min", Enum.min(numbers))
      else
        results
      end
      
      results = if "max" in operations do
        Map.put(results, "max", Enum.max(numbers))
      else
        results
      end
      
      new_state = %{state | processing_count: state.processing_count + 1}
      
      content = [%{
        "type" => "text",
        "text" => "Data Processing Results (#{length(numbers)} values):\n#{Jason.encode!(results, pretty: true)}"
      }]
      
      {:ok, content, new_state}
    end
    
    def handle_call_tool("create_dataset", params, state) do
      name = params["name"]
      data = params["data"]
      metadata = params["metadata"] || %{}
      
      dataset = %{
        "data" => data,
        "metadata" => metadata,
        "created_at" => DateTime.utc_now() |> DateTime.to_iso8601(),
        "size" => length(data)
      }
      
      new_datasets = Map.put(state.datasets, name, dataset)
      new_state = %{state | datasets: new_datasets}
      
      content = [%{
        "type" => "text",
        "text" => "Created dataset '#{name}' with #{length(data)} values"
      }]
      
      {:ok, content, new_state}
    end
    
    def handle_call_tool("get_dataset", params, state) do
      name = params["name"]
      
      case Map.get(state.datasets, name) do
        nil ->
          {:error, "Dataset '#{name}' not found", state}
          
        dataset ->
          content = [%{
            "type" => "text",
            "text" => "Dataset '#{name}':\n#{Jason.encode!(dataset, pretty: true)}"
          }]
          
          {:ok, content, state}
      end
    end
    
    def handle_call_tool("list_datasets", _params, state) do
      dataset_summaries = Enum.map(state.datasets, fn {name, dataset} ->
        %{
          "name" => name,
          "size" => dataset["size"],
          "created_at" => dataset["created_at"],
          "description" => get_in(dataset, ["metadata", "description"]) || "No description"
        }
      end)
      
      content = [%{
        "type" => "text",
        "text" => "Stored Datasets (#{length(dataset_summaries)}):\n#{Jason.encode!(dataset_summaries, pretty: true)}"
      }]
      
      {:ok, content, state}
    end
    
    def handle_call_tool("compare_datasets", params, state) do
      dataset1_name = params["dataset1"]
      dataset2_name = params["dataset2"]
      
      dataset1 = Map.get(state.datasets, dataset1_name)
      dataset2 = Map.get(state.datasets, dataset2_name)
      
      cond do
        is_nil(dataset1) ->
          {:error, "Dataset '#{dataset1_name}' not found", state}
          
        is_nil(dataset2) ->
          {:error, "Dataset '#{dataset2_name}' not found", state}
          
        true ->
          data1 = dataset1["data"]
          data2 = dataset2["data"]
          
          comparison = %{
            "dataset1" => %{
              "name" => dataset1_name,
              "size" => length(data1),
              "mean" => Enum.sum(data1) / length(data1),
              "min" => Enum.min(data1),
              "max" => Enum.max(data1)
            },
            "dataset2" => %{
              "name" => dataset2_name,
              "size" => length(data2),
              "mean" => Enum.sum(data2) / length(data2),
              "min" => Enum.min(data2),
              "max" => Enum.max(data2)
            },
            "comparison" => %{
              "size_difference" => length(data2) - length(data1),
              "mean_difference" => (Enum.sum(data2) / length(data2)) - (Enum.sum(data1) / length(data1))
            }
          }
          
          content = [%{
            "type" => "text",
            "text" => "Dataset Comparison:\n#{Jason.encode!(comparison, pretty: true)}"
          }]
          
          {:ok, content, state}
      end
    end
    
    def handle_call_tool("get_server_stats", _params, state) do
      uptime_seconds = DateTime.diff(DateTime.utc_now(), state.server_start_time, :second)
      
      stats = %{
        "server_type" => "Elixir Data Processing Server",
        "uptime_seconds" => uptime_seconds,
        "processing_operations" => state.processing_count,
        "stored_datasets" => map_size(state.datasets),
        "elixir_version" => System.version(),
        "erlang_version" => System.otp_release(),
        "memory_usage" => %{
          "total" => :erlang.memory(:total),
          "processes" => :erlang.memory(:processes),
          "system" => :erlang.memory(:system)
        },
        "capabilities" => [
          "high_performance_numeric_processing",
          "concurrent_request_handling",
          "fault_tolerant_architecture",
          "json_rpc_compatible"
        ]
      }
      
      content = [%{
        "type" => "text",
        "text" => "Elixir Server Statistics:\n#{Jason.encode!(stats, pretty: true)}"
      }]
      
      {:ok, content, state}
    end
    
    def handle_call_tool(name, _params, state) do
      {:error, "Unknown tool: #{name}", state}
    end
    
    @impl true
    def handle_list_resources(state) do
      # Provide datasets as resources that Python clients can read
      resources = Enum.map(state.datasets, fn {name, dataset} ->
        %{
          "uri" => "dataset://#{name}",
          "name" => "Dataset: #{name}",
          "description" => get_in(dataset, ["metadata", "description"]) || "Numerical dataset",
          "mimeType" => "application/json"
        }
      end)
      
      {:ok, resources, state}
    end
    
    @impl true
    def handle_read_resource("dataset://" <> dataset_name, state) do
      case Map.get(state.datasets, dataset_name) do
        nil ->
          {:error, "Dataset resource not found: #{dataset_name}", state}
          
        dataset ->
          content = %{
            "uri" => "dataset://#{dataset_name}",
            "mimeType" => "application/json",
            "text" => Jason.encode!(dataset, pretty: true)
          }
          
          {:ok, content, state}
      end
    end
    
    @impl true
    def handle_list_prompts(state) do
      prompts = [
        %{
          "name" => "data_analysis_prompt",
          "description" => "Template for data analysis tasks",
          "arguments" => [
            %{
              "name" => "dataset_name",
              "description" => "Name of the dataset to analyze",
              "required" => true
            },
            %{
              "name" => "analysis_type",
              "description" => "Type of analysis to perform",
              "required" => false
            }
          ]
        }
      ]
      
      {:ok, prompts, state}
    end
    
    @impl true
    def handle_get_prompt("data_analysis_prompt", args, state) do
      dataset_name = Map.get(args, "dataset_name", "unknown")
      analysis_type = Map.get(args, "analysis_type", "statistical")
      
      messages = [
        %{
          "role" => "user",
          "content" => %{
            "type" => "text",
            "text" => """
            Please analyze the dataset "#{dataset_name}" using #{analysis_type} analysis.
            
            The Elixir data processing server has the following capabilities:
            - Statistical operations (sum, mean, median, std_dev, min, max)
            - Dataset comparison
            - High-performance concurrent processing
            
            Available tools:
            - process_data: Perform statistical operations
            - get_dataset: Retrieve dataset
            - compare_datasets: Compare two datasets
            
            Please provide insights and recommendations based on the analysis.
            """
          }
        }
      ]
      
      {:ok, %{"messages" => messages}, state}
    end
  end
  
  def run do
    Logger.info("Starting Elixir MCP Servers for Python Clients")
    Logger.info("=" |> String.duplicate(60))
    
    # Start data processing server
    {:ok, data_server} = ExMCP.Server.start_link([
      transport: :stdio,
      handler: DataProcessingServer,
      name: :elixir_data_server
    ])
    
    Logger.info("✅ Elixir Data Processing Server started")
    Logger.info("🐍 Python clients can connect via stdio transport")
    Logger.info("📋 Available capabilities: tools, resources, prompts")
    
    # Display connection instructions for Python clients
    display_python_connection_info()
    
    # Keep the server running
    Process.monitor(data_server)
    
    receive do
      {:DOWN, _ref, :process, ^data_server, reason} ->
        Logger.error("Data server stopped: #{inspect(reason)}")
    end
    
    :ok
  end
  
  def start_data_server do
    """
    Start just the data processing server for Python clients.
    """
    {:ok, _server} = ExMCP.Server.start_link([
      transport: :stdio,
      handler: DataProcessingServer,
      name: :elixir_data_server_standalone
    ])
    
    Logger.info("Elixir Data Processing Server started for Python clients")
    :ok
  end
  
  def start_cli_server do
    """
    Entry point for command-line usage by Python clients.
    This function is designed to be called via:
    elixir -e "ExMCP.Examples.Interop.ElixirServerForPython.start_cli_server()"
    """
    # Start application and dependencies
    Application.ensure_all_started(:ex_mcp)
    
    # Start the server
    {:ok, _server} = ExMCP.Server.start_link([
      transport: :stdio,
      handler: DataProcessingServer
    ])
    
    # Keep process alive
    :timer.sleep(:infinity)
  end
  
  defp display_python_connection_info do
    Logger.info("\n=== Python Client Connection Examples ===")
    
    IO.puts("""
    
    📄 Save this as `python_client_example.py`:
    
    ```python
    import asyncio
    from mcp import ClientSession, StdioServerParameters
    from mcp.client.stdio import stdio_client
    
    async def main():
        # Connect to Elixir MCP server
        server_params = StdioServerParameters(
            command=["elixir", "-e", 
                    "ExMCP.Examples.Interop.ElixirServerForPython.start_cli_server()"]
        )
        
        async with stdio_client(server_params) as (read, write):
            async with ClientSession(read, write) as session:
                # Initialize connection
                await session.initialize()
                
                # List available tools
                tools_response = await session.list_tools()
                print(f"Available tools: {len(tools_response.tools)}")
                for tool in tools_response.tools:
                    print(f"  - {tool.name}: {tool.description}")
                
                # Process some data
                result = await session.call_tool(
                    "process_data", 
                    {
                        "numbers": [1, 2, 3, 4, 5, 10, 15, 20],
                        "operations": ["sum", "mean", "std_dev"]
                    }
                )
                print(f"\\nProcessing result:")
                print(result.content[0].text)
                
                # Create a dataset
                await session.call_tool(
                    "create_dataset",
                    {
                        "name": "sample_data",
                        "data": [1, 4, 7, 2, 9, 3, 8, 5, 6],
                        "metadata": {
                            "description": "Sample dataset from Python client",
                            "source": "python_example",
                            "created_by": "python_client"
                        }
                    }
                )
                
                # List datasets
                datasets = await session.call_tool("list_datasets", {})
                print(f"\\nDatasets:")
                print(datasets.content[0].text)
                
                # Get server stats
                stats = await session.call_tool("get_server_stats", {})
                print(f"\\nServer statistics:")
                print(stats.content[0].text)
    
    if __name__ == "__main__":
        asyncio.run(main())
    ```
    
    🚀 Run the Python client:
    ```bash
    python python_client_example.py
    ```
    
    """)
  end
end

# Demo runner
defmodule ExMCP.Examples.Interop.PythonToElixirDemo do
  def run do
    Logger.info("Starting Python → Elixir MCP Integration Demo")
    Logger.info("=" |> String.duplicate(60))
    
    Logger.info("This demo shows how to run Elixir MCP servers for Python clients")
    Logger.info("The server will start and wait for Python client connections")
    
    # Run the server
    ExMCP.Examples.Interop.ElixirServerForPython.run()
  end
end

# Example usage:
if __FILE__ == Path.absname("#{__DIR__}/elixir_servers_for_python.ex") do
  ExMCP.Examples.Interop.PythonToElixirDemo.run()
end