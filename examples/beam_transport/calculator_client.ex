defmodule Examples.BeamTransport.CalculatorClient do
  @moduledoc """
  Example of using ExMCP.Client with BEAM transport to connect to a calculator server.
  
  This demonstrates:
  - Connecting to a BEAM transport server
  - Calling tools with parameters
  - Handling responses and errors
  - Working with progress notifications
  """
  
  require Logger
  
  def run_example do
    Logger.info("Starting calculator client example...")
    
    # Start the calculator server
    {:ok, server} = ExMCP.Server.start_link(
      transport: :beam,
      name: :calculator_server,
      handler: Examples.BeamTransport.CalculatorServer
    )
    
    # Connect a client
    {:ok, client} = ExMCP.Client.start_link(
      transport: :beam,
      server: :calculator_server
    )
    
    # Wait for initialization
    Process.sleep(100)
    
    # Get server info
    {:ok, info} = ExMCP.Client.server_info(client)
    Logger.info("Connected to server: #{info["name"]} v#{info["version"]}")
    
    # List available tools
    {:ok, %{"tools" => tools}} = ExMCP.Client.list_tools(client)
    Logger.info("Available tools:")
    for tool <- tools do
      Logger.info("  - #{tool["name"]}: #{tool["description"]}")
    end
    
    # Perform some calculations
    examples = [
      {"add", %{"a" => 5, "b" => 3}},
      {"multiply", %{"a" => 4, "b" => 7}},
      {"divide", %{"a" => 10, "b" => 3}},
      {"divide", %{"a" => 10, "b" => 0}},  # This will error
      {"factorial", %{"n" => 5}}
    ]
    
    for {tool, params} <- examples do
      Logger.info("\nCalling #{tool} with #{inspect(params)}")
      
      case ExMCP.Client.call_tool(client, tool, params) do
        {:ok, %{"content" => content}} ->
          text = content |> List.first() |> Map.get("text")
          Logger.info("Result: #{text}")
          
        {:error, reason} ->
          Logger.error("Error: #{inspect(reason)}")
      end
    end
    
    # Get calculation history
    Logger.info("\nGetting calculation history...")
    {:ok, %{"content" => content}} = ExMCP.Client.call_tool(
      client, 
      "history", 
      %{"limit" => 5}
    )
    
    history_text = content |> List.first() |> Map.get("text")
    Logger.info("Recent calculations:\n#{history_text}")
    
    # Calculate factorial with progress
    Logger.info("\nCalculating 10! with progress tracking...")
    
    # Spawn a task to monitor progress (in a real app, you'd handle this differently)
    task = Task.async(fn ->
      # In a real application, you would implement a custom client
      # that handles progress notifications properly
      calculate_with_progress(client, 10)
    end)
    
    result = Task.await(task, 5000)
    Logger.info("Final result: #{result}")
    
    # Cleanup
    GenServer.stop(client)
    GenServer.stop(server)
    
    Logger.info("\nExample completed!")
  end
  
  defp calculate_with_progress(client, n) do
    # Note: This is a simplified example. In a real application,
    # you would implement proper progress handling in the client
    case ExMCP.Client.call_tool(
      client, 
      "factorial", 
      %{"n" => n},
      progress_token: "factorial-#{n}"
    ) do
      {:ok, %{"content" => content}} ->
        content |> List.first() |> Map.get("text")
        
      {:error, reason} ->
        "Error: #{inspect(reason)}"
    end
  end
  
  @doc """
  Run the example from iex:
  
      iex> Examples.BeamTransport.CalculatorClient.run_example()
  """
  def demo do
    run_example()
  end
end