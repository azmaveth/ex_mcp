defmodule Examples.NativeBeam.CalculatorClient do
  @moduledoc """
  Example of using ExMCP's Native BEAM transport to connect to a calculator service.
  
  This demonstrates:
  - Direct service-to-service communication
  - Calling tools with parameters
  - Handling responses and errors
  - Working with progress notifications
  """
  
  require Logger
  
  def run_example do
    Logger.info("Starting calculator client example...")
    
    # Start the calculator service
    {:ok, server_pid} = Examples.NativeBeam.CalculatorServer.start_link([])
    
    # Wait for service registration
    Process.sleep(100)
    
    # Verify service is available
    unless ExMCP.Transport.Native.service_available?(:calculator_server) do
      raise "Calculator service not available"
    end
    
    # Initialize the connection with the service
    {:ok, init_result} = ExMCP.Transport.Native.call(:calculator_server, "initialize", %{
      "protocolVersion" => "2025-03-26",
      "capabilities" => %{},
      "clientInfo" => %{"name" => "calculator-client", "version" => "1.0.0"}
    })
    Logger.info("Connected to server: #{init_result["serverInfo"]["name"]} v#{init_result["serverInfo"]["version"]}")
    
    # List available tools
    {:ok, tools} = ExMCP.Transport.Native.call(:calculator_server, "tools/list", %{})
    Logger.info("Available tools:")
    for tool <- tools["tools"] do
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
      
      case ExMCP.Transport.Native.call(:calculator_server, "tools/call", %{
        "name" => tool,
        "arguments" => params
      }) do
        {:ok, %{"content" => content}} ->
          text = content |> List.first() |> Map.get("text")
          Logger.info("Result: #{text}")
          
        {:error, reason} ->
          Logger.error("Error: #{inspect(reason)}")
      end
    end
    
    # Get calculation history
    Logger.info("\nGetting calculation history...")
    {:ok, %{"content" => content}} = ExMCP.Transport.Native.call(:calculator_server, "tools/call", %{
      "name" => "history",
      "arguments" => %{"limit" => 5}
    })
    
    history_text = content |> List.first() |> Map.get("text")
    Logger.info("Recent calculations:\n#{history_text}")
    
    # Calculate factorial with progress
    Logger.info("\nCalculating 10! with progress tracking...")
    
    # Call with progress token
    {:ok, %{"content" => content}} = ExMCP.Transport.Native.call(
      :calculator_server, 
      "tools/call", 
      %{
        "name" => "factorial",
        "arguments" => %{"n" => 10}
      },
      progress_token: "factorial-10"
    )
    
    result = content |> List.first() |> Map.get("text")
    Logger.info("Final result: #{result}")
    
    # Cleanup
    GenServer.stop(server_pid)
    
    Logger.info("\nExample completed!")
  end
  
  @doc """
  Run the example from iex:
  
      iex> Examples.NativeBeam.CalculatorClient.run_example()
  """
  def demo do
    run_example()
  end
end