defmodule Examples.BeamTransport.DistributedExample do
  @moduledoc """
  Example of using BEAM transport across distributed Erlang nodes.
  
  This demonstrates:
  - Starting a server on one node
  - Connecting from another node
  - Transparent cross-node communication
  - Handling network partitions
  
  To run this example:
  
  1. Start node1 (server node):
     ```
     iex --name node1@localhost --cookie democookie -S mix
     iex> Examples.BeamTransport.DistributedExample.start_server()
     ```
  
  2. Start node2 (client node):
     ```
     iex --name node2@localhost --cookie democookie -S mix
     iex> Examples.BeamTransport.DistributedExample.start_client()
     ```
  """
  
  require Logger
  
  # Simple handler for demonstration
  defmodule WeatherHandler do
    use ExMCP.Server.Handler
    
    @impl true
    def handle_initialize(_params, state) do
      {:ok, %{name: "weather-service", version: "1.0.0"}, state}
    end
    
    @impl true
    def handle_list_tools(state) do
      tools = [
        %{
          name: "get_weather",
          description: "Get current weather for a city",
          inputSchema: %{
            type: "object",
            properties: %{
              city: %{type: "string", description: "City name"}
            },
            required: ["city"]
          }
        },
        %{
          name: "get_forecast",
          description: "Get weather forecast",
          inputSchema: %{
            type: "object",
            properties: %{
              city: %{type: "string"},
              days: %{type: "integer", default: 7}
            },
            required: ["city"]
          }
        }
      ]
      
      {:ok, tools, state}
    end
    
    @impl true
    def handle_call_tool("get_weather", %{"city" => city}, state) do
      # Simulate weather data
      weather = %{
        city: city,
        temperature: Enum.random(15..30),
        conditions: Enum.random(["Sunny", "Cloudy", "Rainy", "Partly Cloudy"]),
        humidity: Enum.random(40..80),
        wind_speed: Enum.random(5..25)
      }
      
      content = [%{
        type: "text",
        text: """
        Current weather in #{city}:
        Temperature: #{weather.temperature}°C
        Conditions: #{weather.conditions}
        Humidity: #{weather.humidity}%
        Wind: #{weather.wind_speed} km/h
        """
      }]
      
      {:ok, content, state}
    end
    
    def handle_call_tool("get_forecast", %{"city" => city} = params, state) do
      days = Map.get(params, "days", 7)
      
      forecast = for day <- 1..days do
        temp = Enum.random(15..30)
        conditions = Enum.random(["Sunny", "Cloudy", "Rainy", "Partly Cloudy"])
        "Day #{day}: #{temp}°C, #{conditions}"
      end
      
      content = [%{
        type: "text",
        text: """
        #{days}-day forecast for #{city}:
        #{Enum.join(forecast, "\n")}
        """
      }]
      
      {:ok, content, state}
    end
  end
  
  @doc """
  Start the weather service on the current node.
  Run this on node1.
  """
  def start_server do
    Logger.info("Starting weather service on #{Node.self()}")
    
    {:ok, server} = ExMCP.Server.start_link(
      transport: :beam,
      name: :weather_service,
      handler: WeatherHandler
    )
    
    Logger.info("Weather service started and registered as :weather_service")
    Logger.info("Other nodes can connect using: {:weather_service, :\"#{Node.self()}\"}")
    
    # Keep the server running and periodically send notifications
    spawn(fn ->
      weather_monitor_loop(server)
    end)
    
    {:ok, server}
  end
  
  @doc """
  Connect to the weather service from another node.
  Run this on node2.
  """
  def start_client(server_node \\ :"node1@localhost") do
    Logger.info("Connecting to weather service on #{server_node} from #{Node.self()}")
    
    # First, ensure we're connected to the server node
    case Node.connect(server_node) do
      true ->
        Logger.info("Connected to #{server_node}")
      false ->
        Logger.error("Failed to connect to #{server_node}")
        Logger.info("Make sure the server node is running with:")
        Logger.info("  iex --name node1@localhost --cookie democookie -S mix")
        return {:error, :node_not_reachable}
      result ->
        Logger.info("Already connected to #{server_node}: #{result}")
    end
    
    # Connect to the weather service
    {:ok, client} = ExMCP.Client.start_link(
      transport: :beam,
      server: {:weather_service, server_node}
    )
    
    Logger.info("Connected to weather service!")
    
    # Get server info
    {:ok, info} = ExMCP.Client.server_info(client)
    Logger.info("Server info: #{inspect(info)}")
    
    # Make some weather queries
    cities = ["London", "New York", "Tokyo", "Sydney"]
    
    for city <- cities do
      Logger.info("\nGetting weather for #{city}...")
      
      {:ok, %{"content" => content}} = ExMCP.Client.call_tool(
        client,
        "get_weather",
        %{"city" => city}
      )
      
      text = content |> List.first() |> Map.get("text")
      Logger.info(text)
    end
    
    # Get a forecast
    Logger.info("\nGetting 5-day forecast for Paris...")
    {:ok, %{"content" => content}} = ExMCP.Client.call_tool(
      client,
      "get_forecast",
      %{"city" => "Paris", "days" => 5}
    )
    
    text = content |> List.first() |> Map.get("text")
    Logger.info(text)
    
    {:ok, client}
  end
  
  @doc """
  Simulate network partition by disconnecting nodes.
  """
  def simulate_partition(node) do
    Logger.warning("Simulating network partition with #{node}")
    Node.disconnect(node)
    
    # Wait a bit
    Process.sleep(5000)
    
    Logger.info("Reconnecting to #{node}")
    Node.connect(node)
  end
  
  # Helper function to simulate weather updates
  defp weather_monitor_loop(server) do
    Process.sleep(30_000)  # Every 30 seconds
    
    # Simulate a weather alert
    if Enum.random(1..3) == 1 do
      Logger.info("Weather alert! Sending notification...")
      ExMCP.Server.notify_resources_changed(server)
    end
    
    weather_monitor_loop(server)
  end
  
  @doc """
  Full demonstration that can be run on a single node for testing.
  """
  def demo_single_node do
    Logger.info("Running distributed example on a single node (for demonstration)")
    
    # Start server
    {:ok, server} = start_server()
    
    # Start client on same node (normally would be different node)
    {:ok, client} = ExMCP.Client.start_link(
      transport: :beam,
      server: :weather_service
    )
    
    # Make a query
    {:ok, %{"content" => content}} = ExMCP.Client.call_tool(
      client,
      "get_weather",
      %{"city" => "London"}
    )
    
    text = content |> List.first() |> Map.get("text")
    Logger.info("Weather result:\n#{text}")
    
    # Cleanup
    GenServer.stop(client)
    GenServer.stop(server)
    
    :ok
  end
end