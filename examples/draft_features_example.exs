#!/usr/bin/env elixir

Mix.install([
  {:ex_mcp, path: Path.expand("..", __DIR__)}
])

defmodule DraftFeaturesServer do
  @moduledoc """
  Example server demonstrating draft MCP specification features.
  
  ⚠️ WARNING: This example uses features from the draft MCP specification
  that are not part of the official MCP 2025-03-26 release. These features
  may change or be removed in future versions.
  
  Draft features demonstrated:
  1. Structured tool output (outputSchema and structuredContent)
  2. Logging level control (logging/setLevel)
  """
  
  use ExMCP.Server.Handler
  require Logger
  
  @impl true
  def init(_args) do
    Logger.info("Draft features server starting...")
    {:ok, %{log_level: :info}}
  end
  
  @impl true
  def handle_initialize(_params, state) do
    {:ok,
     %{
       protocolVersion: "2025-03-26",
       serverInfo: %{
         name: "draft-features-server",
         version: "1.0.0",
         # Note: These capabilities indicate draft feature support
         experimental: %{
           supports_draft_features: true
         }
       },
       capabilities: %{
         tools: %{},
         logging: %{}  # Required for logging/setLevel
       }
     }, state}
  end
  
  @impl true
  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        name: "get_weather",
        description: "Get weather data with structured output (draft feature)",
        inputSchema: %{
          type: "object",
          properties: %{
            location: %{type: "string", description: "City name"}
          },
          required: ["location"]
        },
        # DRAFT FEATURE: outputSchema defines the structure of structuredContent
        outputSchema: %{
          type: "object",
          properties: %{
            temperature: %{type: "number", description: "Temperature in Celsius"},
            conditions: %{type: "string", description: "Weather conditions"},
            humidity: %{type: "number", description: "Humidity percentage"},
            windSpeed: %{type: "number", description: "Wind speed in km/h"}
          },
          required: ["temperature", "conditions"]
        }
      },
      %{
        name: "calculate",
        description: "Basic calculator (standard MCP feature)",
        inputSchema: %{
          type: "object",
          properties: %{
            expression: %{type: "string", description: "Math expression"}
          },
          required: ["expression"]
        }
        # No outputSchema - this is a standard tool
      }
    ]
    
    {:ok, tools, nil, state}
  end
  
  @impl true
  def handle_call_tool("get_weather", %{"location" => location}, state) do
    log_at_level(state.log_level, "Fetching weather for #{location}")
    
    # Simulate weather data
    weather_data = %{
      "temperature" => 22.5 + :rand.uniform() * 10,
      "conditions" => Enum.random(["Sunny", "Cloudy", "Rainy", "Partly cloudy"]),
      "humidity" => 40 + :rand.uniform(40),
      "windSpeed" => 5 + :rand.uniform() * 20
    }
    
    # Return both regular content and structured content
    result = %{
      content: [
        %{
          type: "text",
          text: "Weather in #{location}: #{weather_data["temperature"]}°C, #{weather_data["conditions"]}"
        }
      ],
      # DRAFT FEATURE: structuredContent matches the outputSchema
      structuredContent: weather_data
    }
    
    {:ok, result, state}
  end
  
  def handle_call_tool("calculate", %{"expression" => expr}, state) do
    log_at_level(state.log_level, "Calculating: #{expr}")
    
    # Simple evaluation (for demo only)
    try do
      # WARNING: This is unsafe in production!
      {result, _} = Code.eval_string(expr)
      
      # Standard tool response (no structuredContent)
      {:ok, [%{type: "text", text: "Result: #{result}"}], state}
    rescue
      _ ->
        {:ok, %{
          content: [%{type: "text", text: "Error: Invalid expression"}],
          isError: true
        }, state}
    end
  end
  
  def handle_call_tool(name, _args, state) do
    {:error, "Unknown tool: #{name}", state}
  end
  
  @impl true
  def handle_set_log_level(level, state) do
    # DRAFT FEATURE: Handle logging/setLevel request
    atom_level = case level do
      "debug" -> :debug
      "info" -> :info
      "warning" -> :warning
      "error" -> :error
      _ -> :info
    end
    
    Logger.info("Log level changed from #{state.log_level} to #{atom_level}")
    {:ok, %{state | log_level: atom_level}}
  end
  
  # Helper function to log at the current level
  defp log_at_level(level, message) do
    case level do
      :debug -> Logger.debug(message)
      :info -> Logger.info(message)
      :warning -> Logger.warning(message)
      :error -> Logger.error(message)
      _ -> Logger.info(message)
    end
  end
end

# Example client code to test draft features
defmodule DraftFeaturesClient do
  def demo do
    Logger.info("Starting draft features demo...")
    
    # Start server
    {:ok, server} = ExMCP.Server.start_link(
      transport: :stdio,
      handler: DraftFeaturesServer
    )
    
    # In a real scenario, you would connect from a separate process
    # For this demo, we'll show the expected client usage:
    
    IO.puts("""
    
    Draft Features Demo Server Started!
    
    This server demonstrates draft MCP features:
    
    1. Structured Tool Output:
       - The 'get_weather' tool returns both text and structured data
       - Check the outputSchema field in the tool definition
       - Response includes structuredContent matching the schema
    
    2. Logging Level Control:
       - Clients can send logging/setLevel requests
       - Valid levels: debug, info, warning, error
    
    Connect with a client that supports draft features to test:
    
    # Example client code:
    {:ok, client} = ExMCP.Client.start_link(transport: :stdio)
    
    # Test structured output
    {:ok, result} = ExMCP.Client.call_tool(client, "get_weather", %{"location" => "London"})
    IO.inspect(result.structuredContent)  # Draft feature!
    
    # Test logging level control (draft feature)
    {:ok, _} = ExMCP.Client.set_log_level(client, "debug")
    
    """)
    
    # Keep server running
    Process.sleep(:infinity)
  end
end

# Run the demo
DraftFeaturesClient.demo()