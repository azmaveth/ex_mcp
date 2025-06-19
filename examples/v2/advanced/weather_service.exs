#!/usr/bin/env elixir

# Weather Service HTTP Server Example
# Demonstrates meta blocks and defhandler with HTTP/SSE transport

Mix.install([
  {:ex_mcp, path: Path.expand("../../..", __DIR__)},
  {:plug_cowboy, "~> 2.7"}
])

defmodule WeatherServer do
  use ExMCP.ServerV2
  
  @name "weather-service"
  @version "2.0.0"
  
  # Weather data tool
  deftool "get_weather" do
    meta do
      name "Get Weather"
      description "Get current weather for a location"
    end
    
    input_schema %{
      type: "object", 
      properties: %{
        location: %{type: "string", description: "City name or coordinates"},
        units: %{type: "string", enum: ["celsius", "fahrenheit"], default: "celsius"}
      },
      required: ["location"]
    }
    
  end
  
  # Weather forecast tool
  deftool "get_forecast" do
    meta do
      name "Get Forecast"
      description "Get weather forecast for upcoming days"
    end
    
    args do
      field :location, :string, required: true
      field :days, :integer, default: 5, min: 1, max: 10
      field :units, :string, enum: ["celsius", "fahrenheit"], default: "celsius"
    end
    
  end
  
  # Weather alerts tool with progress
  deftool "check_alerts" do
    meta do
      name "Check Alerts"
      description "Check for weather alerts in multiple locations"
    end
    
    input_schema %{
      type: "object",
      properties: %{
        locations: %{
          type: "array",
          items: %{type: "string"},
          minItems: 1,
          maxItems: 10
        },
        severity: %{
          type: "string",
          enum: ["all", "moderate", "severe", "extreme"],
          default: "all"
        },
        _progressToken: %{type: "string"}
      },
      required: ["locations"]
    }
    
  end
  
  # Weather data resource
  defresource "weather://current/*" do
    meta do
      name "Current Weather Data"
      description "Real-time weather for a specific location"
    end
    mime_type "application/json"
    
  end
  
  # Weather map resource
  defresource "weather://map" do
    meta do
      name "Weather Map"
      description "Interactive weather map data"
    end
    mime_type "application/json"
    annotations %{
      supports_subscribe: true,
      update_frequency: 300  # 5 minutes
    }
    
  end
  
  # Weather assistant prompt
  defprompt "weather_assistant" do
    meta do
      name "Weather Assistant"
      description "Natural language weather queries"
    end
    
    arguments [
      %{name: "query", type: "string", required: true},
      %{name: "user_location", type: "string"},
      %{name: "preferences", type: "object"}
    ]
    
  end
  
  # Tool handlers with mock weather data
  defhandler :tool, "get_weather", args, state do
    location = args["location"]
    units = args["units"] || "celsius"
    
    # Mock weather data
    temp_c = :rand.uniform(35) - 5  # -5 to 30°C
    temp = if units == "fahrenheit", do: temp_c * 9/5 + 32, else: temp_c
    unit_symbol = if units == "fahrenheit", do: "°F", else: "°C"
    
    weather_data = %{
      location: location,
      temperature: Float.round(temp, 1),
      unit: unit_symbol,
      conditions: Enum.random(["Sunny", "Partly Cloudy", "Cloudy", "Rainy", "Stormy"]),
      humidity: :rand.uniform(100),
      wind_speed: :rand.uniform(30),
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }
    
    response = json(weather_data)
    new_state = %{state | requests: state.requests + 1}
    {:ok, [response], new_state}
  end
  
  defhandler :tool, "get_forecast", args, state do
    location = args["location"]
    days = args["days"] || 5
    units = args["units"] || "celsius"
    
    unit_symbol = if units == "fahrenheit", do: "°F", else: "°C"
    
    # Generate mock forecast
    forecast = Enum.map(1..days, fn day ->
      date = Date.add(Date.utc_today(), day)
      temp_high_c = :rand.uniform(30) + 5
      temp_low_c = temp_high_c - :rand.uniform(10) - 5
      
      high = if units == "fahrenheit", do: temp_high_c * 9/5 + 32, else: temp_high_c
      low = if units == "fahrenheit", do: temp_low_c * 9/5 + 32, else: temp_low_c
      
      %{
        date: Date.to_string(date),
        high: Float.round(high, 1),
        low: Float.round(low, 1),
        unit: unit_symbol,
        conditions: Enum.random(["Sunny", "Partly Cloudy", "Cloudy", "Rainy", "Stormy"]),
        precipitation_chance: :rand.uniform(100)
      }
    end)
    
    response = json(%{
      location: location,
      forecast: forecast,
      generated_at: DateTime.utc_now() |> DateTime.to_iso8601()
    })
    
    new_state = %{state | requests: state.requests + 1}
    {:ok, [response], new_state}
  end
  
  defhandler :tool, "check_alerts", args, state do
    locations = args["locations"]
    severity = args["severity"] || "all"
    _progress_token = args["_progressToken"]
    
    # In a real app, we'd send progress updates for each location
    # For now, just generate mock alerts
    
    alerts = Enum.flat_map(locations, fn location ->
      # 30% chance of an alert for each location
      if :rand.uniform(10) <= 3 do
        alert_severity = Enum.random(["moderate", "severe", "extreme"])
        
        if severity == "all" || alert_severity == severity || 
           (severity in ["moderate", "severe"] && alert_severity == "extreme") do
          [%{
            location: location,
            severity: alert_severity,
            type: Enum.random(["Thunderstorm", "Heavy Rain", "High Winds", "Extreme Heat", "Flash Flood"]),
            message: "Weather alert for #{location}",
            valid_until: DateTime.add(DateTime.utc_now(), 3600 * :rand.uniform(24), :second) |> DateTime.to_iso8601()
          }]
        else
          []
        end
      else
        []
      end
    end)
    
    response = json(%{
      alerts: alerts,
      checked_locations: length(locations),
      total_alerts: length(alerts)
    })
    
    new_state = %{state | requests: state.requests + 1}
    {:ok, [response], new_state}
  end
  
  # Resource handlers
  defhandler :resource, "weather://current/" <> location, _uri, state do
    # Reuse the weather logic
    temp_c = :rand.uniform(35) - 5
    
    weather_data = %{
      location: location,
      temperature: Float.round(temp_c, 1),
      unit: "°C",
      conditions: Enum.random(["Sunny", "Partly Cloudy", "Cloudy", "Rainy", "Stormy"]),
      humidity: :rand.uniform(100),
      wind_speed: :rand.uniform(30),
      pressure: 950 + :rand.uniform(100),
      visibility: :rand.uniform(20),
      uv_index: :rand.uniform(11),
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }
    
    content = json(weather_data)
    {:ok, [content], state}
  end
  
  defhandler :resource, "weather://map", _uri, state do
    # Mock map data with several cities
    cities = [
      %{name: "New York", lat: 40.7128, lon: -74.0060},
      %{name: "London", lat: 51.5074, lon: -0.1278},
      %{name: "Tokyo", lat: 35.6762, lon: 139.6503},
      %{name: "Sydney", lat: -33.8688, lon: 151.2093},
      %{name: "Paris", lat: 48.8566, lon: 2.3522}
    ]
    
    map_data = %{
      type: "weather_map",
      last_updated: DateTime.utc_now() |> DateTime.to_iso8601(),
      cities: Enum.map(cities, fn city ->
        Map.merge(city, %{
          temperature: :rand.uniform(35) - 5,
          conditions: Enum.random(["Sunny", "Partly Cloudy", "Cloudy", "Rainy"])
        })
      end)
    }
    
    content = json(map_data)
    {:ok, [content], state}
  end
  
  # Prompt handler
  defhandler :prompt, "weather_assistant", args, state do
    query = args["query"]
    user_location = args["user_location"]
    preferences = args["preferences"] || %{}
    
    context = []
    context = if user_location, do: ["User is in: #{user_location}" | context], else: context
    context = if preferences["units"], do: ["Preferred units: #{preferences["units"]}" | context], else: context
    
    messages = [
      system("You are a helpful weather assistant. Provide accurate, concise weather information. " <>
             if(context != [], do: "Context: #{Enum.join(context, ", ")}", else: "")),
      user(query)
    ]
    
    {:ok, %{messages: messages}, state}
  end
  
  @impl true
  def init(_args) do
    {:ok, %{requests: 0}}
  end
  
  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      name: @name,
      version: @version,
      capabilities: %{
        tools: %{},
        resources: %{subscribe: true},
        prompts: %{}
      }
    }, state}
  end
end

# Web router
defmodule WeatherRouter do
  use Plug.Router
  
  plug :match
  plug :dispatch
  
  # MCP endpoint with SSE support
  post "/mcp" do
    conn
    |> put_resp_header("access-control-allow-origin", "*")
    |> ExMCP.HttpPlug.handle(WeatherServer)
  end
  
  # Health check
  get "/health" do
    send_resp(conn, 200, Jason.encode!(%{status: "ok", service: "weather-server"}))
  end
  
  # CORS preflight
  options "/mcp" do
    conn
    |> put_resp_header("access-control-allow-origin", "*")
    |> put_resp_header("access-control-allow-methods", "POST, OPTIONS")
    |> put_resp_header("access-control-allow-headers", "content-type")
    |> send_resp(204, "")
  end
  
  match _ do
    send_resp(conn, 404, "Not found")
  end
end

# Start server
port = 8080
IO.puts("""
==========================================
ExMCP v2 Weather Service
==========================================

Starting Weather Service on port #{port}...

Features:
- Current weather data
- Multi-day forecasts  
- Weather alerts with progress
- Interactive weather map
- Natural language queries

Endpoints:
  POST http://localhost:#{port}/mcp
  GET  http://localhost:#{port}/health

Example curl commands:

1. Initialize:
   curl -X POST http://localhost:#{port}/mcp \\
     -H "Content-Type: application/json" \\
     -d '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}'

2. Get weather:
   curl -X POST http://localhost:#{port}/mcp \\
     -H "Content-Type: application/json" \\
     -d '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"get_weather","arguments":{"location":"San Francisco"}},"id":2}'

3. Get forecast:
   curl -X POST http://localhost:#{port}/mcp \\
     -H "Content-Type: application/json" \\
     -d '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"get_forecast","arguments":{"location":"New York","days":7}},"id":3}'
""")

Plug.Cowboy.http(WeatherRouter, [], port: port)
Process.sleep(:infinity)