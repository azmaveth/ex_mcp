#!/usr/bin/env elixir

# Weather Service MCP Server
# 
# A practical example showing how to build a weather information
# service using ExMCP's DSL. This demonstrates real-world patterns
# for building useful MCP tools.

Mix.install([
  {:ex_mcp, path: Path.expand("..", __DIR__)},
  {:jason, "~> 1.4"}
])

defmodule WeatherService do
  use ExMCP.Server
  
  @impl true
  def init(_args) do
    # In a real service, you might initialize with API keys, cache, etc.
    {:ok, %{
      cache: %{},
      favorites: ["New York", "London", "Tokyo"],
      units: "celsius"
    }}
  end
  
  # Weather Query Tool
  deftool "get_weather" do
    description "Get current weather for a location"
    
    args do
      field :location, :string, 
        required: true,
        description: "City name or coordinates (lat,lon)"
        
      field :units, :string,
        enum: ["celsius", "fahrenheit", "kelvin"],
        default: "celsius",
        description: "Temperature units"
        
      field :detailed, :boolean,
        default: false,
        description: "Include detailed forecast"
    end
  end
  
  # Weather Forecast Tool
  deftool "get_forecast" do
    description "Get weather forecast for multiple days"
    
    args do
      field :location, :string, required: true
      field :days, :integer,
        minimum: 1,
        maximum: 10,
        default: 5,
        description: "Number of days to forecast"
    end
  end
  
  # Weather Comparison Tool
  deftool "compare_weather" do
    description "Compare weather between multiple locations"
    
    args do
      field :locations, :array do
        items :string
        min_items 2
        max_items 5
        description "List of locations to compare"
      end
    end
  end
  
  # Favorite Locations Resource
  defresource "weather://favorites" do
    name "Favorite Locations"
    description "List of saved favorite locations"
    mime_type "application/json"
  end
  
  # Current Conditions Resource (pattern-based)
  defresource "weather://current/*" do
    name "Current Weather Data"
    description "Real-time weather data for locations"
    mime_type "application/json"
    list_pattern true
    subscribable true
  end
  
  # Weather Assistant Prompt
  defprompt "weather_assistant" do
    name "Weather Planning Assistant"
    description "Helps plan activities based on weather"
    
    arguments do
      arg :activity, required: true, description: "What you want to do"
      arg :location, required: true, description: "Where you'll be"
      arg :date, description: "When (defaults to today)"
      arg :preferences, description: "Weather preferences"
    end
  end
  
  # Travel Weather Prompt
  defprompt "travel_weather" do
    name "Travel Weather Advisor"
    description "Weather advice for travelers"
    
    arguments do
      arg :destination, required: true
      arg :travel_dates, required: true
      arg :activities, description: "Planned activities"
    end
  end
  
  # Handler Implementations
  
  @impl true
  def handle_tool_call("get_weather", args, state) do
    location = args["location"]
    units = Map.get(args, "units", state.units)
    detailed = Map.get(args, "detailed", false)
    
    # Simulate weather data (in production, call actual weather API)
    weather_data = generate_weather_data(location, units)
    
    content = if detailed do
      [
        text("Current weather in #{location}:"),
        json(weather_data),
        text(format_detailed_weather(weather_data))
      ]
    else
      [text(format_simple_weather(location, weather_data, units))]
    end
    
    # Cache the result
    new_cache = Map.put(state.cache, location, weather_data)
    new_state = %{state | cache: new_cache}
    
    {:ok, %{content: content}, new_state}
  end
  
  @impl true
  def handle_tool_call("get_forecast", %{"location" => location, "days" => days}, state) do
    forecast = Enum.map(1..days, fn day ->
      %{
        day: day,
        date: Date.add(Date.utc_today(), day) |> Date.to_string(),
        high: 20 + :rand.uniform(10),
        low: 10 + :rand.uniform(10),
        condition: Enum.random(["sunny", "cloudy", "rainy", "partly cloudy"]),
        precipitation: :rand.uniform(100)
      }
    end)
    
    content = [
      text("#{days}-day forecast for #{location}:"),
      json(%{location: location, forecast: forecast})
    ]
    
    {:ok, %{content: content}, state}
  end
  
  @impl true
  def handle_tool_call("compare_weather", %{"locations" => locations}, state) do
    comparisons = Enum.map(locations, fn location ->
      weather = generate_weather_data(location, state.units)
      %{
        location: location,
        temperature: weather.temperature,
        condition: weather.condition,
        humidity: weather.humidity
      }
    end)
    
    content = [
      text("Weather comparison:"),
      json(%{comparisons: comparisons}),
      text(format_comparison(comparisons))
    ]
    
    {:ok, %{content: content}, state}
  end
  
  @impl true
  def handle_resource_read("weather://favorites", _uri, state) do
    content = [json(%{favorites: state.favorites, default_units: state.units})]
    {:ok, content, state}
  end
  
  @impl true
  def handle_resource_read("weather://current/" <> location, _uri, state) do
    weather = case Map.get(state.cache, location) do
      nil -> generate_weather_data(location, state.units)
      cached -> cached
    end
    
    {:ok, [json(weather)], state}
  end
  
  @impl true
  def handle_resource_subscribe("weather://current/" <> location, state) do
    # In production, this would set up real-time weather updates
    IO.puts("Subscribed to weather updates for: #{location}")
    {:ok, state}
  end
  
  @impl true
  def handle_prompt_get("weather_assistant", args, state) do
    activity = args["activity"]
    location = args["location"]
    date = Map.get(args, "date", "today")
    preferences = Map.get(args, "preferences", "comfortable weather")
    
    messages = [
      system("You are a helpful weather planning assistant. Provide practical advice based on weather conditions."),
      user("""
      I want to #{activity} in #{location} on #{date}.
      My preferences: #{preferences}
      
      What's your weather-based recommendation?
      """),
      assistant("""
      I'll help you plan your #{activity} in #{location}! Let me check the weather conditions for #{date} and provide recommendations based on your preferences for #{preferences}.
      """)
    ]
    
    {:ok, %{messages: messages}, state}
  end
  
  @impl true
  def handle_prompt_get("travel_weather", args, state) do
    destination = args["destination"]
    dates = args["travel_dates"]
    activities = Map.get(args, "activities", "general sightseeing")
    
    messages = [
      system("You are a travel weather advisor. Provide packing and activity suggestions based on weather forecasts."),
      user("I'm traveling to #{destination} during #{dates}. Planning to: #{activities}"),
      assistant("I'll help you prepare for your trip to #{destination}! Let me analyze the weather patterns for #{dates} and suggest what to pack and the best times for your planned activities.")
    ]
    
    {:ok, %{messages: messages}, state}
  end
  
  # Helper Functions
  
  defp generate_weather_data(location, units) do
    # Simulate weather data
    base_temp = 20 + :rand.uniform(15) - 7
    
    temp = case units do
      "fahrenheit" -> round(base_temp * 9/5 + 32)
      "kelvin" -> round(base_temp + 273.15)
      _ -> base_temp
    end
    
    %{
      location: location,
      temperature: temp,
      units: units,
      condition: Enum.random(["sunny", "cloudy", "rainy", "partly cloudy", "stormy"]),
      humidity: 40 + :rand.uniform(40),
      wind_speed: :rand.uniform(30),
      wind_direction: Enum.random(["N", "NE", "E", "SE", "S", "SW", "W", "NW"]),
      pressure: 1000 + :rand.uniform(30),
      uv_index: :rand.uniform(11),
      visibility: 5 + :rand.uniform(10),
      feels_like: temp + :rand.uniform(5) - 2,
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }
  end
  
  defp format_simple_weather(location, data, units) do
    unit_symbol = case units do
      "fahrenheit" -> "°F"
      "kelvin" -> "K"
      _ -> "°C"
    end
    
    "#{location}: #{data.temperature}#{unit_symbol}, #{data.condition}, #{data.humidity}% humidity"
  end
  
  defp format_detailed_weather(data) do
    """
    Temperature: #{data.temperature}° (feels like #{data.feels_like}°)
    Condition: #{data.condition}
    Humidity: #{data.humidity}%
    Wind: #{data.wind_speed} km/h #{data.wind_direction}
    Pressure: #{data.pressure} hPa
    UV Index: #{data.uv_index}
    Visibility: #{data.visibility} km
    """
  end
  
  defp format_comparison(comparisons) do
    comparisons
    |> Enum.map(fn c ->
      "#{c.location}: #{c.temperature}°, #{c.condition}"
    end)
    |> Enum.join("\n")
  end
end

# Server Runner
defmodule WeatherServiceRunner do
  def run do
    IO.puts("🌤️  Starting Weather Service MCP Server...")
    IO.puts("=" <> String.duplicate("=", 50))
    
    {:ok, _server} = WeatherService.start_link(
      transport: :stdio,
      name: :weather_service
    )
    
    IO.puts("\nAvailable Tools:")
    IO.puts("  • get_weather - Get current weather with options")
    IO.puts("  • get_forecast - Multi-day weather forecast")
    IO.puts("  • compare_weather - Compare weather across locations")
    
    IO.puts("\nAvailable Resources:")
    IO.puts("  • weather://favorites - Saved favorite locations")
    IO.puts("  • weather://current/* - Real-time weather (subscribable)")
    
    IO.puts("\nAvailable Prompts:")
    IO.puts("  • weather_assistant - Activity planning help")
    IO.puts("  • travel_weather - Travel weather advisor")
    
    IO.puts("\nServer is ready for connections!")
    
    Process.sleep(:infinity)
  end
end

# Run if executed directly
if System.get_env("MIX_ENV") != "test" do
  WeatherServiceRunner.run()
end