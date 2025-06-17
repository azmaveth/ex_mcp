#!/usr/bin/env elixir

# Example: Bi-directional communication where server makes requests to client

Mix.install([
  {:ex_mcp, path: "."}
])

defmodule WeatherClientHandler do
  @behaviour ExMCP.Client.Handler

  @impl true
  def init(_args) do
    # Initialize with some weather data
    state = %{
      weather_data: %{
        "New York" => %{temp: 72, condition: "Sunny"},
        "London" => %{temp: 59, condition: "Cloudy"},
        "Tokyo" => %{temp: 68, condition: "Clear"}
      },
      roots: [
        %{uri: "weather://current", name: "Current Weather"},
        %{uri: "weather://forecast", name: "Weather Forecast"}
      ]
    }
    {:ok, state}
  end

  @impl true
  def handle_ping(state) do
    IO.puts("[Client] Received ping from server")
    {:ok, %{}, state}
  end

  @impl true
  def handle_list_roots(state) do
    IO.puts("[Client] Server requested roots")
    {:ok, state.roots, state}
  end

  @impl true
  def handle_create_message(params, state) do
    IO.puts("[Client] Server requested LLM sampling")
    messages = params["messages"]
    
    # Extract the question from the last message
    last_message = List.last(messages)
    question = last_message["content"]
    
    # Simple response based on weather data
    response = case String.downcase(question) do
      text when text =~ "weather" ->
        cities = Map.keys(state.weather_data) |> Enum.join(", ")
        "I can provide weather information for: #{cities}. Which city interests you?"
      
      text when text =~ "new york" ->
        data = state.weather_data["New York"]
        "The weather in New York is #{data.condition} with a temperature of #{data.temp}°F."
      
      text when text =~ "london" ->
        data = state.weather_data["London"]
        "The weather in London is #{data.condition} with a temperature of #{data.temp}°F."
      
      text when text =~ "tokyo" ->
        data = state.weather_data["Tokyo"]
        "The weather in Tokyo is #{data.condition} with a temperature of #{data.temp}°F."
      
      _ ->
        "I can help with weather information. Ask me about the weather in New York, London, or Tokyo!"
    end

    result = %{
      "role" => "assistant",
      "content" => %{
        "type" => "text",
        "text" => response
      },
      "model" => "weather-bot-1.0"
    }
    
    {:ok, result, state}
  end

  @impl true
  def terminate(_reason, _state) do
    IO.puts("[Client] Handler terminating")
    :ok
  end
end

defmodule WeatherServer do
  @behaviour ExMCP.Server.Handler

  @impl true
  def init(_args) do
    {:ok, %{}}
  end

  @impl true
  def handle_initialize(_params, state) do
    capabilities = %{
      "tools" => %{},
      "resources" => %{},
      "prompts" => %{},
      "roots" => %{},
      "sampling" => %{}  # Server can make sampling requests
    }
    
    server_info = %{
      "name" => "weather-server",
      "version" => "1.0.0",
      "capabilities" => capabilities
    }
    
    {:ok, server_info, state}
  end

  @impl true
  def handle_list_tools(state) do
    tools = [
      %{
        "name" => "ask_weather_bot",
        "description" => "Ask the weather bot a question",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "question" => %{"type" => "string", "description" => "Question about weather"}
          },
          "required" => ["question"]
        }
      }
    ]
    {:ok, tools, state}
  end

  @impl true
  def handle_call_tool("ask_weather_bot", %{"question" => question}, state) do
    IO.puts("[Server] Tool called with question: #{question}")
    
    # Server makes a request to the client
    case ExMCP.Server.create_message(self(), %{
      "messages" => [
        %{"role" => "user", "content" => question}
      ],
      "modelPreferences" => %{
        "hints" => ["weather-bot"],
        "temperature" => 0.3
      }
    }) do
      {:ok, response} ->
        content = response["content"]["text"]
        {:ok, %{"content" => [%{"type" => "text", "text" => content}]}, state}
      
      {:error, error} ->
        {:error, "Failed to get response: #{inspect(error)}", state}
    end
  end

  # Implement other required callbacks with empty responses
  def handle_list_resources(state), do: {:ok, [], state}
  def handle_read_resource(_uri, state), do: {:error, "Not found", state}
  def handle_list_prompts(state), do: {:ok, [], state}
  def handle_get_prompt(_name, _args, state), do: {:error, "Not found", state}
  def handle_list_resource_templates(state), do: {:ok, [], state}
  def handle_complete(_ref, _arg, state), do: {:ok, %{}, state}
  def terminate(_reason, _state), do: :ok
end

# Run the example
defmodule BidirectionalExample do
  def run do
    IO.puts("=== Bi-directional Communication Example ===\n")

    # Start server
    {:ok, server} = ExMCP.Server.start_link(
      transport: :stdio,
      name: :weather_server,
      handler: WeatherServer
    )

    # Start client with handler
    {:ok, client} = ExMCP.Client.start_link(
      transport: :stdio,
      server: :weather_server,
      handler: WeatherClientHandler
    )

    # Wait for initialization
    Process.sleep(100)

    # Test server -> client communication
    IO.puts("\n1. Testing server pinging client:")
    {:ok, _} = ExMCP.Server.ping(server)

    IO.puts("\n2. Testing server requesting client roots:")
    {:ok, roots} = ExMCP.Server.list_roots(server)
    IO.puts("   Client roots: #{inspect(roots)}")

    IO.puts("\n3. Testing tool that triggers server->client request:")
    {:ok, result} = ExMCP.Client.call_tool(client, "ask_weather_bot", %{
      "question" => "What's the weather like?"
    })
    IO.puts("   Response: #{inspect(result)}")

    IO.puts("\n4. Testing specific city query:")
    {:ok, result} = ExMCP.Client.call_tool(client, "ask_weather_bot", %{
      "question" => "How's the weather in Tokyo?"
    })
    IO.puts("   Response: #{inspect(result)}")

    # Cleanup
    Process.sleep(100)
    ExMCP.Client.stop(client)
    GenServer.stop(server)
    
    IO.puts("\n=== Example completed ===")
  end
end

BidirectionalExample.run()