#!/usr/bin/env elixir

# MCP Server that communicates via stdin/stdout using ExMCP v2 DSL
# Working version that handles stdio communication manually

defmodule StdioServerV2 do
  @moduledoc """
  A simple MCP server using the v2 DSL that reads JSON-RPC from stdin and writes to stdout.
  
  🎯 Key improvements over v1:
  - ✨ 15 lines of DSL vs 151 lines of boilerplate
  - 🛡️ Compile-time validation 
  - 📋 Auto-capability detection
  - 🔧 Built-in JSON Schema compilation
  - 📝 Smart content helpers
  """
  
  use ExMCP.ServerV2

  # Look how simple this is compared to the legacy version!
  deftool "say_hello" do
    tool_description "Say hello to someone via stdio"
    
    args do
      field :name, :string, required: true, description: "Name to greet"
    end
  end

  deftool "echo" do
    tool_description "Echo back the input message"
    
    args do
      field :message, :string, required: true, description: "Message to echo"
      field :uppercase, :boolean, default: false, description: "Convert to uppercase"
    end
  end

  # Add a resource to showcase v2 capabilities
  defresource "config://server/info" do
    resource_name "Server Information"
    resource_description "Information about this stdio server"
    mime_type "application/json"
  end

  # Add a prompt to showcase v2 capabilities  
  defprompt "greeting_style" do
    prompt_name "Greeting Style Prompt"
    prompt_description "Generate greetings in different styles"
    
    arguments do
      arg :style, required: true, description: "Greeting style (formal, casual, funny)"
      arg :name, required: true, description: "Name to greet"
    end
  end

  # Handler implementations - much cleaner than manual JSON-RPC!
  @impl true
  def handle_tool_call("say_hello", %{"name" => name}, state) do
    content = [text("Hello, #{name}! Welcome to ExMCP v2 via stdio! 📝✨")]
    {:ok, %{content: content}, state}
  end

  @impl true
  def handle_tool_call("echo", %{"message" => message, "uppercase" => uppercase}, state) do
    result_message = if uppercase, do: String.upcase(message), else: message
    content = [text("Echo: #{result_message}")]
    {:ok, %{content: content}, state}
  end

  def handle_tool_call("echo", %{"message" => message}, state) do
    # Handle case where uppercase is not provided
    handle_tool_call("echo", %{"message" => message, "uppercase" => false}, state)
  end

  @impl true
  def handle_resource_read("config://server/info", _uri, state) do
    server_info = %{
      name: "StdioServerV2",
      version: "2.0.0",
      dsl_version: "v2",
      capabilities: get_capabilities(),
      improvements: [
        "15 lines of DSL vs 151 lines of boilerplate",
        "Compile-time validation",
        "Auto-capability detection", 
        "Built-in JSON Schema compilation",
        "Smart content helpers"
      ]
    }
    
    content = [json(server_info)]
    {:ok, content, state}
  end

  @impl true
  def handle_prompt_get("greeting_style", args, state) do
    style = Map.get(args, "style", "casual")
    name = Map.get(args, "name", "Friend")
    
    messages = case style do
      "formal" ->
        [
          system("You are a formal and professional assistant."),
          user("Please provide a formal greeting for #{name}"),
          assistant("Good day, #{name}. I hope this message finds you well.")
        ]
      "funny" ->
        [
          system("You are a humorous and playful assistant."),
          user("Give #{name} a funny greeting"),
          assistant("Hey there, #{name}! *tips hat* Ready to conquer the world... or at least this conversation? 😄")
        ]
      _ -> # casual
        [
          system("You are a friendly and casual assistant."),
          user("Say hi to #{name} in a casual way"),
          assistant("Hey #{name}! How's it going? Great to see you here! 👋")
        ]
    end
    
    {:ok, %{messages: messages}, state}
  end

end

# stdio communication handler
defmodule StdioHandler do
  def start do
    IO.puts(:stderr, """
    📡 ExMCP v2 stdio Server Ready!
    
    🎯 Compare this to the legacy version:
    - v1: 151 lines of manual JSON-RPC handling
    - v2: ~50 lines with rich DSL functionality
    
    Example usage with ExMCP client:
    {"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"test","version":"1.0.0"}},"id":1}
    {"jsonrpc":"2.0","method":"tools/list","params":{},"id":2}
    {"jsonrpc":"2.0","method":"tools/call","params":{"name":"say_hello","arguments":{"name":"World"}},"id":3}
    {"jsonrpc":"2.0","method":"tools/call","params":{"name":"echo","arguments":{"message":"Hello DSL!","uppercase":true}},"id":4}
    {"jsonrpc":"2.0","method":"resources/read","params":{"uri":"config://server/info"},"id":5}
    {"jsonrpc":"2.0","method":"prompts/get","params":{"name":"greeting_style","arguments":{"style":"funny","name":"Developer"}},"id":6}
    
    Press Ctrl+D to exit.
    """)

    # Start the v2 server
    {:ok, server} = StdioServerV2.start_link()
    
    # Handle stdio communication manually (like v1 but with v2 server)
    loop(server, "")
  end
  
  defp loop(server, buffer) do
    case IO.read(:stdio, :line) do
      :eof ->
        IO.puts(:stderr, "EOF received, shutting down")
        GenServer.stop(server)
        :ok
        
      {:error, reason} ->
        IO.puts(:stderr, "Read error: #{inspect(reason)}")
        GenServer.stop(server)
        :ok
        
      data ->
        buffer = buffer <> data
        
        # Try to parse complete JSON objects
        case Jason.decode(buffer) do
          {:ok, message} ->
            handle_message(server, message)
            loop(server, "")  # Reset buffer after successful parse
            
          {:error, _} ->
            # Not a complete JSON object yet, keep buffering
            loop(server, buffer)
        end
    end
  end
  
  defp handle_message(server, message) do
    # Use the v2 server to handle the message
    case ExMCP.ServerV2.handle_message(server, message) do
      {:ok, response} ->
        IO.puts(Jason.encode!(response))
      {:error, error} ->
        # Send error response
        error_response = %{
          "jsonrpc" => "2.0",
          "id" => Map.get(message, "id"),
          "error" => %{
            "code" => -32603,
            "message" => "Internal error",
            "data" => inspect(error)
          }
        }
        IO.puts(Jason.encode!(error_response))
    end
  end
end

# Start the server
StdioHandler.start()