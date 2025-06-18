#!/usr/bin/env elixir

# Simple HTTP MCP Server using ExMCP v2 DSL with built-in HTTP transport
# Compare this to the legacy version - dramatically cleaner!

Mix.install([
  {:ex_mcp, path: "../../../"},
  {:plug_cowboy, "~> 2.7"},
  {:jason, "~> 1.4"}
])

defmodule SimpleHTTPServerV2 do
  @moduledoc """
  A simple HTTP MCP server using the v2 DSL with built-in HTTP transport.
  
  🎯 Key improvements over v1:
  - ✨ 20 lines of DSL vs 175 lines of manual Plug handling
  - 🛡️ Built-in JSON-RPC protocol handling
  - 📋 Auto-capability detection
  - 🔧 Built-in CORS support
  - 📝 Smart content helpers
  - 🚀 Zero-config HTTP transport
  """
  
  use ExMCP.ServerV2

  # Clean DSL instead of manual JSON-RPC handling!
  deftool "say_hello" do
    tool_description "Say hello via HTTP"
    
    args do
      field :name, :string, required: true, description: "Name to greet"
      field :enthusiasm, :integer, min: 1, max: 10, default: 5, description: "Enthusiasm level (1-10)"
    end
  end

  deftool "calculate" do
    tool_description "Perform basic calculations"
    
    args do
      field :operation, :string, required: true, description: "Operation: add, subtract, multiply, divide"
      field :a, :number, required: true, description: "First number"
      field :b, :number, required: true, description: "Second number"
    end
  end

  # Resources are automatically exposed via HTTP
  defresource "config://server/status" do
    resource_name "Server Status"
    resource_description "Current server status and metrics"
    mime_type "application/json"
  end

  defresource "api://documentation" do
    resource_name "API Documentation"
    resource_description "Server API documentation"
    mime_type "text/markdown"
  end

  # Prompts work seamlessly over HTTP
  defprompt "code_assistant" do
    prompt_name "Code Assistant"
    prompt_description "Helps with coding tasks"
    
    arguments do
      arg :language, required: true, description: "Programming language"
      arg :task, required: true, description: "What you want to do"
      arg :difficulty, description: "Difficulty level (beginner, intermediate, advanced)"
    end
  end

  # Handler implementations - much cleaner than manual HTTP handling!
  @impl true
  def handle_tool_call("say_hello", %{"name" => name, "enthusiasm" => enthusiasm}, state) do
    exclamation = String.duplicate("!", min(enthusiasm, 5))
    emoji = if enthusiasm > 7, do: " 🎉", else: " 🌐"
    
    content = [text("Hello, #{name}#{exclamation} Welcome to ExMCP v2 via HTTP#{emoji}")]
    {:ok, %{content: content}, state}
  end

  def handle_tool_call("say_hello", %{"name" => name}, state) do
    # Handle case where enthusiasm is not provided
    handle_tool_call("say_hello", %{"name" => name, "enthusiasm" => 5}, state)
  end

  @impl true
  def handle_tool_call("calculate", %{"operation" => op, "a" => a, "b" => b}, state) do
    result = case op do
      "add" -> a + b
      "subtract" -> a - b
      "multiply" -> a * b
      "divide" when b != 0 -> a / b
      "divide" -> {:error, "Division by zero"}
      _ -> {:error, "Unknown operation: #{op}"}
    end
    
    case result do
      {:error, message} ->
        error_content = [text("Error: #{message}")]
        {:ok, %{content: error_content, is_error: true}, state}
      
      value ->
        success_content = [text("Result: #{op}(#{a}, #{b}) = #{value}")]
        {:ok, %{content: success_content}, state}
    end
  end

  @impl true
  def handle_resource_read("config://server/status", _uri, state) do
    status = %{
      server: "SimpleHTTPServerV2",
      version: "2.0.0",
      dsl_version: "v2",
      uptime_seconds: System.system_time(:second) - (state[:start_time] || 0),
      capabilities: get_capabilities(),
      tools_count: map_size(get_tools()),
      resources_count: map_size(get_resources()),
      prompts_count: map_size(get_prompts()),
      improvements: [
        "20 lines of DSL vs 175 lines of manual HTTP handling",
        "Built-in JSON-RPC protocol support",
        "Auto-capability detection",
        "Built-in CORS support",
        "Zero-config HTTP transport"
      ]
    }
    
    content = [json(status)]
    {:ok, content, state}
  end

  @impl true
  def handle_resource_read("api://documentation", _uri, state) do
    docs = """
    # ExMCP v2 HTTP Server API

    ## Available Tools
    - `say_hello`: Greet someone with configurable enthusiasm
    - `calculate`: Perform basic math operations

    ## Available Resources  
    - `config://server/status`: Server status and metrics
    - `api://documentation`: This documentation

    ## Available Prompts
    - `code_assistant`: Get help with coding tasks

    ## v2 Improvements
    This server uses the ExMCP v2 DSL which provides:
    - ✨ Clean, expressive syntax
    - 🛡️ Compile-time validation
    - 📋 Auto-capability detection
    - 🔧 Built-in JSON Schema compilation
    - 🚀 Zero-config transport setup
    """
    
    content = [text(docs)]
    {:ok, content, state}
  end

  @impl true
  def handle_prompt_get("code_assistant", args, state) do
    language = Map.get(args, "language", "elixir")
    task = Map.get(args, "task", "general coding")
    difficulty = Map.get(args, "difficulty", "intermediate")
    
    messages = [
      system("You are an expert #{language} programmer who helps with coding tasks. Adjust your explanations based on the user's skill level."),
      user("I need help with #{task} in #{language}. My skill level is #{difficulty}."),
      assistant("I'd be happy to help you with #{task} in #{language}! Since you mentioned your level is #{difficulty}, I'll tailor my response accordingly. What specific aspect would you like to focus on?")
    ]
    
    {:ok, %{messages: messages}, state}
  end

  @impl true
  def init(_args) do
    start_time = System.system_time(:second)
    
    IO.puts("""
    🚀 ExMCP v2 HTTP Server Initializing...
    
    ✨ Auto-detected capabilities: #{inspect(Map.keys(get_capabilities()))}
    🔧 Registered tools: #{inspect(Map.keys(get_tools()))}
    📁 Available resources: #{inspect(Map.keys(get_resources()))}
    💭 Available prompts: #{inspect(Map.keys(get_prompts()))}
    
    🎯 v2 DSL Benefits:
    - Clean syntax vs manual HTTP handling
    - Built-in protocol support
    - Auto-capability detection
    - Zero-config transport
    """)
    
    {:ok, %{start_time: start_time}}
  end
end

# Start the server with built-in HTTP transport (much simpler than manual Plug!)
defmodule HTTPRunner do
  def start do
    port = 8321
    
    IO.puts("""
    🌐 Starting ExMCP v2 HTTP Server on port #{port}...
    
    🎯 Compare this to the legacy version:
    - v1: 175 lines of manual Plug/JSON-RPC handling  
    - v2: ~80 lines with rich DSL functionality
    - Built-in CORS, protocol handling, and more!
    """)

    # Start the v2 server
    {:ok, server} = SimpleHTTPServerV2.start_link()
    
    # Use the built-in HTTP transport (zero configuration!)
    {:ok, _transport} = ExMCP.Transport.HTTP.start_link(
      server: server,
      port: port,
      endpoint: "/",
      use_sse: false
    )
    
    IO.puts("""
    ✅ ExMCP v2 HTTP Server started on port #{port}!

    🧪 Test with curl:
    
    # Initialize
    curl -X POST http://localhost:#{port} \\
      -H "Content-Type: application/json" \\
      -d '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}'
    
    # List tools
    curl -X POST http://localhost:#{port} \\
      -H "Content-Type: application/json" \\
      -d '{"jsonrpc":"2.0","method":"tools/list","params":{},"id":2}'
    
    # Call say_hello tool
    curl -X POST http://localhost:#{port} \\
      -H "Content-Type: application/json" \\
      -d '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"say_hello","arguments":{"name":"Developer","enthusiasm":8}},"id":3}'
    
    # Call calculate tool
    curl -X POST http://localhost:#{port} \\
      -H "Content-Type: application/json" \\
      -d '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"calculate","arguments":{"operation":"add","a":10,"b":5}},"id":4}'
    
    # Read server status resource
    curl -X POST http://localhost:#{port} \\
      -H "Content-Type: application/json" \\
      -d '{"jsonrpc":"2.0","method":"resources/read","params":{"uri":"config://server/status"},"id":5}'
    
    # Get code assistant prompt
    curl -X POST http://localhost:#{port} \\
      -H "Content-Type: application/json" \\
      -d '{"jsonrpc":"2.0","method":"prompts/get","params":{"name":"code_assistant","arguments":{"language":"elixir","task":"web scraping","difficulty":"beginner"}},"id":6}'

    🎉 The v2 DSL makes MCP servers incredibly easy to build!
    Keep this script running to maintain the server.
    Press Ctrl+C to stop.
    """)
    
    # Keep running
    Process.sleep(:infinity)
  end
end

# Start the server
HTTPRunner.start()