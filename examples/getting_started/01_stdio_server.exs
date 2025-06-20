#!/usr/bin/env elixir

# STDIO Server with Hello World Tool
# 
# This server demonstrates the simplest transport: stdio
# It provides a single "hello" tool that greets users

# CRITICAL: Configure logging BEFORE Mix.install to prevent stdout contamination
# MCP STDIO protocol requires clean JSON-RPC on stdout - no other output allowed
Application.put_env(:ex_mcp, :stdio_mode, true)

# Set minimal startup delay for faster response
Application.put_env(:ex_mcp, :stdio_startup_delay, 10)

# Suppress all logging for clean STDIO JSON-RPC
System.put_env("ELIXIR_LOG_LEVEL", "emergency")
Application.put_env(:logger, :level, :emergency)
Application.put_env(:horde, :log_level, :emergency)
:logger.set_primary_config(:level, :emergency)

Mix.install([
  {:ex_mcp, path: Path.expand("../..", __DIR__)},
  {:jason, "~> 1.4"},
  {:ex_json_schema, "~> 0.10"},
  {:html_entities, "~> 0.5"}
], verbose: false)

# Configure logging again after Mix.install
Logger.configure(level: :emergency)
Application.put_env(:logger, :level, :emergency)
Application.put_env(:horde, :log_level, :emergency)

defmodule StdioHelloServer do
  use ExMCP.Server
  
  @impl true
  def init(_args) do
    IO.puts(:stderr, "Starting STDIO Hello Server...")
    IO.puts(:stderr, "Server ready to accept connections on stdio")
    {:ok, %{call_count: 0}}
  end
  
  # Define a simple hello tool
  deftool "hello" do
    meta do
      description "Says hello to someone"
    end
    
    args do
      field :name, :string, 
        required: true, 
        description: "Name of the person to greet"
        
      field :language, :string,
        enum: ["english", "spanish", "french", "japanese"],
        default: "english",
        description: "Language for the greeting"
    end
  end
  
  @impl true
  def handle_tool_call("hello", args, state) do
    name = args["name"]
    language = Map.get(args, "language", "english")
    
    greeting = case language do
      "spanish" -> "¡Hola, #{name}!"
      "french" -> "Bonjour, #{name}!"
      "japanese" -> "こんにちは、#{name}さん!"
      _ -> "Hello, #{name}!"
    end
    
    new_state = %{state | call_count: state.call_count + 1}
    
    result = %{
      content: [
        text(greeting),
        text("This is greeting ##{new_state.call_count} from the STDIO server! 🎉")
      ]
    }
    
    {:ok, result, new_state}
  end
end

# Start the server
if System.get_env("MCP_ENV") != "test" do
  IO.puts(:stderr, "About to start STDIO server...")
  
  case StdioHelloServer.start_link(transport: :stdio) do
    {:ok, server} ->
      IO.puts(:stderr, "STDIO server started successfully: #{inspect(server)}")
      # Keep the server running
      Process.sleep(:infinity)
      
    {:error, error} ->
      IO.puts(:stderr, "Failed to start STDIO server: #{inspect(error)}")
      System.halt(1)
  end
end