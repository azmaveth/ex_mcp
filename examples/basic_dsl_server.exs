#!/usr/bin/env elixir

# Basic MCP Server using DSL
# 
# This example demonstrates the simplest way to create an MCP server
# using ExMCP's DSL (Domain Specific Language) features.

Mix.install([
  {:ex_mcp, path: Path.expand("..", __DIR__)}
])

defmodule BasicServer do
  use ExMCP.Server
  
  # Define a simple tool
  deftool "greet" do
    description "Greets a person by name"
    
    args do
      field :name, :string, required: true, description: "Name of the person to greet"
    end
  end
  
  # Define a resource
  defresource "info://about" do
    name "About This Server"
    description "Information about this MCP server"
    mime_type "text/plain"
  end
  
  # Define a prompt
  defprompt "motivate" do
    name "Motivational Message"
    description "Generates a motivational message"
    
    arguments do
      arg :topic, description: "Topic for motivation (optional)"
    end
  end
  
  # Tool handler implementation
  @impl true
  def handle_tool_call("greet", %{"name" => name}, state) do
    result = %{
      content: [text("Hello, #{name}! Welcome to ExMCP! 👋")]
    }
    {:ok, result, state}
  end
  
  # Resource handler implementation
  @impl true
  def handle_resource_read("info://about", _uri, state) do
    content = [
      text("""
      Basic MCP Server Example
      Version: 1.0.0
      
      This server demonstrates:
      - Simple tool definition using deftool
      - Resource definition using defresource
      - Prompt definition using defprompt
      
      Built with ExMCP's DSL for clean, declarative server definitions.
      """)
    ]
    {:ok, content, state}
  end
  
  # Prompt handler implementation
  @impl true
  def handle_prompt_get("motivate", args, state) do
    topic = Map.get(args, "topic", "your goals")
    
    messages = [
      user("I need motivation about #{topic}"),
      assistant("You've got this! Every step forward in #{topic} is progress. Keep going! 💪")
    ]
    
    {:ok, %{messages: messages}, state}
  end
end

# Start the server
defmodule BasicServerRunner do
  def run do
    IO.puts("Starting Basic MCP Server with DSL...")
    IO.puts("This server demonstrates simple tool, resource, and prompt definitions.\n")
    
    # Start server on stdio transport
    {:ok, _server} = BasicServer.start_link(
      transport: :stdio,
      name: :basic_server
    )
    
    IO.puts("Server is running on stdio transport")
    IO.puts("You can connect a client to test the functionality")
    IO.puts("\nAvailable features:")
    IO.puts("- Tool: greet (takes a name parameter)")
    IO.puts("- Resource: info://about")
    IO.puts("- Prompt: motivate (optional topic parameter)")
    
    # Keep the server running
    Process.sleep(:infinity)
  end
end

# Run if executed directly
if System.get_env("MIX_ENV") != "test" do
  BasicServerRunner.run()
end