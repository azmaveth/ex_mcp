#!/usr/bin/env elixir

# Basic ExMCP v2 Server Example
# 
# This example demonstrates:
# - v2 DSL syntax
# - Structured responses
# - Simple server setup

Mix.install([
  {:ex_mcp, path: "../.."}
])

defmodule MyServer do
  use ExMCP.Server.Handler
  use ExMCP.DSL.Tool
  use ExMCP.DSL.Resource
  use ExMCP.DSL.Prompt
  
  # Define tools using v2 DSL
  tool "echo" do
    description "Echoes back the input message"
    
    input_schema %{
      type: "object",
      properties: %{
        message: %{type: "string", description: "Message to echo"}
      },
      required: ["message"]
    }
    
    handler fn args ->
      ExMCP.Response.text("Echo: #{args["message"]}", "echo")
    end
  end
  
  tool "calculator" do
    description "Performs basic arithmetic operations"
    
    input_schema %{
      type: "object",
      properties: %{
        operation: %{
          type: "string",
          enum: ["add", "subtract", "multiply", "divide"]
        },
        a: %{type: "number"},
        b: %{type: "number"}
      },
      required: ["operation", "a", "b"]
    }
    
    handler fn args ->
      result = case args["operation"] do
        "add" -> args["a"] + args["b"]
        "subtract" -> args["a"] - args["b"]
        "multiply" -> args["a"] * args["b"]
        "divide" when args["b"] != 0 -> args["a"] / args["b"]
        "divide" -> 
          return ExMCP.Response.error("Division by zero", "calculator")
      end
      
      ExMCP.Response.json(%{
        operation: args["operation"],
        a: args["a"],
        b: args["b"],
        result: result
      }, "calculator")
    end
  end
  
  # Define resources using v2 DSL
  resource "readme" do
    name "README"
    description "Server readme file"
    uri "file:///readme.txt"
    mime_type "text/plain"
    
    reader fn _uri ->
      content = """
      ExMCP v2 Example Server
      ======================
      
      This server demonstrates the new v2 API with:
      - Structured responses
      - Enhanced DSL syntax
      - Type-safe tool definitions
      """
      
      ExMCP.Response.text(content, "readme")
    end
  end
  
  # Define prompts using v2 DSL
  prompt "greeting" do
    name "Personalized Greeting"
    description "Generates a personalized greeting message"
    
    arguments [
      %{name: "name", type: "string", required: true},
      %{name: "language", type: "string", required: false}
    ]
    
    handler fn args ->
      lang = args["language"] || "en"
      greeting = case lang do
        "es" -> "¡Hola, #{args["name"]}!"
        "fr" -> "Bonjour, #{args["name"]}!"
        "de" -> "Hallo, #{args["name"]}!"
        _ -> "Hello, #{args["name"]}!"
      end
      
      ExMCP.Response.text(greeting, "greeting")
    end
  end
  
  # Server handler callbacks
  @impl true
  def init(_args) do
    IO.puts("Server initializing...")
    {:ok, %{request_count: 0}}
  end
  
  @impl true
  def handle_initialize(_params, state) do
    IO.puts("Client connected!")
    
    {:ok, %{
      name: "example-server-v2",
      version: "2.0.0",
      capabilities: %{
        tools: %{},
        resources: %{},
        prompts: %{}
      }
    }, state}
  end
  
  # The DSL automatically handles list_tools, call_tool, etc.
  # You can override them if needed for custom behavior
  
  @impl true
  def terminate(reason, state) do
    IO.puts("Server terminating: #{inspect(reason)}")
    IO.puts("Total requests handled: #{state.request_count}")
    :ok
  end
end

# Start the server
IO.puts("Starting ExMCP v2 server on stdio...")

{:ok, _server} = ExMCP.Server.start(
  handler: MyServer,
  transport: :stdio
)

# Keep the server running
Process.sleep(:infinity)