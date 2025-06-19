#!/usr/bin/env elixir

# Simple Hello World Server - STDIO Transport
# Uses handler callbacks instead of DSL to avoid dependency issues

# Add lib to path
Code.prepend_path("_build/dev/lib/ex_mcp/ebin")
Code.prepend_path("_build/dev/lib/jason/ebin")

defmodule SimpleHelloServer do
  use ExMCP.Server.Handler
  
  @impl true
  def init(_args) do
    {:ok, %{}}
  end
  
  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      name: "simple-hello-server",
      version: "1.0.0",
      capabilities: %{
        tools: %{},
        resources: %{}
      }
    }, state}
  end
  
  @impl true
  def handle_list_tools(state) do
    tools = [
      %{
        name: "say_hello",
        description: "Says hello to someone",
        input_schema: %{
          type: "object",
          properties: %{
            name: %{type: "string", description: "Name to greet"}
          },
          required: ["name"]
        }
      }
    ]
    {:ok, tools, state}
  end
  
  @impl true
  def handle_call_tool("say_hello", %{"name" => name}, state) do
    content = [%{
      type: "text",
      text: "Hello, #{name}! 👋"
    }]
    {:ok, content, state}
  end
  
  def handle_call_tool(_name, _args, state) do
    {:error, "Tool not found", state}
  end
  
  @impl true
  def handle_list_resources(state) do
    resources = [
      %{
        uri: "hello://info",
        name: "Server Info",
        description: "Basic server information",
        mimeType: "text/plain"
      }
    ]
    {:ok, resources, state}
  end
  
  @impl true
  def handle_read_resource("hello://info", state) do
    content = [%{
      type: "text",
      text: """
      Hello World MCP Server
      Version: 1.0.0
      Transport: STDIO
      """
    }]
    {:ok, content, state}
  end
  
  def handle_read_resource(_uri, state) do
    {:error, "Resource not found", state}
  end
end

# Start the server
IO.puts("Starting Simple Hello World server on STDIO...")
IO.puts("\nAvailable tool: say_hello")
IO.puts("Available resource: hello://info")
IO.puts("\nWaiting for client connection...")

{:ok, _server} = ExMCP.Server.start_link(
  handler: SimpleHelloServer,
  transport: :stdio
)

Process.sleep(:infinity)