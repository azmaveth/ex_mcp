#!/usr/bin/env elixir

# Hello World Example - Standard MCP server using stdio transport
# This example shows how to create a server that can communicate with any MCP-compliant client

Mix.install([
  {:ex_mcp, path: "../.."}
])

defmodule HelloWorldHandler do
  use ExMCP.Server.Handler

  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      name: "hello-world-server",
      version: "1.0.0",
      capabilities: %{
        tools: %{}
      }
    }, state}
  end

  @impl true
  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        name: "say_hello",
        description: "Say hello to someone",
        inputSchema: %{
          type: "object",
          properties: %{
            name: %{type: "string", description: "Name to greet"}
          },
          required: ["name"]
        }
      }
    ]
    
    {:ok, tools, nil, state}
  end

  @impl true
  def handle_call_tool("say_hello", params, state) do
    name = params["name"]
    content = [%{type: "text", text: "Hello, #{name}! Welcome to ExMCP! 👋"}]
    {:ok, content, state}
  end
end

# This creates a stdio server that reads from stdin and writes to stdout
# To test this, you would typically:
# 1. Run this script: elixir hello_world_stdio.exs
# 2. Send JSON-RPC messages to stdin
# 3. Receive responses on stdout

{:ok, _server} = ExMCP.Server.start_link(
  handler: HelloWorldHandler,
  transport: :stdio
)

IO.puts(:stderr, """
MCP Server started! This server communicates via stdio.

To test it, send JSON-RPC messages to stdin. For example:

{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"1.0.0","capabilities":{},"clientInfo":{"name":"test","version":"1.0.0"}},"id":1}
{"jsonrpc":"2.0","method":"tools/list","params":{},"id":2}
{"jsonrpc":"2.0","method":"tools/call","params":{"name":"say_hello","arguments":{"name":"World"}},"id":3}

The server will respond with JSON-RPC messages on stdout.
""")

# Keep the script running
Process.sleep(:infinity)