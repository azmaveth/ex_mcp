#!/usr/bin/env elixir

# Hello World Example - The simplest MCP server and client

Mix.install([
  {:ex_mcp, path: "../.."}
])

defmodule HelloWorldServer do
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

# Start the server
{:ok, _server} = ExMCP.Server.start_link(
  handler: HelloWorldServer,
  transport: :stdio,
  name: :hello_server
)

# Start the client
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  server: :hello_server
)

# Wait for initialization
Process.sleep(100)

# List available tools
{:ok, %{"tools" => tools}} = ExMCP.Client.list_tools(client)
IO.puts("\nAvailable tools:")
for tool <- tools do
  IO.puts("  • #{tool["name"]}: #{tool["description"]}")
end

# Call the tool
IO.puts("\nCalling say_hello tool...")
{:ok, %{"content" => content}} = ExMCP.Client.call_tool(client, "say_hello", %{
  "name" => "ExMCP User"
})

# Display the result
for %{"text" => text} <- content do
  IO.puts("Server says: #{text}")
end

IO.puts("\nThat's it! You've successfully created your first MCP server and client!")