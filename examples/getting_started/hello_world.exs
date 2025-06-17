#!/usr/bin/env elixir

# Hello World Example - The simplest MCP service using the Native Service Dispatcher

Mix.install([
  {:ex_mcp, path: "../.."}
])

# For this demo, we'll use the Native Service Dispatcher for simplicity
# In a real-world scenario, you'd typically use stdio or HTTP transport for cross-language compatibility

defmodule HelloWorldService do
  use ExMCP.Service, name: :hello_world_service

  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [
      %{
        "name" => "say_hello",
        "description" => "Say hello to someone",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "name" => %{"type" => "string", "description" => "Name to greet"}
          },
          "required" => ["name"]
        }
      }
    ]
    
    {:ok, %{"tools" => tools}, state}
  end

  @impl true
  def handle_mcp_request("tools/call", %{"name" => "say_hello", "arguments" => params}, state) do
    name = params["name"]
    content = [%{"type" => "text", "text" => "Hello, #{name}! Welcome to ExMCP! 👋"}]
    {:ok, %{"content" => content}, state}
  end

  def handle_mcp_request(_method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found"}, state}
  end
end

# Start the service
{:ok, _service} = HelloWorldService.start_link()

# Wait for initialization
Process.sleep(100)

# List available tools using Native dispatcher
{:ok, %{"tools" => tools}} = ExMCP.Native.call(:hello_world_service, "list_tools", %{})
IO.puts("\nAvailable tools:")
for tool <- tools do
  IO.puts("  • #{tool["name"]}: #{tool["description"]}")
end

# Call the tool
IO.puts("\nCalling say_hello tool...")
{:ok, %{"content" => content}} = ExMCP.Native.call(:hello_world_service, "tools/call", %{
  "name" => "say_hello",
  "arguments" => %{"name" => "ExMCP User"}
})

# Display the result
for %{"text" => text} <- content do
  IO.puts("Service says: #{text}")
end

IO.puts("\nThat's it! You've successfully created your first MCP service using the Native dispatcher!")
IO.puts("\nFor standard MCP communication, see the stdio and HTTP examples in transports/")