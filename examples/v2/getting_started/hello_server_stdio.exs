#!/usr/bin/env elixir

# Hello World Server - STDIO Transport
# The simplest possible MCP server using stdio transport

# Add lib to path instead of using Mix.install to avoid HTTP dependencies
Code.prepend_path("_build/dev/lib/ex_mcp/ebin")
Code.prepend_path("_build/dev/lib/jason/ebin")

defmodule HelloServer do
  use ExMCP.ServerV2
  
  @name "hello-server"
  @version "1.0.0"
  
  # Define a simple hello tool using DSL
  deftool "say_hello" do
    meta do
      name "Hello Tool"
      description "Says hello to someone"
    end
    
    input_schema %{
      type: "object",
      properties: %{
        name: %{type: "string", description: "Name to greet"}
      },
      required: ["name"]
    }
  end
  
  # Define a simple resource using DSL
  defresource "hello://info" do
    meta do
      name "Server Info"
      description "Basic server information"
    end
    
    mime_type "text/plain"
  end
  
  # Implement handlers using the defhandler macro
  defhandler :tool, "say_hello", %{"name" => name}, state do
    response = text("Hello, #{name}! 👋")
    {:ok, [response], state}
  end
  
  defhandler :resource, "hello://info", _uri, state do
    content = text("""
    Hello World MCP Server
    Version: #{@version}
    Transport: STDIO
    """)
    {:ok, [content], state}
  end
end

# Start the server
IO.puts("Starting Hello World server on STDIO...")
IO.puts("\nAvailable tool: say_hello")
IO.puts("Available resource: hello://info")
IO.puts("\nWaiting for client connection...")

{:ok, _server} = HelloServer.start_link(transport: :stdio)

Process.sleep(:infinity)