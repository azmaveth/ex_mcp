#!/usr/bin/env elixir

# Hello World Server - HTTP Transport
# Simple HTTP server with JSON-RPC endpoint

# Add lib to path instead of using Mix.install to avoid dependency conflicts
Code.prepend_path("_build/dev/lib/ex_mcp/ebin")
Code.prepend_path("_build/dev/lib/jason/ebin")
Code.prepend_path("_build/dev/lib/plug/ebin")
Code.prepend_path("_build/dev/lib/cowboy/ebin")
Code.prepend_path("_build/dev/lib/plug_cowboy/ebin")

defmodule HelloHTTPServer do
  use ExMCP.ServerV2
  
  @name "hello-http-server"
  @version "1.0.0"
  
  deftool "hello" do
    meta do
      name "HTTP Hello"
      description "Simple HTTP hello"
    end
    
    input_schema %{
      type: "object",
      properties: %{
        name: %{type: "string", default: "HTTP"}
      }
    }
  end
  
  deftool "echo" do
    meta do
      name "Echo Tool"
      description "Echo back JSON data"
    end
    
    input_schema %{
      type: "object",
      properties: %{
        data: %{type: "object", description: "Any JSON data"}
      },
      required: ["data"]
    }
  end
  
  defresource "http://server/info" do
    meta do
      name "HTTP Server Info"
      description "Server information"
    end
    
    mime_type "application/json"
  end
  
  # Implement handlers using defhandler with pattern matching
  defhandler :tool, "hello", args, state do
    name = args["name"] || "HTTP"
    response = text("Hello #{name} from HTTP server!")
    {:ok, [response], state}
  end
  
  # Pattern matching on specific argument structure
  defhandler :tool, "echo", %{"data" => data}, state do
    response = json(data)
    {:ok, [response], state}
  end
  
  # Resource handler with defhandler
  defhandler :resource, "http://server/info", _uri, state do
    content = json(%{
      name: @name,
      version: @version,
      transport: "HTTP",
      endpoint: "http://localhost:3000/mcp"
    })
    {:ok, [content], state}
  end
end

# Start server using the DSL
port = 3000
IO.puts("""
Hello World HTTP Server
=======================
Starting on port #{port}...

Tools: hello, echo
Resources: http://server/info

Test with:
curl -X POST http://localhost:#{port}/mcp \\
  -H "Content-Type: application/json" \\
  -d '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}'
""")

{:ok, _server} = HelloHTTPServer.start_link(transport: :http, port: port)
Process.sleep(:infinity)