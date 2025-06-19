#!/usr/bin/env elixir

# Hello World Server - HTTP with SSE (Server-Sent Events)
# Demonstrates streaming capabilities with progress notifications

Mix.install([
  {:ex_mcp, path: Path.expand("../../..", __DIR__)},
  {:plug_cowboy, "~> 2.7"}
])

defmodule HelloSSEServer do
  use ExMCP.ServerV2
  
  @name "hello-sse-server"
  @version "1.0.0"
  
  # Simple instant response tool
  deftool "instant_hello" do
    meta do
      name "Instant Hello"
      description "Says hello immediately"
    end
    
    input_schema %{
      type: "object",
      properties: %{
        name: %{type: "string", default: "SSE"}
      }
    }
  end
  
  # Tool that sends progress notifications
  deftool "counting_hello" do
    meta do
      name "Counting Hello"
      description "Says hello with counting progress"
    end
    
    input_schema %{
      type: "object",
      properties: %{
        name: %{type: "string", default: "SSE"},
        count: %{type: "integer", minimum: 1, maximum: 10, default: 5},
        _progressToken: %{type: "string", description: "Progress token"}
      }
    }
  end
  
  # Subscribable resource
  defresource "sse://time" do
    meta do
      name "Current Time"
      description "Server time (updates every second via SSE)"
    end
    mime_type "text/plain"
    annotations %{
      supports_subscribe: true,
      update_interval: 1000
    }
  end
  
  # Tool handler callbacks using defhandler
  defhandler :tool, "instant_hello", args, state do
    name = args["name"] || "SSE"
    response = text("Hello #{name}! (instant response)")
    {:ok, [response], state}
  end
  
  defhandler :tool, "counting_hello", args, state do
    name = args["name"] || "SSE"
    count = args["count"] || 5
    progress_token = args["_progressToken"]
    
    # Simulate work with progress updates
    if progress_token do
      # In a real implementation, you'd send progress via SSE
      # For this example, we'll just simulate the work
      Process.sleep(count * 100)
    end
    
    # Return final result
    message = "Hello #{name}! Counted to #{count}."
    response = text(message)
    {:ok, [response], state}
  end
  
  # Resource handler using defhandler
  defhandler :resource, "sse://time", _uri, state do
    time = DateTime.utc_now() |> DateTime.to_iso8601()
    response = text("Server time: #{time}")
    {:ok, [response], state}
  end
  
  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      name: @name,
      version: @version,
      capabilities: %{
        tools: %{},
        resources: %{
          subscribe: true  # Enable SSE subscriptions
        }
      }
    }, state}
  end
end

# HTTP router with SSE support
defmodule SSERouter do
  use Plug.Router
  
  plug :match
  plug :dispatch
  
  # MCP endpoint with SSE support
  post "/mcp" do
    conn
    |> put_resp_header("access-control-allow-origin", "*")
    |> put_resp_header("access-control-allow-headers", "content-type")
    |> ExMCP.HttpPlug.handle(HelloSSEServer)
  end
  
  # CORS preflight
  options "/mcp" do
    conn
    |> put_resp_header("access-control-allow-origin", "*")
    |> put_resp_header("access-control-allow-methods", "POST, OPTIONS")
    |> put_resp_header("access-control-allow-headers", "content-type")
    |> send_resp(204, "")
  end
  
  get "/" do
    send_resp(conn, 200, """
    Hello World SSE Server
    ======================
    
    This server supports Server-Sent Events for:
    - Progress notifications during long operations
    - Resource subscriptions for real-time updates
    
    Tools:
    - instant_hello: Immediate response
    - counting_hello: Response with progress updates
    
    Resources:
    - sse://time: Subscribe for time updates
    
    Endpoint: POST /mcp
    """)
  end
  
  match _ do
    send_resp(conn, 404, "Not found")
  end
end

# Start server
port = 3001
IO.puts("""
Hello World SSE Server
======================
Starting on port #{port}...

This server demonstrates:
- Server-Sent Events (SSE) for streaming
- Progress notifications
- Resource subscriptions

Endpoints:
- GET  http://localhost:#{port}/     (info)
- POST http://localhost:#{port}/mcp  (MCP with SSE)

Tools:
- instant_hello: Immediate response
- counting_hello: Progress notifications

Resources:
- sse://time: Subscribable time updates

Note: SSE features require a compatible client
that supports Server-Sent Events.
""")

Plug.Cowboy.http(SSERouter, [], port: port)
Process.sleep(:infinity)