#!/usr/bin/env elixir

# HTTP Server (without SSE) with Hello World Resource
# 
# This server demonstrates HTTP transport without Server-Sent Events
# It provides a single resource that can be read

Mix.install([
  {:ex_mcp, path: Path.expand("../..", __DIR__)},
  {:plug_cowboy, "~> 2.6"},
  {:jason, "~> 1.4"},
  {:ex_json_schema, "~> 0.10"},
  {:html_entities, "~> 0.5"}
], verbose: false)

# Configure logging for cleaner demo output
Logger.configure(level: :info)

defmodule HttpHelloServer do
  use ExMCP.Server
  
  @impl true
  def init(_args) do
    {:ok, %{
      access_count: 0,
      last_accessed: nil
    }}
  end
  
  # Define a hello world resource
  defresource "hello://world" do
    meta do
      name "Hello World Resource"
      description "A friendly greeting from the HTTP server"
    end
    mime_type "text/plain"
  end
  
  # Define another resource to show HTTP capabilities
  defresource "hello://stats" do
    meta do
      name "Server Statistics"
      description "Current server statistics"
    end
    mime_type "application/json"
  end
  
  @impl true
  def handle_resource_read("hello://world", _uri, state) do
    new_state = %{
      state | 
      access_count: state.access_count + 1,
      last_accessed: DateTime.utc_now()
    }
    
    content = [
      text("""
      Hello from the HTTP Server! 🌐
      
      This is a resource served over HTTP without SSE.
      You've accessed this resource #{new_state.access_count} time(s).
      
      HTTP transport is great for:
      - Simple request/response patterns
      - RESTful-style APIs
      - Stateless operations
      """)
    ]
    
    {:ok, content, new_state}
  end
  
  @impl true
  def handle_resource_read("hello://stats", _uri, state) do
    stats = %{
      access_count: state.access_count,
      last_accessed: state.last_accessed && DateTime.to_iso8601(state.last_accessed),
      server_time: DateTime.utc_now() |> DateTime.to_iso8601(),
      transport: "HTTP (no SSE)"
    }
    
    {:ok, [json(stats)], state}
  end
  
  @impl true
  def handle_resource_list(state) do
    resources = [
      %{
        uri: "hello://world",
        name: "Hello World Resource",
        mimeType: "text/plain"
      },
      %{
        uri: "hello://stats",
        name: "Server Statistics", 
        mimeType: "application/json"
      }
    ]
    
    {:ok, resources, state}
  end
end

# Start the HTTP server
if System.get_env("MCP_ENV") != "test" do
  port = String.to_integer(System.get_env("HTTP_PORT", "8080"))
  
  IO.puts("Starting HTTP Hello Server on port #{port}...")
  IO.puts("Server ready to accept HTTP connections")
  IO.puts("Resources available:")
  IO.puts("  - hello://world")
  IO.puts("  - hello://stats")
  
  {:ok, _server} = HttpHelloServer.start_link(
    transport: :http,
    port: port,
    use_sse: false,  # Explicitly disable SSE
    name: :http_hello_server
  )
  
  # Keep the server running
  Process.sleep(:infinity)
end