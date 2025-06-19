#!/usr/bin/env elixir

# HTTP/SSE Server Example for ExMCP v2
# 
# This example demonstrates:
# - HTTP transport with SSE support
# - Progress notifications
# - Resource subscriptions
# - Backpressure handling

Mix.install([
  {:ex_mcp, path: "../.."},
  {:plug_cowboy, "~> 2.0"}
])

defmodule MyHTTPServer do
  use ExMCP.Server.Handler
  use ExMCP.DSL.Tool
  use ExMCP.DSL.Resource
  
  # Tool that demonstrates progress notifications
  tool "long_operation" do
    description "Performs a long-running operation with progress updates"
    
    input_schema %{
      type: "object",
      properties: %{
        duration: %{
          type: "integer",
          description: "Operation duration in seconds",
          minimum: 1,
          maximum: 30
        }
      },
      required: ["duration"]
    }
    
    handler fn args ->
      # Extract progress token if provided
      progress_token = args["_progressToken"]
      duration = args["duration"]
      
      # Start async task for progress updates
      if progress_token do
        server = self()
        Task.start(fn ->
          steps = 10
          step_duration = duration * 1000 / steps
          
          for i <- 1..steps do
            Process.sleep(trunc(step_duration))
            progress = i / steps
            
            ExMCP.Server.send_progress(server, progress_token, %{
              progress: progress,
              message: "Processing step #{i} of #{steps}"
            })
          end
        end)
      end
      
      # Simulate work
      Process.sleep(duration * 1000)
      
      ExMCP.Response.json(%{
        status: "completed",
        duration: duration,
        timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
      }, "long_operation")
    end
  end
  
  # Resource that can be subscribed to
  resource "status" do
    name "Server Status"
    description "Current server status and metrics"
    uri "status://current"
    mime_type "application/json"
    
    reader fn _uri ->
      status = %{
        uptime: System.monotonic_time(:second),
        memory: :erlang.memory(:total),
        process_count: :erlang.system_info(:process_count),
        timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
      }
      
      ExMCP.Response.json(status, "status")
    end
  end
  
  @impl true
  def init(_args) do
    # Start a timer to periodically update status resource
    {:ok, timer} = :timer.send_interval(5000, :update_status)
    
    {:ok, %{
      timer: timer,
      start_time: System.monotonic_time(:second),
      subscriptions: MapSet.new()
    }}
  end
  
  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      name: "http-server-v2",
      version: "2.0.0",
      capabilities: %{
        tools: %{},
        resources: %{
          subscribe: true
        },
        prompts: %{}
      }
    }, state}
  end
  
  @impl true
  def handle_subscribe_resource("status://current", state) do
    # Track subscription
    new_state = %{state | subscriptions: MapSet.put(state.subscriptions, "status://current")}
    {:ok, %{}, new_state}
  end
  
  @impl true
  def handle_info(:update_status, state) do
    # Send update notification if anyone is subscribed
    if MapSet.member?(state.subscriptions, "status://current") do
      ExMCP.Server.send_notification(self(), "notifications/resources/updated", %{
        uri: "status://current"
      })
    end
    
    {:noreply, state}
  end
  
  @impl true
  def terminate(_reason, state) do
    :timer.cancel(state.timer)
    :ok
  end
end

# Start the HTTP server
port = 8080
IO.puts("Starting ExMCP v2 HTTP/SSE server on port #{port}...")

{:ok, _} = Plug.Cowboy.http(ExMCP.HttpPlug, [
  handler: MyHTTPServer,
  server_info: %{
    name: "http-example-v2",
    version: "2.0.0"
  }
], port: port)

IO.puts("""

Server is running! You can test it with:

1. Using ExMCP client:
   config = ExMCP.client_config()
            |> ExMCP.put_transport(:http)
            |> ExMCP.put_url("http://localhost:#{port}")
            |> ExMCP.put_transport_options(use_sse: true)
   
   {:ok, client} = ExMCP.connect(config)

2. Using curl:
   # Initialize
   curl -X POST http://localhost:#{port}/mcp/v1 \\
     -H "Content-Type: application/json" \\
     -d '{"jsonrpc":"2.0","method":"initialize","params":{},"id":"1"}'
   
   # List tools
   curl -X POST http://localhost:#{port}/mcp/v1 \\
     -H "Content-Type: application/json" \\
     -d '{"jsonrpc":"2.0","method":"tools/list","params":{},"id":"2"}'
   
   # SSE endpoint
   curl -N -H "Accept: text/event-stream" http://localhost:#{port}/mcp/v1/sse

Press Ctrl+C to stop the server.
""")

# Keep running
Process.sleep(:infinity)