#!/usr/bin/env elixir

# Simple HTTP MCP Server using Plug directly

Mix.install([
  {:ex_mcp, path: "../.."},
  {:plug_cowboy, "~> 2.7"},
  {:jason, "~> 1.4"}
])

defmodule SimpleMCPPlug do
  use Plug.Router
  
  plug :match
  plug :dispatch
  
  @mcp_handler %{
    "protocolVersion" => "2025-03-26",
    "capabilities" => %{
      "tools" => %{}
    },
    "serverInfo" => %{
      "name" => "hello-http-server",
      "version" => "1.0.0"
    }
  }
  
  @tools [
    %{
      "name" => "say_hello",
      "description" => "Say hello via HTTP",
      "inputSchema" => %{
        "type" => "object",
        "properties" => %{
          "name" => %{"type" => "string", "description" => "Name to greet"}
        },
        "required" => ["name"]
      }
    }
  ]
  
  # Handle CORS preflight
  options "/" do
    conn
    |> put_resp_header("access-control-allow-origin", "*")
    |> put_resp_header("access-control-allow-methods", "GET, POST, OPTIONS")
    |> put_resp_header("access-control-allow-headers", "content-type")
    |> send_resp(200, "")
  end
  
  # Main MCP endpoint
  post "/" do
    handle_mcp_request(conn)
  end
  
  # SSE endpoint for streaming responses
  get "/sse" do
    conn
    |> put_resp_header("content-type", "text/event-stream")
    |> put_resp_header("cache-control", "no-cache")
    |> put_resp_header("connection", "keep-alive")
    |> put_resp_header("access-control-allow-origin", "*")
    |> send_chunked(200)
    |> then(fn conn ->
      # Send a keep-alive message every 30 seconds
      # For this demo, we'll just leave the connection open
      # In a real implementation, this would handle SSE events
      :timer.sleep(30000)
      chunk(conn, "data: {\"type\":\"keep-alive\"}\n\n")
      conn
    end)
  end
  
  defp handle_mcp_request(conn) do
    {:ok, body, conn} = read_body(conn)
    
    IO.puts("Received request body: #{body}")
    IO.puts("Body type: #{inspect(body)}")
    
    response = 
      try do
        decoded = Jason.decode(body)
        IO.puts("Jason.decode result: #{inspect(decoded)}")
        
        case decoded do
          {:ok, %{"method" => "initialize", "id" => id} = _request} ->
            # Handle initialize regardless of whether params is present
            %{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => @mcp_handler
            }
          
          {:ok, %{"method" => "notifications/initialized"}} ->
            # This is a notification - no response needed according to JSON-RPC
            # But we need to return something for the HTTP response
            :notification_received
            
          {:ok, %{"method" => "tools/list", "id" => id}} ->
            %{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => %{"tools" => @tools}
            }
            
          {:ok, %{"method" => "tools/call", "params" => %{"name" => "say_hello", "arguments" => args}, "id" => id}} ->
            name = args["name"] || "World"
            %{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => %{
                "content" => [
                  %{
                    "type" => "text",
                    "text" => "Hello, #{name}! Welcome to ExMCP via HTTP! 🌐"
                  }
                ]
              }
            }
            
          {:ok, %{"method" => method, "id" => id}} ->
            %{
              "jsonrpc" => "2.0",
              "id" => id,
              "error" => %{
                "code" => -32601,
                "message" => "Method not found: #{method}"
              }
            }
            
          {:error, reason} ->
            IO.puts("JSON decode error: #{inspect(reason)}")
            %{
              "jsonrpc" => "2.0",
              "id" => nil,
              "error" => %{
                "code" => -32700,
                "message" => "Parse error"
              }
            }
        end
      rescue
        e ->
          IO.puts("Error handling request: #{inspect(e)}")
          %{
            "jsonrpc" => "2.0",
            "id" => nil,
            "error" => %{
              "code" => -32603,
              "message" => "Internal error: #{inspect(e)}"
            }
          }
      end
    
    IO.puts("Sending response: #{inspect(response)}")
    
    case response do
      :notification_received ->
        # For notifications, return 202 Accepted with no body as per MCP spec
        conn
        |> put_resp_header("access-control-allow-origin", "*")
        |> send_resp(202, "")
        
      _ ->
        conn
        |> put_resp_header("content-type", "application/json")
        |> put_resp_header("access-control-allow-origin", "*")
        |> send_resp(200, Jason.encode!(response))
    end
  end
  
  match _ do
    send_resp(conn, 404, "Not found")
  end
end

# Start the server on port 8321 (uncommon port to avoid conflicts)
port = 8321
{:ok, _} = Plug.Cowboy.http(SimpleMCPPlug, [], port: port)

IO.puts("""
Simple HTTP MCP Server started on port #{port}!

Test with curl:
curl -X POST http://localhost:#{port} -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}'
curl -X POST http://localhost:#{port} -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","method":"tools/list","params":{},"id":2}'
curl -X POST http://localhost:#{port} -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"say_hello","arguments":{"name":"Developer"}},"id":3}'

Keep this script running to maintain the server.
Press Ctrl+C to stop.
""")

# Keep the script running
Process.sleep(:infinity)