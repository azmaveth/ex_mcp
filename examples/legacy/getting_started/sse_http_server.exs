#!/usr/bin/env elixir

# SSE-enabled HTTP MCP Server using Plug with proper response streaming

Mix.install([
  {:ex_mcp, path: "../.."},
  {:plug_cowboy, "~> 2.7"},
  {:jason, "~> 1.4"}
])

defmodule SSEMCPServer do
  use GenServer
  
  # Store SSE connections by session ID
  def start_link(_) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end
  
  def init(state) do
    {:ok, state}
  end
  
  def register_sse_connection(session_id, conn_pid) do
    GenServer.cast(__MODULE__, {:register_sse, session_id, conn_pid})
  end
  
  def send_sse_response(session_id, response) do
    GenServer.cast(__MODULE__, {:send_response, session_id, response})
  end
  
  def handle_cast({:register_sse, session_id, conn_pid}, state) do
    IO.puts("Registering SSE connection for session: #{session_id}")
    new_state = Map.put(state, session_id, conn_pid)
    {:noreply, new_state}
  end
  
  def handle_cast({:send_response, session_id, response}, state) do
    IO.puts("DEBUG: Looking for session #{session_id}")
    IO.puts("DEBUG: Available sessions: #{inspect(Map.keys(state))}")
    
    case Map.get(state, session_id) do
      nil ->
        IO.puts("No SSE connection found for session: #{session_id}")
        {:noreply, state}
      conn_pid when is_pid(conn_pid) ->
        IO.puts("Sending SSE response for session: #{session_id}")
        send(conn_pid, {:sse_response, response})
        {:noreply, state}
    end
  end
end

defmodule SSEMCPPlug do
  use Plug.Router
  
  plug :match
  plug :dispatch
  
  @mcp_handler %{
    "protocolVersion" => "2025-03-26",
    "capabilities" => %{
      "tools" => %{}
    },
    "serverInfo" => %{
      "name" => "hello-sse-server",
      "version" => "1.0.0"
    }
  }
  
  @tools [
    %{
      "name" => "say_hello",
      "description" => "Say hello via HTTP with SSE",
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
    |> put_resp_header("access-control-allow-headers", "content-type, mcp-session-id")
    |> send_resp(200, "")
  end
  
  # Main MCP endpoint - receives requests and sends responses via SSE
  post "/" do
    handle_mcp_request(conn)
  end
  
  # SSE endpoint for streaming responses
  get "/sse" do
    session_id = get_req_header(conn, "mcp-session-id") |> List.first() || "default"
    IO.puts("SSE connection established for session: #{session_id}")
    
    conn
    |> put_resp_header("content-type", "text/event-stream")
    |> put_resp_header("cache-control", "no-cache")
    |> put_resp_header("connection", "keep-alive")
    |> put_resp_header("access-control-allow-origin", "*")
    |> send_chunked(200)
    |> handle_sse_connection(session_id)
  end
  
  defp handle_sse_connection(conn, session_id) do
    # Register this connection with the server
    SSEMCPServer.register_sse_connection(session_id, self())
    
    # Send initial keep-alive
    {:ok, conn} = chunk(conn, "data: {\"type\":\"keep-alive\"}\n\n")
    
    # Keep the connection alive and handle responses
    sse_loop(conn)
  end
  
  defp sse_loop(conn) do
    receive do
      {:sse_response, response} ->
        IO.puts("Sending SSE event: #{inspect(response)}")
        json_data = Jason.encode!(response)
        case chunk(conn, "data: #{json_data}\n\n") do
          {:ok, conn} -> sse_loop(conn)
          {:error, _} -> conn
        end
    after
      1_000 ->
        # Send keep-alive every 1 second (for demo purposes)
        case chunk(conn, "data: {\"type\":\"keep-alive\"}\n\n") do
          {:ok, conn} -> sse_loop(conn)
          {:error, _} -> conn
        end
    end
  end
  
  defp handle_mcp_request(conn) do
    {:ok, body, conn} = read_body(conn)
    session_id = get_req_header(conn, "mcp-session-id") |> List.first() || "default"
    
    IO.puts("Received request body: #{body}")
    IO.puts("Session ID: #{session_id}")
    
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
                    "text" => "Hello, #{name}! Welcome to ExMCP via HTTP+SSE! 🌊"
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
    
    case response do
      :notification_received ->
        # For notifications, return 202 Accepted with no body
        IO.puts("Notification received, sending 202")
        conn
        |> put_resp_header("access-control-allow-origin", "*")
        |> send_resp(202, "")
        
      _ ->
        # Send response via SSE
        IO.puts("Sending response via SSE: #{inspect(response)}")
        SSEMCPServer.send_sse_response(session_id, response)
        
        # Return 200 OK to indicate request was accepted
        conn
        |> put_resp_header("access-control-allow-origin", "*")
        |> send_resp(200, "")
    end
  end
  
  match _ do
    send_resp(conn, 404, "Not found")
  end
end

# Start the SSE server
{:ok, _} = SSEMCPServer.start_link([])

# Start the HTTP server on port 8322 (different from simple server)
port = 8322
{:ok, _} = Plug.Cowboy.http(SSEMCPPlug, [], port: port)

IO.puts("""
SSE-enabled HTTP MCP Server started on port #{port}!

This server demonstrates proper SSE streaming:
- POST requests to / trigger responses
- Responses are sent via SSE on /sse endpoint
- Session management via Mcp-Session-Id header

Test with curl (SSE mode):
# Terminal 1 - Open SSE connection:
curl -N -H "Mcp-Session-Id: test-session" http://localhost:#{port}/sse

# Terminal 2 - Send requests:
curl -X POST http://localhost:#{port} -H "Content-Type: application/json" -H "Mcp-Session-Id: test-session" -d '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}'
curl -X POST http://localhost:#{port} -H "Content-Type: application/json" -H "Mcp-Session-Id: test-session" -d '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"say_hello","arguments":{"name":"Developer"}},"id":2}'

Keep this script running to maintain the server.
Press Ctrl+C to stop.
""")

# Keep the script running
Process.sleep(:infinity)