#!/usr/bin/env elixir

# Synchronous Client Demo
# 
# This demonstrates the v2 synchronous client pattern where
# the client is guaranteed to be ready when start_link returns.

Mix.install([
  {:ex_mcp, path: "../../../"}
])

defmodule SyncDemo do
  def run do
    IO.puts("🚀 ExMCP v2 Synchronous Client Demo\n")
    
    # Start HTTP server in background
    server_pid = start_http_server()
    Process.sleep(500)  # Let server start
    
    try do
      demonstrate_sync_client()
    after
      GenServer.stop(server_pid)
    end
  end
  
  defp start_http_server do
    {:ok, pid} = DemoServer.start_link(port: 8765)
    IO.puts("✅ Started demo server on port 8765")
    pid
  end
  
  defp demonstrate_sync_client do
    IO.puts("\n📡 Connecting client...")
    start_time = System.monotonic_time(:millisecond)
    
    # With v2, the client is ready immediately after start_link
    {:ok, client} = ExMCP.SimpleClient.start_link(
      transport: :http,
      url: "http://localhost:8765"
    )
    
    elapsed = System.monotonic_time(:millisecond) - start_time
    IO.puts("✅ Client connected and ready in #{elapsed}ms")
    
    # No sleep needed! Client is guaranteed to be ready
    IO.puts("\n🔧 Listing tools immediately...")
    {:ok, result} = ExMCP.SimpleClient.list_tools(client)
    
    tools = result["tools"] || []
    IO.puts("✅ Found #{length(tools)} tools:")
    Enum.each(tools, fn tool ->
      IO.puts("   - #{tool["name"]}: #{tool["description"]}")
    end)
    
    # Call a tool
    IO.puts("\n🎯 Calling greet tool...")
    {:ok, response} = ExMCP.SimpleClient.call_tool(client, "greet", %{"name" => "Developer"})
    
    [content | _] = response["content"] || []
    IO.puts("✅ Response: #{content["text"]}")
    
    GenServer.stop(client)
    IO.puts("\n✨ Demo completed!")
  end
end

# Simple demo server using v2 DSL
defmodule DemoServer do
  use ExMCP.ServerV2
  use Plug.Router
  
  plug :match
  plug :dispatch
  
  deftool "greet" do
    tool_description "Says hello to someone"
    args do
      field :name, :string, required: true, description: "Name to greet"
    end
  end
  
  deftool "calculate" do
    tool_description "Performs basic calculations"
    args do
      field :operation, :string, required: true
      field :a, :number, required: true
      field :b, :number, required: true
    end
  end
  
  @impl true
  def handle_tool_call("greet", %{"name" => name}, state) do
    {:ok, %{content: [text("Hello, #{name}! Welcome to ExMCP v2! 🎉")]}, state}
  end
  
  @impl true
  def handle_tool_call("calculate", %{"operation" => op, "a" => a, "b" => b}, state) do
    result = case op do
      "add" -> a + b
      "subtract" -> a - b
      "multiply" -> a * b
      "divide" when b != 0 -> a / b
      _ -> nil
    end
    
    if result do
      {:ok, %{content: [text("Result: #{result}")]}, state}
    else
      {:ok, %{content: [text("Invalid operation or division by zero")], is_error?: true}, state}
    end
  end
  
  # HTTP transport setup
  def start_link(opts) do
    port = Keyword.get(opts, :port, 8080)
    
    Plug.Cowboy.http(__MODULE__, [], port: port)
    |> case do
      {:ok, _pid} = result -> result
      {:error, reason} -> {:error, reason}
    end
  end
  
  post "/mcp" do
    {:ok, body, conn} = Plug.Conn.read_body(conn)
    
    # Handle MCP request
    response = case Jason.decode(body) do
      {:ok, %{"method" => "initialize"} = request} ->
        ExMCP.Internal.Protocol.encode_response(%{
          "protocolVersion" => "2024-11-05",
          "capabilities" => get_capabilities(),
          "serverInfo" => %{
            "name" => "DemoServer",
            "version" => "1.0.0"
          }
        }, request["id"])
        
      {:ok, %{"method" => "tools/list"} = request} ->
        tools = get_tools() |> Enum.map(fn {_name, tool} ->
          %{
            "name" => tool.name,
            "description" => tool.description,
            "inputSchema" => tool.input_schema
          }
        end)
        
        ExMCP.Internal.Protocol.encode_response(%{"tools" => tools}, request["id"])
        
      {:ok, %{"method" => "tools/call", "params" => params} = request} ->
        case handle_tool_call(params["name"], params["arguments"], %{}) do
          {:ok, result, _state} ->
            ExMCP.Internal.Protocol.encode_response(result, request["id"])
          {:error, reason, _state} ->
            ExMCP.Internal.Protocol.encode_error(-32602, "Tool call failed: #{inspect(reason)}", nil, request["id"])
        end
        
      _ ->
        ExMCP.Internal.Protocol.encode_error(-32600, "Invalid request", nil, nil)
    end
    
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(response))
  end
  
  match _ do
    send_resp(conn, 404, "Not found")
  end
end

# Run the demo
SyncDemo.run()