#!/usr/bin/env elixir

# Universal Hello World Client
# Demonstrates connecting to all transport types

# Add lib to path instead of using Mix.install
Code.prepend_path("_build/dev/lib/ex_mcp/ebin")
Code.prepend_path("_build/dev/lib/jason/ebin")

defmodule HelloClient do
  def run do
    IO.puts("""
    ==========================================
    ExMCP v2 Universal Client Demo
    ==========================================
    
    This client will connect to each transport type.
    Make sure the servers are running!
    """)
    
    # Test each transport
    test_stdio()
    test_native()
    test_http()
    test_sse()
    
    IO.puts("\n✅ All transport tests complete!")
  end
  
  defp test_stdio do
    IO.puts("\n1. Testing STDIO Transport")
    IO.puts("   (Requires hello_server_stdio.exs running)")
    
    config = ExMCP.ClientConfig.new()
             |> ExMCP.ClientConfig.put_transport(:stdio, 
                 command: ["elixir", "hello_server_stdio.exs"]
               )
    
    case ExMCP.connect(config) do
      {:ok, client} ->
        IO.puts("   ✓ Connected via STDIO")
        
        # Make a tool call
        {:ok, resp} = ExMCP.call_tool(client, "say_hello", %{"name" => "STDIO Client"})
        IO.puts("   Response: #{ExMCP.Response.text_content(resp)}")
        
        # Read resource
        {:ok, resp} = ExMCP.read_resource(client, "hello://info")
        IO.puts("   Info: #{String.trim(ExMCP.Response.text_content(resp))}")
        
        ExMCP.disconnect(client)
        IO.puts("   ✓ Disconnected")
        
      {:error, reason} ->
        IO.puts("   ✗ Failed: #{inspect(reason)}")
    end
  end
  
  defp test_native do
    IO.puts("\n2. Testing Native (BEAM) Transport")
    IO.puts("   (Requires hello_server_native.exs running in distributed mode)")
    
    # For native transport between separate processes, we need distributed Erlang
    # First, start this node with a name
    node_name = :"client@127.0.0.1"
    case Node.start(node_name, :shortnames) do
      {:ok, _} -> IO.puts("   Started node: #{node_name}")
      {:error, {:already_started, _}} -> IO.puts("   Node already started: #{node_name}")
      error -> IO.puts("   Failed to start node: #{inspect(error)}")
    end
    
    # Set the cookie (must match server)
    Node.set_cookie(:hello_mcp_cookie)
    
    # Connect to server node
    server_node = :"server@127.0.0.1"
    case Node.connect(server_node) do
      true -> 
        IO.puts("   ✓ Connected to server node: #{server_node}")
        
        # Wait a moment for connection to stabilize
        Process.sleep(100)
        
        # Find the server process on the remote node
        case :rpc.call(server_node, Process, :whereis, [:hello_native_server]) do
          pid when is_pid(pid) ->
            IO.puts("   ✓ Found server process: #{inspect(pid)}")
            
            # Create client config with remote PID
            config = ExMCP.ClientConfig.new()
                     |> ExMCP.ClientConfig.put_transport(:native, server_pid: pid)
            
            case ExMCP.connect(config) do
              {:ok, client} ->
                IO.puts("   ✓ Connected via Native transport")
                
                # Make a tool call
                {:ok, resp} = ExMCP.call_tool(client, "greet", %{"name" => "Distributed BEAM", "language" => "en"})
                IO.puts("   Response: #{ExMCP.Response.text_content(resp)}")
                
                # Read resource
                {:ok, resp} = ExMCP.read_resource(client, "native://status")
                status = ExMCP.Response.data_content(resp)
                IO.puts("   Server node: #{status["node"]}")
                IO.puts("   Server PID: #{status["pid"]}")
                
                ExMCP.disconnect(client)
                IO.puts("   ✓ Disconnected")
                
              {:error, reason} ->
                IO.puts("   ✗ Failed to connect: #{inspect(reason)}")
            end
            
          nil ->
            IO.puts("   ✗ Server process not found on #{server_node}")
            IO.puts("   Make sure to start the server with: elixir --name server@127.0.0.1 --cookie hello_mcp_cookie hello_server_native.exs")
            
          {:badrpc, reason} ->
            IO.puts("   ✗ RPC failed: #{inspect(reason)}")
        end
        
      false -> 
        IO.puts("   ✗ Could not connect to server node: #{server_node}")
        IO.puts("   Make sure to start the server with: elixir --name server@127.0.0.1 --cookie hello_mcp_cookie hello_server_native.exs")
    end
  end
  
  defp test_http do
    IO.puts("\n3. Testing HTTP Transport")
    IO.puts("   (Requires hello_server_http.exs running on port 3000)")
    
    config = ExMCP.ClientConfig.new()
             |> ExMCP.ClientConfig.put_transport(:http,
                 url: "http://localhost:3000",
                 path: "/mcp"
               )
    
    case ExMCP.connect(config) do
      {:ok, client} ->
        IO.puts("   ✓ Connected via HTTP")
        
        # Make tool calls
        {:ok, resp} = ExMCP.call_tool(client, "hello", %{"name" => "HTTP Client"})
        IO.puts("   Hello: #{ExMCP.Response.text_content(resp)}")
        
        {:ok, resp} = ExMCP.call_tool(client, "echo", %{"data" => %{test: "echo", number: 42}})
        IO.puts("   Echo: #{inspect(ExMCP.Response.data_content(resp))}")
        
        # Read resource
        {:ok, resp} = ExMCP.read_resource(client, "http://server/info")
        info = ExMCP.Response.data_content(resp)
        IO.puts("   Server: #{info["name"]} v#{info["version"]}")
        
        ExMCP.disconnect(client)
        IO.puts("   ✓ Disconnected")
        
      {:error, reason} ->
        IO.puts("   ✗ Failed: #{inspect(reason)}")
        IO.puts("   Make sure hello_server_http.exs is running")
    end
  end
  
  defp test_sse do
    IO.puts("\n4. Testing HTTP+SSE Transport")
    IO.puts("   (Requires hello_server_sse.exs running on port 3001)")
    
    config = ExMCP.ClientConfig.new()
             |> ExMCP.ClientConfig.put_transport(:http,
                 url: "http://localhost:3001",
                 path: "/mcp"
               )
    
    case ExMCP.connect(config) do
      {:ok, client} ->
        IO.puts("   ✓ Connected via HTTP+SSE")
        
        # Test instant response
        {:ok, resp} = ExMCP.call_tool(client, "instant_hello", %{"name" => "SSE Client"})
        IO.puts("   Instant: #{ExMCP.Response.text_content(resp)}")
        
        # Test with progress (though we won't see progress in this simple client)
        {:ok, resp} = ExMCP.call_tool(client, "counting_hello", %{
          "name" => "SSE Progress",
          "count" => 3
        })
        IO.puts("   Counting: #{ExMCP.Response.text_content(resp)}")
        
        # Read subscribable resource
        {:ok, resp} = ExMCP.read_resource(client, "sse://time")
        IO.puts("   Time: #{ExMCP.Response.text_content(resp)}")
        
        ExMCP.disconnect(client)
        IO.puts("   ✓ Disconnected")
        
      {:error, reason} ->
        IO.puts("   ✗ Failed: #{inspect(reason)}")
        IO.puts("   Make sure hello_server_sse.exs is running")
    end
  end
end

# Run the demo
HelloClient.run()