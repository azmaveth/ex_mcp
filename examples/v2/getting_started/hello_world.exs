#!/usr/bin/env elixir

# Hello World Client - Demonstrates connecting to all four MCP server types
# Shows how the v2 DSL dramatically simplifies both servers AND clients

Mix.install([
  {:ex_mcp, path: "../../../"}
])

defmodule HelloWorldClient do
  @moduledoc """
  Client that connects to MCP servers using different transports.
  
  This demonstrates:
  1. Native BEAM - Ultra-fast Elixir-to-Elixir communication  
  2. stdio - Standard MCP communication via stdin/stdout
  3. HTTP - Network communication using HTTP
  4. HTTP+SSE - Network communication with Server-Sent Events
  """

  def run do
    IO.puts("""
    ==========================================
    ExMCP v2 Hello World Client
    ==========================================
    
    🚀 This client will connect to v2 servers
       demonstrating all transport types.
       
    📊 v2 Benefits:
    - Servers: ~15-100 lines vs v1's 150-275 lines
    - Clean DSL for both servers and clients
    - Auto-capability detection
    - Smart content helpers
    """)

    # Try each transport type
    test_native_transport()
    test_stdio_transport()
    test_http_transport()
    test_sse_transport()

    IO.puts("""
    
    ==========================================
    Demo Complete!
    
    ✨ Key v2 improvements demonstrated:
    - 🎯 60-80% code reduction
    - 🏗️ Clean, declarative DSL
    - 🛡️ Type-safe with compile-time validation
    - 📝 Auto JSON Schema generation
    - 🔧 Smart content helpers
    
    To run the servers yourself:
    1. Native: elixir native_server.exs
    2. stdio: elixir stdio_server.exs
    3. HTTP: elixir simple_http_server.exs
    4. SSE: elixir sse_http_server.exs
    ==========================================
    """)
  end

  defp test_native_transport do
    IO.puts("\n1. Testing Native BEAM Transport")
    IO.puts("   " <> String.duplicate("-", 50))
    
    # Native transport only works within the same BEAM instance
    # Different OS processes can't share process registrations
    IO.puts("   📝 Note: Native transport requires same BEAM instance")
    IO.puts("   🚀 Starting local demo server...")
    
    test_local_native_server()
    
    IO.puts("   💡 Native: Direct BEAM communication, no serialization!")
    IO.puts("   💡 Use distributed Erlang for cross-node native transport")
  end
  
  defp test_native_connection(service_name) do
    case ExMCP.Client.start_link(transport: :native, service_name: service_name) do
      {:ok, client} ->
        Process.sleep(100)
        test_native_client(client)
        GenServer.stop(client)
      {:error, reason} ->
        IO.puts("   ❌ Failed to connect: #{inspect(reason)}")
    end
  end
  
  defp test_native_connection_by_pid(pid) do
    case ExMCP.Client.start_link(transport: :native, server_pid: pid) do
      {:ok, client} ->
        Process.sleep(100)
        test_native_client(client)
        GenServer.stop(client)
      {:error, reason} ->
        IO.puts("   ❌ Failed to connect: #{inspect(reason)}")
    end
  end
  
  defp test_native_client(client) do
    # Initialize with timeout
    init_task = Task.async(fn -> ExMCP.Client.initialize(client) end)
    
    case Task.yield(init_task, 2000) || Task.shutdown(init_task) do
      {:ok, {:ok, _}} -> 
        IO.puts("   ✅ Connected and initialized!")
        
        # List capabilities
        case ExMCP.Client.list_tools(client) do
          {:ok, tools} -> 
            IO.puts("   🔧 Available tools: #{tools |> Enum.map(& &1["name"]) |> inspect()}")
          {:error, _} -> 
            IO.puts("   ⚠️  Could not list tools")
        end
        
        # Call tool
        case ExMCP.Client.call_tool(client, "say_hello", %{"name" => "Developer"}) do
          {:ok, result} ->
            content = get_content(result)
            IO.puts("   📤 Response: #{content}")
          {:error, reason} ->
            IO.puts("   ❌ Tool call failed: #{inspect(reason)}")
        end
      
      _ ->
        IO.puts("   ⚠️  Initialize timed out")
    end
  end
  
  defp test_local_native_server do
    # Start a local server for demo
    defmodule LocalNativeServer do
      use ExMCP.ServerV2
      
      deftool "say_hello" do
        tool_description "Say hello via native BEAM"
        args do
          field :name, :string, required: true
        end
      end
      
      @impl true
      def handle_tool_call("say_hello", %{"name" => name}, state) do
        {:ok, %{content: [text("Hello, #{name}! (Native BEAM transport)")]}, state}
      end
    end
    
    {:ok, server} = LocalNativeServer.start_link()
    ExMCP.Registry.register("hello_native_local", server)
    
    # Test the local server
    test_native_connection("hello_native_local")
    
    # Clean up
    GenServer.stop(server)
  end

  defp test_stdio_transport do
    IO.puts("\n2. Testing stdio Transport")
    IO.puts("   " <> String.duplicate("-", 50))
    
    stdio_path = Path.join([__DIR__, "stdio_server.exs"])
    
    if File.exists?(stdio_path) do
      # Start client with timeout
      task = Task.async(fn ->
        case ExMCP.Client.start_link(
          transport: :stdio,
          command: ["elixir", stdio_path]
        ) do
          {:ok, client} -> {:ok, client}
          {:error, reason} -> {:error, reason}
        end
      end)
      
      case Task.yield(task, 3000) || Task.shutdown(task) do
        {:ok, {:ok, client}} ->
          Process.sleep(500)
          
          # Initialize with timeout
          init_task = Task.async(fn -> ExMCP.Client.initialize(client) end)
          
          case Task.yield(init_task, 2000) || Task.shutdown(init_task) do
            {:ok, {:ok, _}} -> 
              IO.puts("   ✅ Connected via stdio!")
              
              # Call tool
              case ExMCP.Client.call_tool(client, "say_hello", %{"name" => "Developer"}) do
                {:ok, result} ->
                  content = get_content(result)
                  IO.puts("   📤 Response: #{content}")
                {:error, reason} ->
                  IO.puts("   ❌ Tool call failed: #{inspect(reason)}")
              end
              
              # Try echo tool
              case ExMCP.Client.call_tool(client, "echo", %{"message" => "Hello DSL!", "uppercase" => true}) do
                {:ok, result} ->
                  content = get_content(result)
                  IO.puts("   📤 Echo: #{content}")
                {:error, _} -> :ok
              end
              
            _ -> 
              IO.puts("   ⚠️  Initialize timed out - stdio server may be slow to start")
          end
          
          # Clean up
          if Process.alive?(client), do: GenServer.stop(client)
          
        _ ->
          IO.puts("   ⚠️  Failed to start stdio client (timeout)")
          IO.puts("   📝 This is normal - stdio servers can be slow to start")
      end
    else
      IO.puts("   📝 stdio server not found at: #{stdio_path}")
    end
    
    IO.puts("   💡 stdio: JSON-RPC over stdin/stdout, ~50 lines vs v1's ~150!")
  end

  defp test_http_transport do
    IO.puts("\n3. Testing HTTP Transport")
    IO.puts("   " <> String.duplicate("-", 50))
    
    case ExMCP.Client.start_link(
      transport: :http,
      url: "http://localhost:8321",
      endpoint: "/"
    ) do
      {:ok, client} ->
        Process.sleep(500)
        
        # Check if connected
        case ExMCP.Client.initialize(client) do
          {:ok, _} ->
            IO.puts("   ✅ Connected to HTTP server!")
            
            # Call tool
            case ExMCP.Client.call_tool(client, "say_hello", %{"name" => "Developer", "enthusiasm" => 8}) do
              {:ok, result} ->
                content = get_content(result)
                IO.puts("   📤 Response: #{content}")
              {:error, reason} ->
                IO.puts("   ❌ Tool call failed: #{inspect(reason)}")
            end
            
            # Read resource
            case ExMCP.Client.read_resource(client, "config://server/status") do
              {:ok, result} ->
                IO.puts("   📁 Resource read successful!")
              {:error, _} -> :ok
            end
            
          {:error, _} ->
            IO.puts("   ⚠️  HTTP server not running on port 8321")
            IO.puts("   📝 Run: elixir simple_http_server.exs")
        end
        
        GenServer.stop(client)
        
      {:error, reason} ->
        IO.puts("   ⚠️  Could not connect to HTTP server")
        IO.puts("   📝 Run: elixir simple_http_server.exs")
    end
    
    IO.puts("   💡 HTTP: Built-in CORS & JSON-RPC, ~80 lines vs v1's ~175!")
  end

  defp test_sse_transport do
    IO.puts("\n4. Testing HTTP+SSE Transport")
    IO.puts("   " <> String.duplicate("-", 50))
    
    case ExMCP.Client.start_link(
      transport: :http,
      url: "http://localhost:8322",
      endpoint: "/",
      use_sse: true
    ) do
      {:ok, client} ->
        Process.sleep(500)
        
        # Check if connected
        case ExMCP.Client.initialize(client) do
          {:ok, _} ->
            IO.puts("   ✅ Connected to SSE server!")
            
            # Call streaming tool
            case ExMCP.Client.call_tool(client, "say_hello", %{"name" => "Developer", "streaming" => true}) do
              {:ok, result} ->
                content = get_content(result)
                IO.puts("   📤 Response: #{content}")
              {:error, reason} ->
                IO.puts("   ❌ Tool call failed: #{inspect(reason)}")
            end
            
          {:error, _} ->
            IO.puts("   ⚠️  SSE server not running on port 8322")
            IO.puts("   📝 Run: elixir sse_http_server.exs")
        end
        
        GenServer.stop(client)
        
      {:error, reason} ->
        IO.puts("   ⚠️  Could not connect to SSE server")
        IO.puts("   📝 Run: elixir sse_http_server.exs")
    end
    
    IO.puts("   💡 SSE: Real-time streaming, ~100 lines vs v1's ~275!")
  end

  # Helper to extract content from various response formats
  defp get_content(result) do
    cond do
      is_map(result) and Map.has_key?(result, "content") ->
        extract_text(result["content"])
      is_map(result) and Map.has_key?(result, :content) ->
        extract_text(result[:content])
      true ->
        inspect(result)
    end
  end

  defp extract_text(content) when is_list(content) do
    content
    |> Enum.map(fn item ->
      cond do
        is_map(item) and Map.has_key?(item, "text") -> item["text"]
        is_map(item) and Map.has_key?(item, :text) -> item[:text]
        true -> inspect(item)
      end
    end)
    |> Enum.join(" ")
  end
  defp extract_text(content), do: inspect(content)
end

# Run the client demo
HelloWorldClient.run()