#!/usr/bin/env elixir

# Convenience Client Demo
# 
# This demonstrates the ExMCP v2 convenience features that make
# MCP client development much easier and more developer-friendly.

Mix.install([
  {:ex_mcp, path: "../../../"}
])

defmodule ConvenienceDemo do
  use ExMCP.Helpers  # Import convenience macros
  
  def run do
    IO.puts("🚀 ExMCP v2 Convenience Features Demo\n")
    
    # Start demo server
    {:ok, server_pid} = start_demo_server()
    Process.sleep(500)
    
    try do
      demonstrate_basic_usage()
      IO.puts("\n" <> String.duplicate("=", 50) <> "\n")
      demonstrate_helper_macros()
      IO.puts("\n" <> String.duplicate("=", 50) <> "\n")
      demonstrate_error_handling()
      IO.puts("\n" <> String.duplicate("=", 50) <> "\n")
      demonstrate_batch_operations()
    after
      GenServer.stop(server_pid)
    end
  end
  
  defp demonstrate_basic_usage do
    IO.puts("📡 Basic Convenience Usage")
    
    # Connect with automatic URL parsing
    {:ok, client} = ExMCP.ConvenienceClient.connect("http://localhost:8766")
    
    try do
      # List tools with normalized response
      tools = ExMCP.ConvenienceClient.tools(client)
      IO.puts("✅ Found #{length(tools)} tools:")
      
      Enum.each(tools, fn tool ->
        schema_info = if tool.input_schema, do: " (with schema)", else: ""
        IO.puts("   - #{tool.name}: #{tool.description}#{schema_info}")
      end)
      
      # Call tool with normalized response
      result = ExMCP.ConvenienceClient.call(client, "greet", %{name: "Developer"})
      IO.puts("🎯 Tool result: #{inspect(result)}")
      
      # Find tool with fuzzy search
      calc_tools = ExMCP.ConvenienceClient.find_tool(client, "calc", fuzzy: true)
      IO.puts("🔍 Fuzzy search for 'calc': #{length(calc_tools || [])} matches")
      
      # Get server info
      {:ok, server_info} = ExMCP.ConvenienceClient.server_info(client)
      IO.puts("🖥️  Server: #{server_info.name} v#{server_info.version}")
      
    after
      ExMCP.ConvenienceClient.disconnect(client)
    end
  end
  
  defp demonstrate_helper_macros do
    IO.puts("🎪 Helper Macros with Automatic Cleanup")
    
    # Use macro for automatic connection management
    with_mcp_client "http://localhost:8766" do
      IO.puts("🔗 Connected automatically!")
      
      # Use bang methods for simple error handling
      tools = list_tools!()
      IO.puts("✅ Listed #{length(tools)} tools with list_tools!()")
      
      # Call tool with automatic error handling
      result = call_tool!("greet", %{name: "Macro User"})
      IO.puts("🎯 Called tool: #{inspect(result)}")
      
      # Measure execution time
      {resources, time_ms} = measure do
        list_resources!()
      end
      IO.puts("📊 Listed #{length(resources)} resources in #{time_ms}ms")
      
      # Get status
      status = get_status!()
      IO.puts("📈 Status: #{status.status} (#{status.pending_requests} pending)")
    end
    
    IO.puts("🔌 Client automatically disconnected!")
  end
  
  defp demonstrate_error_handling do
    IO.puts("⚠️  Enhanced Error Handling")
    
    with_mcp_client "http://localhost:8766" do
      # Try to call a non-existent tool
      case ExMCP.ConvenienceClient.call(client, "nonexistent_tool", %{}) do
        {:error, error} ->
          IO.puts("❌ Tool call failed:")
          IO.puts("   Type: #{error.type}")
          IO.puts("   Category: #{error.category}")
          IO.puts("   Message: #{error.message}")
          IO.puts("   Suggestions:")
          Enum.each(error.suggestions, fn suggestion ->
            IO.puts("   - #{suggestion}")
          end)
          
        result ->
          IO.puts("✅ Unexpected success: #{inspect(result)}")
      end
      
      # Demonstrate retry logic
      IO.puts("\n🔄 Testing retry logic:")
      
      result = retry max_attempts: 3, base_delay: 100 do
        # This will succeed immediately, but shows retry syntax
        call_tool!("greet", %{name: "Retry Test"})
      end
      
      IO.puts("✅ Retry succeeded: #{inspect(result)}")
    end
  end
  
  defp demonstrate_batch_operations do
    IO.puts("📦 Batch Operations")
    
    with_mcp_client "http://localhost:8766" do
      # Execute multiple operations efficiently
      operations = [
        {:call_tool, "greet", %{name: "Alice"}},
        {:call_tool, "greet", %{name: "Bob"}},
        {:call_tool, "calculate", %{operation: "add", a: 10, b: 20}},
        {:list_resources, %{}}
      ]
      
      IO.puts("🚀 Executing #{length(operations)} operations in batch...")
      
      {results, time_ms} = measure do
        batch_execute!(operations, max_concurrency: 2)
      end
      
      IO.puts("✅ Batch completed in #{time_ms}ms:")
      
      Enum.with_index(results, 1) do |{result, index}|
        case result do
          result when is_binary(result) ->
            IO.puts("   #{index}. Tool result: #{result}")
          result when is_list(result) ->
            IO.puts("   #{index}. List result: #{length(result)} items")
          result ->
            IO.puts("   #{index}. Other result: #{inspect(result)}")
        end
      end
    end
  end
  
  defp start_demo_server do
    Task.start_link(fn ->
      {:ok, listen_socket} = :gen_tcp.listen(8766, [
        :binary, packet: :http_bin, active: false, reuseaddr: true
      ])
      
      IO.puts("🎯 Demo server listening on port 8766")
      accept_loop(listen_socket)
    end)
  end
  
  defp accept_loop(listen_socket) do
    {:ok, socket} = :gen_tcp.accept(listen_socket)
    spawn(fn -> handle_http_request(socket) end)
    accept_loop(listen_socket)
  end
  
  defp handle_http_request(socket) do
    case :gen_tcp.recv(socket, 0) do
      {:ok, {:http_request, :POST, {:abs_path, "/mcp"}, _}} ->
        # Read headers
        read_headers(socket)
        # Read body (simplified)
        {:ok, body} = :gen_tcp.recv(socket, 0)
        
        # Handle MCP request
        response = handle_mcp_request(body)
        
        # Send HTTP response
        http_response = [
          "HTTP/1.1 200 OK\r\n",
          "Content-Type: application/json\r\n",
          "Content-Length: #{byte_size(response)}\r\n",
          "\r\n",
          response
        ]
        
        :gen_tcp.send(socket, http_response)
        :gen_tcp.close(socket)
        
      _ ->
        :gen_tcp.close(socket)
    end
  end
  
  defp read_headers(socket) do
    case :gen_tcp.recv(socket, 0) do
      {:ok, :http_eoh} -> :ok
      {:ok, _header} -> read_headers(socket)
      _ -> :ok
    end
  end
  
  defp handle_mcp_request(body) do
    case Jason.decode(body) do
      {:ok, %{"method" => "initialize", "id" => id}} ->
        Jason.encode!(%{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => %{
            "protocolVersion" => "2024-11-05",
            "capabilities" => %{"tools" => %{}, "resources" => %{}},
            "serverInfo" => %{"name" => "ConvenienceDemo", "version" => "1.0.0"}
          }
        })
        
      {:ok, %{"method" => "tools/list", "id" => id}} ->
        Jason.encode!(%{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => %{
            "tools" => [
              %{
                "name" => "greet",
                "description" => "Says hello to someone",
                "inputSchema" => %{
                  "type" => "object",
                  "properties" => %{"name" => %{"type" => "string"}},
                  "required" => ["name"]
                }
              },
              %{
                "name" => "calculate",
                "description" => "Performs basic calculations",
                "inputSchema" => %{
                  "type" => "object",
                  "properties" => %{
                    "operation" => %{"type" => "string"},
                    "a" => %{"type" => "number"},
                    "b" => %{"type" => "number"}
                  }
                }
              }
            ]
          }
        })
        
      {:ok, %{"method" => "tools/call", "params" => %{"name" => "greet", "arguments" => args}, "id" => id}} ->
        name = Map.get(args, "name", "World")
        Jason.encode!(%{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => %{
            "content" => [%{"type" => "text", "text" => "Hello, #{name}! 👋"}]
          }
        })
        
      {:ok, %{"method" => "tools/call", "params" => %{"name" => "calculate", "arguments" => args}, "id" => id}} ->
        result = case args do
          %{"operation" => "add", "a" => a, "b" => b} -> a + b
          %{"operation" => "subtract", "a" => a, "b" => b} -> a - b
          %{"operation" => "multiply", "a" => a, "b" => b} -> a * b
          %{"operation" => "divide", "a" => a, "b" => b} when b != 0 -> a / b
          _ -> "Invalid operation"
        end
        
        Jason.encode!(%{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => %{
            "content" => [%{"type" => "text", "text" => "Result: #{result}"}]
          }
        })
        
      {:ok, %{"method" => "resources/list", "id" => id}} ->
        Jason.encode!(%{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => %{
            "resources" => [
              %{
                "uri" => "demo://sample.txt",
                "name" => "Sample Resource",
                "description" => "A demo resource"
              }
            ]
          }
        })
        
      {:ok, %{"method" => "tools/call", "params" => %{"name" => "nonexistent_tool"}, "id" => id}} ->
        Jason.encode!(%{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{
            "code" => -32601,
            "message" => "Tool 'nonexistent_tool' not found"
          }
        })
        
      _ ->
        Jason.encode!(%{
          "jsonrpc" => "2.0",
          "error" => %{"code" => -32600, "message" => "Invalid request"}
        })
    end
  end
end

# Run the demo
ConvenienceDemo.run()