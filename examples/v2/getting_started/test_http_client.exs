#!/usr/bin/env elixir

# Test client for HTTP server on port 8321

Mix.install([
  {:ex_mcp, path: "../../../"}
])

defmodule TestHTTPClient do
  def run do
    IO.puts("🧪 Testing HTTP Server Connection...")
    IO.puts("   Server URL: http://localhost:8321")
    
    # Connect to HTTP server
    case ExMCP.Client.start_link(
      transport: :http,
      url: "http://localhost:8321",
      endpoint: "/"
    ) do
      {:ok, client} ->
        IO.puts("✅ Client started successfully")
        
        # Initialize
        IO.puts("\n📡 Initializing connection...")
        case ExMCP.Client.initialize(client) do
          {:ok, response} ->
            IO.puts("✅ Initialized successfully!")
            IO.puts("   Protocol: #{response["protocolVersion"]}")
            IO.puts("   Capabilities: #{inspect(response["capabilities"])}")
            
            # List tools
            IO.puts("\n🔧 Listing tools...")
            case ExMCP.Client.list_tools(client) do
              {:ok, tools} ->
                IO.puts("✅ Found #{length(tools)} tools:")
                Enum.each(tools, fn tool ->
                  IO.puts("   - #{tool["name"]}: #{tool["description"]}")
                end)
                
                # Test a tool call
                if length(tools) > 0 do
                  test_tool_call(client, hd(tools)["name"])
                end
                
              {:error, reason} ->
                IO.puts("❌ Failed to list tools: #{inspect(reason)}")
            end
            
            # List resources
            IO.puts("\n📁 Listing resources...")
            case ExMCP.Client.list_resources(client) do
              {:ok, resources} ->
                IO.puts("✅ Found #{length(resources)} resources:")
                Enum.each(resources, fn resource ->
                  IO.puts("   - #{resource["uri"]}: #{resource["name"]}")
                end)
                
              {:error, reason} ->
                IO.puts("❌ Failed to list resources: #{inspect(reason)}")
            end
            
          {:error, reason} ->
            IO.puts("❌ Initialize failed: #{inspect(reason)}")
        end
        
        # Clean up
        GenServer.stop(client)
        
      {:error, reason} ->
        IO.puts("❌ Failed to connect to HTTP server: #{inspect(reason)}")
        IO.puts("\n📝 Make sure the HTTP server is running:")
        IO.puts("   elixir simple_http_server.exs")
    end
  end
  
  defp test_tool_call(client, "say_hello") do
    IO.puts("\n🧪 Testing say_hello tool...")
    case ExMCP.Client.call_tool(client, "say_hello", %{"name" => "HTTP Test", "enthusiasm" => 8}) do
      {:ok, result} ->
        content = get_content(result)
        IO.puts("✅ Tool response: #{content}")
      {:error, reason} ->
        IO.puts("❌ Tool call failed: #{inspect(reason)}")
    end
  end
  
  defp test_tool_call(client, "calculate") do
    IO.puts("\n🧪 Testing calculate tool...")
    case ExMCP.Client.call_tool(client, "calculate", %{"operation" => "add", "a" => 10, "b" => 20}) do
      {:ok, result} ->
        content = get_content(result)
        IO.puts("✅ Tool response: #{content}")
      {:error, reason} ->
        IO.puts("❌ Tool call failed: #{inspect(reason)}")
    end
  end
  
  defp test_tool_call(client, tool_name) do
    IO.puts("\n📝 Tool '#{tool_name}' - no test implemented")
  end
  
  defp get_content(result) do
    case result do
      %{"content" => [%{"text" => text} | _]} -> text
      %{content: [%{text: text} | _]} -> text
      _ -> inspect(result)
    end
  end
end

# Run the test
TestHTTPClient.run()