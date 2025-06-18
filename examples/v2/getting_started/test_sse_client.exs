#!/usr/bin/env elixir

# Test client for SSE server on port 8322

Mix.install([
  {:ex_mcp, path: "../../../"}
])

defmodule TestSSEClient do
  def run do
    IO.puts("🧪 Testing SSE Server Connection...")
    IO.puts("   Server URL: http://localhost:8322 (with SSE)")
    
    # Connect to SSE server
    case ExMCP.Client.start_link(
      transport: :http,
      url: "http://localhost:8322",
      endpoint: "/",
      use_sse: true
    ) do
      {:ok, client} ->
        IO.puts("✅ Client started successfully")
        
        # Initialize
        IO.puts("\n📡 Initializing SSE connection...")
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
                
                # Test streaming tool
                test_streaming_hello(client)
                
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
                  if resource["uri"] == "live://server/metrics" do
                    test_live_metrics(client)
                  end
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
        IO.puts("❌ Failed to connect to SSE server: #{inspect(reason)}")
        IO.puts("\n📝 Make sure the SSE server is running:")
        IO.puts("   elixir sse_http_server.exs")
    end
  end
  
  defp test_streaming_hello(client) do
    IO.puts("\n🌊 Testing streaming hello...")
    case ExMCP.Client.call_tool(client, "say_hello", %{"name" => "SSE Test", "streaming" => true}) do
      {:ok, result} ->
        content = get_content(result)
        IO.puts("✅ Streaming response: #{content}")
      {:error, reason} ->
        IO.puts("❌ Streaming call failed: #{inspect(reason)}")
    end
  end
  
  defp test_live_metrics(client) do
    IO.puts("\n📊 Reading live metrics...")
    case ExMCP.Client.read_resource(client, "live://server/metrics") do
      {:ok, result} ->
        case result do
          %{"contents" => [%{"text" => json_text} | _]} ->
            metrics = Jason.decode!(json_text)
            IO.puts("✅ Live metrics:")
            IO.puts("   - Uptime: #{metrics["uptime_seconds"]}s")
            IO.puts("   - Memory: #{metrics["memory_usage_mb"]}MB")
            IO.puts("   - Processes: #{metrics["process_count"]}")
          _ ->
            IO.puts("✅ Metrics: #{inspect(result)}")
        end
      {:error, reason} ->
        IO.puts("❌ Failed to read metrics: #{inspect(reason)}")
    end
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
TestSSEClient.run()