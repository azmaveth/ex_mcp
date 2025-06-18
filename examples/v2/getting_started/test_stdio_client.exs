#!/usr/bin/env elixir

# Test client for stdio transport
# The client will spawn the stdio server as a subprocess

Mix.install([
  {:ex_mcp, path: "../../../"}
])

defmodule TestStdioClient do
  def run do
    IO.puts("🧪 Testing stdio Transport...")
    
    stdio_server_path = Path.join([__DIR__, "stdio_server.exs"])
    
    unless File.exists?(stdio_server_path) do
      IO.puts("❌ Server script not found at: #{stdio_server_path}")
      System.halt(1)
    end
    
    IO.puts("📝 Starting stdio client...")
    IO.puts("   This will spawn the server as a subprocess")
    IO.puts("   Server: #{stdio_server_path}")
    
    # Start client connected to stdio server
    case ExMCP.Client.start_link(
      transport: :stdio,
      command: ["elixir", stdio_server_path]
    ) do
      {:ok, client} ->
        IO.puts("✅ Client started, initializing...")
        
        # Give the server time to start
        Process.sleep(3000)
        
        # List tools
        IO.puts("\n🔧 Listing tools...")
        case ExMCP.Client.list_tools(client) do
          {:ok, %{"tools" => tools}} ->
            IO.puts("✅ Found #{length(tools)} tools:")
            Enum.each(tools, fn tool ->
              IO.puts("   - #{tool["name"]}: #{tool["description"]}")
            end)
            
            # Call say_hello
            test_say_hello(client)
            
            # Call echo
            test_echo(client)
            
          {:error, reason} ->
            IO.puts("❌ Error listing tools: #{inspect(reason)}")
        end
        
        # List resources
        test_resources(client)
        
        # List prompts
        test_prompts(client)
        
        # Clean up
        IO.puts("\n🧹 Shutting down...")
        GenServer.stop(client)
        IO.puts("✅ Test completed!")
        
      {:error, reason} ->
        IO.puts("❌ Failed to start client: #{inspect(reason)}")
    end
  end
  
  defp test_say_hello(client) do
    IO.puts("\n📤 Testing say_hello tool...")
    case ExMCP.Client.call_tool(client, "say_hello", %{"name" => "stdio Tester"}) do
      {:ok, result} ->
        content = extract_content(result)
        IO.puts("✅ Response: #{content}")
      {:error, reason} ->
        IO.puts("❌ Error: #{inspect(reason)}")
    end
  end
  
  defp test_echo(client) do
    IO.puts("\n📤 Testing echo tool...")
    case ExMCP.Client.call_tool(client, "echo", %{"message" => "Hello, v2 DSL!", "uppercase" => true}) do
      {:ok, result} ->
        content = extract_content(result)
        IO.puts("✅ Response: #{content}")
      {:error, reason} ->
        IO.puts("❌ Error: #{inspect(reason)}")
    end
  end
  
  defp test_resources(client) do
    IO.puts("\n📁 Listing resources...")
    case ExMCP.Client.list_resources(client) do
      {:ok, %{"resources" => resources}} ->
        IO.puts("✅ Found #{length(resources)} resources:")
        Enum.each(resources, fn resource ->
          IO.puts("   - #{resource["uri"]}: #{resource["name"]}")
        end)
        
        # Read server info
        if Enum.any?(resources, & &1["uri"] == "config://server/info") do
          IO.puts("\n📖 Reading server info...")
          case ExMCP.Client.read_resource(client, "config://server/info") do
            {:ok, result} ->
              case result do
                %{"contents" => [%{"text" => json_text} | _]} ->
                  info = Jason.decode!(json_text)
                  IO.puts("✅ Server: #{info["name"]} v#{info["version"]}")
                  IO.puts("   DSL improvements:")
                  Enum.each(info["improvements"], fn improvement ->
                    IO.puts("   - #{improvement}")
                  end)
                _ ->
                  IO.puts("✅ Info: #{inspect(result)}")
              end
            {:error, reason} ->
              IO.puts("❌ Error: #{inspect(reason)}")
          end
        end
        
      {:error, reason} ->
        IO.puts("❌ Error: #{inspect(reason)}")
    end
  end
  
  defp test_prompts(client) do
    IO.puts("\n💭 Listing prompts...")
    case ExMCP.Client.list_prompts(client) do
      {:ok, %{"prompts" => prompts}} ->
        IO.puts("✅ Found #{length(prompts)} prompts:")
        Enum.each(prompts, fn prompt ->
          IO.puts("   - #{prompt["name"]}: #{prompt["description"]}")
        end)
        
        # Test greeting_style prompt
        if Enum.any?(prompts, & &1["name"] == "greeting_style") do
          IO.puts("\n💬 Testing greeting_style prompt...")
          case ExMCP.Client.get_prompt(client, "greeting_style", %{"style" => "funny", "name" => "Developer"}) do
            {:ok, result} ->
              IO.puts("✅ Got prompt with #{length(result["messages"])} messages")
              if msg = List.last(result["messages"]) do
                IO.puts("   Assistant says: #{msg["content"]}")
              end
            {:error, reason} ->
              IO.puts("❌ Error: #{inspect(reason)}")
          end
        end
        
      {:error, reason} ->
        IO.puts("❌ Error: #{inspect(reason)}")
    end
  end
  
  defp extract_content(%{"content" => [%{"text" => text} | _]}), do: text
  defp extract_content(%{content: [%{text: text} | _]}), do: text
  defp extract_content(result), do: inspect(result)
end

# Run the test
TestStdioClient.run()