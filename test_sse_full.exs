#!/usr/bin/env elixir

# Comprehensive test of SSE transport with MCP client

Mix.install([
  {:ex_mcp, path: Path.expand(".", __DIR__)},
  {:jason, "~> 1.4"}
])

defmodule SSEFullTest do
  def run(url) do
    IO.puts("Testing MCP SSE transport with: #{url}\n")
    
    config = [
      transport: :http,
      url: url,
      use_sse: true,
      endpoint: "/"
    ]
    
    case ExMCP.Client.start_link(config) do
      {:ok, client} ->
        IO.puts("✅ Client connected successfully!")
        
        # Test 1: List tools
        IO.puts("\n1. Testing tools/list...")
        case ExMCP.Client.list_tools(client) do
          {:ok, response} ->
            IO.puts("   ✅ Tools: #{length(response.tools)} found")
            Enum.each(response.tools, fn tool ->
              IO.puts("      - #{tool["name"]}: #{tool["description"]}")
            end)
          {:error, error} ->
            IO.puts("   ❌ Failed: #{inspect(error)}")
        end
        
        # Test 2: List prompts
        IO.puts("\n2. Testing prompts/list...")
        case ExMCP.Client.list_prompts(client) do
          {:ok, response} ->
            IO.puts("   ✅ Prompts: #{length(response.prompts)} found")
            Enum.each(response.prompts, fn prompt ->
              IO.puts("      - #{prompt["name"]}: #{prompt["description"]}")
            end)
          {:error, error} ->
            IO.puts("   ❌ Failed: #{inspect(error)}")
        end
        
        # Test 3: Get a prompt
        IO.puts("\n3. Testing prompts/get...")
        case ExMCP.Client.get_prompt(client, "hello_generator", %{"recipient" => "World"}) do
          {:ok, response} ->
            IO.puts("   ✅ Prompt retrieved successfully!")
            IO.puts("   Messages: #{length(response.messages)}")
            Enum.each(response.messages, fn msg ->
              content = case msg["content"] do
                %{"text" => text} -> text
                text when is_binary(text) -> text
                _ -> "???"
              end
              IO.puts("      [#{msg["role"]}]: #{String.slice(content, 0, 50)}...")
            end)
          {:error, error} ->
            IO.puts("   ❌ Failed: #{inspect(error)}")
        end
        
        # Test 4: Ping
        IO.puts("\n4. Testing ping...")
        case ExMCP.Client.ping(client) do
          {:ok, _} ->
            IO.puts("   ✅ Ping successful!")
          {:error, error} ->
            IO.puts("   ❌ Failed: #{inspect(error)}")
        end
        
        # Test 5: Server status
        IO.puts("\n5. Testing server status...")
        case ExMCP.Client.get_status(client) do
          {:ok, status} ->
            IO.puts("   ✅ Server status:")
            IO.puts("      - Connection: #{status.connection_status}")
            IO.puts("      - Protocol: #{status.protocol_version}")
            IO.puts("      - Server: #{inspect(status.server_info)}")
          {:error, error} ->
            IO.puts("   ❌ Failed: #{inspect(error)}")
        end
        
        IO.puts("\n✅ All tests completed!")
        ExMCP.Client.stop(client)
        
      {:error, reason} ->
        IO.puts("❌ Failed to connect: #{inspect(reason)}")
    end
  end
end

# Run the test
if System.argv() |> length() > 0 do
  url = List.first(System.argv())
  SSEFullTest.run(url)
else
  IO.puts("Usage: elixir test_sse_full.exs <server_url>")
  IO.puts("Example: elixir test_sse_full.exs http://localhost:8081")
end