#!/usr/bin/env elixir

# Test MCP client with SSE to debug the request-response cycle

Mix.install([
  {:ex_mcp, path: Path.expand(".", __DIR__)},
  {:jason, "~> 1.4"}
])

defmodule SSEMCPDebug do
  def test_mcp_sse(url) do
    IO.puts("Testing MCP client with SSE to: #{url}")
    
    # Configure debug logging
    Logger.configure(level: :debug)
    
    config = [
      transport: :http,
      url: url,
      use_sse: true,
      endpoint: "/",  # Use root endpoint
      timeout: 10_000
    ]
    
    IO.puts("Starting MCP client with config: #{inspect(config)}")
    
    case ExMCP.Client.start_link(config) do
      {:ok, client} ->
        IO.puts("✅ MCP client connected successfully!")
        
        # Try to list tools (simpler request than prompts)
        IO.puts("Requesting tools list...")
        case ExMCP.Client.list_tools(client, timeout: 5_000) do
          {:ok, response} ->
            IO.puts("✅ Tools list received!")
            IO.puts("  Tools: #{inspect(response.tools)}")
            
          {:error, error} ->
            IO.puts("❌ Tools list failed: #{inspect(error)}")
        end
        
        ExMCP.Client.stop(client)
        
      {:error, reason} ->
        IO.puts("❌ MCP client connection failed: #{inspect(reason)}")
    end
  end
end

# Test the MCP SSE
if System.argv() |> length() > 0 do
  url = List.first(System.argv())
  SSEMCPDebug.test_mcp_sse(url)
else
  IO.puts("Usage: elixir debug_sse_mcp.exs <server_url>")
  IO.puts("Example: elixir debug_sse_mcp.exs http://localhost:8081")
end