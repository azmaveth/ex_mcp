#!/usr/bin/env elixir

# Debug HTTP Transport
# Simple test to isolate the HTTP transport issue

Mix.install([
  {:ex_mcp, path: Path.expand("../..", __DIR__)},
  {:jason, "~> 1.4"},
  {:ex_json_schema, "~> 0.10"},
  {:html_entities, "~> 0.5"}
], verbose: false)

# Enable debug logging for HTTP transport only
Logger.configure(level: :debug)
Logger.put_module_level([ExMCP.Transport.HTTP], :debug)

defmodule DebugHTTP do
  def run do
    # Start HTTP server
    port = 8080
    server_path = Path.join(__DIR__, "02_http_server.exs")
    
    IO.puts("🔧 Debugging HTTP Transport")
    IO.puts("Starting HTTP server on port #{port}...")
    
    server_pid = spawn(fn ->
      System.cmd("elixir", [server_path], env: [{"HTTP_PORT", "#{port}"}])
    end)
    
    Process.sleep(3000)  # Give server time to start
    
    # Test HTTP client connection
    IO.puts("Testing HTTP client connection...")
    
    case ExMCP.Client.start_link(
      transport: :http,
      url: "http://localhost:#{port}",
      use_sse: false,
      name: :debug_http_client
    ) do
      {:ok, client} ->
        IO.puts("✓ Client connected successfully")
        
        # Test list_resources
        IO.puts("Testing list_resources...")
        case ExMCP.Client.list_resources(client) do
          {:ok, response} ->
            IO.puts("✓ list_resources succeeded: #{inspect(response)}")
          {:error, error} ->
            IO.puts("❌ list_resources failed: #{inspect(error)}")
        end
        
        ExMCP.Client.stop(client)
        
      {:error, error} ->
        IO.puts("❌ Client connection failed: #{inspect(error)}")
    end
    
    Process.exit(server_pid, :kill)
    IO.puts("Debug complete")
  end
end

DebugHTTP.run()