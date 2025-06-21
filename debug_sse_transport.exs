#!/usr/bin/env elixir

# Test the SSE transport directly to isolate the timeout issue

Mix.install([
  {:ex_mcp, path: Path.expand(".", __DIR__)},
  {:jason, "~> 1.4"}
])

defmodule SSETransportDebug do
  def test_sse_transport(url) do
    IO.puts("Testing ExMCP SSE transport to: #{url}")
    
    # Configure debug logging
    Logger.configure(level: :debug)
    
    config = [
      url: url,
      use_sse: true,
      stream_handshake_timeout: 5_000  # Shorter timeout for testing
    ]
    
    IO.puts("Connecting with config: #{inspect(config)}")
    
    case ExMCP.Transport.HTTP.connect(config) do
      {:ok, state} ->
        IO.puts("Connection successful!")
        IO.puts("SSE PID: #{inspect(state.sse_pid)}")
        
        # Try to receive a message
        IO.puts("Testing message receive...")
        case ExMCP.Transport.HTTP.receive_message(state) do
          {:ok, message, new_state} ->
            IO.puts("Received message: #{inspect(message)}")
            {:ok, new_state}
            
          {:error, reason} ->
            IO.puts("Receive failed: #{inspect(reason)}")
            {:error, reason}
        end
        
      {:error, reason} ->
        IO.puts("Connection failed: #{inspect(reason)}")
        {:error, reason}
    end
  end
end

# Test the SSE transport
if System.argv() |> length() > 0 do
  url = List.first(System.argv())
  SSETransportDebug.test_sse_transport(url)
else
  IO.puts("Usage: elixir debug_sse_transport.exs <sse_url>")
  IO.puts("Example: elixir debug_sse_transport.exs http://localhost:8081")
end