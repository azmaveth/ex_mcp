#!/usr/bin/env elixir

# Simple SSE client test to debug the timeout issue

Mix.install([
  {:jason, "~> 1.4"}
])

defmodule SSEDebug do
  def test_sse_connection(url) do
    # Start the HTTP client
    :inets.start()
    
    IO.puts("Testing SSE connection to: #{url}")
    
    headers = [
      {"accept", "text/event-stream"},
      {"cache-control", "no-cache"},
      {"connection", "keep-alive"}
    ]
    
    request = {
      String.to_charlist(url),
      Enum.map(headers, fn {k, v} ->
        {String.to_charlist(k), String.to_charlist(v)}
      end)
    }
    
    IO.puts("Starting HTTP request...")
    
    case :httpc.request(:get, request, [], [{:sync, false}, {:stream, :self}]) do
      {:ok, request_id} ->
        IO.puts("Request started with ID: #{inspect(request_id)}")
        wait_for_messages(request_id, 10_000)
        
      {:error, reason} ->
        IO.puts("Request failed: #{inspect(reason)}")
    end
  end
  
  defp wait_for_messages(request_id, timeout) do
    IO.puts("Waiting for messages (timeout: #{timeout}ms)...")
    
    receive do
      message ->
        IO.puts("Received message: #{inspect(message)}")
        
        case message do
          {:http, {^request_id, :stream_start, headers}} ->
            IO.puts("Stream started! Headers: #{inspect(headers)}")
            wait_for_messages(request_id, timeout)
            
          {:http, {^request_id, :stream, data}} ->
            IO.puts("Stream data: #{inspect(data)}")
            wait_for_messages(request_id, timeout)
            
          {:http, {^request_id, :stream_end, headers}} ->
            IO.puts("Stream ended. Headers: #{inspect(headers)}")
            
          {:http, {^request_id, {:error, reason}}} ->
            IO.puts("Stream error: #{inspect(reason)}")
            
          _ ->
            wait_for_messages(request_id, timeout)
        end
    after
      timeout ->
        IO.puts("Timeout waiting for messages")
    end
  end
end

# Test the SSE connection
if System.argv() |> length() > 0 do
  url = List.first(System.argv())
  SSEDebug.test_sse_connection(url)
else
  IO.puts("Usage: elixir debug_sse_client.exs <sse_url>")
  IO.puts("Example: elixir debug_sse_client.exs http://localhost:8081/mcp/v1/sse")
end