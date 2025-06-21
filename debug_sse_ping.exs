#!/usr/bin/env elixir

# Test SSE connection and ping request

Mix.install([
  {:ex_mcp, path: Path.expand(".", __DIR__)},
  {:jason, "~> 1.4"},
  {:mint, "~> 1.0"},
  {:mint_web_socket, "~> 1.0"}
])

defmodule SSEPingDebug do
  def test_sse_ping(base_url) do
    IO.puts("Testing SSE connection and ping to: #{base_url}")
    
    # Generate a session ID
    session_id = "debug-#{System.unique_integer([:positive])}"
    IO.puts("Session ID: #{session_id}")
    
    # Step 1: Connect to SSE endpoint
    sse_url = "#{base_url}/sse"
    IO.puts("\n1. Connecting to SSE: #{sse_url}")
    
    {:ok, sse_pid} = start_sse_listener(sse_url, session_id)
    
    # Give SSE connection time to establish
    Process.sleep(500)
    
    # Step 2: Send ping request via POST
    IO.puts("\n2. Sending ping request...")
    
    ping_request = %{
      "jsonrpc" => "2.0",
      "id" => "ping-1",
      "method" => "ping",
      "params" => %{}
    }
    
    case send_post_request(base_url, ping_request, session_id) do
      {:ok, status, body} ->
        IO.puts("POST Response: status=#{status}, body=#{inspect(body)}")
        
      {:error, reason} ->
        IO.puts("POST Error: #{inspect(reason)}")
    end
    
    # Wait for SSE response
    IO.puts("\n3. Waiting for SSE response...")
    Process.sleep(2000)
    
    # Stop SSE listener
    Process.exit(sse_pid, :normal)
    IO.puts("\nTest complete!")
  end
  
  defp start_sse_listener(url, session_id) do
    parent = self()
    
    spawn(fn ->
      uri = URI.parse(url)
      
      {:ok, conn} = Mint.HTTP.connect(:http, uri.host, uri.port || 80)
      
      {:ok, conn, _ref} = Mint.HTTP.request(
        conn,
        "GET",
        uri.path || "/",
        [
          {"Accept", "text/event-stream"},
          {"Mcp-Session-Id", session_id},
          {"Cache-Control", "no-cache"}
        ],
        nil
      )
      
      listen_for_sse_events(conn, parent)
    end)
    |> fn pid -> {:ok, pid} end.()
  end
  
  defp listen_for_sse_events(conn, parent) do
    receive do
      message ->
        case Mint.HTTP.stream(conn, message) do
          {:ok, conn, responses} ->
            handle_sse_responses(responses, parent)
            listen_for_sse_events(conn, parent)
            
          {:error, _conn, reason, _responses} ->
            send(parent, {:sse_error, reason})
            
          :unknown ->
            listen_for_sse_events(conn, parent)
        end
    after
      10_000 ->
        send(parent, {:sse_timeout})
    end
  end
  
  defp handle_sse_responses(responses, parent) do
    Enum.each(responses, fn
      {:status, _ref, status} ->
        IO.puts("SSE Status: #{status}")
        
      {:headers, _ref, headers} ->
        IO.puts("SSE Headers: #{inspect(headers)}")
        
      {:data, _ref, data} ->
        IO.puts("SSE Data received: #{data}")
        parse_sse_data(data, parent)
        
      {:done, _ref} ->
        IO.puts("SSE Connection closed")
        
      other ->
        IO.puts("SSE Other: #{inspect(other)}")
    end)
  end
  
  defp parse_sse_data(data, _parent) do
    # Parse SSE format
    lines = String.split(data, "\n")
    
    Enum.each(lines, fn line ->
      case line do
        "event: " <> event_type ->
          IO.puts("  Event type: #{event_type}")
          
        "data: " <> event_data ->
          IO.puts("  Event data: #{event_data}")
          
          # Try to parse as JSON
          case Jason.decode(event_data) do
            {:ok, json} ->
              IO.puts("  Parsed JSON: #{inspect(json, pretty: true)}")
            {:error, _} ->
              :ok
          end
          
        "id: " <> event_id ->
          IO.puts("  Event ID: #{event_id}")
          
        "" ->
          :ok  # Empty line
          
        other ->
          IO.puts("  Other line: #{other}")
      end
    end)
  end
  
  defp send_post_request(base_url, request, session_id) do
    uri = URI.parse(base_url)
    
    body = Jason.encode!(request)
    
    {:ok, conn} = Mint.HTTP.connect(:http, uri.host, uri.port || 80)
    
    {:ok, conn, ref} = Mint.HTTP.request(
      conn,
      "POST",
      "/",
      [
        {"Content-Type", "application/json"},
        {"Mcp-Session-Id", session_id},
        {"Content-Length", to_string(byte_size(body))}
      ],
      body
    )
    
    receive_response(conn, ref, nil, "")
  end
  
  defp receive_response(conn, ref, status, body) do
    receive do
      message ->
        case Mint.HTTP.stream(conn, message) do
          {:ok, _conn, responses} ->
            {new_status, new_body} = process_responses(responses, ref, status, body)
            
            if done?(responses, ref) do
              {:ok, new_status, new_body}
            else
              receive_response(conn, ref, new_status, new_body)
            end
            
          {:error, _conn, reason, _} ->
            {:error, reason}
        end
    after
      5_000 ->
        {:error, :timeout}
    end
  end
  
  defp process_responses(responses, ref, status, body) do
    Enum.reduce(responses, {status, body}, fn
      {:status, ^ref, new_status}, {_, acc_body} ->
        {new_status, acc_body}
        
      {:data, ^ref, data}, {acc_status, acc_body} ->
        {acc_status, acc_body <> data}
        
      _, acc ->
        acc
    end)
  end
  
  defp done?(responses, ref) do
    Enum.any?(responses, fn
      {:done, ^ref} -> true
      _ -> false
    end)
  end
end

# Run the test
if System.argv() |> length() > 0 do
  url = List.first(System.argv())
  SSEPingDebug.test_sse_ping(url)
else
  IO.puts("Usage: elixir debug_sse_ping.exs <server_url>")
  IO.puts("Example: elixir debug_sse_ping.exs http://localhost:8081")
end