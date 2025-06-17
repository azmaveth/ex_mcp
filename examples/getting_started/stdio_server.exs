#!/usr/bin/env elixir

# MCP Server that communicates via stdin/stdout
# This can be used with any MCP-compliant client

Mix.install([
  {:ex_mcp, path: "../.."}
])

defmodule StdioServer do
  @moduledoc """
  A simple MCP server that reads JSON-RPC from stdin and writes to stdout.
  This is the standard way MCP servers communicate.
  """
  
  def start do
    IO.puts(:stderr, "MCP Server starting...")
    loop("")
  end
  
  defp loop(buffer) do
    case IO.read(:stdio, :line) do
      :eof ->
        IO.puts(:stderr, "EOF received, shutting down")
        :ok
        
      {:error, reason} ->
        IO.puts(:stderr, "Read error: #{inspect(reason)}")
        :ok
        
      data ->
        buffer = buffer <> data
        
        # Try to parse complete JSON objects
        case Jason.decode(buffer) do
          {:ok, message} ->
            handle_message(message)
            loop("")  # Reset buffer after successful parse
            
          {:error, _} ->
            # Not a complete JSON object yet, keep buffering
            loop(buffer)
        end
    end
  end
  
  defp handle_message(%{"method" => "initialize", "id" => id}) do
    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{
        "protocolVersion" => "2025-03-26",
        "capabilities" => %{
          "tools" => %{}
        },
        "serverInfo" => %{
          "name" => "hello-world-server",
          "version" => "1.0.0"
        }
      }
    }
    
    IO.puts(Jason.encode!(response))
  end
  
  defp handle_message(%{"method" => "tools/list", "id" => id}) do
    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{
        "tools" => [
          %{
            "name" => "say_hello",
            "description" => "Say hello to someone",
            "inputSchema" => %{
              "type" => "object",
              "properties" => %{
                "name" => %{"type" => "string", "description" => "Name to greet"}
              },
              "required" => ["name"]
            }
          }
        ]
      }
    }
    
    IO.puts(Jason.encode!(response))
  end
  
  defp handle_message(%{"method" => "tools/call", "params" => %{"name" => "say_hello", "arguments" => args}, "id" => id}) do
    name = args["name"] || "World"
    
    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{
        "content" => [
          %{
            "type" => "text",
            "text" => "Hello, #{name}! Welcome to ExMCP via stdio! 📝"
          }
        ]
      }
    }
    
    IO.puts(Jason.encode!(response))
  end
  
  defp handle_message(%{"method" => method, "id" => id}) do
    # Method not found
    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "error" => %{
        "code" => -32601,
        "message" => "Method not found: #{method}"
      }
    }
    
    IO.puts(Jason.encode!(response))
  end
  
  defp handle_message(message) do
    IO.puts(:stderr, "Unhandled message: #{inspect(message)}")
  end
end

# Start the server
IO.puts(:stderr, """
MCP Server ready! Send JSON-RPC messages to stdin.

Example messages:
{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"1.0.0","capabilities":{},"clientInfo":{"name":"test","version":"1.0.0"}},"id":1}
{"jsonrpc":"2.0","method":"tools/list","params":{},"id":2}
{"jsonrpc":"2.0","method":"tools/call","params":{"name":"say_hello","arguments":{"name":"World"}},"id":3}

Press Ctrl+D to exit.
""")

StdioServer.start()