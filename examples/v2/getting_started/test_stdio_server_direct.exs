#!/usr/bin/env elixir

# Test the stdio server directly by sending JSON-RPC messages

Mix.install([
  {:jason, "~> 1.4"}
])

# Send a JSON-RPC message to stdout
defmodule StdioTest do
  def send_message(message) do
    json = Jason.encode!(message)
    IO.puts(json)
  end
  
  def test_initialize do
    send_message(%{
      "jsonrpc" => "2.0",
      "method" => "initialize",
      "params" => %{
        "protocolVersion" => "2025-03-26",
        "capabilities" => %{},
        "clientInfo" => %{"name" => "test", "version" => "1.0.0"}
      },
      "id" => 1
    })
  end
  
  def test_list_tools do
    send_message(%{
      "jsonrpc" => "2.0",
      "method" => "tools/list",
      "params" => %{},
      "id" => 2
    })
  end
end

# Test sequence
IO.puts(:stderr, "🧪 Testing stdio server directly...\n")

IO.puts(:stderr, "1. Sending initialize...")
StdioTest.test_initialize()

# Give it time to process
Process.sleep(500)

IO.puts(:stderr, "\n2. Sending tools/list...")
StdioTest.test_list_tools()

# Wait for response
Process.sleep(500)

IO.puts(:stderr, "\n✅ Messages sent. Check if server responds.")