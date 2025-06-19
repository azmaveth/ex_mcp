#!/usr/bin/env elixir

# Simple test client to verify v2 APIs
# Run with: elixir examples/v2/simple_test_client.exs

# Add the lib directory to the code path
Code.prepend_path("_build/dev/lib/ex_mcp/ebin")
Code.prepend_path("_build/dev/lib/jason/ebin")

# Now we can use the modules
alias ExMCP.ClientConfig
alias ExMCP.Response
alias ExMCP.Error

IO.puts("Testing ExMCP v2 APIs...")

# Test 1: ClientConfig builder
IO.puts("\n1. Testing ClientConfig builder:")
config = ClientConfig.new()
         |> ClientConfig.put_transport(:stdio, command: ["echo", "test"])

IO.inspect(config, label: "Config")

# Test 2: Response creation
IO.puts("\n2. Testing Response creation:")
text_response = Response.text("Hello, world!", "test_tool")
IO.inspect(text_response, label: "Text response")

json_response = Response.json(%{result: 42}, "calculator")
IO.inspect(json_response, label: "JSON response")

# Test 3: Error creation
IO.puts("\n3. Testing Error creation:")
error = Error.tool_error("Division by zero", "calculator")
IO.inspect(error, label: "Tool error")

json_error = Error.to_json_rpc(error)
IO.inspect(json_error, label: "JSON-RPC error")

# Test 4: Response extraction
IO.puts("\n4. Testing Response extraction:")
IO.puts("Text content: #{inspect(Response.text_content(text_response))}")
IO.puts("Data content: #{inspect(Response.data_content(json_response))}")

IO.puts("\nAll v2 API tests completed!")