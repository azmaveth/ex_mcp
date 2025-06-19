#!/usr/bin/env elixir

# Working Demo Script
# Creates an in-process demo without external servers

# Add lib to path
Code.prepend_path("_build/dev/lib/ex_mcp/ebin")
Code.prepend_path("_build/dev/lib/jason/ebin")

IO.puts("""
==========================================
ExMCP v2 Working Demo
==========================================

This demo shows v2 Response and Error types
in action without server/client complexity.
""")

# Test Response creation
IO.puts("\n1. Creating Responses:")

text_resp = ExMCP.Response.text("Hello from v2!", "demo_tool")
IO.puts("Text response: #{ExMCP.Response.text_content(text_resp)}")

json_resp = ExMCP.Response.json(%{status: "success", data: [1, 2, 3]}, "demo_tool")
IO.puts("JSON response: #{inspect(ExMCP.Response.data_content(json_resp))}")

error_resp = ExMCP.Response.error("Something went wrong", "demo_tool")
IO.puts("Error response: #{ExMCP.Response.text_content(error_resp)}")
IO.puts("Is error? #{error_resp.is_error}")

# Test Error types
IO.puts("\n2. Creating Errors:")

parse_error = ExMCP.Error.parse_error("Invalid JSON at position 42")
IO.puts("Parse error: #{parse_error.message} (code: #{parse_error.code})")

method_error = ExMCP.Error.method_not_found("tools/unknown")  
IO.puts("Method error: #{method_error.message} (code: #{method_error.code})")

tool_error = ExMCP.Error.tool_error("Division by zero", "calculator", 
  data: %{operation: "divide", a: 10, b: 0})
IO.puts("Tool error: #{tool_error.message}")
IO.puts("Error data: #{inspect(tool_error.data)}")

# Test error categories
IO.puts("\n3. Error Categories:")
errors = [parse_error, method_error, tool_error]
for error <- errors do
  category = ExMCP.Error.category(error)
  IO.puts("#{error.message} → #{category}")
end

# Test JSON-RPC conversion
IO.puts("\n4. JSON-RPC Format:")
json_rpc = ExMCP.Error.to_json_rpc(tool_error)
IO.puts("JSON-RPC: #{inspect(json_rpc)}")

# Test ClientConfig
IO.puts("\n5. Client Configuration:")

if Code.ensure_loaded?(ExMCP.ClientConfig) do
  config = ExMCP.ClientConfig.new()
  IO.puts("Default config profile: #{config.profile}")
  IO.puts("Default timeout: #{config.timeouts.connect}ms")
  
  http_config = config
                |> ExMCP.ClientConfig.put_transport(:http, url: "http://localhost:8080")
                |> ExMCP.ClientConfig.put_timeout(connect: 10000)
  
  IO.puts("HTTP config URL: #{http_config.transport.url}")
  IO.puts("HTTP config timeout: #{http_config.timeouts.connect}ms")
else
  IO.puts("ClientConfig not available (module loading issue)")
end

IO.puts("""

==========================================
Key v2 Features Demonstrated:
==========================================

✓ Structured Response types (text, json, error)
✓ Content extraction functions  
✓ Error types with categories and data
✓ JSON-RPC error format conversion
✓ ClientConfig builder pattern

The v2 API provides type safety and
consistency across all MCP operations.
""")

IO.puts("\n✅ Demo complete!")

# Show what a simple handler implementation might look like
IO.puts("""

==========================================
Example Handler Implementation:
==========================================

defmodule MyHandler do
  use ExMCP.Server.Handler
  
  @impl true
  def handle_call_tool("greet", %{"name" => name}, state) do
    content = [%{type: "text", text: "Hello, " <> name <> "!"}]
    {:ok, content, state}
  end
  
  @impl true
  def handle_list_tools(state) do
    tools = [%{
      name: "greet",
      description: "Greets a person",
      input_schema: %{
        type: "object",
        properties: %{name: %{type: "string"}},
        required: ["name"]
      }
    }]
    {:ok, tools, state}
  end
end

# Start server:
# {:ok, server} = ExMCP.Server.start_link(
#   handler: MyHandler,
#   transport: transport_config
# )
""")