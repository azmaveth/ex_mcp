#!/usr/bin/env elixir

# Error Handling Example for ExMCP v2
# 
# This example demonstrates:
# - Structured error responses
# - Error categorization
# - Graceful error handling patterns

Mix.install([
  {:ex_mcp, path: "../.."}
])

defmodule ErrorDemoServer do
  use ExMCP.Server.Handler
  use ExMCP.DSL.Tool
  
  # Tool that demonstrates various error scenarios
  tool "risky_operation" do
    description "Demonstrates different error handling scenarios"
    
    input_schema %{
      type: "object",
      properties: %{
        scenario: %{
          type: "string",
          enum: ["success", "validation", "runtime", "timeout", "not_found"]
        },
        value: %{type: "string"}
      },
      required: ["scenario"]
    }
    
    handler fn args ->
      case args["scenario"] do
        "success" ->
          ExMCP.Response.text("Operation completed successfully!", "risky_operation")
          
        "validation" ->
          # Input validation error
          ExMCP.Response.error("Invalid value: #{args["value"] || "missing"}", "risky_operation")
          
        "runtime" ->
          # Simulate runtime error
          try do
            _ = 1 / 0
          rescue
            ArithmeticError ->
              ExMCP.Response.error("Runtime error: division by zero", "risky_operation")
          end
          
        "timeout" ->
          # Simulate timeout
          Process.sleep(5000)
          ExMCP.Response.error("Operation timed out", "risky_operation")
          
        "not_found" ->
          # Resource not found error
          ExMCP.Error.tool_error("Required resource not found", "risky_operation", 
            data: %{resource: "config.json", path: "/app/config"})
          |> ExMCP.Response.from_error()
      end
    end
  end
  
  tool "safe_divide" do
    description "Division with comprehensive error handling"
    
    input_schema %{
      type: "object",
      properties: %{
        dividend: %{type: "number"},
        divisor: %{type: "number"}
      },
      required: ["dividend", "divisor"]
    }
    
    handler fn args ->
      dividend = args["dividend"]
      divisor = args["divisor"]
      
      cond do
        !is_number(dividend) ->
          ExMCP.Response.error("Dividend must be a number", "safe_divide")
          
        !is_number(divisor) ->
          ExMCP.Response.error("Divisor must be a number", "safe_divide")
          
        divisor == 0 ->
          error = ExMCP.Error.tool_error("Division by zero", "safe_divide",
            data: %{
              dividend: dividend,
              divisor: divisor,
              suggestion: "Please provide a non-zero divisor"
            }
          )
          ExMCP.Response.from_error(error)
          
        true ->
          result = dividend / divisor
          ExMCP.Response.json(%{
            dividend: dividend,
            divisor: divisor,
            result: result
          }, "safe_divide")
      end
    end
  end
  
  @impl true
  def init(_args) do
    {:ok, %{}}
  end
  
  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      name: "error-demo-server",
      version: "2.0.0",
      capabilities: %{tools: %{}}
    }, state}
  end
end

# Client-side error handling demonstration
defmodule ErrorDemoClient do
  def demonstrate_error_handling do
    # Start server
    {:ok, server} = ExMCP.Server.start(
      handler: ErrorDemoServer,
      transport: :stdio
    )
    
    # Create client config
    config = ExMCP.client_config()
             |> ExMCP.put_transport(:stdio)
             |> ExMCP.put_server_pid(server)
    
    {:ok, client} = ExMCP.connect(config)
    
    IO.puts("=== ExMCP v2 Error Handling Demo ===\n")
    
    # 1. Successful operation
    IO.puts("1. Successful operation:")
    {:ok, response} = ExMCP.call_tool(client, "risky_operation", %{scenario: "success"})
    IO.puts("   Result: #{ExMCP.Response.text_content(response)}")
    
    # 2. Validation error
    IO.puts("\n2. Validation error:")
    {:ok, response} = ExMCP.call_tool(client, "risky_operation", %{scenario: "validation"})
    if response.type == :error do
      IO.puts("   Error: #{response.content.message}")
    end
    
    # 3. Runtime error
    IO.puts("\n3. Runtime error:")
    {:ok, response} = ExMCP.call_tool(client, "risky_operation", %{scenario: "runtime"})
    if response.type == :error do
      IO.puts("   Error: #{response.content.message}")
    end
    
    # 4. Not found error with data
    IO.puts("\n4. Not found error with additional data:")
    {:ok, response} = ExMCP.call_tool(client, "risky_operation", %{scenario: "not_found"})
    if response.type == :error do
      error = response.content
      IO.puts("   Error: #{error.message}")
      IO.puts("   Code: #{error.code}")
      IO.puts("   Data: #{inspect(error.data)}")
    end
    
    # 5. Division examples
    IO.puts("\n5. Safe division examples:")
    
    # Valid division
    {:ok, response} = ExMCP.call_tool(client, "safe_divide", %{dividend: 10, divisor: 2})
    result = ExMCP.Response.json_content(response)
    IO.puts("   10 / 2 = #{result["result"]}")
    
    # Division by zero
    {:ok, response} = ExMCP.call_tool(client, "safe_divide", %{dividend: 10, divisor: 0})
    if response.type == :error do
      error = response.content
      IO.puts("   10 / 0 = Error: #{error.message}")
      IO.puts("   Suggestion: #{error.data["suggestion"]}")
    end
    
    # 6. Error categorization
    IO.puts("\n6. Error categorization:")
    errors = [
      ExMCP.Error.parse_error("Invalid JSON"),
      ExMCP.Error.method_not_found("unknown/method"),
      ExMCP.Error.tool_error("Execution failed", "tool"),
      ExMCP.Error.internal_error("Server error")
    ]
    
    for error <- errors do
      category = ExMCP.Error.category(error)
      IO.puts("   #{error.message} -> Category: #{category}")
    end
    
    # 7. Transport vs Application errors
    IO.puts("\n7. Transport vs Application errors:")
    
    # Simulate transport error
    case ExMCP.call_tool(client, "nonexistent_tool", %{}) do
      {:ok, response} ->
        if response.type == :error do
          IO.puts("   Application error: #{response.content.message}")
        end
      {:error, error} ->
        IO.puts("   Transport/Protocol error: #{error.message}")
        IO.puts("   Error type: #{error.__struct__}")
    end
    
    ExMCP.disconnect(client)
  end
end

# Run the demonstration
ErrorDemoClient.demonstrate_error_handling()

IO.puts("\n=== Error Handling Best Practices ===")
IO.puts("""
1. Always return ExMCP.Response types from handlers
2. Use ExMCP.Response.error/2 for simple errors
3. Use ExMCP.Error for detailed error information
4. Include helpful data in error responses
5. Check response.type before accessing content
6. Handle both {:ok, error_response} and {:error, error}
7. Use error categories to determine handling strategy
""")