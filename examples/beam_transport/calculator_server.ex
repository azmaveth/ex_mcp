defmodule Examples.Native.CalculatorService do
  @moduledoc """
  Example MCP service using Native Service Dispatcher that provides calculator tools.
  
  This demonstrates:
  - Service registration with ExMCP.Service macro
  - Basic tool implementation
  - Error handling
  - Service state management
  - Progress notifications for long operations
  """
  
  use ExMCP.Service, name: :calculator_service
  require Logger
  
  defmodule State do
    defstruct history: [], operation_count: 0
  end

  @impl true
  def init(_args) do
    Logger.info("Calculator service starting...")
    {:ok, %State{}}
  end

  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [
      %{
        "name" => "add",
        "description" => "Add two numbers",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "a" => %{"type" => "number", "description" => "First number"},
            "b" => %{"type" => "number", "description" => "Second number"}
          },
          "required" => ["a", "b"]
        }
      },
      %{
        "name" => "multiply",
        "description" => "Multiply two numbers",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "a" => %{"type" => "number", "description" => "First number"},
            "b" => %{"type" => "number", "description" => "Second number"}
          },
          "required" => ["a", "b"]
        }
      },
      %{
        "name" => "divide",
        "description" => "Divide two numbers",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "a" => %{"type" => "number", "description" => "Dividend"},
            "b" => %{"type" => "number", "description" => "Divisor (cannot be zero)"}
          },
          "required" => ["a", "b"]
        }
      },
      %{
        "name" => "factorial",
        "description" => "Calculate factorial of a number (with progress)",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "n" => %{"type" => "integer", "description" => "Non-negative integer"}
          },
          "required" => ["n"]
        }
      },
      %{
        "name" => "history",
        "description" => "Get calculation history",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "limit" => %{"type" => "integer", "description" => "Number of recent operations to return"}
          }
        }
      }
    ]
    
    {:ok, %{"tools" => tools}, state}
  end
  
  @impl true
  def handle_mcp_request("tools/call", %{"name" => "add", "arguments" => %{"a" => a, "b" => b}}, state) do
    result = a + b
    new_state = add_to_history(state, "add", {a, b}, result)
    
    content = [%{
      "type" => "text",
      "text" => "#{a} + #{b} = #{result}"
    }]
    
    {:ok, %{"content" => content}, new_state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "multiply", "arguments" => %{"a" => a, "b" => b}}, state) do
    result = a * b
    new_state = add_to_history(state, "multiply", {a, b}, result)
    
    content = [%{
      "type" => "text",
      "text" => "#{a} × #{b} = #{result}"
    }]
    
    {:ok, %{"content" => content}, new_state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "divide", "arguments" => %{"a" => _a, "b" => 0}}, state) do
    {:error, %{"code" => -32001, "message" => "Division by zero"}, state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "divide", "arguments" => %{"a" => a, "b" => b}}, state) do
    result = a / b
    new_state = add_to_history(state, "divide", {a, b}, result)
    
    content = [%{
      "type" => "text",
      "text" => "#{a} ÷ #{b} = #{result}"
    }]
    
    {:ok, %{"content" => content}, new_state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "factorial", "arguments" => %{"n" => n}}, state) when n < 0 do
    {:error, %{"code" => -32001, "message" => "Factorial is not defined for negative numbers"}, state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "factorial", "arguments" => %{"n" => n}} = params, state) do
    # Use progress token if provided
    progress_token = get_in(params, ["meta", "progressToken"])
    
    # Calculate factorial with progress updates
    result = factorial_with_progress(n, progress_token)
    new_state = add_to_history(state, "factorial", n, result)
    
    content = [%{
      "type" => "text",
      "text" => "#{n}! = #{result}"
    }]
    
    {:ok, %{"content" => content}, new_state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "history", "arguments" => params}, state) do
    limit = Map.get(params, "limit", 10)
    
    history = state.history
    |> Enum.take(limit)
    |> Enum.map(fn {op, args, result, timestamp} ->
      "#{format_timestamp(timestamp)}: #{format_operation(op, args)} = #{result}"
    end)
    |> Enum.join("\n")
    
    content = [%{
      "type" => "text",
      "text" => if(history == "", do: "No calculations yet", else: history)
    }]
    
    {:ok, %{"content" => content}, state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => tool}, state) do
    {:error, %{"code" => -32601, "message" => "Unknown tool: #{tool}"}, state}
  end

  def handle_mcp_request("list_resources", _params, state) do
    {:ok, %{"resources" => []}, state}
  end

  def handle_mcp_request("list_prompts", _params, state) do
    {:ok, %{"prompts" => []}, state}
  end

  def handle_mcp_request(method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
  end
  
  # Optional: Handle notifications (fire-and-forget)
  @impl true
  def handle_mcp_notification("reset_history", _params, state) do
    Logger.info("Resetting calculation history")
    {:noreply, %{state | history: [], operation_count: 0}}
  end
  
  # Helper functions
  
  defp add_to_history(state, operation, args, result) do
    entry = {operation, args, result, DateTime.utc_now()}
    %{state | 
      history: [entry | state.history],
      operation_count: state.operation_count + 1
    }
  end
  
  defp factorial_with_progress(n, progress_token) do
    # For simplicity in Native service, we calculate directly
    # In a real implementation, you could send progress notifications
    # to subscribed clients via ExMCP.Native.notify
    if progress_token do
      Logger.info("Calculating factorial with progress token: #{progress_token}")
    end
    
    factorial(n)
  end
  
  defp factorial(0), do: 1
  defp factorial(n) when n > 0, do: n * factorial(n - 1)
  
  defp format_timestamp(datetime) do
    Calendar.strftime(datetime, "%H:%M:%S")
  end
  
  defp format_operation("add", {a, b}), do: "#{a} + #{b}"
  defp format_operation("multiply", {a, b}), do: "#{a} × #{b}"
  defp format_operation("divide", {a, b}), do: "#{a} ÷ #{b}"
  defp format_operation("factorial", n), do: "#{n}!"
end

# Example usage:
if __FILE__ == Path.absname("#{__DIR__}/calculator_server.ex") do
  # Start the service
  {:ok, _} = Examples.Native.CalculatorService.start_link()
  
  # Wait for registration
  Process.sleep(100)
  
  # Test some operations
  IO.puts("Calculator Service Demo")
  IO.puts("=" |> String.duplicate(30))
  
  # List tools
  {:ok, %{"tools" => tools}} = ExMCP.Native.call(:calculator_service, "list_tools", %{})
  IO.puts("\nAvailable tools: #{length(tools)}")
  Enum.each(tools, fn tool ->
    IO.puts("  - #{tool["name"]}: #{tool["description"]}")
  end)
  
  # Test calculations
  operations = [
    {"add", %{"a" => 5, "b" => 3}},
    {"multiply", %{"a" => 4, "b" => 7}},
    {"divide", %{"a" => 10, "b" => 2}},
    {"factorial", %{"n" => 5}}
  ]
  
  IO.puts("\nPerforming calculations:")
  Enum.each(operations, fn {tool, args} ->
    case ExMCP.Native.call(:calculator_service, "tools/call", %{"name" => tool, "arguments" => args}) do
      {:ok, %{"content" => [%{"text" => text}]}} ->
        IO.puts("  #{text}")
      {:error, %{"message" => error}} ->
        IO.puts("  Error: #{error}")
    end
  end)
  
  # Show history
  {:ok, %{"content" => [%{"text" => history}]}} = 
    ExMCP.Native.call(:calculator_service, "tools/call", %{"name" => "history", "arguments" => %{}})
  
  IO.puts("\nCalculation history:")
  IO.puts(history)
  
  IO.puts("\nService is running. Press Ctrl+C to stop.")
  Process.sleep(:infinity)
end