defmodule Examples.NativeBeam.CalculatorServer do
  @moduledoc """
  Example MCP service using Native BEAM transport that provides calculator tools.
  
  This demonstrates:
  - Service registration with Native BEAM transport
  - Basic tool implementation
  - Error handling
  - Server state management
  - Progress notifications for long operations
  """
  
  use ExMCP.Server.Handler
  require Logger
  
  defmodule State do
    defstruct history: [], operation_count: 0
  end
  
  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    Logger.info("Calculator server starting...")
    # Register with the Native BEAM transport
    :ok = ExMCP.Transport.Native.register_service(:calculator_server)
    Logger.info("Calculator service registered as :calculator_server")
    {:ok, %State{}}
  end
  
  # Handle MCP requests through Native BEAM transport
  def handle_call({:mcp_request, %{"method" => "initialize", "params" => _params}}, _from, state) do
    server_info = %{
      "serverInfo" => %{
        "name" => "calculator-server",
        "version" => "1.0.0",
        "capabilities" => %{
          "tools" => %{},
          "resources" => %{},
          "prompts" => %{}
        }
      }
    }
    
    {:reply, {:ok, server_info}, state}
  end

  def handle_call({:mcp_request, %{"method" => "tools/list"}}, _from, state) do
    {:ok, tools, new_state} = handle_list_tools(state)
    {:reply, {:ok, %{"tools" => tools}}, new_state}
  end

  def handle_call({:mcp_request, %{"method" => "tools/call", "params" => %{"name" => tool_name, "arguments" => arguments}}}, _from, state) do
    case handle_call_tool(tool_name, arguments, state) do
      {:ok, content, new_state} ->
        {:reply, {:ok, %{"content" => content}}, new_state}
      {:error, message, new_state} ->
        {:reply, {:error, %{"code" => -32001, "message" => message}}, new_state}
    end
  end

  # Legacy MCP handler methods for compatibility
  @impl true
  def handle_initialize(_params, state) do
    server_info = %{
      "name" => "calculator-server",
      "version" => "1.0.0",
      "capabilities" => %{
        "tools" => %{},
        "resources" => %{},
        "prompts" => %{}
      }
    }
    
    {:ok, server_info, state}
  end
  
  @impl true
  def handle_list_tools(state) do
    tools = [
      %{
        name: "add",
        description: "Add two numbers",
        inputSchema: %{
          type: "object",
          properties: %{
            a: %{type: "number", description: "First number"},
            b: %{type: "number", description: "Second number"}
          },
          required: ["a", "b"]
        }
      },
      %{
        name: "multiply",
        description: "Multiply two numbers",
        inputSchema: %{
          type: "object",
          properties: %{
            a: %{type: "number", description: "First number"},
            b: %{type: "number", description: "Second number"}
          },
          required: ["a", "b"]
        }
      },
      %{
        name: "divide",
        description: "Divide two numbers",
        inputSchema: %{
          type: "object",
          properties: %{
            a: %{type: "number", description: "Dividend"},
            b: %{type: "number", description: "Divisor (cannot be zero)"}
          },
          required: ["a", "b"]
        }
      },
      %{
        name: "factorial",
        description: "Calculate factorial of a number (with progress)",
        inputSchema: %{
          type: "object",
          properties: %{
            n: %{type: "integer", description: "Non-negative integer"}
          },
          required: ["n"]
        }
      },
      %{
        name: "history",
        description: "Get calculation history",
        inputSchema: %{
          type: "object",
          properties: %{
            limit: %{type: "integer", description: "Number of recent operations to return"}
          }
        }
      }
    ]
    
    {:ok, tools, state}
  end
  
  @impl true
  def handle_call_tool("add", %{"a" => a, "b" => b}, state) do
    result = a + b
    new_state = add_to_history(state, "add", {a, b}, result)
    
    content = [%{
      type: "text",
      text: "#{a} + #{b} = #{result}"
    }]
    
    {:ok, content, new_state}
  end
  
  def handle_call_tool("multiply", %{"a" => a, "b" => b}, state) do
    result = a * b
    new_state = add_to_history(state, "multiply", {a, b}, result)
    
    content = [%{
      type: "text",
      text: "#{a} × #{b} = #{result}"
    }]
    
    {:ok, content, new_state}
  end
  
  def handle_call_tool("divide", %{"a" => a, "b" => 0}, state) do
    {:error, "Division by zero", state}
  end
  
  def handle_call_tool("divide", %{"a" => a, "b" => b}, state) do
    result = a / b
    new_state = add_to_history(state, "divide", {a, b}, result)
    
    content = [%{
      type: "text",
      text: "#{a} ÷ #{b} = #{result}"
    }]
    
    {:ok, content, new_state}
  end
  
  def handle_call_tool("factorial", %{"n" => n}, state) when n < 0 do
    {:error, "Factorial is not defined for negative numbers", state}
  end
  
  def handle_call_tool("factorial", %{"n" => n} = params, state) do
    # Use progress token if provided
    progress_token = get_in(params, ["_meta", "progressToken"])
    
    # Calculate factorial with progress updates
    result = factorial_with_progress(n, progress_token, self())
    new_state = add_to_history(state, "factorial", n, result)
    
    content = [%{
      type: "text",
      text: "#{n}! = #{result}"
    }]
    
    {:ok, content, new_state}
  end
  
  def handle_call_tool("history", params, state) do
    limit = Map.get(params, "limit", 10)
    
    history = state.history
    |> Enum.take(limit)
    |> Enum.map(fn {op, args, result, timestamp} ->
      "#{format_timestamp(timestamp)}: #{format_operation(op, args)} = #{result}"
    end)
    |> Enum.join("\n")
    
    content = [%{
      type: "text",
      text: if(history == "", do: "No calculations yet", else: history)
    }]
    
    {:ok, content, state}
  end
  
  def handle_call_tool(tool, _params, state) do
    {:error, "Unknown tool: #{tool}", state}
  end
  
  # Helper functions
  
  defp add_to_history(state, operation, args, result) do
    entry = {operation, args, result, DateTime.utc_now()}
    %{state | 
      history: [entry | state.history],
      operation_count: state.operation_count + 1
    }
  end
  
  defp factorial_with_progress(n, progress_token, server_pid) do
    # Simulate a long operation with progress updates
    factorial_with_progress(n, 1, 1, n, progress_token, server_pid)
  end
  
  defp factorial_with_progress(0, _current, acc, _total, _token, _server), do: acc
  
  defp factorial_with_progress(n, current, acc, total, token, server) when token != nil do
    # Send progress update
    if rem(current, max(1, div(total, 10))) == 0 do
      ExMCP.Server.notify_progress(server, token, current, total)
    end
    
    # Simulate some work
    Process.sleep(10)
    
    factorial_with_progress(n - 1, current + 1, acc * n, total, token, server)
  end
  
  defp factorial_with_progress(n, current, acc, total, nil, server) do
    # No progress token, just calculate
    factorial_with_progress(n - 1, current + 1, acc * n, total, nil, server)
  end
  
  defp format_timestamp(datetime) do
    Calendar.strftime(datetime, "%H:%M:%S")
  end
  
  defp format_operation("add", {a, b}), do: "#{a} + #{b}"
  defp format_operation("multiply", {a, b}), do: "#{a} × #{b}"
  defp format_operation("divide", {a, b}), do: "#{a} ÷ #{b}"
  defp format_operation("factorial", n), do: "#{n}!"
end