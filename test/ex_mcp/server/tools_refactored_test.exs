defmodule ExMCP.Server.ToolsRefactoredTest do
  use ExUnit.Case

  # Test server module using the refactored tools
  defmodule TestServer do
    use ExMCP.Server.Handler
    use ExMCP.Server.ToolsRefactored

    tool "echo", "Echo back the input" do
      param(:message, :string, required: true)
      param(:uppercase, :boolean, default: false)

      handle(fn %{message: msg} = args, state ->
        result =
          if Map.get(args, :uppercase, false) do
            String.upcase(msg)
          else
            msg
          end

        {:ok, %{text: result}, state}
      end)
    end

    tool "calculator" do
      description("Perform basic calculations")
      title("Calculator Tool")

      input_schema(%{
        type: "object",
        properties: %{
          operation: %{type: "string", enum: ["add", "subtract", "multiply", "divide"]},
          a: %{type: "number"},
          b: %{type: "number"}
        },
        required: ["operation", "a", "b"]
      })

      output_schema(%{
        type: "object",
        properties: %{
          result: %{type: "number"}
        }
      })

      annotations(%{
        readOnlyHint: true,
        category: "math"
      })

      handle(fn %{operation: op, a: a, b: b}, state ->
        result =
          case op do
            "add" -> a + b
            "subtract" -> a - b
            "multiply" -> a * b
            "divide" when b != 0 -> a / b
            "divide" -> {:error, "Division by zero"}
            _ -> {:error, "Unknown operation"}
          end

        case result do
          {:error, reason} -> {:error, reason}
          value -> {:ok, %{result: value}, state}
        end
      end)
    end

    tool "stateful_counter" do
      description("A counter that maintains state")

      param(:action, :string, enum: ["increment", "decrement", "reset", "get"])
      param(:amount, :integer, default: 1)

      handle(fn %{action: action} = args, state ->
        current_count = Map.get(state, :counter, 0)
        amount = Map.get(args, :amount, 1)

        case action do
          "increment" ->
            new_count = current_count + amount
            new_state = Map.put(state, :counter, new_count)
            {:ok, %{count: new_count, action: "incremented"}, new_state}

          "decrement" ->
            new_count = current_count - amount
            new_state = Map.put(state, :counter, new_count)
            {:ok, %{count: new_count, action: "decremented"}, new_state}

          "reset" ->
            new_state = Map.put(state, :counter, 0)
            {:ok, %{count: 0, action: "reset"}, new_state}

          "get" ->
            {:ok, %{count: current_count, action: "retrieved"}, state}
        end
      end)
    end

    # Handler returning response without new state
    tool "simple_response" do
      description("Returns response without state management")

      handle(fn _args, _state ->
        {:ok, %{message: "simple response"}}
      end)
    end
  end

  setup do
    # Start the test server's tool registry
    {:ok, _} = TestServer.__tool_registry__()
    TestServer.__init_tools__()
    :ok
  end

  describe "tool registration and listing" do
    test "lists all registered tools" do
      {:ok, tools, _state} = TestServer.handle_list_tools(%{}, %{})

      tool_names = Enum.map(tools, & &1.name)
      assert "echo" in tool_names
      assert "calculator" in tool_names
      assert "stateful_counter" in tool_names
      assert "simple_response" in tool_names
      assert length(tools) == 4
    end

    test "tool definitions include correct metadata" do
      {:ok, tools, _state} = TestServer.handle_list_tools(%{}, %{})

      echo_tool = Enum.find(tools, &(&1.name == "echo"))
      assert echo_tool.description == "Echo back the input"

      calc_tool = Enum.find(tools, &(&1.name == "calculator"))
      assert calc_tool.description == "Perform basic calculations"
      assert calc_tool.title == "Calculator Tool"
      assert calc_tool.annotations.readOnlyHint == true
      assert calc_tool.annotations.category == "math"
    end

    test "tool input schemas are properly generated" do
      {:ok, tools, _state} = TestServer.handle_list_tools(%{}, %{})

      echo_tool = Enum.find(tools, &(&1.name == "echo"))
      assert echo_tool.inputSchema.type == "object"
      assert echo_tool.inputSchema.required == ["message"]

      calc_tool = Enum.find(tools, &(&1.name == "calculator"))

      assert calc_tool.inputSchema.properties.operation.enum == [
               "add",
               "subtract",
               "multiply",
               "divide"
             ]

      assert calc_tool.inputSchema.required == ["operation", "a", "b"]
    end
  end

  describe "tool execution" do
    test "executes echo tool successfully" do
      request = %{name: "echo", arguments: %{message: "Hello World"}}
      {:ok, response, _state} = TestServer.handle_call_tool(request, %{})

      assert response.content == [%{type: "text", text: "Hello World"}]
    end

    test "echo tool with uppercase option" do
      request = %{name: "echo", arguments: %{message: "hello", uppercase: true}}
      {:ok, response, _state} = TestServer.handle_call_tool(request, %{})

      assert response.content == [%{type: "text", text: "HELLO"}]
    end

    test "executes calculator tool successfully" do
      request = %{name: "calculator", arguments: %{operation: "add", a: 5, b: 3}}
      {:ok, response, _state} = TestServer.handle_call_tool(request, %{})

      # Should be normalized by ResponseNormalizer
      assert response.content == [%{type: "text", text: inspect(%{result: 8})}]
    end

    test "calculator tool handles division by zero" do
      request = %{name: "calculator", arguments: %{operation: "divide", a: 10, b: 0}}
      {:ok, response, _state} = TestServer.handle_call_tool(request, %{})

      assert response.isError == true
      assert hd(response.content).text =~ "Division by zero"
    end

    test "stateful counter maintains state" do
      initial_state = %{}

      # Increment counter
      request1 = %{name: "stateful_counter", arguments: %{action: "increment", amount: 5}}
      {:ok, response1, state1} = TestServer.handle_call_tool(request1, initial_state)

      assert response1.content == [
               %{type: "text", text: inspect(%{count: 5, action: "incremented"})}
             ]

      assert state1.counter == 5

      # Increment again
      request2 = %{name: "stateful_counter", arguments: %{action: "increment", amount: 3}}
      {:ok, response2, state2} = TestServer.handle_call_tool(request2, state1)

      assert response2.content == [
               %{type: "text", text: inspect(%{count: 8, action: "incremented"})}
             ]

      assert state2.counter == 8

      # Get current count
      request3 = %{name: "stateful_counter", arguments: %{action: "get"}}
      {:ok, response3, state3} = TestServer.handle_call_tool(request3, state2)

      assert response3.content == [
               %{type: "text", text: inspect(%{count: 8, action: "retrieved"})}
             ]

      # State unchanged
      assert state3.counter == 8
    end

    test "handles tool returning response without state" do
      request = %{name: "simple_response", arguments: %{}}
      {:ok, response, state} = TestServer.handle_call_tool(request, %{initial: "state"})

      assert response.content == [%{type: "text", text: inspect(%{message: "simple response"})}]
      # Original state preserved
      assert state == %{initial: "state"}
    end
  end

  describe "error handling" do
    test "handles unknown tool" do
      request = %{name: "nonexistent", arguments: %{}}
      {:ok, response, _state} = TestServer.handle_call_tool(request, %{})

      assert response.isError == true
      assert hd(response.content).text =~ "Tool 'nonexistent' not found"
    end

    test "handles tool execution errors" do
      request = %{name: "calculator", arguments: %{operation: "unknown", a: 1, b: 2}}
      {:ok, response, _state} = TestServer.handle_call_tool(request, %{})

      assert response.isError == true
      assert hd(response.content).text =~ "Unknown operation"
    end
  end

  describe "response normalization" do
    test "normalizes string responses" do
      # This would be tested if we had a tool returning a raw string
      # The ResponseNormalizer handles this conversion
      normalized = ExMCP.Server.Tools.ResponseNormalizer.normalize("Hello")
      assert normalized == %{content: [%{type: "text", text: "Hello"}]}
    end

    test "preserves structured responses" do
      response = %{
        content: [%{type: "text", text: "Result"}],
        structuredOutput: %{result: 42}
      }

      normalized = ExMCP.Server.Tools.ResponseNormalizer.normalize(response)
      assert normalized == response
    end
  end

  describe "builder pattern integration" do
    test "can create tools programmatically" do
      alias ExMCP.Server.Tools.Builder

      {:ok, {tool_def, handler}} =
        Builder.new("test_tool")
        |> Builder.description("A test tool")
        |> Builder.param(:input, :string, required: true)
        |> Builder.handler(fn %{input: input}, state ->
          {:ok, %{output: String.upcase(input)}, state}
        end)
        |> Builder.build()

      assert tool_def.name == "test_tool"
      assert tool_def.description == "A test tool"
      assert tool_def.inputSchema.required == ["input"]

      # Test the handler
      {:ok, result, _state} = handler.(%{input: "hello"}, %{})
      assert result.output == "HELLO"
    end
  end
end
