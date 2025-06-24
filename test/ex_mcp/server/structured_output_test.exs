defmodule ExMCP.Server.StructuredOutputTest do
  @moduledoc """
  Tests for MCP 2025-06-18 structured tool output feature.
  """
  use ExUnit.Case, async: true

  alias ExMCP.Server.Tools

  defmodule TestServer do
    use ExMCP.Server.Handler
    use ExMCP.Server.Tools

    tool "calculate" do
      description("Perform mathematical calculations with structured output")

      input_schema(%{
        type: "object",
        properties: %{
          expression: %{type: "string"}
        },
        required: ["expression"]
      })

      output_schema(%{
        type: "object",
        properties: %{
          result: %{type: "number"},
          expression: %{type: "string"}
        },
        required: ["result"]
      })

      handle(fn %{expression: expr}, state ->
        case eval_expression(expr) do
          {:ok, result} ->
            {:ok,
             %{
               content: [%{type: "text", text: "Result: #{result}"}],
               structuredOutput: %{result: result, expression: expr}
             }, state}

          {:error, reason} ->
            {:error, "Calculation failed: #{reason}"}
        end
      end)
    end

    tool "echo" do
      description("Echo input without output schema")

      input_schema(%{
        type: "object",
        properties: %{
          message: %{type: "string"}
        },
        required: ["message"]
      })

      handle(fn %{message: msg}, state ->
        {:ok, %{content: [%{type: "text", text: msg}]}, state}
      end)
    end

    tool "structured_only" do
      description("Returns only structured output")

      input_schema(%{
        type: "object",
        properties: %{
          data: %{type: "string"}
        }
      })

      output_schema(%{
        type: "object",
        properties: %{
          processed: %{type: "string"},
          timestamp: %{type: "integer"}
        },
        required: ["processed"]
      })

      handle(fn %{data: data}, state ->
        {:ok,
         %{
           structuredOutput: %{
             processed: String.upcase(data),
             timestamp: System.system_time(:second)
           }
         }, state}
      end)
    end

    tool "invalid_output" do
      description("Tool that returns invalid structured output")

      input_schema(%{
        type: "object",
        properties: %{
          value: %{type: "string"}
        }
      })

      output_schema(%{
        type: "object",
        properties: %{
          result: %{type: "number"}
        },
        required: ["result"]
      })

      handle(fn _args, state ->
        {:ok,
         %{
           content: [%{type: "text", text: "Invalid output"}],
           # Invalid per schema
           structuredOutput: %{result: "not a number"}
         }, state}
      end)
    end

    tool "legacy_structured_content" do
      description("Tool using legacy structuredContent field")

      input_schema(%{
        type: "object",
        properties: %{
          input: %{type: "string"}
        }
      })

      handle(fn %{input: input}, state ->
        {:ok,
         %{
           content: [%{type: "text", text: "Processing..."}],
           # Legacy field
           structuredContent: %{output: input}
         }, state}
      end)
    end

    defp eval_expression("2+2"), do: {:ok, 4}
    defp eval_expression("10*5"), do: {:ok, 50}
    defp eval_expression("invalid"), do: {:error, "invalid expression"}
    defp eval_expression(_), do: {:ok, 42}
  end

  describe "structured output with validation" do
    test "validates structured output against schema successfully" do
      state = %{}
      params = %{name: "calculate", arguments: %{expression: "2+2"}}

      assert {:ok, response, ^state} = TestServer.handle_call_tool(params, state)

      assert %{
               content: [%{type: "text", text: "Result: 4"}],
               structuredOutput: %{result: 4, expression: "2+2"}
             } = response

      refute Map.has_key?(response, :isError)
    end

    test "returns validation error for invalid structured output" do
      state = %{}
      params = %{name: "invalid_output", arguments: %{value: "test"}}

      assert {:ok, response, ^state} = TestServer.handle_call_tool(params, state)

      # Should get validation error now that schema conversion is fixed
      assert %{
               content: [%{type: "text", text: text}],
               isError: true
             } = response

      assert String.contains?(text, "Output validation failed")
    end

    test "handles tool without output schema normally" do
      state = %{}
      params = %{name: "echo", arguments: %{message: "hello"}}

      assert {:ok, response, ^state} = TestServer.handle_call_tool(params, state)

      assert %{
               content: [%{type: "text", text: "hello"}]
             } = response

      refute Map.has_key?(response, :structuredOutput)
      refute Map.has_key?(response, :isError)
    end

    test "adds empty content array when only structured output provided" do
      state = %{}
      params = %{name: "structured_only", arguments: %{data: "test"}}

      assert {:ok, response, ^state} = TestServer.handle_call_tool(params, state)

      assert %{
               content: [],
               structuredOutput: %{processed: "TEST", timestamp: _}
             } = response
    end

    test "maps legacy structuredContent to structuredOutput" do
      state = %{}
      params = %{name: "legacy_structured_content", arguments: %{input: "test"}}

      assert {:ok, response, ^state} = TestServer.handle_call_tool(params, state)

      assert %{
               content: [%{type: "text", text: "Processing..."}],
               structuredOutput: %{output: "test"}
             } = response

      refute Map.has_key?(response, :structuredContent)
    end
  end

  describe "response normalization integration" do
    test "structured output response includes both content and structuredOutput" do
      state = %{}

      # Test through the actual tool call to verify normalization
      params = %{name: "calculate", arguments: %{expression: "10*5"}}
      assert {:ok, response, ^state} = TestServer.handle_call_tool(params, state)

      # Should have both content and structuredOutput
      assert Map.has_key?(response, :content)
      assert Map.has_key?(response, :structuredOutput)
      assert response.content == [%{type: "text", text: "Result: 50"}]
      assert response.structuredOutput == %{result: 50, expression: "10*5"}
    end

    test "legacy structuredContent is mapped correctly" do
      state = %{}

      params = %{name: "legacy_structured_content", arguments: %{input: "test"}}
      assert {:ok, response, ^state} = TestServer.handle_call_tool(params, state)

      # Should have structuredOutput, not structuredContent
      assert Map.has_key?(response, :structuredOutput)
      refute Map.has_key?(response, :structuredContent)
      assert response.structuredOutput == %{output: "test"}
    end
  end

  describe "list tools includes output schema" do
    test "output schema is included in tool definitions" do
      state = %{}

      assert {:ok, tools, ^state} = TestServer.handle_list_tools(%{}, state)

      calculate_tool = Enum.find(tools, &(&1.name == "calculate"))

      assert calculate_tool.outputSchema == %{
               type: "object",
               properties: %{
                 result: %{type: "number"},
                 expression: %{type: "string"}
               },
               required: ["result"]
             }

      echo_tool = Enum.find(tools, &(&1.name == "echo"))
      refute Map.has_key?(echo_tool, :outputSchema)
    end
  end
end
