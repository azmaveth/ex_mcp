defmodule ExMCP.Server.ToolsTest do
  use ExUnit.Case, async: true


  describe "Tool DSL" do
    test "defines simple tool with automatic schema generation" do
      defmodule SimpleToolServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.Tools

        tool "echo", "Echo back the input" do
          param :message, :string, required: true

          handle fn %{message: message}, _state ->
            {:ok, text: message}
          end
        end
      end

      # Test that the tool is properly defined
      {:ok, tools, _state} = SimpleToolServer.handle_list_tools(%{}, %{})
      assert length(tools) == 1

      [tool] = tools
      assert tool.name == "echo"
      assert tool.description == "Echo back the input"

      # Test the generated schema
      assert tool.inputSchema == %{
               type: "object",
               properties: %{
                 message: %{type: "string"}
               },
               required: ["message"]
             }

      # Test tool execution
      {:ok, result, _state} = SimpleToolServer.handle_call_tool(
        %{name: "echo", arguments: %{message: "Hello, World!"}},
        %{}
      )

      assert result.content == [%{type: "text", text: "Hello, World!"}]
    end

    test "defines tool with multiple parameters and types" do
      defmodule MultiParamServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.Tools

        tool "user_info", "Get user information" do
          param :name, :string, required: true
          param :age, :integer, required: true
          param :active, :boolean, default: true
          param :tags, {:array, :string}

          handle fn args, _state ->
            info = "User: #{args.name}, Age: #{args.age}, Active: #{args.active}"
            info = if args[:tags], do: info <> ", Tags: #{Enum.join(args.tags, ", ")}", else: info
            {:ok, text: info}
          end
        end
      end

      {:ok, tools, _} = MultiParamServer.handle_list_tools(%{}, %{})
      [tool] = tools

      assert tool.inputSchema == %{
               type: "object",
               properties: %{
                 name: %{type: "string"},
                 age: %{type: "integer"},
                 active: %{type: "boolean", default: true},
                 tags: %{type: "array", items: %{type: "string"}}
               },
               required: ["name", "age"]
             }
    end

    test "defines advanced tool with full control" do
      defmodule AdvancedToolServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.Tools

        tool "calculate" do
          description "Perform mathematical calculations"

          input_schema %{
            type: "object",
            properties: %{
              expression: %{type: "string", pattern: "^[0-9+\\-*/().\\s]+$"}
            },
            required: ["expression"]
          }

          output_schema %{
            type: "object",
            properties: %{
              result: %{type: "number"},
              expression: %{type: "string"}
            }
          }

          annotations %{
            readOnlyHint: true
          }

          handle fn %{expression: expr}, state ->
            # Simple calculator implementation for testing
            result = case expr do
              "2 + 2" -> 4
              "10 * 5" -> 50
              _ -> 0
            end

            {:ok, %{
              content: [%{type: "text", text: "Result: #{result}"}],
              structuredContent: %{result: result, expression: expr}
            }, state}
          end
        end
      end

      {:ok, tools, _} = AdvancedToolServer.handle_list_tools(%{}, %{})
      [tool] = tools

      assert tool.name == "calculate"
      assert tool.description == "Perform mathematical calculations"
      assert tool.readOnlyHint == true

      # Test execution
      {:ok, result, _} = AdvancedToolServer.handle_call_tool(
        %{name: "calculate", arguments: %{expression: "2 + 2"}},
        %{}
      )

      assert result.content == [%{type: "text", text: "Result: 4"}]
      assert result.structuredContent == %{result: 4, expression: "2 + 2"}
    end

    test "supports all annotation types" do
      defmodule AnnotatedToolServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.Tools

        tool "delete_file" do
          description "Delete a file from the system"

          param :path, :string, required: true

          annotations %{
            destructiveHint: true,
            readOnlyHint: false,
            idempotentHint: true,
            openWorldHint: false
          }

          handle fn %{path: path}, _state ->
            {:ok, text: "Deleted #{path}"}
          end
        end
      end

      {:ok, tools, _} = AnnotatedToolServer.handle_list_tools(%{}, %{})
      [tool] = tools

      assert tool.destructiveHint == true
      assert tool.readOnlyHint == false
      assert tool.idempotentHint == true
      assert tool.openWorldHint == false
    end

    test "handles tool execution errors gracefully" do
      defmodule ErrorToolServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.Tools

        tool "failing_tool", "A tool that fails" do
          param :trigger_error, :boolean, default: false

          handle fn %{trigger_error: trigger_error}, _state ->
            if trigger_error do
              {:error, "Tool execution failed"}
            else
              {:ok, text: "Success"}
            end
          end
        end
      end

      # Success case
      {:ok, result, _} = ErrorToolServer.handle_call_tool(
        %{name: "failing_tool", arguments: %{trigger_error: false}},
        %{}
      )
      assert result.content == [%{type: "text", text: "Success"}]

      # Error case
      {:ok, result, _} = ErrorToolServer.handle_call_tool(
        %{name: "failing_tool", arguments: %{trigger_error: true}},
        %{}
      )
      assert result.isError == true
      assert result.content == [%{type: "text", text: "Tool execution failed"}]
    end

    test "allows mixing DSL tools with traditional handler methods" do
      defmodule MixedServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.Tools

        # DSL tool
        tool "simple", "Simple tool" do
          param :input, :string

          handle fn %{input: input}, _state ->
            {:ok, text: input}
          end
        end

        # Traditional handler implementation
        @impl true
        def handle_list_resources(_params, state) do
          resources = [
            %{
              uri: "file:///example.txt",
              name: "Example file",
              mimeType: "text/plain"
            }
          ]
          {:ok, resources, state}
        end
      end

      # Test tools
      {:ok, tools, _} = MixedServer.handle_list_tools(%{}, %{})
      assert length(tools) == 1
      assert hd(tools).name == "simple"

      # Test resources (traditional handler)
      {:ok, resources, _} = MixedServer.handle_list_resources(%{}, %{})
      assert length(resources) == 1
      assert hd(resources).uri == "file:///example.txt"
    end

    test "validates parameter types at compile time" do
      # This should compile successfully
      defmodule ValidToolServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.Tools

        tool "valid", "Valid tool" do
          param :string_param, :string
          param :integer_param, :integer
          param :boolean_param, :boolean
          param :number_param, :number
          param :array_param, {:array, :string}
          param :object_param, :object

          handle fn _, _state -> {:ok, text: "valid"} end
        end
      end

      assert true
    end

    test "generates correct schema for nested types" do
      defmodule NestedTypeServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.Tools

        tool "nested", "Tool with nested types" do
          param :users, {:array, :object}, schema: %{
            type: "array",
            items: %{
              type: "object",
              properties: %{
                name: %{type: "string"},
                email: %{type: "string", format: "email"}
              },
              required: ["name", "email"]
            }
          }

          handle fn %{users: users}, _state ->
            names = Enum.map(users, & &1["name"]) |> Enum.join(", ")
            {:ok, text: "Users: #{names}"}
          end
        end
      end

      {:ok, tools, _} = NestedTypeServer.handle_list_tools(%{}, %{})
      [tool] = tools

      assert tool.inputSchema.properties.users == %{
               type: "array",
               items: %{
                 type: "object",
                 properties: %{
                   name: %{type: "string"},
                   email: %{type: "string", format: "email"}
                 },
                 required: ["name", "email"]
               }
             }
    end

    test "supports custom validation in handle function" do
      defmodule ValidationServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.Tools

        tool "divide", "Divide two numbers" do
          param :dividend, :number, required: true
          param :divisor, :number, required: true

          handle fn %{dividend: dividend, divisor: divisor}, _state ->
            if divisor == 0 do
              {:error, "Division by zero is not allowed"}
            else
              result = dividend / divisor
              {:ok, text: "#{dividend} / #{divisor} = #{result}"}
            end
          end
        end
      end

      # Valid division
      {:ok, result, _} = ValidationServer.handle_call_tool(
        %{name: "divide", arguments: %{dividend: 10, divisor: 2}},
        %{}
      )
      assert result.content == [%{type: "text", text: "10 / 2 = 5.0"}]

      # Division by zero
      {:ok, result, _} = ValidationServer.handle_call_tool(
        %{name: "divide", arguments: %{dividend: 10, divisor: 0}},
        %{}
      )
      assert result.isError == true
      assert result.content == [%{type: "text", text: "Division by zero is not allowed"}]
    end

    test "handles missing required parameters" do
      defmodule RequiredParamServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.Tools

        tool "greet", "Greet a user" do
          param :name, :string, required: true
          param :title, :string

          handle fn args, _state ->
            name = args[:name] || "Guest"
            greeting = if args[:title], do: "Hello, #{args.title} #{name}!", else: "Hello, #{name}!"
            {:ok, text: greeting}
          end
        end
      end

      # With all parameters
      {:ok, result, _} = RequiredParamServer.handle_call_tool(
        %{name: "greet", arguments: %{name: "Alice", title: "Dr."}},
        %{}
      )
      assert result.content == [%{type: "text", text: "Hello, Dr. Alice!"}]

      # Missing optional parameter
      {:ok, result, _} = RequiredParamServer.handle_call_tool(
        %{name: "greet", arguments: %{name: "Bob"}},
        %{}
      )
      assert result.content == [%{type: "text", text: "Hello, Bob!"}]

      # Missing required parameter - handler should handle gracefully
      {:ok, result, _} = RequiredParamServer.handle_call_tool(
        %{name: "greet", arguments: %{title: "Mr."}},
        %{}
      )
      # Without validation, the handler uses default value
      assert result.content == [%{type: "text", text: "Hello, Mr. Guest!"}]
    end

    test "supports tool composition" do
      defmodule CompositeServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.Tools

        tool "fetch_data", "Fetch data from source" do
          param :source, :string

          handle fn %{source: source}, state ->
            # Simulate fetching data
            data = %{source: source, items: ["item1", "item2", "item3"]}
            {:ok, %{
              content: [%{type: "text", text: "Fetched 3 items"}],
              structuredContent: data
            }, state}
          end
        end

        tool "process_data", "Process fetched data" do
          param :data, :object

          handle fn %{data: data}, _state ->
            count = length(data[:items] || data["items"] || [])
            source = data[:source] || data["source"] || ""
            {:ok, text: "Processed #{count} items from #{source}"}
          end
        end
      end

      # First tool execution
      {:ok, fetch_result, state} = CompositeServer.handle_call_tool(
        %{name: "fetch_data", arguments: %{source: "database"}},
        %{}
      )

      # Extract data for second tool
      data = fetch_result.structuredContent

      # Second tool execution
      {:ok, process_result, _} = CompositeServer.handle_call_tool(
        %{name: "process_data", arguments: %{data: data}},
        state
      )

      assert process_result.content == [%{type: "text", text: "Processed 3 items from database"}]
    end
  end
end