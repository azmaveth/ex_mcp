defmodule ExMCP.ToolsTestMigrated do
  use ExUnit.Case, async: true

  alias ExMCP.{Client, Server}

  # DSL-based server replacing TestToolsHandler
  defmodule TestToolsServer do
    use ExMCP.Server

    # Tool definitions using DSL
    deftool "calculate" do
      meta do
        description("Perform mathematical calculations")
      end

      input_schema(%{
        type: "object",
        properties: %{
          operation: %{
            type: "string",
            enum: ["add", "subtract", "multiply", "divide"]
          },
          a: %{type: "number"},
          b: %{type: "number"}
        },
        required: ["operation", "a", "b"]
      })
    end

    deftool "string_tools" do
      meta do
        description("String manipulation utilities")
      end

      input_schema(%{
        type: "object",
        properties: %{
          operation: %{
            type: "string",
            enum: ["uppercase", "lowercase", "reverse", "length"]
          },
          text: %{type: "string"}
        },
        required: ["operation", "text"]
      })
    end

    deftool "data_fetch" do
      meta do
        description("Fetch data from various sources")
      end

      input_schema(%{
        type: "object",
        properties: %{
          source: %{type: "string"},
          query: %{type: "string"},
          format: %{
            type: "string",
            enum: ["json", "csv", "xml"],
            default: "json"
          }
        },
        required: ["source", "query"]
      })
    end

    deftool "image_process" do
      meta do
        description("Process images")
      end

      input_schema(%{
        type: "object",
        properties: %{
          operation: %{type: "string"},
          imageData: %{type: "string", format: "base64"}
        },
        required: ["operation", "imageData"]
      })
    end

    deftool "async_task" do
      meta do
        description("Long-running task with progress")
      end

      input_schema(%{
        type: "object",
        properties: %{
          duration: %{type: "integer", minimum: 1, maximum: 10},
          steps: %{type: "integer", minimum: 1, maximum: 5}
        },
        required: ["duration", "steps"]
      })
    end

    # Custom initialization to maintain state compatibility
    @impl true
    def init(args) do
      initial_state = %{
        call_count: 0,
        last_progress_token: nil
      }

      {:ok, Map.merge(Map.new(args), initial_state)}
    end

    # Tool call implementations
    @impl true
    def handle_tool_call("calculate", arguments, state) do
      # Extract progress token if provided
      progress_token = get_in(arguments, ["_meta", "progressToken"])
      new_state = %{state | call_count: state.call_count + 1, last_progress_token: progress_token}

      handle_calculate(arguments, new_state)
    end

    def handle_tool_call("string_tools", arguments, state) do
      progress_token = get_in(arguments, ["_meta", "progressToken"])
      new_state = %{state | call_count: state.call_count + 1, last_progress_token: progress_token}

      handle_string_tools(arguments, new_state)
    end

    def handle_tool_call("data_fetch", arguments, state) do
      progress_token = get_in(arguments, ["_meta", "progressToken"])
      new_state = %{state | call_count: state.call_count + 1, last_progress_token: progress_token}

      handle_data_fetch(arguments, new_state)
    end

    def handle_tool_call("image_process", arguments, state) do
      progress_token = get_in(arguments, ["_meta", "progressToken"])
      new_state = %{state | call_count: state.call_count + 1, last_progress_token: progress_token}

      handle_image_process(arguments, new_state)
    end

    def handle_tool_call("async_task", arguments, state) do
      progress_token = get_in(arguments, ["_meta", "progressToken"])
      new_state = %{state | call_count: state.call_count + 1, last_progress_token: progress_token}

      handle_async_task(arguments, progress_token, new_state)
    end

    def handle_tool_call(name, _arguments, state) do
      {:error, "Unknown tool: #{name}", state}
    end

    # Implementation functions (copied from original handler)
    defp handle_calculate(%{"operation" => op, "a" => a, "b" => b}, state) do
      case op do
        "add" ->
          {:ok, %{content: [%{type: "text", text: "Result: #{a + b}"}]}, state}

        "subtract" ->
          {:ok, %{content: [%{type: "text", text: "Result: #{a - b}"}]}, state}

        "multiply" ->
          {:ok, %{content: [%{type: "text", text: "Result: #{a * b}"}]}, state}

        "divide" when b != 0 ->
          {:ok, %{content: [%{type: "text", text: "Result: #{a / b}"}]}, state}

        "divide" ->
          # Return error result with isError flag
          {:ok,
           %{
             content: [
               %{
                 type: "text",
                 text: "Error: Division by zero"
               }
             ],
             is_error: true
           }, state}
      end
    end

    defp handle_string_tools(%{"operation" => op, "text" => text}, state) do
      result =
        case op do
          "uppercase" -> String.upcase(text)
          "lowercase" -> String.downcase(text)
          "reverse" -> String.reverse(text)
          "length" -> "Length: #{String.length(text)}"
        end

      {:ok,
       %{
         content: [
           %{
             type: "text",
             text: result
           }
         ]
       }, state}
    end

    defp handle_data_fetch(%{"source" => source, "query" => query} = args, state) do
      format = Map.get(args, "format", "json")

      # Simulate data fetching
      data = %{
        source: source,
        query: query,
        results: [
          %{id: 1, name: "Item 1", value: 100},
          %{id: 2, name: "Item 2", value: 200}
        ]
      }

      formatted =
        case format do
          "json" -> Jason.encode!(data, pretty: true)
          "csv" -> "id,name,value\n1,Item 1,100\n2,Item 2,200"
          "xml" -> "<data><item><id>1</id><name>Item 1</name></item></data>"
        end

      {:ok,
       %{
         content: [
           %{
             type: "text",
             text: formatted
           }
         ]
       }, state}
    end

    defp handle_image_process(%{"operation" => _op, "imageData" => data}, state) do
      # Simulate image processing - return a modified base64 string
      processed = "processed_" <> String.slice(data, 0..10) <> "..."

      {:ok,
       %{
         content: [
           %{
             type: "image",
             data: processed,
             mime_type: "image/png"
           }
         ]
       }, state}
    end

    defp handle_async_task(%{"duration" => duration, "steps" => steps}, progress_token, state) do
      # Simulate async work with progress updates
      if progress_token do
        # Send progress updates
        Task.start(fn ->
          for i <- 1..steps do
            Process.sleep(div(duration * 1000, steps))
            progress = div(i * 100, steps)
            Server.notify_progress(self(), progress_token, progress, 100)
          end
        end)
      end

      {:ok,
       %{
         content: [
           %{
             type: "text",
             text: "Task started with #{steps} steps over #{duration} seconds"
           }
         ]
       }, state}
    end

    # Override the default tool list to handle pagination like the original
    def handle_tool_list(state) do
      tools = [
        %{
          name: "calculate",
          description: "Perform mathematical calculations",
          inputSchema: %{
            type: "object",
            properties: %{
              operation: %{
                type: "string",
                enum: ["add", "subtract", "multiply", "divide"]
              },
              a: %{type: "number"},
              b: %{type: "number"}
            },
            required: ["operation", "a", "b"]
          }
        },
        %{
          name: "string_tools",
          description: "String manipulation utilities",
          inputSchema: %{
            type: "object",
            properties: %{
              operation: %{
                type: "string",
                enum: ["uppercase", "lowercase", "reverse", "length"]
              },
              text: %{type: "string"}
            },
            required: ["operation", "text"]
          }
        },
        %{
          name: "data_fetch",
          description: "Fetch data from various sources",
          inputSchema: %{
            type: "object",
            properties: %{
              source: %{type: "string"},
              query: %{type: "string"},
              format: %{
                type: "string",
                enum: ["json", "csv", "xml"],
                default: "json"
              }
            },
            required: ["source", "query"]
          }
        },
        %{
          name: "image_process",
          description: "Process images",
          inputSchema: %{
            type: "object",
            properties: %{
              operation: %{type: "string"},
              imageData: %{type: "string", format: "base64"}
            },
            required: ["operation", "imageData"]
          }
        },
        %{
          name: "async_task",
          description: "Long-running task with progress",
          inputSchema: %{
            type: "object",
            properties: %{
              duration: %{type: "integer", minimum: 1, maximum: 10},
              steps: %{type: "integer", minimum: 1, maximum: 5}
            },
            required: ["duration", "steps"]
          }
        }
      ]

      {:ok, tools, state}
    end
  end

  setup do
    # Start DSL server directly (no more handler pattern)
    {:ok, server} = TestToolsServer.start_link(transport: :native)

    on_exit(fn ->
      if Process.alive?(server), do: GenServer.stop(server)
    end)

    %{server: server}
  end

  describe "migrated tools functionality" do
    test "DSL server exposes tools correctly" do
      tools = TestToolsServer.get_tools()
      assert Map.has_key?(tools, "calculate")
      assert Map.has_key?(tools, "string_tools")
      assert Map.has_key?(tools, "data_fetch")
      assert Map.has_key?(tools, "image_process")
      assert Map.has_key?(tools, "async_task")
    end

    test "DSL server has correct capabilities" do
      capabilities = TestToolsServer.get_capabilities()
      assert Map.has_key?(capabilities, "tools")
      assert capabilities["tools"]["listChanged"] == true
    end

    test "calculate tool works", %{server: server} do
      state = :sys.get_state(server)

      {:ok, result, _new_state} =
        TestToolsServer.handle_tool_call(
          "calculate",
          %{"operation" => "add", "a" => 5, "b" => 3},
          state
        )

      assert %{content: [content]} = result
      assert content[:type] == "text"
      assert content[:text] == "Result: 8"
    end

    test "string_tools work", %{server: server} do
      state = :sys.get_state(server)

      {:ok, result, _new_state} =
        TestToolsServer.handle_tool_call(
          "string_tools",
          %{"operation" => "uppercase", "text" => "hello"},
          state
        )

      assert %{content: [content]} = result
      assert content[:type] == "text"
      assert content[:text] == "HELLO"
    end

    test "unknown tool returns error", %{server: server} do
      state = :sys.get_state(server)

      {:error, message, _state} = TestToolsServer.handle_tool_call("unknown", %{}, state)
      assert message == "Unknown tool: unknown"
    end

    test "state tracking works", %{server: server} do
      initial_state = :sys.get_state(server)
      assert initial_state.call_count == 0

      TestToolsServer.handle_tool_call(
        "calculate",
        %{"operation" => "add", "a" => 1, "b" => 1},
        initial_state
      )

      # Verify state would be updated
      {:ok, _result, new_state} =
        TestToolsServer.handle_tool_call(
          "calculate",
          %{"operation" => "add", "a" => 1, "b" => 1},
          initial_state
        )

      assert new_state.call_count == 1
    end
  end
end
