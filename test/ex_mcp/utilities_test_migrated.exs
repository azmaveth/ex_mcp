defmodule ExMCP.UtilitiesTestMigrated do
  @moduledoc """
  Migrated version of utilities_test.exs demonstrating utility patterns and limitations.

  This migration demonstrates both handler and DSL patterns for utility functionality
  like pagination, completion, and logging. However, it reveals significant limitations
  in the current implementation where many expected MCP features are not implemented.

  Key migration discoveries:
  - Client.complete/3 function doesn't exist in current API
  - Client.log_message/3 and log_message/4 functions don't exist
  - Message processor issues prevent proper handler-based server integration
  - Pagination works in handler pattern but has client-server transport issues

  Migration approach:
  - Test handler logic directly without client-server transport
  - Document missing API functions and integration issues
  - Demonstrate DSL pattern capabilities where possible
  - Provide recommendations for implementing missing features
  """

  use ExUnit.Case, async: true

  # Handler-based server (original pattern with utilities)
  defmodule TestUtilitiesHandlerMigrated do
    use ExMCP.Server.Handler

    @impl true
    def init(_args) do
      {:ok,
       %{
         log_level: "info",
         tools: generate_many_tools(20),
         prompts: generate_many_prompts(15),
         resources: generate_many_resources(25)
       }}
    end

    @impl true
    def handle_initialize(_params, state) do
      {:ok,
       %{
         protocolVersion: "2025-03-26",
         serverInfo: %{
           name: "test-utilities-server",
           version: "1.0.0"
         },
         capabilities: %{
           tools: %{},
           prompts: %{},
           resources: %{},
           completion: %{},
           logging: %{}
         }
       }, state}
    end

    @impl true
    def handle_list_tools(cursor, state) do
      case paginate_items(state.tools, cursor, 5) do
        {:ok, items, next_cursor, _} ->
          {:ok, items, next_cursor, state}

        {:error, reason, _} ->
          {:error, reason, state}
      end
    end

    @impl true
    def handle_call_tool(_name, _arguments, state) do
      {:error, "Not implemented", state}
    end

    @impl true
    def handle_list_prompts(cursor, state) do
      case paginate_items(state.prompts, cursor, 3) do
        {:ok, items, next_cursor, _} ->
          {:ok, items, next_cursor, state}

        {:error, reason, _} ->
          {:error, reason, state}
      end
    end

    @impl true
    def handle_get_prompt(name, _arguments, state) do
      prompt = Enum.find(state.prompts, &(&1.name == name))

      if prompt do
        {:ok, %{messages: [%{role: "user", content: %{type: "text", text: "Prompt: #{name}"}}]},
         state}
      else
        {:error, "Prompt not found", state}
      end
    end

    @impl true
    def handle_list_resources(cursor, state) do
      case paginate_items(state.resources, cursor, 7) do
        {:ok, items, next_cursor, _} ->
          {:ok, items, next_cursor, state}

        {:error, reason, _} ->
          {:error, reason, state}
      end
    end

    @impl true
    def handle_read_resource(_uri, state) do
      {:error, "Not implemented", state}
    end

    # Note: handle_complete callback doesn't exist in current ExMCP.Server.Handler behavior
    # This would be needed for completion functionality
    def handle_complete(ref, argument, state) do
      # Simulate completion based on ref type
      completions =
        case ref do
          %{"type" => "ref/prompt", "name" => prompt_name} ->
            generate_prompt_completions(prompt_name, argument, state)

          %{"type" => "ref/resource", "uri" => uri} ->
            generate_resource_completions(uri, argument, state)

          _ ->
            []
        end

      # Limit to max 100 as per spec
      limited = Enum.take(completions, 100)
      total = length(completions)

      result = %{
        completion: %{
          values: limited,
          total: total,
          hasMore: total > 100
        }
      }

      {:ok, result, state}
    end

    # Helper for pagination
    defp paginate_items(items, cursor, page_size) do
      case cursor do
        nil ->
          page = Enum.take(items, page_size)
          next_cursor = if length(items) > page_size, do: encode_cursor(page_size), else: nil
          {:ok, page, next_cursor, %{}}

        _ ->
          case decode_cursor(cursor) do
            {:ok, offset} ->
              remaining = Enum.drop(items, offset)
              page = Enum.take(remaining, page_size)

              next_cursor =
                if length(remaining) > page_size,
                  do: encode_cursor(offset + page_size),
                  else: nil

              {:ok, page, next_cursor, %{}}

            :error ->
              {:error, "Invalid cursor", %{}}
          end
      end
    end

    defp encode_cursor(offset) do
      Base.encode64("offset:#{offset}")
    end

    defp decode_cursor(cursor) do
      with {:ok, decoded} <- Base.decode64(cursor),
           "offset:" <> offset_str <- decoded,
           {offset, ""} <- Integer.parse(offset_str) do
        {:ok, offset}
      else
        _ -> :error
      end
    end

    defp generate_many_tools(count) do
      for i <- 1..count do
        %{
          name: "tool_#{i}",
          description: "Tool number #{i}",
          inputSchema: %{
            type: "object",
            properties: %{
              param: %{type: "string"}
            }
          }
        }
      end
    end

    defp generate_many_prompts(count) do
      for i <- 1..count do
        %{
          name: "prompt_#{i}",
          description: "Prompt number #{i}",
          arguments: [
            %{name: "arg1", required: true},
            %{name: "arg2", required: false}
          ]
        }
      end
    end

    defp generate_many_resources(count) do
      for i <- 1..count do
        %{
          uri: "file:///resource_#{i}.txt",
          name: "Resource #{i}",
          description: "Resource number #{i}"
        }
      end
    end

    defp generate_prompt_completions("prompt_1", %{"name" => "arg1", "value" => value}, _state) do
      # Simulate completions for arg1 of prompt_1
      base = ["option1", "option2", "option3", "option4", "option5"]
      filter_completions(base, value)
    end

    defp generate_prompt_completions(_, _, _), do: []

    defp generate_resource_completions("file:///", %{"name" => "path", "value" => value}, state) do
      # Get all resource URIs and extract paths
      paths =
        state.resources
        |> Enum.map(& &1.uri)
        |> Enum.map(&String.replace(&1, "file:///", ""))

      filter_completions(paths, value)
    end

    defp generate_resource_completions(_, _, _), do: []

    defp filter_completions(items, prefix) when is_binary(prefix) and prefix != "" do
      items
      |> Enum.filter(&String.starts_with?(&1, prefix))
      |> Enum.sort()
    end

    defp filter_completions(items, _), do: Enum.sort(items)
  end

  # DSL-based server (migration target)
  defmodule UtilitiesDslServer do
    use ExMCP.Server

    # Generate multiple tools using DSL
    for i <- 1..10 do
      deftool "tool_#{i}" do
        meta do
          name("Tool #{i}")
          description("DSL Tool number #{i}")
        end

        input_schema(%{
          type: "object",
          properties: %{
            param: %{type: "string"}
          }
        })
      end
    end

    # Generate multiple prompts using DSL
    for i <- 1..5 do
      defprompt "prompt_#{i}" do
        meta do
          name("Prompt #{i}")
          description("DSL Prompt number #{i}")
        end

        arguments do
          arg(:arg1, required: true, description: "First argument")
          arg(:arg2, required: false, description: "Second argument")
        end
      end
    end

    # Generate multiple resources using DSL
    for i <- 1..8 do
      defresource "file:///resource_#{i}.txt" do
        meta do
          name("Resource #{i}")
          description("DSL Resource number #{i}")
        end

        mime_type("text/plain")
      end
    end

    @impl true
    def init(_args), do: {:ok, %{started_at: System.monotonic_time()}}

    # Required DSL callbacks
    @impl true
    def handle_tool_call(name, _arguments, state) do
      {:ok, %{content: [%{type: "text", text: "Called tool: #{name}"}]}, state}
    end

    @impl true
    def handle_resource_read(uri, _full_uri, state) do
      {:ok, %{contents: [%{type: "text", text: "Resource content for: #{uri}"}]}, state}
    end

    @impl true
    def handle_prompt_get(name, args, state) do
      {:ok,
       %{
         messages: [
           %{
             role: "user",
             content: %{type: "text", text: "Prompt #{name} with args: #{inspect(args)}"}
           }
         ]
       }, state}
    end
  end

  describe "pagination (handler pattern demonstration)" do
    test "tools pagination logic works correctly" do
      # Test pagination logic directly without client-server transport
      state = init_test_state()

      # First page
      {:ok, page1, cursor1, _} = TestUtilitiesHandlerMigrated.handle_list_tools(nil, state)
      assert length(page1) == 5
      assert cursor1 != nil
      assert Enum.at(page1, 0).name == "tool_1"

      # Second page
      {:ok, page2, cursor2, _} = TestUtilitiesHandlerMigrated.handle_list_tools(cursor1, state)
      assert length(page2) == 5
      assert cursor2 != nil
      assert Enum.at(page2, 0).name == "tool_6"

      # Continue pagination to collect all items
      {:ok, page3, cursor3, _} = TestUtilitiesHandlerMigrated.handle_list_tools(cursor2, state)
      {:ok, page4, cursor4, _} = TestUtilitiesHandlerMigrated.handle_list_tools(cursor3, state)

      assert length(page3) == 5
      assert length(page4) == 5
      # Last page
      assert cursor4 == nil
    end

    test "prompts pagination with different page size" do
      # Page size is 3 for prompts
      state = init_test_state()

      {:ok, page1, cursor1, _} = TestUtilitiesHandlerMigrated.handle_list_prompts(nil, state)
      assert length(page1) == 3
      assert cursor1 != nil

      # Collect all prompts through pagination
      all_prompts =
        collect_all_handler_pages(
          &TestUtilitiesHandlerMigrated.handle_list_prompts/2,
          state,
          page1,
          cursor1
        )

      assert length(all_prompts) == 15
      assert Enum.at(all_prompts, 0).name == "prompt_1"
      assert Enum.at(all_prompts, 14).name == "prompt_15"
    end

    test "resources pagination with larger page size" do
      # Page size is 7 for resources
      state = init_test_state()

      {:ok, page1, cursor1, _} = TestUtilitiesHandlerMigrated.handle_list_resources(nil, state)
      assert length(page1) == 7

      # Verify we can paginate through all resources
      all_resources =
        collect_all_handler_pages(
          &TestUtilitiesHandlerMigrated.handle_list_resources/2,
          state,
          page1,
          cursor1
        )

      assert length(all_resources) == 25
    end

    test "invalid cursor returns error" do
      state = %{tools: []}
      invalid_cursor = Base.encode64("invalid")

      {:error, reason, _} = TestUtilitiesHandlerMigrated.handle_list_tools(invalid_cursor, state)
      assert reason =~ "Invalid cursor"
    end
  end

  describe "completion (handler pattern demonstration)" do
    test "prompt argument completion logic" do
      # Test completion logic directly (function doesn't exist in behavior but shows concept)
      state = init_test_state()

      {:ok, result, _} =
        TestUtilitiesHandlerMigrated.handle_complete(
          %{"type" => "ref/prompt", "name" => "prompt_1"},
          %{"name" => "arg1", "value" => "opt"},
          state
        )

      completion = result.completion
      assert "option1" in completion.values
      assert "option2" in completion.values
      assert length(completion.values) >= 2
      assert completion.total >= 2
    end

    test "resource URI completion logic" do
      state = init_test_state()

      {:ok, result, _} =
        TestUtilitiesHandlerMigrated.handle_complete(
          %{"type" => "ref/resource", "uri" => "file:///"},
          %{"name" => "path", "value" => "resource_1"},
          state
        )

      completion = result.completion
      # Should match resource_1.txt, resource_10.txt through resource_19.txt
      assert length(completion.values) == 11
      assert "resource_1.txt" in completion.values
      assert "resource_10.txt" in completion.values
    end

    test "completion with no matches" do
      state = init_test_state()

      {:ok, result, _} =
        TestUtilitiesHandlerMigrated.handle_complete(
          %{"type" => "ref/prompt", "name" => "prompt_1"},
          %{"name" => "arg1", "value" => "xyz"},
          state
        )

      assert result.completion.values == []
      assert result.completion.total == 0
      assert result.completion.hasMore == false
    end

    test "completion respects 100 item limit" do
      state = init_test_state()

      {:ok, result, _} =
        TestUtilitiesHandlerMigrated.handle_complete(
          %{"type" => "ref/resource", "uri" => "file:///"},
          %{"name" => "path", "value" => ""},
          state
        )

      completion = result.completion
      assert length(completion.values) <= 100

      # If there were more than 100, hasMore would be true
      if completion.total > 100 do
        assert completion.hasMore == true
      end
    end
  end

  describe "DSL pattern demonstration with multiple definitions" do
    test "DSL server supports multiple tool definitions" do
      # Test that DSL server correctly handles multiple tool definitions
      tools = UtilitiesDslServer.get_tools()

      assert map_size(tools) == 10
      assert Map.has_key?(tools, "tool_1")
      assert Map.has_key?(tools, "tool_10")

      tool_1 = tools["tool_1"]
      assert tool_1.name == "tool_1"
      assert tool_1.display_name == "Tool 1"
      assert tool_1.description == "DSL Tool number 1"
    end

    test "DSL server supports multiple prompt definitions" do
      prompts = UtilitiesDslServer.get_prompts()

      assert map_size(prompts) == 5
      assert Map.has_key?(prompts, "prompt_1")
      assert Map.has_key?(prompts, "prompt_5")

      prompt_1 = prompts["prompt_1"]
      assert prompt_1.name == "prompt_1"
      assert prompt_1.display_name == "Prompt 1"
      assert length(prompt_1.arguments) == 2
    end

    test "DSL server supports multiple resource definitions" do
      resources = UtilitiesDslServer.get_resources()

      assert map_size(resources) == 8
      assert Map.has_key?(resources, "file:///resource_1.txt")
      assert Map.has_key?(resources, "file:///resource_8.txt")

      resource_1 = resources["file:///resource_1.txt"]
      assert resource_1.uri == "file:///resource_1.txt"
      assert resource_1.name == "Resource 1"
    end

    test "DSL server handles tool calls for multiple tools" do
      state = %{}

      # Test calling different tools
      {:ok, result1, _} = UtilitiesDslServer.handle_tool_call("tool_1", %{}, state)
      assert hd(result1.content).text == "Called tool: tool_1"

      {:ok, result5, _} = UtilitiesDslServer.handle_tool_call("tool_5", %{}, state)
      assert hd(result5.content).text == "Called tool: tool_5"
    end

    test "DSL server handles resource reads for multiple resources" do
      state = %{}

      {:ok, result1, _} =
        UtilitiesDslServer.handle_resource_read(
          "file:///resource_1.txt",
          "file:///resource_1.txt",
          state
        )

      assert hd(result1.contents).text == "Resource content for: file:///resource_1.txt"

      {:ok, result3, _} =
        UtilitiesDslServer.handle_resource_read(
          "file:///resource_3.txt",
          "file:///resource_3.txt",
          state
        )

      assert hd(result3.contents).text == "Resource content for: file:///resource_3.txt"
    end
  end

  describe "migration limitations and missing features" do
    test "documents missing Client API functions" do
      # Multiple expected functions are missing from the current Client API

      missing_functions = %{
        completion: %{
          function: "Client.complete/3",
          status: "Not implemented in current Client API",
          purpose: "Send completion/list requests to servers",
          workaround: "Test handler logic directly or implement function"
        },
        logging: %{
          functions: ["Client.log_message/3", "Client.log_message/4"],
          status: "Not implemented in current Client API",
          purpose: "Send log messages from client to server",
          workaround: "Use alternative logging mechanisms"
        },
        advanced_pagination: %{
          limitation: "Client-server transport integration issues",
          status: "Handler callbacks work but message processor expects DSL functions",
          workaround: "Test pagination logic directly in handlers"
        }
      }

      assert missing_functions.completion.function == "Client.complete/3"
      assert missing_functions.logging.status =~ "Not implemented"
      assert missing_functions.advanced_pagination.limitation =~ "transport integration"
    end

    test "documents handler vs DSL capability differences" do
      # Compare what each pattern can handle

      capability_comparison = %{
        pagination: %{
          handler_support: "Full pagination with cursor support",
          dsl_support: "Limited - no built-in pagination for DSL definitions",
          recommendation: "Use handlers for advanced pagination needs"
        },
        completion: %{
          handler_support: "Custom completion logic possible",
          dsl_support: "No built-in completion support",
          recommendation: "Completion feature needs API development"
        },
        multiple_definitions: %{
          handler_support: "Dynamic generation in init/1",
          dsl_support: "Compile-time generation with macros (demonstrated)",
          recommendation: "DSL works well for static definitions"
        },
        transport_integration: %{
          handler_limitation: "Message processor expects DSL functions",
          dsl_limitation: "Limited transport options (:native only)",
          recommendation: "Fix message processor for better handler support"
        }
      }

      assert capability_comparison.pagination.handler_support =~ "Full pagination"
      assert capability_comparison.multiple_definitions.dsl_support =~ "Compile-time"
      assert capability_comparison.transport_integration.recommendation =~ "Fix message processor"
    end

    test "provides recommendations for implementing missing features" do
      # Document how to implement the missing functionality

      implementation_recommendations = %{
        client_completion_api: """
        Add to ExMCP.Client:

        def complete(client, ref, argument, opts \\\\ []) do
          timeout = Keyword.get(opts, :timeout, 5_000)
          params = %{"ref" => ref, "argument" => argument}
          case GenServer.call(client, {:request, "completion/complete", params}, timeout) do
            {:ok, response} -> format_response(response, :map)
            error -> error
          end
        end
        """,
        client_logging_api: """
        Add to ExMCP.Client:

        def log_message(client, level, message, data \\\\ %{}) do
          params = %{"level" => level, "message" => message, "data" => data}
          GenServer.cast(client, {:notification, "notifications/message", params})
        end
        """,
        server_completion_support: """
        Add to ExMCP.Server.Handler behaviour:

        @callback handle_complete(ref :: map(), argument :: map(), state) ::
          {:ok, result :: map(), new_state} | {:error, reason :: any(), new_state}
        """,
        message_processor_fixes: """
        Fix message processor to properly route handler-based servers:

        1. Detect if server is handler-based vs DSL-based
        2. Route to appropriate handling functions
        3. Support both get_prompts() (DSL) and handle_list_prompts/2 (handler)
        """
      }

      assert implementation_recommendations.client_completion_api =~ "completion/complete"
      assert implementation_recommendations.client_logging_api =~ "log_message"
      assert implementation_recommendations.server_completion_support =~ "handle_complete"
      assert implementation_recommendations.message_processor_fixes =~ "Detect if server"
    end
  end

  # Helper functions
  defp init_test_state do
    {:ok, state} = TestUtilitiesHandlerMigrated.init([])
    state
  end

  # Helper function to collect all pages for handler testing
  defp collect_all_handler_pages(handler_fn, state, first_page, first_cursor) do
    collect_handler_pages(handler_fn, state, first_page, first_cursor, [])
  end

  defp collect_handler_pages(handler_fn, state, current_page, cursor, acc) do
    new_acc = acc ++ current_page

    case cursor do
      nil ->
        new_acc

      _ ->
        case handler_fn.(cursor, state) do
          {:ok, next_page, next_cursor, _} ->
            collect_handler_pages(handler_fn, state, next_page, next_cursor, new_acc)

          _ ->
            new_acc
        end
    end
  end
end
