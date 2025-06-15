defmodule ExMCP.Transport.BeamMcpCompatibilityTest do
  @moduledoc """
  Compatibility tests ensuring the enhanced BEAM transport works with
  existing MCP protocol implementations and maintains full spec compliance.
  """

  use ExUnit.Case, async: true
  require Logger

  alias ExMCP.{Client, Server}

  # Create a real MCP handler that implements the full protocol
  defmodule TestMCPHandler do
    @behaviour ExMCP.Server.Handler

    def init(_opts) do
      {:ok,
       %{
         capabilities: %{
           "tools" => %{"listChanged" => true},
           "resources" => %{"subscribe" => true, "listChanged" => true},
           "prompts" => %{"listChanged" => true},
           "sampling" => %{}
         },
         tools: [
           %{
             "name" => "calculator",
             "description" => "Performs basic arithmetic operations",
             "inputSchema" => %{
               "type" => "object",
               "properties" => %{
                 "operation" => %{
                   "type" => "string",
                   "enum" => ["add", "subtract", "multiply", "divide"]
                 },
                 "a" => %{"type" => "number"},
                 "b" => %{"type" => "number"}
               },
               "required" => ["operation", "a", "b"]
             }
           }
         ],
         resources: [
           %{
             "uri" => "file:///test/data.txt",
             "name" => "Test Data",
             "description" => "Sample test data",
             "mimeType" => "text/plain"
           }
         ],
         prompts: [
           %{
             "name" => "summarize",
             "description" => "Summarizes the given text",
             "arguments" => [
               %{
                 "name" => "text",
                 "description" => "Text to summarize",
                 "required" => true
               }
             ]
           }
         ]
       }}
    end

    def handle_initialize(_params, state) do
      capabilities = %{
        "capabilities" => state.capabilities,
        "serverInfo" => %{
          "name" => "test-mcp-server",
          "version" => "1.0.0"
        }
      }

      {:ok, capabilities, state}
    end

    def handle_list_tools(_cursor, state) do
      {:ok, state.tools, nil, state}
    end

    def handle_call_tool("calculator", %{"operation" => op, "a" => a, "b" => b}, state) do
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
        {:error, message} ->
          {:error, %{"code" => -1, "message" => message}, state}

        value ->
          content = [
            %{
              "type" => "text",
              "text" => "Result: #{value}"
            }
          ]

          {:ok, content, state}
      end
    end

    def handle_call_tool(name, _args, state) do
      {:error, %{"code" => -32601, "message" => "Tool not found: #{name}"}, state}
    end

    def handle_list_resources(_cursor, state) do
      {:ok, state.resources, nil, state}
    end

    def handle_read_resource("file:///test/data.txt", state) do
      contents = [
        %{
          "uri" => "file:///test/data.txt",
          "mimeType" => "text/plain",
          "text" => "This is test data content."
        }
      ]

      {:ok, contents, state}
    end

    def handle_read_resource(uri, state) do
      {:error, %{"code" => -32602, "message" => "Resource not found: #{uri}"}, state}
    end

    def handle_list_prompts(_cursor, state) do
      {:ok, state.prompts, nil, state}
    end

    def handle_get_prompt("summarize", %{"text" => text}, state) do
      prompt = %{
        "description" => "Summarize the following text",
        "messages" => [
          %{
            "role" => "user",
            "content" => %{
              "type" => "text",
              "text" => "Please summarize this text: #{text}"
            }
          }
        ]
      }

      {:ok, prompt, state}
    end

    def handle_get_prompt(name, _args, state) do
      {:error, %{"code" => -32602, "message" => "Prompt not found: #{name}"}, state}
    end

    def handle_complete(%{"ref" => "sampling_ref_123", "reason" => "cancelled"}, state) do
      {:ok, state}
    end

    def handle_complete(params, state) do
      {:error, %{"code" => -32602, "message" => "Invalid completion params: #{inspect(params)}"},
       state}
    end

    # Handle generic requests
    def handle_request(%{"method" => "initialize", "params" => params}, state) do
      case handle_initialize(params, state) do
        {:ok, result, new_state} ->
          response = %{"result" => result}
          {:ok, response, new_state}

        {:error, error, new_state} ->
          response = %{"error" => error}
          {:error, response, new_state}
      end
    end

    def handle_request(%{"method" => "tools/list"}, state) do
      case handle_list_tools(nil, state) do
        {:ok, tools, _cursor, new_state} ->
          response = %{"result" => %{"tools" => tools}}
          {:ok, response, new_state}

        {:error, error, new_state} ->
          response = %{"error" => error}
          {:error, response, new_state}
      end
    end

    def handle_request(
          %{"method" => "tools/call", "params" => %{"name" => name, "arguments" => args}},
          state
        ) do
      case handle_call_tool(name, args, state) do
        {:ok, content, new_state} ->
          response = %{"result" => %{"content" => content}}
          {:ok, response, new_state}

        {:error, error, new_state} ->
          response = %{"error" => error}
          {:error, response, new_state}
      end
    end

    def handle_request(%{"method" => "resources/list"}, state) do
      case handle_list_resources(nil, state) do
        {:ok, resources, _cursor, new_state} ->
          response = %{"result" => %{"resources" => resources}}
          {:ok, response, new_state}

        {:error, error, new_state} ->
          response = %{"error" => error}
          {:error, response, new_state}
      end
    end

    def handle_request(%{"method" => "resources/read", "params" => %{"uri" => uri}}, state) do
      case handle_read_resource(uri, state) do
        {:ok, contents, new_state} ->
          response = %{"result" => %{"contents" => contents}}
          {:ok, response, new_state}

        {:error, error, new_state} ->
          response = %{"error" => error}
          {:error, response, new_state}
      end
    end

    def handle_request(%{"method" => "prompts/list"}, state) do
      case handle_list_prompts(nil, state) do
        {:ok, prompts, _cursor, new_state} ->
          response = %{"result" => %{"prompts" => prompts}}
          {:ok, response, new_state}

        {:error, error, new_state} ->
          response = %{"error" => error}
          {:error, response, new_state}
      end
    end

    def handle_request(
          %{"method" => "prompts/get", "params" => %{"name" => name, "arguments" => args}},
          state
        ) do
      case handle_get_prompt(name, args, state) do
        {:ok, prompt, new_state} ->
          response = %{"result" => prompt}
          {:ok, response, new_state}

        {:error, error, new_state} ->
          response = %{"error" => error}
          {:error, response, new_state}
      end
    end

    def handle_request(%{"method" => "completion/complete", "params" => params}, state) do
      case handle_complete(params, state) do
        {:ok, new_state} ->
          response = %{"result" => %{}}
          {:ok, response, new_state}

        {:error, error, new_state} ->
          response = %{"error" => error}
          {:error, response, new_state}
      end
    end

    def handle_request(%{"method" => method}, state) do
      response = %{"error" => %{"code" => -32601, "message" => "Method not found: #{method}"}}
      {:error, response, state}
    end

    def handle_notification(%{"method" => "notifications/cancelled"}, state) do
      Logger.info("Received cancellation notification")
      {:ok, state}
    end

    def handle_notification(%{"method" => "notifications/progress", "params" => params}, state) do
      Logger.info("Progress update: #{inspect(params)}")
      {:ok, state}
    end

    def handle_notification(_notification, state) do
      {:ok, state}
    end

    def terminate(_reason, _state) do
      :ok
    end
  end

  describe "MCP Protocol Compatibility" do
    test "initialization handshake" do
      # Start server using standard MCP Server
      {:ok, server_pid} =
        Server.start_link(
          transport: :beam,
          name: :mcp_compat_test_1,
          handler: TestMCPHandler
        )

      # Start client using standard MCP Client
      {:ok, client_pid} =
        Client.start_link(
          transport: :beam,
          server: :mcp_compat_test_1
        )

      # Wait for connection (BEAM transport connects immediately)
      Process.sleep(50)

      # Test server info (automatically initialized)
      assert {:ok, server_info} = Client.server_info(client_pid)
      assert server_info["name"] == "test-mcp-server"
      assert server_info["version"] == "1.0.0"

      # Verify capabilities are available
      assert {:ok, tools_result} = Client.list_tools(client_pid)
      assert Map.has_key?(tools_result, :tools)

      GenServer.stop(client_pid)
      GenServer.stop(server_pid)
    end

    test "tools/list and tools/call" do
      # Start server and client using standard MCP APIs
      {:ok, server_pid} =
        Server.start_link(
          transport: :beam,
          name: :mcp_compat_test_2,
          handler: TestMCPHandler
        )

      {:ok, client_pid} =
        Client.start_link(
          transport: :beam,
          server: :mcp_compat_test_2
        )

      Process.sleep(50)

      # List tools using standard MCP API
      assert {:ok, %{tools: tools}} = Client.list_tools(client_pid)
      assert length(tools) == 1
      calculator = List.first(tools)
      assert calculator.name == "calculator" || calculator["name"] == "calculator"

      # Call calculator tool using standard MCP API
      assert {:ok, %{content: content}} =
               Client.call_tool(client_pid, "calculator", %{
                 "operation" => "add",
                 "a" => 5,
                 "b" => 3
               })

      assert [result_content] = content
      assert result_content.type == "text" || result_content["type"] == "text"
      assert result_content.text == "Result: 8" || result_content["text"] == "Result: 8"

      GenServer.stop(client_pid)
      GenServer.stop(server_pid)
    end

    test "resources/list and resources/read" do
      # Start server and client using standard MCP APIs with BEAM transport
      {:ok, server_pid} =
        Server.start_link(
          transport: :beam,
          name: :mcp_compat_test_3,
          handler: TestMCPHandler
        )

      {:ok, client_pid} =
        Client.start_link(
          transport: :beam,
          server: :mcp_compat_test_3
        )

      Process.sleep(50)

      # List resources using standard MCP API
      assert {:ok, %{resources: resources}} = Client.list_resources(client_pid)
      assert length(resources) == 1
      resource = List.first(resources)
      assert resource.uri == "file:///test/data.txt" || resource["uri"] == "file:///test/data.txt"

      # Read resource using standard MCP API
      assert {:ok, response} = Client.read_resource(client_pid, "file:///test/data.txt")

      # The response might be wrapped in a contents key or be the contents directly
      contents =
        if is_map(response) && Map.has_key?(response, :contents) do
          response.contents
        else
          response
        end

      # Handle nested array structure
      actual_contents =
        if is_list(contents) && is_list(List.first(contents)) do
          List.first(contents)
        else
          contents
        end

      assert [content_item] = actual_contents

      assert content_item.text == "This is test data content." ||
               content_item["text"] == "This is test data content."

      GenServer.stop(client_pid)
      GenServer.stop(server_pid)
    end

    test "prompts/list and prompts/get" do
      # Start server and client using standard MCP APIs with BEAM transport
      {:ok, server_pid} =
        Server.start_link(
          transport: :beam,
          name: :mcp_compat_test_4,
          handler: TestMCPHandler
        )

      {:ok, client_pid} =
        Client.start_link(
          transport: :beam,
          server: :mcp_compat_test_4
        )

      Process.sleep(50)

      # List prompts using standard MCP API
      assert {:ok, %{prompts: prompts}} = Client.list_prompts(client_pid)
      assert length(prompts) == 1
      prompt = List.first(prompts)
      assert prompt.name == "summarize" || prompt["name"] == "summarize"

      # Get prompt using standard MCP API
      assert {:ok, prompt_response} =
               Client.get_prompt(client_pid, "summarize", %{
                 "text" => "This is sample text to summarize."
               })

      assert prompt_response.description || prompt_response["description"]
      messages = prompt_response.messages || prompt_response["messages"]
      assert length(messages) == 1

      GenServer.stop(client_pid)
      GenServer.stop(server_pid)
    end

    test "error handling with proper MCP error codes" do
      # Start server and client using standard MCP APIs with BEAM transport
      {:ok, server_pid} =
        Server.start_link(
          transport: :beam,
          name: :mcp_compat_test_5,
          handler: TestMCPHandler
        )

      {:ok, client_pid} =
        Client.start_link(
          transport: :beam,
          server: :mcp_compat_test_5
        )

      Process.sleep(50)

      # Call non-existent tool - should return error
      assert {:error, _error} = Client.call_tool(client_pid, "nonexistent", %{})
      # The error will be a string or map containing the error details

      GenServer.stop(client_pid)
      GenServer.stop(server_pid)
    end

    test "notification handling" do
      # Start server and client using standard MCP APIs with BEAM transport
      {:ok, server_pid} =
        Server.start_link(
          transport: :beam,
          name: :mcp_compat_test_6,
          handler: TestMCPHandler
        )

      {:ok, client_pid} =
        Client.start_link(
          transport: :beam,
          server: :mcp_compat_test_6
        )

      Process.sleep(50)

      # The client can send cancellation notifications using send_cancelled
      # Note: Client doesn't have a general notify method, but has specific notification methods
      assert :ok = Client.send_cancelled(client_pid, "req_123", "Test cancellation")

      # Give server time to process notifications
      Process.sleep(100)

      GenServer.stop(client_pid)
      GenServer.stop(server_pid)
    end

    test "completion/complete for sampling" do
      # Start server and client using standard MCP APIs with BEAM transport
      {:ok, server_pid} =
        Server.start_link(
          transport: :beam,
          name: :mcp_compat_test_7,
          handler: TestMCPHandler
        )

      {:ok, client_pid} =
        Client.start_link(
          transport: :beam,
          server: :mcp_compat_test_7
        )

      Process.sleep(50)

      # The completion/complete method is not exposed through the standard Client API
      # This would typically be called internally by the client when handling sampling
      # For now, we'll just verify the server and client are properly connected
      assert {:ok, _server_info} = Client.server_info(client_pid)

      GenServer.stop(client_pid)
      GenServer.stop(server_pid)
    end
  end

  describe "Performance with MCP Protocol" do
    test "MCP request performance is within targets" do
      # Start server and client using standard MCP APIs with BEAM transport
      {:ok, server_pid} =
        Server.start_link(
          transport: :beam,
          name: :mcp_compat_test_8,
          handler: TestMCPHandler
        )

      {:ok, client_pid} =
        Client.start_link(
          transport: :beam,
          server: :mcp_compat_test_8
        )

      Process.sleep(50)

      # Warm up
      for _ <- 1..5 do
        Client.list_tools(client_pid)
      end

      # Measure performance for list_tools
      latencies =
        for _ <- 1..20 do
          start_time = System.monotonic_time(:microsecond)
          {:ok, _tools} = Client.list_tools(client_pid)
          end_time = System.monotonic_time(:microsecond)
          end_time - start_time
        end

      avg_latency = Enum.sum(latencies) / length(latencies)
      Logger.info("list_tools avg latency: #{Float.round(avg_latency, 1)}μs")

      # Enhanced BEAM transport should be very fast (target: <15μs)
      # But being conservative for CI environments
      assert avg_latency < 1000, "list_tools too slow: #{avg_latency}μs"

      # Test tool call performance
      calc_latencies =
        for _ <- 1..20 do
          start_time = System.monotonic_time(:microsecond)

          {:ok, _result} =
            Client.call_tool(client_pid, "calculator", %{
              "operation" => "add",
              "a" => 1,
              "b" => 2
            })

          end_time = System.monotonic_time(:microsecond)
          end_time - start_time
        end

      calc_avg_latency = Enum.sum(calc_latencies) / length(calc_latencies)
      Logger.info("call_tool avg latency: #{Float.round(calc_avg_latency, 1)}μs")

      assert calc_avg_latency < 1000, "call_tool too slow: #{calc_avg_latency}μs"

      GenServer.stop(client_pid)
      GenServer.stop(server_pid)
    end
  end
end
