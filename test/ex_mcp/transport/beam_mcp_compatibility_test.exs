defmodule ExMCP.Transport.BeamMcpCompatibilityTest do
  @moduledoc """
  Compatibility tests ensuring the enhanced BEAM transport works with
  existing MCP protocol implementations and maintains full spec compliance.
  """

  use ExUnit.Case, async: true
  require Logger

  alias ExMCP.{Client, Server, Protocol, Types}

  @test_port_base 19100

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
      assert %{"name" => "calculator"} = List.first(tools)

      # Call calculator tool using standard MCP API
      assert {:ok, %{content: content}} =
               Client.call_tool(client_pid, "calculator", %{
                 "operation" => "add",
                 "a" => 5,
                 "b" => 3
               })

      assert [%{"type" => "text", "text" => "Result: 8"}] = content

      GenServer.stop(client_pid)
      GenServer.stop(server_pid)
    end

    test "resources/list and resources/read" do
      port = @test_port_base + 3

      {:ok, server_pid, actual_port} =
        Server.start_test_server(
          port: port,
          handler: TestMCPHandler
        )

      {:ok, client_pid} = Client.connect(host: "localhost", port: actual_port)
      eventually(fn -> Client.status(client_pid) == :connected end)

      # List resources
      assert {:ok, response} = Client.call(client_pid, %{"method" => "resources/list"})
      assert %{"result" => %{"resources" => resources}} = response
      assert length(resources) == 1
      assert %{"uri" => "file:///test/data.txt"} = List.first(resources)

      # Read resource
      assert {:ok, response} =
               Client.call(client_pid, %{
                 "method" => "resources/read",
                 "params" => %{"uri" => "file:///test/data.txt"}
               })

      assert %{"result" => %{"contents" => contents}} = response
      assert [%{"uri" => "file:///test/data.txt", "text" => text}] = contents
      assert text == "This is test data content."

      Client.close(client_pid)
      Server.stop(server_pid)
    end

    test "prompts/list and prompts/get" do
      port = @test_port_base + 4

      {:ok, server_pid, actual_port} =
        Server.start_test_server(
          port: port,
          handler: TestMCPHandler
        )

      {:ok, client_pid} = Client.connect(host: "localhost", port: actual_port)
      eventually(fn -> Client.status(client_pid) == :connected end)

      # List prompts
      assert {:ok, response} = Client.call(client_pid, %{"method" => "prompts/list"})
      assert %{"result" => %{"prompts" => prompts}} = response
      assert length(prompts) == 1
      assert %{"name" => "summarize"} = List.first(prompts)

      # Get prompt
      assert {:ok, response} =
               Client.call(client_pid, %{
                 "method" => "prompts/get",
                 "params" => %{
                   "name" => "summarize",
                   "arguments" => %{"text" => "This is sample text to summarize."}
                 }
               })

      assert %{"result" => prompt} = response
      assert %{"description" => _, "messages" => messages} = prompt
      assert length(messages) == 1

      Client.close(client_pid)
      Server.stop(server_pid)
    end

    test "error handling with proper MCP error codes" do
      port = @test_port_base + 5

      {:ok, server_pid, actual_port} =
        Server.start_test_server(
          port: port,
          handler: TestMCPHandler
        )

      {:ok, client_pid} = Client.connect(host: "localhost", port: actual_port)
      eventually(fn -> Client.status(client_pid) == :connected end)

      # Call non-existent tool
      assert {:ok, response} =
               Client.call(client_pid, %{
                 "method" => "tools/call",
                 "params" => %{
                   "name" => "nonexistent",
                   "arguments" => %{}
                 }
               })

      assert %{"error" => error} = response
      assert %{"code" => -32601, "message" => message} = error
      assert String.contains?(message, "not found")

      # Call non-existent method
      assert {:ok, response} =
               Client.call(client_pid, %{
                 "method" => "nonexistent/method"
               })

      assert %{"error" => %{"code" => -32601}} = response

      Client.close(client_pid)
      Server.stop(server_pid)
    end

    test "notification handling" do
      port = @test_port_base + 6

      {:ok, server_pid, actual_port} =
        Server.start_test_server(
          port: port,
          handler: TestMCPHandler
        )

      {:ok, client_pid} = Client.connect(host: "localhost", port: actual_port)
      eventually(fn -> Client.status(client_pid) == :connected end)

      # Send progress notification
      assert :ok =
               Client.notify(client_pid, %{
                 "method" => "notifications/progress",
                 "params" => %{
                   "progressToken" => "task_123",
                   "progress" => 50,
                   "total" => 100
                 }
               })

      # Send cancellation notification
      assert :ok =
               Client.notify(client_pid, %{
                 "method" => "notifications/cancelled"
               })

      # Give server time to process notifications
      Process.sleep(100)

      Client.close(client_pid)
      Server.stop(server_pid)
    end

    test "completion/complete for sampling" do
      port = @test_port_base + 7

      {:ok, server_pid, actual_port} =
        Server.start_test_server(
          port: port,
          handler: TestMCPHandler
        )

      {:ok, client_pid} = Client.connect(host: "localhost", port: actual_port)
      eventually(fn -> Client.status(client_pid) == :connected end)

      # Send completion request
      assert {:ok, response} =
               Client.call(client_pid, %{
                 "method" => "completion/complete",
                 "params" => %{
                   "ref" => "sampling_ref_123",
                   "reason" => "cancelled"
                 }
               })

      assert %{"result" => %{}} = response

      Client.close(client_pid)
      Server.stop(server_pid)
    end
  end

  describe "Performance with MCP Protocol" do
    test "MCP request performance is within targets" do
      port = @test_port_base + 8

      {:ok, server_pid, actual_port} =
        Server.start_test_server(
          port: port,
          handler: TestMCPHandler
        )

      {:ok, client_pid} = Client.connect(host: "localhost", port: actual_port)
      eventually(fn -> Client.status(client_pid) == :connected end)

      # Warm up
      for _ <- 1..5 do
        Client.call(client_pid, %{"method" => "tools/list"})
      end

      # Measure performance for different MCP operations
      operations = [
        %{"method" => "tools/list"},
        %{"method" => "resources/list"},
        %{"method" => "prompts/list"},
        %{
          "method" => "tools/call",
          "params" => %{
            "name" => "calculator",
            "arguments" => %{"operation" => "add", "a" => 1, "b" => 2}
          }
        }
      ]

      for operation <- operations do
        latencies =
          for _ <- 1..20 do
            start_time = System.monotonic_time(:microsecond)
            {:ok, _response} = Client.call(client_pid, operation)
            end_time = System.monotonic_time(:microsecond)
            end_time - start_time
          end

        avg_latency = Enum.sum(latencies) / length(latencies)
        Logger.info("#{operation["method"]} avg latency: #{Float.round(avg_latency, 1)}μs")

        # All MCP operations should be fast
        assert avg_latency < 100, "#{operation["method"]} too slow: #{avg_latency}μs"
      end

      Client.close(client_pid)
      Server.stop(server_pid)
    end
  end

  # Helper function
  defp eventually(fun, timeout \\ 5_000) do
    eventually(fun, timeout, 50)
  end

  defp eventually(fun, timeout, interval) when timeout > 0 do
    if fun.() do
      :ok
    else
      Process.sleep(interval)
      eventually(fun, timeout - interval, interval)
    end
  end

  defp eventually(_fun, _timeout, _interval) do
    flunk("Eventually condition not met within timeout")
  end
end
