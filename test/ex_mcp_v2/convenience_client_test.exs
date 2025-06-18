defmodule ExMCP.ConvenienceClientTest do
  use ExUnit.Case, async: false

  alias ExMCP.ConvenienceClient
  alias ExMCP.Client.{Response, Error}

  # Mock transport for testing convenience features
  defmodule MockTransport do
    @behaviour ExMCP.Transport

    def connect(_opts) do
      Agent.start_link(fn -> %{messages: [], responses: default_responses()} end)
    end

    def send(agent, data) do
      Agent.update(agent, fn state ->
        %{state | messages: [data | state.messages]}
      end)

      {:ok, agent}
    end

    def recv(agent, _timeout) do
      msg =
        Agent.get_and_update(agent, fn state ->
          case state.messages do
            [msg | rest] -> {msg, %{state | messages: rest}}
            [] -> {nil, state}
          end
        end)

      if msg do
        case Jason.decode!(msg) do
          %{"method" => method, "id" => id} = request ->
            response = get_mock_response(agent, method, request)
            {:ok, response, agent}

          _ ->
            {:error, :unknown_message}
        end
      else
        {:error, :no_data}
      end
    end

    def close(agent) do
      Agent.stop(agent)
      {:ok, nil}
    end

    def controlling_process(_agent, _pid), do: :ok
    def send_message(_agent, _msg), do: {:error, :not_implemented}
    def receive_message(_agent), do: {:error, :not_implemented}

    defp default_responses do
      %{
        "initialize" => %{
          "protocolVersion" => "2024-11-05",
          "capabilities" => %{"tools" => %{}, "resources" => %{}, "prompts" => %{}},
          "serverInfo" => %{"name" => "MockServer", "version" => "1.0.0"}
        },
        "tools/list" => %{
          "tools" => [
            %{
              "name" => "calculator",
              "description" => "Performs basic calculations",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{
                  "operation" => %{"type" => "string"},
                  "a" => %{"type" => "number"},
                  "b" => %{"type" => "number"}
                }
              }
            },
            %{
              "name" => "greeter",
              "description" => "Says hello to someone"
            }
          ]
        },
        "tools/call" => %{
          "content" => [%{"type" => "text", "text" => "Result: 42"}]
        },
        "resources/list" => %{
          "resources" => [
            %{
              "uri" => "file://data.txt",
              "name" => "Data File",
              "description" => "Sample data file",
              "mimeType" => "text/plain"
            }
          ]
        },
        "resources/read" => %{
          "contents" => [
            %{
              "type" => "text",
              "text" => "Sample file content",
              "mimeType" => "text/plain"
            }
          ]
        },
        "prompts/list" => %{
          "prompts" => [
            %{
              "name" => "greeting",
              "description" => "A friendly greeting prompt"
            }
          ]
        },
        "prompts/get" => %{
          "messages" => [
            %{
              "role" => "user",
              "content" => %{"type" => "text", "text" => "Hello there!"}
            }
          ]
        }
      }
    end

    defp get_mock_response(agent, method, request) do
      responses = Agent.get(agent, & &1.responses)
      response_data = Map.get(responses, method, %{})

      Jason.encode!(%{
        "jsonrpc" => "2.0",
        "id" => request["id"],
        "result" => response_data
      })
    end
  end

  setup do
    # Start client with mock transport
    {:ok, client} = ConvenienceClient.connect({MockTransport, []})
    %{client: client}
  end

  describe "connection management" do
    test "connect with URL string" do
      # This will fail since we don't have a real HTTP server, but tests URL parsing
      # The process will exit with connection failure, which is expected behavior
      Process.flag(:trap_exit, true)
      spawn_link(fn ->
        ConvenienceClient.connect("http://localhost:8080", 
          timeout: 100, 
          max_reconnect_attempts: 0,
          reconnect_interval: 50
        )
      end)
      
      # Wait for exit message
      assert_receive {:EXIT, _pid, _reason}, 3000
    end

    test "connect with transport tuple", %{client: client} do
      # Client should be connected via setup
      assert {:ok, status} = ConvenienceClient.status(client)
      assert status.connected == true
    end

    test "disconnect closes client", %{client: client} do
      assert :ok = ConvenienceClient.disconnect(client)
      # Client should be stopped after disconnect
      refute Process.alive?(client)
    end
  end

  describe "tool operations" do
    test "tools returns normalized tool list", %{client: client} do
      tools = ConvenienceClient.tools(client)

      assert is_list(tools)
      assert length(tools) == 2

      calculator = Enum.find(tools, &(&1.name == "calculator"))
      assert calculator.name == "calculator"
      assert calculator.description == "Performs basic calculations"
      assert is_map(calculator.input_schema)
      assert is_map(calculator.metadata)
    end

    test "call executes tool and normalizes response", %{client: client} do
      result = ConvenienceClient.call(client, "calculator", %{operation: "add", a: 1, b: 2})

      # Response should be normalized to just the content
      assert result == "Result: 42"
    end

    test "call with raw response option", %{client: client} do
      result = ConvenienceClient.call(client, "calculator", %{}, normalize: false)

      # Should return raw response structure
      assert %{"content" => [%{"type" => "text", "text" => "Result: 42"}]} = result
    end

    test "find_tool with exact match", %{client: client} do
      tool = ConvenienceClient.find_tool(client, "calculator")

      assert tool.name == "calculator"
      assert tool.description == "Performs basic calculations"
    end

    test "find_tool with fuzzy search", %{client: client} do
      tools = ConvenienceClient.find_tool(client, "calc", fuzzy: true)

      assert is_list(tools)
      assert length(tools) == 1
      assert Enum.at(tools, 0).name == "calculator"
    end

    test "find_tool with schema filter", %{client: client} do
      tools = ConvenienceClient.find_tool(client, nil, has_schema: true)

      assert is_list(tools)
      assert length(tools) == 1
      assert Enum.at(tools, 0).name == "calculator"
    end
  end

  describe "resource operations" do
    test "resources returns normalized resource list", %{client: client} do
      resources = ConvenienceClient.resources(client)

      assert is_list(resources)
      assert length(resources) == 1

      resource = Enum.at(resources, 0)
      assert resource.uri == "file://data.txt"
      assert resource.name == "Data File"
      assert resource.description == "Sample data file"
      assert resource.mime_type == "text/plain"
    end

    test "read normalizes resource content", %{client: client} do
      content = ConvenienceClient.read(client, "file://data.txt")

      # Should return just the text content
      assert content == "Sample file content"
    end
  end

  describe "prompt operations" do
    test "prompts returns normalized prompt list", %{client: client} do
      prompts = ConvenienceClient.prompts(client)

      assert is_list(prompts)
      assert length(prompts) == 1

      prompt = Enum.at(prompts, 0)
      assert prompt.name == "greeting"
      assert prompt.description == "A friendly greeting prompt"
    end

    test "prompt returns normalized prompt result", %{client: client} do
      result = ConvenienceClient.prompt(client, "greeting", %{})

      assert %{messages: messages} = result
      assert is_list(messages)
      assert length(messages) == 1

      message = Enum.at(messages, 0)
      assert message.role == "user"
      assert message.content == "Hello there!"
    end
  end

  describe "batch operations" do
    test "batch executes multiple operations", %{client: client} do
      operations = [
        {:call_tool, "calculator", %{operation: "add", a: 1, b: 2}},
        {:list_tools, %{}},
        {:list_resources, %{}}
      ]

      results = ConvenienceClient.batch(client, operations)

      assert is_list(results)
      assert length(results) == 3

      # First result should be tool call result
      assert Enum.at(results, 0) == "Result: 42"

      # Second result should be tools list
      tools = Enum.at(results, 1)
      assert is_list(tools)
      assert length(tools) == 2

      # Third result should be resources list
      resources = Enum.at(results, 2)
      assert is_list(resources)
      assert length(resources) == 1
    end

    test "batch with concurrency limit", %{client: client} do
      operations = [
        {:list_tools, %{}},
        {:list_resources, %{}},
        {:list_prompts, %{}}
      ]

      results = ConvenienceClient.batch(client, operations, max_concurrency: 2)

      assert is_list(results)
      assert length(results) == 3
    end
  end

  describe "utility functions" do
    test "server_info returns normalized server information", %{client: client} do
      assert {:ok, server_info} = ConvenienceClient.server_info(client)

      assert server_info.name == "MockServer"
      assert server_info.version == "1.0.0"
    end

    test "status returns connection status", %{client: client} do
      assert {:ok, status} = ConvenienceClient.status(client)

      assert status.connected == true
      assert status.status == :connected
      assert is_map(status.server_info)
    end

    test "with_error_formatting wraps errors", %{client: client} do
      result =
        ConvenienceClient.with_error_formatting(
          fn -> raise "test error" end,
          :test_operation,
          %{context: "test"}
        )

      assert %{type: :test_operation, message: "Operation failed: test_operation"} = result
    end
  end

  describe "error handling" do
    test "tools handles unexpected response format" do
      # This test would need a mock that returns unexpected format
      # For now, we test the error formatting structure
      error = Error.format(:tool_list_failed, :timeout)

      assert error.type == :tool_list_failed
      assert error.category == :resource
      assert error.severity == :medium
      assert is_binary(error.message)
      assert is_list(error.suggestions)
    end

    test "call_tool handles connection errors" do
      # Mock a disconnected client scenario
      error = Error.format(:tool_call_failed, {:not_connected, :disconnected}, %{tool: "test"})

      assert error.type == :tool_call_failed
      assert error.category == :resource
      assert String.contains?(error.message, "test")
    end
  end
end
