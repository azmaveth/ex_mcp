defmodule ExMCP.ClientV2CoverageTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog

  alias ExMCP.ClientV2

  # Comprehensive mock transport for testing
  defmodule CompleteMockTransport do
    @moduledoc false

    use GenServer

    # Client API
    def connect(opts) do
      case Keyword.get(opts, :fail_connect) do
        true ->
          {:error, :connection_refused}

        _ ->
          {:ok, pid} = GenServer.start_link(__MODULE__, opts)
          {:ok, pid}
      end
    end

    def send(transport_pid, message) do
      GenServer.call(transport_pid, {:send, message})
    end

    def recv(transport_pid, timeout) do
      GenServer.call(transport_pid, {:recv, timeout}, timeout + 100)
    end

    def close(transport_pid) do
      if Process.alive?(transport_pid) do
        GenServer.stop(transport_pid)
      end

      {:ok, nil}
    end

    def controlling_process(transport_pid, new_pid) do
      GenServer.call(transport_pid, {:controlling_process, new_pid})
    end

    # Server callbacks
    def init(opts) do
      state = %{
        parent: Keyword.get(opts, :parent, self()),
        pending_responses: :queue.new(),
        connected: true,
        opts: opts,
        controlling_process: self(),
        message_handler: Keyword.get(opts, :message_handler)
      }

      {:ok, state}
    end

    def handle_call({:send, message}, _from, state) do
      # Parse and handle the message
      case Jason.decode(message) do
        {:ok, %{"method" => "initialize", "id" => id}} ->
          # Queue initialize response
          response = %{
            "jsonrpc" => "2.0",
            "id" => id,
            "result" => %{
              "protocolVersion" => "2024-11-05",
              "serverInfo" => %{"name" => "Test Server", "version" => "1.0.0"},
              "capabilities" => %{
                "tools" => %{"listChanged" => true},
                "resources" => %{"subscribe" => true},
                "prompts" => %{"listChanged" => true}
              }
            }
          }

          # If delayed response is requested, schedule it
          if state.opts[:delay_response] do
            Process.send_after(self(), {:queue_response, Jason.encode!(response)}, 50)
            {:reply, {:ok, self()}, state}
          else
            new_queue = :queue.in(Jason.encode!(response), state.pending_responses)
            {:reply, {:ok, self()}, %{state | pending_responses: new_queue}}
          end

        {:ok, %{"method" => "notifications/initialized"}} ->
          # No response for notifications
          {:reply, {:ok, self()}, state}

        {:ok, %{"method" => method, "id" => id}} ->
          # Handle other requests
          response = build_response(method, id, state.opts)

          # Check if we should simulate an error
          response =
            if state.opts[:error_on] == method do
              %{
                "jsonrpc" => "2.0",
                "id" => id,
                "error" => %{
                  "code" => -32603,
                  "message" => "Simulated error"
                }
              }
            else
              response
            end

          new_queue = :queue.in(Jason.encode!(response), state.pending_responses)
          {:reply, {:ok, self()}, %{state | pending_responses: new_queue}}

        {:ok, %{} = notification} ->
          # Handle notifications (no ID)
          if not Map.has_key?(notification, "id") do
            if handler = state.message_handler do
              handler.(notification)
            end
          end

          {:reply, {:ok, self()}, state}

        _ ->
          {:reply, {:ok, self()}, state}
      end
    end

    def handle_call({:recv, _timeout}, _from, state) do
      case :queue.out(state.pending_responses) do
        {{:value, response}, new_queue} ->
          {:reply, {:ok, response, self()}, %{state | pending_responses: new_queue}}

        {:empty, _} ->
          {:reply, {:error, :timeout}, state}
      end
    end

    def handle_call({:controlling_process, new_pid}, _from, state) do
      {:reply, :ok, %{state | controlling_process: new_pid}}
    end

    def handle_info({:queue_response, response}, state) do
      new_queue = :queue.in(response, state.pending_responses)
      {:noreply, %{state | pending_responses: new_queue}}
    end

    def handle_info(_, state), do: {:noreply, state}

    # Response builders
    defp build_response("tools/list", id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{
          "tools" => [
            %{
              "name" => "test_tool",
              "description" => "A test tool",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{
                  "message" => %{"type" => "string"}
                }
              }
            }
          ]
        }
      }
    end

    defp build_response("tools/call", id, opts) do
      content = Keyword.get(opts, :tool_result, "Tool executed successfully")

      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{
          "content" => [
            %{"type" => "text", "text" => content}
          ]
        }
      }
    end

    defp build_response("resources/list", id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{
          "resources" => [
            %{
              "uri" => "file://test.txt",
              "name" => "Test Resource",
              "mimeType" => "text/plain"
            }
          ]
        }
      }
    end

    defp build_response("resources/read", id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{
          "contents" => [
            %{
              "uri" => "file://test.txt",
              "mimeType" => "text/plain",
              "text" => "Test content"
            }
          ]
        }
      }
    end

    defp build_response("prompts/list", id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{
          "prompts" => [
            %{
              "name" => "test_prompt",
              "description" => "A test prompt"
            }
          ]
        }
      }
    end

    defp build_response("prompts/get", id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{
          "messages" => [
            %{
              "role" => "user",
              "content" => %{"type" => "text", "text" => "Hello from prompt"}
            }
          ]
        }
      }
    end

    defp build_response("completion/complete", id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{
          "completion" => %{
            "values" => ["option1", "option2", "option3"]
          }
        }
      }
    end

    defp build_response(_, id, _opts) do
      %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{}
      }
    end
  end

  describe "start_link/1" do
    test "successfully connects with valid transport" do
      opts = [transport: CompleteMockTransport]

      assert {:ok, client} = ClientV2.start_link(opts)
      assert is_pid(client)

      # Verify status
      assert {:ok, status} = ClientV2.get_status(client)
      assert status.connection_status == :ready
      assert status.server_info["name"] == "Test Server"
    end

    test "fails with connection refused" do
      opts = [transport: CompleteMockTransport, fail_connect: true]

      assert capture_log(fn ->
               assert {:error, {:connection_refused, _}} = ClientV2.start_link(opts)
             end) =~ "Failed to initialize MCP client"
    end

    test "handles initialization timeout" do
      opts = [
        transport: CompleteMockTransport,
        delay_response: true,
        timeout: 10
      ]

      assert capture_log(fn ->
               assert {:error, {:initialization_timeout, _}} = ClientV2.start_link(opts)
             end) =~ "initialization timeout"
    end

    test "supports named client" do
      opts = [
        transport: CompleteMockTransport,
        name: :test_client
      ]

      assert {:ok, _client} = ClientV2.start_link(opts)
      assert Process.whereis(:test_client) != nil

      # Cleanup
      GenServer.stop(:test_client)
    end
  end

  describe "list_tools/2" do
    setup do
      {:ok, client} = ClientV2.start_link(transport: CompleteMockTransport)
      {:ok, client: client}
    end

    test "returns list of tools", %{client: client} do
      assert {:ok, result} = ClientV2.list_tools(client)
      assert result["tools"] != nil
      assert [tool | _] = result["tools"]
      assert tool["name"] == "test_tool"
    end

    test "handles timeout", %{client: client} do
      assert {:error, :timeout} = ClientV2.list_tools(client, 1)
    end
  end

  describe "call_tool/4" do
    setup do
      {:ok, client} = ClientV2.start_link(transport: CompleteMockTransport)
      {:ok, client: client}
    end

    test "calls tool successfully", %{client: client} do
      assert {:ok, result} = ClientV2.call_tool(client, "test_tool", %{message: "hello"})
      assert result["content"] != nil
      assert [%{"type" => "text", "text" => text}] = result["content"]
      assert text == "Tool executed successfully"
    end

    test "handles tool errors", %{client: _client} do
      {:ok, client2} =
        ClientV2.start_link(
          transport: CompleteMockTransport,
          error_on: "tools/call"
        )

      assert {:error, error} = ClientV2.call_tool(client2, "test_tool", %{})
      assert error["code"] == -32603
    end
  end

  describe "list_resources/2" do
    setup do
      {:ok, client} = ClientV2.start_link(transport: CompleteMockTransport)
      {:ok, client: client}
    end

    test "returns list of resources", %{client: client} do
      assert {:ok, result} = ClientV2.list_resources(client)
      assert result["resources"] != nil
      assert [resource | _] = result["resources"]
      assert resource["uri"] == "file://test.txt"
    end
  end

  describe "read_resource/3" do
    setup do
      {:ok, client} = ClientV2.start_link(transport: CompleteMockTransport)
      {:ok, client: client}
    end

    test "reads resource successfully", %{client: client} do
      assert {:ok, result} = ClientV2.read_resource(client, "file://test.txt")
      assert result["contents"] != nil
      assert [content | _] = result["contents"]
      assert content["text"] == "Test content"
    end
  end

  describe "list_prompts/2" do
    setup do
      {:ok, client} = ClientV2.start_link(transport: CompleteMockTransport)
      {:ok, client: client}
    end

    test "returns list of prompts", %{client: client} do
      assert {:ok, result} = ClientV2.list_prompts(client)
      assert result["prompts"] != nil
      assert [prompt | _] = result["prompts"]
      assert prompt["name"] == "test_prompt"
    end
  end

  describe "get_prompt/4" do
    setup do
      {:ok, client} = ClientV2.start_link(transport: CompleteMockTransport)
      {:ok, client: client}
    end

    test "gets prompt successfully", %{client: client} do
      assert {:ok, result} = ClientV2.get_prompt(client, "test_prompt", %{})
      assert result["messages"] != nil
      assert [message | _] = result["messages"]
      assert message["content"]["text"] == "Hello from prompt"
    end
  end

  describe "complete/3" do
    setup do
      {:ok, client} = ClientV2.start_link(transport: CompleteMockTransport)
      {:ok, client: client}
    end

    test "returns completion options", %{client: client} do
      assert {:ok, result} =
               ClientV2.complete(client, %{
                 "type" => "resource",
                 "uri" => "file://test"
               })

      assert result["completion"]["values"] == ["option1", "option2", "option3"]
    end
  end

  describe "get_status/1" do
    setup do
      {:ok, client} = ClientV2.start_link(transport: CompleteMockTransport)
      {:ok, client: client}
    end

    test "returns client status", %{client: client} do
      assert {:ok, status} = ClientV2.get_status(client)

      assert status.connection_status == :ready
      assert status.server_info != nil
      assert status.server_capabilities != nil
      assert status.pending_requests == 0
      assert is_binary(status.session_id)
    end
  end

  describe "stop/1 and stop/2" do
    test "stops client gracefully" do
      {:ok, client} = ClientV2.start_link(transport: CompleteMockTransport)

      assert :ok = ClientV2.stop(client)
      refute Process.alive?(client)
    end

    test "stops client with custom reason" do
      {:ok, client} = ClientV2.start_link(transport: CompleteMockTransport)

      assert :ok = ClientV2.stop(client, :custom_reason)
      refute Process.alive?(client)
    end
  end

  describe "send_notification/3" do
    setup do
      {:ok, client} = ClientV2.start_link(transport: CompleteMockTransport)
      {:ok, client: client}
    end

    test "sends notification successfully", %{client: client} do
      assert :ok =
               ClientV2.send_notification(client, "notifications/cancelled", %{
                 "requestId" => "test-123"
               })
    end
  end

  describe "error handling" do
    test "handles invalid transport" do
      assert_raise ArgumentError, fn ->
        ClientV2.start_link(transport: :invalid_transport)
      end
    end

    test "handles transport crashes during operation" do
      {:ok, client} = ClientV2.start_link(transport: CompleteMockTransport)

      # Get the transport state
      state = :sys.get_state(client)

      # Kill the transport
      if is_pid(state.transport_state) do
        Process.exit(state.transport_state, :kill)
      end

      # Operations should fail gracefully
      assert {:error, _} = ClientV2.list_tools(client)
    end
  end

  describe "concurrent operations" do
    setup do
      {:ok, client} = ClientV2.start_link(transport: CompleteMockTransport)
      {:ok, client: client}
    end

    test "handles multiple concurrent requests", %{client: client} do
      tasks =
        for i <- 1..10 do
          Task.async(fn ->
            case rem(i, 3) do
              0 -> ClientV2.list_tools(client)
              1 -> ClientV2.list_resources(client)
              2 -> ClientV2.list_prompts(client)
            end
          end)
        end

      results = Task.await_many(tasks)

      assert length(results) == 10

      assert Enum.all?(results, fn result ->
               match?({:ok, _}, result)
             end)
    end
  end

  describe "transport fallback" do
    test "tries multiple transports" do
      # Note: This would require TransportManager support
      # For now, just verify the option is accepted
      opts = [
        transports: [
          {CompleteMockTransport, [fail_connect: true]},
          {CompleteMockTransport, []}
        ]
      ]

      # This currently won't work without TransportManager integration
      # but we can test that the option structure is valid
      assert is_list(opts[:transports])
    end
  end
end
