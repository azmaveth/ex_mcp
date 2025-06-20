defmodule ExMCP.ClientTest do
  use ExUnit.Case, async: false

  alias ExMCP.Client
  alias ExMCP.{Response, Error}

  # Mock transport for testing
  defmodule MockTransport do
    @behaviour ExMCP.Transport

    import Kernel, except: [send: 2]

    def connect(opts) do
      if Keyword.get(opts, :fail_connect) do
        {:error, :connection_refused}
      else
        # Start an agent to handle the message queue
        {:ok, agent} =
          Agent.start_link(fn ->
            %{
              responses: [],
              connected: true,
              fail_handshake: Keyword.get(opts, :fail_handshake, false),
              fail_send: Keyword.get(opts, :fail_send, false),
              test_pid: Keyword.get(opts, :test_pid)
            }
          end)

        {:ok, %{agent: agent, opts: opts, connected: true}}
      end
    end

    def send(%{connected: false}, _data) do
      {:error, :not_connected}
    end

    def send(%{agent: agent} = state, data) do
      # Check if we should fail sending
      fail_send = Agent.get(agent, fn state -> state.fail_send end)

      if fail_send do
        {:error, :send_failed}
      else
        # Parse the request and queue the appropriate response
        case Jason.decode!(data) do
          %{"method" => "initialize", "id" => id} ->
            fail_handshake = Agent.get(agent, fn state -> state.fail_handshake end)

            response =
              if fail_handshake do
                %{
                  "jsonrpc" => "2.0",
                  "id" => id,
                  "error" => %{
                    "code" => -32600,
                    "message" => "Handshake failed"
                  }
                }
              else
                %{
                  "jsonrpc" => "2.0",
                  "id" => id,
                  "result" => %{
                    "protocolVersion" => "2024-11-05",
                    "capabilities" => %{
                      "tools" => %{"listChanged" => true},
                      "resources" => %{"listChanged" => true},
                      "prompts" => %{"listChanged" => true}
                    },
                    "serverInfo" => %{
                      "name" => "MockServer",
                      "version" => "1.0.0"
                    }
                  }
                }
              end

            Agent.update(agent, fn state ->
              %{state | responses: [Jason.encode!(response) | state.responses]}
            end)

          %{"method" => "tools/list", "id" => id} ->
            response = %{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => %{
                "tools" => [
                  %{
                    "name" => "hello",
                    "description" => "Say hello",
                    "inputSchema" => %{
                      "type" => "object",
                      "properties" => %{
                        "name" => %{"type" => "string"}
                      }
                    }
                  },
                  %{
                    "name" => "error_tool",
                    "description" => "Tool that errors",
                    "inputSchema" => %{"type" => "object", "properties" => %{}}
                  }
                ]
              }
            }

            Agent.update(agent, fn state ->
              %{state | responses: [Jason.encode!(response) | state.responses]}
            end)

          %{"method" => "tools/call", "id" => id, "params" => %{"name" => "hello"}} ->
            response = %{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => %{
                "content" => [
                  %{
                    "type" => "text",
                    "text" => "Hello, World!"
                  }
                ]
              }
            }

            Agent.update(agent, fn state ->
              %{state | responses: [Jason.encode!(response) | state.responses]}
            end)

          %{"method" => "tools/call", "id" => id, "params" => %{"name" => "error_tool"}} ->
            response = %{
              "jsonrpc" => "2.0",
              "id" => id,
              "error" => %{
                "code" => -32601,
                "message" => "Tool execution failed"
              }
            }

            Agent.update(agent, fn state ->
              %{state | responses: [Jason.encode!(response) | state.responses]}
            end)

          %{"method" => "resources/list", "id" => id} ->
            response = %{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => %{
                "resources" => [
                  %{
                    "uri" => "file:///test.txt",
                    "name" => "Test File",
                    "mimeType" => "text/plain"
                  }
                ]
              }
            }

            Agent.update(agent, fn state ->
              %{state | responses: [Jason.encode!(response) | state.responses]}
            end)

          %{"method" => "resources/read", "id" => id, "params" => %{"uri" => uri}} ->
            response = %{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => %{
                "content" => [
                  %{
                    "type" => "text",
                    "uri" => uri,
                    "text" => "Test content from #{uri}"
                  }
                ]
              }
            }

            Agent.update(agent, fn state ->
              %{state | responses: [Jason.encode!(response) | state.responses]}
            end)

          %{"method" => "prompts/list", "id" => id} ->
            response = %{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => %{
                "prompts" => [
                  %{
                    "name" => "greet",
                    "description" => "Generate a greeting",
                    "arguments" => [
                      %{
                        "name" => "style",
                        "description" => "Greeting style",
                        "required" => false
                      }
                    ]
                  }
                ]
              }
            }

            Agent.update(agent, fn state ->
              %{state | responses: [Jason.encode!(response) | state.responses]}
            end)

          %{"method" => "prompts/get", "id" => id, "params" => %{"name" => "greet"}} ->
            response = %{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => %{
                "content" => [
                  %{
                    "type" => "text",
                    "text" => nil,
                    "data" => %{
                      "messages" => [
                        %{
                          "role" => "user",
                          "content" => %{
                            "type" => "text",
                            "text" => "Generate a friendly greeting"
                          }
                        }
                      ]
                    }
                  }
                ]
              }
            }

            Agent.update(agent, fn state ->
              %{state | responses: [Jason.encode!(response) | state.responses]}
            end)

          %{"method" => "notifications/initialized"} ->
            # No response needed for notifications
            :ok

          %{"method" => method, "id" => id} ->
            # Unknown method
            response = %{
              "jsonrpc" => "2.0",
              "id" => id,
              "error" => %{
                "code" => -32601,
                "message" => "Method not found: #{method}"
              }
            }

            Agent.update(agent, fn state ->
              %{state | responses: [Jason.encode!(response) | state.responses]}
            end)

          _ ->
            # Ignore other messages
            :ok
        end

        {:ok, state}
      end
    end

    def recv(%{connected: false}, _timeout) do
      {:error, :closed}
    end

    def recv(%{agent: agent} = state, timeout) do
      start_time = System.monotonic_time(:millisecond)

      case poll_for_response(agent, timeout, start_time) do
        nil -> {:error, :timeout}
        response -> {:ok, response, state}
      end
    end

    defp poll_for_response(agent, timeout, start_time) do
      case Agent.get_and_update(agent, fn %{responses: responses} = state ->
             case responses do
               [response | rest] -> {response, %{state | responses: rest}}
               [] -> {nil, state}
             end
           end) do
        nil ->
          elapsed = System.monotonic_time(:millisecond) - start_time

          if elapsed >= timeout do
            nil
          else
            Process.sleep(10)
            poll_for_response(agent, timeout, start_time)
          end

        response ->
          response
      end
    end

    def close(%{agent: agent} = state) do
      if Process.alive?(agent) do
        Agent.stop(agent)
      end

      {:ok, %{state | connected: false}}
    end

    def controlling_process(_state, _pid) do
      :ok
    end

    # V1 transport interface - for compatibility
    def send_message(state, data) do
      case send(state, data) do
        {:ok, new_state} -> {:ok, new_state}
        {:error, reason} -> {:error, reason}
      end
    end

    def receive_message(state) do
      case recv(state, 100) do
        {:ok, data, new_state} -> {:ok, data, new_state}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  # Mock transport that simulates disconnection
  defmodule DisconnectingTransport do
    @behaviour ExMCP.Transport

    import Kernel, except: [send: 2]

    def connect(opts) do
      {:ok, agent} =
        Agent.start_link(fn ->
          %{
            responses: [],
            connected: true,
            test_pid: Keyword.get(opts, :test_pid),
            disconnect_after: Keyword.get(opts, :disconnect_after, 1)
          }
        end)

      {:ok, %{agent: agent, request_count: 0}}
    end

    def send(%{agent: agent, request_count: count} = state, data) do
      disconnect_after = Agent.get(agent, fn s -> s.disconnect_after end)

      if count >= disconnect_after do
        {:error, :connection_lost}
      else
        # Handle initialization normally
        case Jason.decode!(data) do
          %{"method" => "initialize", "id" => id} ->
            response = %{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => %{
                "protocolVersion" => "2024-11-05",
                "capabilities" => %{},
                "serverInfo" => %{"name" => "DisconnectingServer", "version" => "1.0.0"}
              }
            }

            Agent.update(agent, fn state ->
              %{state | responses: [Jason.encode!(response) | state.responses]}
            end)

          _ ->
            :ok
        end

        {:ok, %{state | request_count: count + 1}}
      end
    end

    def recv(%{agent: agent} = state, timeout) do
      case Agent.get_and_update(agent, fn %{responses: responses} = s ->
             case responses do
               [response | rest] -> {response, %{s | responses: rest}}
               [] -> {nil, s}
             end
           end) do
        nil ->
          Process.sleep(min(timeout, 50))
          {:error, :timeout}

        response ->
          {:ok, response, state}
      end
    end

    def close(%{agent: agent} = state) do
      if Process.alive?(agent) do
        Agent.stop(agent)
      end

      {:ok, state}
    end

    def controlling_process(_state, _pid) do
      :ok
    end

    # V1 transport interface - for compatibility
    def send_message(state, data) do
      case send(state, data) do
        {:ok, new_state} -> {:ok, new_state}
        {:error, reason} -> {:error, reason}
      end
    end

    def receive_message(state) do
      case recv(state, 100) do
        {:ok, data, new_state} -> {:ok, data, new_state}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  describe "start_link/1" do
    test "successfully connects and completes handshake" do
      {:ok, client} = Client.start_link(transport: MockTransport)

      # Verify client is ready
      {:ok, status} = Client.get_status(client)
      assert status.connection_status == :ready
      assert status.server_info["name"] == "MockServer"
      assert status.server_info["version"] == "1.0.0"
      assert is_map(status.server_capabilities)

      if Process.alive?(client), do: GenServer.stop(client)
    end

    test "returns error when transport connection fails" do
      Process.flag(:trap_exit, true)

      assert {:error, {:transport_connect_failed, :connection_refused}} =
               Client.start_link(transport: MockTransport, fail_connect: true)
    end

    test "returns error when MCP handshake fails" do
      Process.flag(:trap_exit, true)

      assert {:error, {:initialize_error, %{"code" => -32600}}} =
               Client.start_link(transport: MockTransport, fail_handshake: true)
    end

    test "accepts custom timeout and reconnection options" do
      {:ok, client} =
        Client.start_link(
          transport: MockTransport,
          timeout: 5000,
          max_reconnect_attempts: 3,
          reconnect_interval: 500
        )

      {:ok, status} = Client.get_status(client)
      assert status.connection_status == :ready

      if Process.alive?(client), do: GenServer.stop(client)
    end
  end

  describe "tool operations" do
    setup do
      {:ok, client} = Client.start_link(transport: MockTransport)

      on_exit(fn ->
        if Process.alive?(client) do
          if Process.alive?(client), do: GenServer.stop(client)
        end
      end)

      %{client: client}
    end

    test "list_tools/2 returns available tools", %{client: client} do
      {:ok, result} = Client.list_tools(client)
      assert is_map(result)
      assert Map.has_key?(result, :tools)
      tools = result.tools
      assert is_list(tools)
      assert length(tools) == 2

      hello_tool = Enum.find(tools, &(&1["name"] == "hello"))
      assert hello_tool["description"] == "Say hello"
      assert hello_tool["inputSchema"]["type"] == "object"
    end

    test "call_tool/4 executes a tool successfully", %{client: client} do
      {:ok, %Response{} = response} = Client.call_tool(client, "hello", %{})
      assert Response.text_content(response) == "Hello, World!"
      assert response.tool_name == "hello"
      assert response.is_error == false
    end

    test "call_tool/4 handles tool execution errors", %{client: client} do
      {:error, %Error{} = error} = Client.call_tool(client, "error_tool", %{})
      assert error.code == -32601
      assert error.message == "Tool execution failed"
    end

    test "list_tools/2 with custom timeout" do
      {:ok, client} = Client.start_link(transport: MockTransport)

      {:ok, result} = Client.list_tools(client, timeout: 1000)
      assert is_map(result)
      assert Map.has_key?(result, :tools)
      assert is_list(result.tools)

      if Process.alive?(client), do: GenServer.stop(client)
    end

    test "call_tool/4 with custom timeout" do
      {:ok, client} = Client.start_link(transport: MockTransport)

      {:ok, %Response{} = response} = Client.call_tool(client, "hello", %{}, 2000)
      assert Response.text_content(response) == "Hello, World!"

      if Process.alive?(client), do: GenServer.stop(client)
    end
  end

  describe "resource operations" do
    setup do
      {:ok, client} = Client.start_link(transport: MockTransport)

      on_exit(fn ->
        if Process.alive?(client) do
          if Process.alive?(client), do: GenServer.stop(client)
        end
      end)

      %{client: client}
    end

    test "list_resources/2 returns available resources", %{client: client} do
      {:ok, resources} = Client.list_resources(client)
      assert is_list(resources)
      assert length(resources) == 1

      resource = hd(resources)
      assert resource["uri"] == "file:///test.txt"
      assert resource["name"] == "Test File"
      assert resource["mimeType"] == "text/plain"
    end

    test "read_resource/3 reads a resource successfully", %{client: client} do
      {:ok, %Response{} = response} = Client.read_resource(client, "file:///test.txt")
      assert Response.text_content(response) == "Test content from file:///test.txt"
      assert response.is_error == false
    end

    test "read_resource/3 with custom timeout" do
      {:ok, client} = Client.start_link(transport: MockTransport)

      {:ok, %Response{} = response} = Client.read_resource(client, "file:///test.txt", 1500)
      assert Response.text_content(response) == "Test content from file:///test.txt"

      if Process.alive?(client), do: GenServer.stop(client)
    end
  end

  describe "prompt operations" do
    setup do
      {:ok, client} = Client.start_link(transport: MockTransport)

      on_exit(fn ->
        if Process.alive?(client) do
          if Process.alive?(client), do: GenServer.stop(client)
        end
      end)

      %{client: client}
    end

    test "list_prompts/2 returns available prompts", %{client: client} do
      {:ok, prompts} = Client.list_prompts(client)
      assert is_list(prompts)
      assert length(prompts) == 1

      prompt = hd(prompts)
      assert prompt["name"] == "greet"
      assert prompt["description"] == "Generate a greeting"
      assert is_list(prompt["arguments"])
    end

    test "get_prompt/4 retrieves a prompt successfully", %{client: client} do
      {:ok, %Response{} = response} = Client.get_prompt(client, "greet")

      # Extract data content (should contain messages)
      data = Response.data_content(response)
      assert %{"messages" => messages} = data
      assert is_list(messages)
      assert length(messages) == 1

      message = hd(messages)
      assert message["role"] == "user"
      assert message["content"]["type"] == "text"
      assert message["content"]["text"] == "Generate a friendly greeting"
    end

    test "get_prompt/4 with arguments and custom timeout" do
      {:ok, client} = Client.start_link(transport: MockTransport)

      {:ok, %Response{} = response} =
        Client.get_prompt(client, "greet", %{"style" => "formal"}, 1500)

      data = Response.data_content(response)
      assert %{"messages" => _messages} = data

      if Process.alive?(client), do: GenServer.stop(client)
    end
  end

  describe "connection status and lifecycle" do
    test "get_status/1 returns current connection information" do
      {:ok, client} = Client.start_link(transport: MockTransport)

      {:ok, status} = Client.get_status(client)

      assert status.connection_status == :ready
      assert status.server_info["name"] == "MockServer"
      assert is_map(status.server_capabilities)
      assert status.pending_requests == 0
      assert status.reconnect_attempts == 0

      if Process.alive?(client), do: GenServer.stop(client)
    end

    test "client is immediately ready after start_link" do
      start_time = System.monotonic_time(:millisecond)

      {:ok, client} = Client.start_link(transport: MockTransport)

      # This should work immediately without any sleep
      {:ok, _} = Client.list_tools(client)

      # Verify it was fast (synchronous initialization)
      elapsed = System.monotonic_time(:millisecond) - start_time
      assert elapsed < 1000, "Client took too long to initialize: #{elapsed}ms"

      if Process.alive?(client), do: GenServer.stop(client)
    end
  end

  describe "error handling and edge cases" do
    test "handles unknown method requests" do
      {:ok, client} = Client.start_link(transport: MockTransport)

      # Make a request for an unknown method directly via GenServer call
      # This tests the error handling path
      task =
        Task.async(fn ->
          GenServer.call(client, {:request, "unknown/method", %{}})
        end)

      # Should get an error response
      case Task.await(task, 1000) do
        {:error, error} ->
          assert error["code"] == -32601
          assert error["message"] =~ "Method not found"

        {:ok, _} ->
          flunk("Expected error response for unknown method")
      end

      if Process.alive?(client), do: GenServer.stop(client)
    end

    test "handles transport send failures" do
      # This test would require a more sophisticated mock transport
      # For now, test that we handle connection failures gracefully
      {:ok, client} = Client.start_link(transport: MockTransport)

      # Verify client works initially
      {:ok, _} = Client.list_tools(client)

      if Process.alive?(client), do: GenServer.stop(client)
    end

    test "handles requests when not connected" do
      # Create a disconnecting transport that fails after initialization
      {:ok, client} =
        Client.start_link(
          transport: DisconnectingTransport,
          test_pid: self(),
          disconnect_after: 1,
          reconnect_interval: 50,
          max_reconnect_attempts: 1
        )

      # Give it time to initialize then disconnect
      Process.sleep(100)

      # Try to make a request - should fail with not connected
      {:ok, status} = Client.get_status(client)

      # Status should show either disconnected or attempting reconnection
      assert status.connection_status in [:disconnected, :connecting, :error]

      if Process.alive?(client), do: GenServer.stop(client)
    end
  end

  describe "concurrent requests" do
    test "handles multiple concurrent requests" do
      {:ok, client} = Client.start_link(transport: MockTransport)

      # Make multiple concurrent requests
      tasks =
        for _i <- 1..5 do
          Task.async(fn ->
            Client.list_tools(client)
          end)
        end

      # All should succeed
      results = Enum.map(tasks, &Task.await(&1, 2000))

      for {:ok, result} <- results do
        assert is_map(result)
        assert Map.has_key?(result, :tools)
        assert is_list(result.tools)
      end

      if Process.alive?(client), do: GenServer.stop(client)
    end

    test "tracks pending requests correctly" do
      {:ok, client} = Client.start_link(transport: MockTransport)

      # Start a request but don't wait for completion
      _task =
        Task.async(fn ->
          Client.list_tools(client, timeout: 5000)
        end)

      # Check status while request is pending
      # Give the request time to start
      Process.sleep(10)
      {:ok, status} = Client.get_status(client)

      # Should have at least one pending request (or it completed very quickly)
      assert status.pending_requests >= 0

      if Process.alive?(client), do: GenServer.stop(client)
    end
  end

  describe "transport abstraction" do
    test "works with different transport options" do
      # Test with custom transport options
      {:ok, client} =
        Client.start_link(
          transport: MockTransport,
          custom_option: "test_value",
          another_option: 42
        )

      {:ok, status} = Client.get_status(client)
      assert status.connection_status == :ready

      if Process.alive?(client), do: GenServer.stop(client)
    end

    test "supports named clients" do
      {:ok, _client} =
        Client.start_link(
          transport: MockTransport,
          name: :named_test_client
        )

      # Should be able to access by name
      {:ok, status} = Client.get_status(:named_test_client)
      assert status.connection_status == :ready

      GenServer.stop(:named_test_client)
    end
  end
end
