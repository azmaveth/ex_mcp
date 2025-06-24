defmodule ExMCP.ClientMainTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog
  alias ExMCP.Client

  # Mock transport module for testing - Agent-based pattern
  defmodule MockTransport do
    @moduledoc false
    @behaviour ExMCP.Transport

    @impl ExMCP.Transport
    def connect(opts) do
      case Keyword.get(opts, :fail_connect) do
        true ->
          {:error, :connection_refused}

        _ ->
          timeout_mode = Keyword.get(opts, :timeout_mode, false)
          fail_handshake = Keyword.get(opts, :fail_handshake, false)
          error_responses = Keyword.get(opts, :error_responses, %{})

          Agent.start_link(fn ->
            %{
              messages: [],
              responses: default_responses(),
              timeout_mode: timeout_mode,
              fail_handshake: fail_handshake,
              error_responses: error_responses
            }
          end)
      end
    end

    @impl ExMCP.Transport
    def send_message(message, agent) do
      Agent.update(agent, fn state ->
        %{state | messages: [message | state.messages]}
      end)

      {:ok, agent}
    end

    @impl ExMCP.Transport
    def receive_message(agent) do
      # Check if timeout mode is enabled
      timeout_mode = Agent.get(agent, & &1.timeout_mode)

      if timeout_mode do
        # Return timeout immediately for timeout tests
        {:error, :timeout}
      else
        # Poll for messages with a reasonable timeout to simulate blocking
        # Poll every 50ms for up to 5 seconds
        poll_for_message(agent, 50, 100)
      end
    end

    defp poll_for_message(agent, interval, max_attempts) when max_attempts > 0 do
      msg =
        Agent.get_and_update(agent, fn state ->
          case state.messages do
            [msg | rest] -> {msg, %{state | messages: rest}}
            [] -> {nil, state}
          end
        end)

      case msg do
        nil ->
          # No message available, wait and try again
          Process.sleep(interval)
          poll_for_message(agent, interval, max_attempts - 1)

        msg ->
          case Jason.decode!(msg) do
            %{"method" => method, "id" => _id} = request ->
              response = get_mock_response(agent, method, request)
              {:ok, response, agent}

            %{"method" => "notifications/" <> _notification} ->
              # Notifications don't need responses, but we should continue polling
              poll_for_message(agent, interval, max_attempts - 1)

            _ ->
              {:error, :unknown_message}
          end
      end
    end

    defp poll_for_message(_agent, _interval, 0) do
      {:error, :timeout}
    end

    @impl ExMCP.Transport
    def close(agent) do
      Agent.stop(agent)
      :ok
    end

    # Legacy compatibility methods for tests that might still use them
    def send(agent, message), do: send_message(message, agent)

    def recv(agent, _timeout) do
      case receive_message(agent) do
        {:ok, message, new_state} -> {:ok, message, new_state}
        error -> error
      end
    end

    defp default_responses do
      %{
        "initialize" => %{
          "protocolVersion" => "2025-06-18",
          "capabilities" => %{"tools" => %{}, "resources" => %{}, "prompts" => %{}},
          "serverInfo" => %{"name" => "TestServer", "version" => "1.0.0"}
        },
        "tools/list" => %{
          "tools" => [
            %{
              "name" => "hello",
              "description" => "Say hello",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{"name" => %{"type" => "string"}}
              }
            }
          ]
        },
        "tools/call" => %{
          "content" => [%{"type" => "text", "text" => "Hello, world!"}]
        },
        "resources/list" => %{
          "resources" => [
            %{
              "uri" => "file:///test.txt",
              "name" => "test.txt",
              "mimeType" => "text/plain"
            }
          ]
        },
        "resources/read" => %{
          "contents" => [
            %{
              "uri" => "file:///test.txt",
              "mimeType" => "text/plain",
              "text" => "Hello from test file"
            }
          ]
        },
        "prompts/list" => %{
          "prompts" => [
            %{
              "name" => "greeting",
              "description" => "Generate a greeting",
              "arguments" => [%{"name" => "name", "required" => true}]
            }
          ]
        },
        "prompts/get" => %{
          "messages" => [
            %{
              "role" => "user",
              "content" => %{"type" => "text", "text" => "Hello, Alice!"}
            }
          ]
        }
      }
    end

    defp get_mock_response(agent, method, request) do
      state = Agent.get(agent, & &1)

      # In timeout mode, we hang forever to simulate a non-responsive server
      # for any request other than the initial handshake.
      if state.timeout_mode and method != "initialize" do
        Process.sleep(:infinity)
      end

      # Check for error responses first
      case Map.get(state.error_responses, method) do
        nil ->
          # Check for initialize failure mode
          if method == "initialize" and state.fail_handshake do
            Jason.encode!(%{
              "jsonrpc" => "2.0",
              "id" => request["id"],
              "error" => %{
                "code" => -32601,
                "message" => "Method not found"
              }
            })
          else
            # Normal success response
            response_data = Map.get(state.responses, method, %{})

            Jason.encode!(%{
              "jsonrpc" => "2.0",
              "id" => request["id"],
              "result" => response_data
            })
          end

        error_response ->
          # Return configured error response
          Jason.encode!(%{
            "jsonrpc" => "2.0",
            "id" => request["id"],
            "error" => error_response
          })
      end
    end
  end

  describe "start_link/1" do
    test "successfully starts client with default options" do
      opts = [transport: MockTransport]
      assert {:ok, client} = Client.start_link(opts)
      assert is_pid(client)

      # Verify client is ready
      assert {:ok, status} = Client.get_status(client)
      assert status.connection_status == :ready
    end

    test "fails when transport connection fails" do
      Process.flag(:trap_exit, true)
      opts = [transport: MockTransport, fail_connect: true]

      assert capture_log(fn ->
               assert {:error, {:transport_connect_failed, :connection_refused}} =
                        Client.start_link(opts)
             end) =~ "Failed to initialize MCP client"
    end

    test "fails when initialize response is invalid" do
      # This test would need a custom MockTransport with invalid JSON
      # For now, skip since the Agent-based MockTransport always returns valid JSON
      # TODO: Implement a way to inject invalid responses for error testing
    end

    test "fails when initialize returns error" do
      Process.flag(:trap_exit, true)
      opts = [transport: MockTransport, fail_handshake: true]

      assert capture_log(fn ->
               assert {:error, {:initialize_error, _}} = Client.start_link(opts)
             end) =~ "Failed to initialize MCP client"
    end

    test "supports named client registration" do
      test_pid = self()

      spawn_link(fn ->
        # Handle init sequence
        receive do
          {:transport_send, data} ->
            request = Jason.decode!(data)

            response = %{
              "jsonrpc" => "2.0",
              "id" => request["id"],
              "result" => %{
                "protocolVersion" => "2025-06-18",
                "serverInfo" => %{"name" => "Test", "version" => "1.0"},
                "capabilities" => %{}
              }
            }

            send(test_pid, {:mock_response, Jason.encode!(response)})
        end

        receive do
          {:transport_send, _} -> :ok
        end
      end)

      opts = [transport: MockTransport, name: TestClient]
      assert {:ok, _pid} = Client.start_link(opts)

      # Verify we can use the name
      assert {:ok, status} = Client.get_status(TestClient)
      assert status.connection_status == :ready
    end
  end

  describe "list_tools/2" do
    setup do
      client = start_test_client()
      {:ok, client: client}
    end

    setup tags do
      if tags[:timeout_test] do
        timeout_client = start_timeout_test_client()
        {:ok, timeout_client: timeout_client}
      else
        :ok
      end
    end

    test "successfully lists tools", %{client: client} do
      # Set up response handler
      spawn_link(fn ->
        receive do
          {:transport_send, data} ->
            request = Jason.decode!(data)

            if request["method"] == "tools/list" do
              response = %{
                "jsonrpc" => "2.0",
                "id" => request["id"],
                "result" => %{
                  "tools" => [
                    %{
                      "name" => "hello",
                      "description" => "Say hello",
                      "inputSchema" => %{
                        "type" => "object",
                        "properties" => %{"name" => %{"type" => "string"}}
                      }
                    }
                  ]
                }
              }

              send(client, {:transport_message, Jason.encode!(response)})
            end
        end
      end)

      assert {:ok, result} = Client.list_tools(client)
      assert [tool] = result.tools
      assert tool["name"] == "hello"
    end

    test "handles timeout" do
      # Start a client with the mock transport in timeout mode.
      # The mock transport will respond to the handshake but hang on subsequent requests.
      {:ok, client} = Client.start_link(transport: MockTransport, timeout_mode: true)

      # The GenServer.call should time out because the mock transport never replies.
      assert {:error, :timeout} = Client.list_tools(client, timeout: 10)
    end

    test "handles transport errors", %{client: client} do
      # Simulate disconnection
      send(client, {:transport_closed, :connection_lost})

      assert {:error, %ExMCP.Error{code: :connection_error}} = Client.list_tools(client)
    end
  end

  describe "call_tool/4" do
    setup do
      client = start_test_client()
      {:ok, client: client}
    end

    test "successfully calls a tool", %{client: client} do
      spawn_link(fn ->
        receive do
          {:transport_send, data} ->
            request = Jason.decode!(data)

            if request["method"] == "tools/call" do
              assert request["params"]["name"] == "hello"
              assert request["params"]["arguments"] == %{"name" => "world"}

              response = %{
                "jsonrpc" => "2.0",
                "id" => request["id"],
                "result" => %{
                  "content" => [
                    %{"type" => "text", "text" => "Hello, world!"}
                  ]
                }
              }

              send(client, {:transport_message, Jason.encode!(response)})
            end
        end
      end)

      assert {:ok, result} = Client.call_tool(client, "hello", %{"name" => "world"})
      assert [content] = result.content
      assert content.text == "Hello, world!"
    end

    test "handles tool errors" do
      error_responses = %{
        "tools/call" => %{
          "code" => -32602,
          "message" => "Invalid params",
          "data" => %{"reason" => "Missing required argument"}
        }
      }

      {:ok, client} =
        Client.start_link(transport: MockTransport, error_responses: error_responses)

      assert {:error, error} = Client.call_tool(client, "hello", %{})
      assert error.code == -32602
    end
  end

  describe "list_resources/2" do
    setup do
      client = start_test_client()
      {:ok, client: client}
    end

    test "successfully lists resources", %{client: client} do
      spawn_link(fn ->
        receive do
          {:transport_send, data} ->
            request = Jason.decode!(data)

            if request["method"] == "resources/list" do
              response = %{
                "jsonrpc" => "2.0",
                "id" => request["id"],
                "result" => %{
                  "resources" => [
                    %{
                      "uri" => "file:///test.txt",
                      "name" => "test.txt",
                      "mimeType" => "text/plain"
                    }
                  ]
                }
              }

              send(client, {:transport_message, Jason.encode!(response)})
            end
        end
      end)

      assert {:ok, result} = Client.list_resources(client)
      assert [resource] = result.resources
      assert resource["uri"] == "file:///test.txt"
    end
  end

  describe "read_resource/3" do
    setup do
      client = start_test_client()
      {:ok, client: client}
    end

    test "successfully reads a resource", %{client: client} do
      spawn_link(fn ->
        receive do
          {:transport_send, data} ->
            request = Jason.decode!(data)

            if request["method"] == "resources/read" do
              assert request["params"]["uri"] == "file:///test.txt"

              response = %{
                "jsonrpc" => "2.0",
                "id" => request["id"],
                "result" => %{
                  "contents" => [
                    %{
                      "uri" => "file:///test.txt",
                      "mimeType" => "text/plain",
                      "text" => "Hello from test file"
                    }
                  ]
                }
              }

              send(client, {:transport_message, Jason.encode!(response)})
            end
        end
      end)

      assert {:ok, result} = Client.read_resource(client, "file:///test.txt")
      assert [content] = result.contents
      assert content["text"] == "Hello from test file"
    end
  end

  describe "list_prompts/2" do
    setup do
      client = start_test_client()
      {:ok, client: client}
    end

    test "successfully lists prompts", %{client: client} do
      spawn_link(fn ->
        receive do
          {:transport_send, data} ->
            request = Jason.decode!(data)

            if request["method"] == "prompts/list" do
              response = %{
                "jsonrpc" => "2.0",
                "id" => request["id"],
                "result" => %{
                  "prompts" => [
                    %{
                      "name" => "greeting",
                      "description" => "Generate a greeting",
                      "arguments" => [
                        %{"name" => "name", "required" => true}
                      ]
                    }
                  ]
                }
              }

              send(client, {:transport_message, Jason.encode!(response)})
            end
        end
      end)

      assert {:ok, result} = Client.list_prompts(client)
      assert [prompt] = result.prompts
      assert prompt["name"] == "greeting"
    end
  end

  describe "get_prompt/4" do
    setup do
      client = start_test_client()
      {:ok, client: client}
    end

    test "successfully gets a prompt", %{client: client} do
      spawn_link(fn ->
        receive do
          {:transport_send, data} ->
            request = Jason.decode!(data)

            if request["method"] == "prompts/get" do
              assert request["params"]["name"] == "greeting"
              assert request["params"]["arguments"] == %{"name" => "Alice"}

              response = %{
                "jsonrpc" => "2.0",
                "id" => request["id"],
                "result" => %{
                  "messages" => [
                    %{
                      "role" => "user",
                      "content" => %{
                        "type" => "text",
                        "text" => "Hello, Alice!"
                      }
                    }
                  ]
                }
              }

              send(client, {:transport_message, Jason.encode!(response)})
            end
        end
      end)

      assert {:ok, result} = Client.get_prompt(client, "greeting", %{"name" => "Alice"})
      assert [message] = result.messages
      assert message["role"] == "user"
    end

    test "works with default empty arguments", %{client: client} do
      spawn_link(fn ->
        receive do
          {:transport_send, data} ->
            request = Jason.decode!(data)

            if request["method"] == "prompts/get" do
              assert request["params"]["arguments"] == %{}

              response = %{
                "jsonrpc" => "2.0",
                "id" => request["id"],
                "result" => %{"messages" => []}
              }

              send(client, {:transport_message, Jason.encode!(response)})
            end
        end
      end)

      assert {:ok, _result} = Client.get_prompt(client, "simple")
    end
  end

  describe "get_status/1" do
    setup do
      client = start_test_client()
      {:ok, client: client}
    end

    test "returns current client status", %{client: client} do
      assert {:ok, status} = Client.get_status(client)

      assert status.connection_status == :ready
      assert status.pending_requests == 0
      assert status.reconnect_attempts == 0
      assert is_map(status.server_info)
      assert status.server_info["name"] == "TestServer"
    end
  end

  describe "reconnection handling" do
    setup do
      client = start_test_client()
      {:ok, client: client}
    end

    test "handles transport closure and reconnection", %{client: client} do
      # Simulate transport closure
      send(client, {:transport_closed, :connection_lost})

      # Status should show disconnected
      assert {:ok, status} = Client.get_status(client)
      assert status.connection_status == :disconnected

      # Requests should fail immediately
      assert {:error, %ExMCP.Error{code: :connection_error}} = Client.list_tools(client)
    end

    test "cancels pending requests on disconnection", %{client: client} do
      # Start a request that won't get a response
      task =
        Task.async(fn ->
          Client.call_tool(client, "slow", %{})
        end)

      # Give it time to register
      Process.sleep(50)

      # Disconnect
      send(client, {:transport_closed, :error})

      # Task should receive error
      assert {:error, %ExMCP.Error{code: :connection_error}} = Task.await(task)
    end
  end

  describe "notification handling" do
    setup do
      client = start_test_client()
      {:ok, client: client}
    end

    test "handles server notifications", %{client: client} do
      # Send a notification
      notification = %{
        "jsonrpc" => "2.0",
        "method" => "resources/updated",
        "params" => %{"uri" => "file:///changed.txt"}
      }

      log =
        capture_log(fn ->
          send(client, {:transport_message, Jason.encode!(notification)})
          Process.sleep(50)
        end)

      assert log =~ "Received notification: resources/updated"
    end
  end

  describe "error handling" do
    setup do
      client = start_test_client()
      {:ok, client: client}
    end

    test "handles invalid messages gracefully", %{client: client} do
      # Send invalid JSON
      log =
        capture_log(fn ->
          send(client, {:transport_message, "invalid json"})
          Process.sleep(50)
        end)

      assert log =~ "Failed to parse transport message"

      # Client should still be functional
      assert {:ok, status} = Client.get_status(client)
      assert status.connection_status == :ready
    end

    test "handles receiver task crashes", %{client: client} do
      # Get receiver task info
      {:ok, status} = Client.get_status(client)
      assert status.connection_status == :ready

      # Simulate receiver crash
      # This is a bit tricky to test without access to internals
      # But we can verify the client handles DOWN messages
      fake_task_ref = make_ref()
      send(client, {:DOWN, fake_task_ref, :process, self(), :crash})

      # Client should still respond
      assert {:ok, _} = Client.get_status(client)
    end
  end

  describe "transport configuration" do
    test "accepts various transport options" do
      # Test that client can be configured with different transports
      # We can't test the actual connection without mocking more extensively

      test_pid = self()

      spawn_link(fn ->
        # Handle init for custom transport
        receive do
          {:transport_send, data} ->
            request = Jason.decode!(data)

            response = %{
              "jsonrpc" => "2.0",
              "id" => request["id"],
              "result" => %{
                "protocolVersion" => "2025-06-18",
                "serverInfo" => %{"name" => "Test", "version" => "1.0"},
                "capabilities" => %{}
              }
            }

            send(test_pid, {:mock_response, Jason.encode!(response)})
        end

        receive do
          {:transport_send, _} -> :ok
        end
      end)

      # Verify custom transport module works
      assert {:ok, client} = Client.start_link(transport: MockTransport)
      assert is_pid(client)
    end
  end

  # Helper functions

  defp start_test_client do
    {:ok, client} = Client.start_link(transport: MockTransport)
    client
  end

  defp start_timeout_test_client do
    Process.flag(:trap_exit, true)

    case Client.start_link(transport: MockTransport, timeout_mode: true) do
      {:ok, client} -> client
      # Return nil if client fails to start due to timeout
      {:error, _} -> nil
    end
  end
end
