defmodule ExMCP.ClientV2MainTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog
  alias ExMCP.ClientV2

  # Mock transport module for testing
  defmodule MockTransport do
    @moduledoc false
    
    def connect(opts) do
      case Keyword.get(opts, :fail_connect) do
        true -> {:error, :connection_refused}
        _ -> {:ok, %{pid: self(), opts: opts}}
      end
    end
    
    def send(%{pid: pid}, message) do
      Kernel.send(pid, {:transport_send, message})
      {:ok, %{pid: pid}}
    end
    
    def recv(%{pid: _pid} = state, timeout) do
      receive do
        {:mock_response, data} -> 
          {:ok, data, state}
        {:mock_error, reason} -> 
          {:error, reason}
      after
        timeout -> {:error, :timeout}
      end
    end
  end

  describe "start_link/1" do
    test "successfully starts client with default options" do
      # Set up mock responses
      test_pid = self()
      
      spawn_link(fn ->
        # Wait for init request
        receive do
          {:transport_send, data} ->
            request = Jason.decode!(data)
            assert request["method"] == "initialize"
            
            # Send init response
            response = %{
              "jsonrpc" => "2.0",
              "id" => request["id"],
              "result" => %{
                "protocolVersion" => "2024-11-05",
                "serverInfo" => %{
                  "name" => "TestServer",
                  "version" => "1.0.0"
                },
                "capabilities" => %{}
              }
            }
            send(test_pid, {:mock_response, Jason.encode!(response)})
        end
        
        # Wait for initialized notification
        receive do
          {:transport_send, data} ->
            notification = Jason.decode!(data)
            assert notification["method"] == "notifications/initialized"
        end
      end)
      
      opts = [transport: MockTransport]
      assert {:ok, client} = ClientV2.start_link(opts)
      assert is_pid(client)
      
      # Verify client is ready
      assert {:ok, status} = ClientV2.get_status(client)
      assert status.connection_status == :ready
    end

    test "fails when transport connection fails" do
      opts = [transport: MockTransport, fail_connect: true]
      
      assert capture_log(fn ->
        assert {:error, {:connection_refused, _}} = ClientV2.start_link(opts)
      end) =~ "Failed to initialize MCP client"
    end

    test "fails when initialize response is invalid" do
      test_pid = self()
      
      spawn_link(fn ->
        receive do
          {:transport_send, _data} ->
            # Send invalid response
            send(test_pid, {:mock_response, "invalid json"})
        end
      end)
      
      opts = [transport: MockTransport]
      
      assert capture_log(fn ->
        assert {:error, _} = ClientV2.start_link(opts)
      end) =~ "Failed to initialize MCP client"
    end

    test "fails when initialize returns error" do
      test_pid = self()
      
      spawn_link(fn ->
        receive do
          {:transport_send, data} ->
            request = Jason.decode!(data)
            
            # Send error response
            response = %{
              "jsonrpc" => "2.0",
              "id" => request["id"],
              "error" => %{
                "code" => -32601,
                "message" => "Method not found"
              }
            }
            send(test_pid, {:mock_response, Jason.encode!(response)})
        end
      end)
      
      opts = [transport: MockTransport]
      
      assert capture_log(fn ->
        assert {:error, {:initialize_error, _}} = ClientV2.start_link(opts)
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
                "protocolVersion" => "2024-11-05",
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
      assert {:ok, _pid} = ClientV2.start_link(opts)
      
      # Verify we can use the name
      assert {:ok, status} = ClientV2.get_status(TestClient)
      assert status.connection_status == :ready
    end
  end

  describe "list_tools/2" do
    setup do
      client = start_test_client()
      {:ok, client: client}
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
      
      assert {:ok, result} = ClientV2.list_tools(client)
      assert %{"tools" => [tool]} = result
      assert tool["name"] == "hello"
    end

    test "handles timeout", %{client: client} do
      assert {:error, _} = ClientV2.list_tools(client, 100)
    end

    test "handles transport errors", %{client: client} do
      # Simulate disconnection
      send(client, {:transport_closed, :connection_lost})
      
      assert {:error, {:not_connected, :disconnected}} = ClientV2.list_tools(client)
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
      
      assert {:ok, result} = ClientV2.call_tool(client, "hello", %{"name" => "world"})
      assert %{"content" => [content]} = result
      assert content["text"] == "Hello, world!"
    end

    test "handles tool errors", %{client: client} do
      spawn_link(fn ->
        receive do
          {:transport_send, data} ->
            request = Jason.decode!(data)
            
            if request["method"] == "tools/call" do
              response = %{
                "jsonrpc" => "2.0",
                "id" => request["id"],
                "error" => %{
                  "code" => -32602,
                  "message" => "Invalid params",
                  "data" => %{"reason" => "Missing required argument"}
                }
              }
              
              send(client, {:transport_message, Jason.encode!(response)})
            end
        end
      end)
      
      assert {:error, error} = ClientV2.call_tool(client, "hello", %{})
      assert error["code"] == -32602
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
      
      assert {:ok, result} = ClientV2.list_resources(client)
      assert %{"resources" => [resource]} = result
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
      
      assert {:ok, result} = ClientV2.read_resource(client, "file:///test.txt")
      assert %{"contents" => [content]} = result
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
      
      assert {:ok, result} = ClientV2.list_prompts(client)
      assert %{"prompts" => [prompt]} = result
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
      
      assert {:ok, result} = ClientV2.get_prompt(client, "greeting", %{"name" => "Alice"})
      assert %{"messages" => [message]} = result
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
      
      assert {:ok, _result} = ClientV2.get_prompt(client, "simple")
    end
  end

  describe "get_status/1" do
    setup do
      client = start_test_client()
      {:ok, client: client}
    end

    test "returns current client status", %{client: client} do
      assert {:ok, status} = ClientV2.get_status(client)
      
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
      assert {:ok, status} = ClientV2.get_status(client)
      assert status.connection_status == :disconnected
      
      # Requests should fail immediately
      assert {:error, {:not_connected, :disconnected}} = ClientV2.list_tools(client)
    end

    test "cancels pending requests on disconnection", %{client: client} do
      # Start a request that won't get a response
      task = Task.async(fn ->
        ClientV2.call_tool(client, "slow", %{})
      end)
      
      # Give it time to register
      Process.sleep(50)
      
      # Disconnect
      send(client, {:transport_closed, :error})
      
      # Task should receive error
      assert {:error, :disconnected} = Task.await(task)
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
      
      log = capture_log(fn ->
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
      log = capture_log(fn ->
        send(client, {:transport_message, "invalid json"})
        Process.sleep(50)
      end)
      
      assert log =~ "Failed to parse transport message"
      
      # Client should still be functional
      assert {:ok, status} = ClientV2.get_status(client)
      assert status.connection_status == :ready
    end

    test "handles receiver task crashes", %{client: client} do
      # Get receiver task info
      {:ok, status} = ClientV2.get_status(client)
      assert status.connection_status == :ready
      
      # Simulate receiver crash
      # This is a bit tricky to test without access to internals
      # But we can verify the client handles DOWN messages
      fake_task_ref = make_ref()
      send(client, {:DOWN, fake_task_ref, :process, self(), :crash})
      
      # Client should still respond
      assert {:ok, _} = ClientV2.get_status(client)
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
                "protocolVersion" => "2024-11-05",
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
      assert {:ok, client} = ClientV2.start_link(transport: MockTransport)
      assert is_pid(client)
    end
  end

  # Helper functions

  defp start_test_client do
    test_pid = self()
    
    spawn_link(fn ->
      # Handle initialization sequence
      receive do
        {:transport_send, data} ->
          request = Jason.decode!(data)
          response = %{
            "jsonrpc" => "2.0",
            "id" => request["id"],
            "result" => %{
              "protocolVersion" => "2024-11-05",
              "serverInfo" => %{
                "name" => "TestServer",
                "version" => "1.0.0"
              },
              "capabilities" => %{
                "tools" => %{},
                "resources" => %{},
                "prompts" => %{}
              }
            }
          }
          send(test_pid, {:mock_response, Jason.encode!(response)})
      end
      
      receive do
        {:transport_send, _} -> :ok
      end
      
      # Keep process alive to handle requests
      Process.sleep(:infinity)
    end)
    
    {:ok, client} = ClientV2.start_link(transport: MockTransport)
    client
  end
end