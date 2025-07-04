defmodule ExMCP.Integration.ServerRefactorTest do
  @moduledoc """
  Integration tests to verify that the refactored server components work together correctly.
  Tests the interaction between ResponseBuilder, RequestTracker, RequestProcessor, and CodeGenerator.
  """

  use ExUnit.Case, async: true

  alias ExMCP.DSL.CodeGenerator
  alias ExMCP.Protocol.{RequestProcessor, RequestTracker, ResponseBuilder}
  alias ExMCP.TestHelpers

  setup do
    {:ok, server} = TestHelpers.start_test_server(TestHelpers.RefactoredTestServer)
    {:ok, server: server}
  end

  describe "component integration" do
    test "ResponseBuilder is used for all responses", %{server: server} do
      # Test successful response
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/list",
        "id" => 1
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      assert response["jsonrpc"] == "2.0"
      assert response["id"] == 1
      assert Map.has_key?(response, "result")
      assert is_list(response["result"]["tools"])
    end

    test "RequestTracker properly tracks pending requests", %{server: server} do
      # Start an async tool call
      task =
        Task.async(fn ->
          request = %{
            "jsonrpc" => "2.0",
            "method" => "tools/call",
            "id" => "async-1",
            "params" => %{
              "name" => "test_tool",
              "arguments" => %{}
            }
          }

          GenServer.call(server, {:process_request, request}, 5000)
        end)

      # Give it time to register
      Process.sleep(50)

      # Check server state has pending request
      state = :sys.get_state(server)
      assert Map.has_key?(state.pending_requests, "async-1")

      # Wait for completion
      {:ok, response} = Task.await(task)
      assert response["result"]["content"] == [%{"type" => "text", "text" => "Tool executed"}]

      # Verify request is no longer pending
      state = :sys.get_state(server)
      refute Map.has_key?(state.pending_requests, "async-1")
    end

    test "RequestProcessor routes all method types correctly", %{server: server} do
      # Test different method types
      methods = [
        {"initialize", %{"protocolVersion" => "2025-06-18"}},
        {"tools/list", %{}},
        {"resources/list", %{}},
        {"prompts/list", %{}}
      ]

      for {method, params} <- methods do
        request = %{
          "jsonrpc" => "2.0",
          "method" => method,
          "id" => method,
          "params" => params
        }

        {:ok, response} = GenServer.call(server, {:process_request, request})

        assert response["jsonrpc"] == "2.0"
        assert response["id"] == method
        assert Map.has_key?(response, "result")
        refute Map.has_key?(response, "error")
      end
    end

    test "CodeGenerator produces working DSL code", %{server: server} do
      # The server should have all DSL-generated functions
      state = :sys.get_state(server)
      module = state.__module__

      # Check generated functions exist
      assert function_exported?(module, :get_tools, 0)
      assert function_exported?(module, :get_resources, 0)
      assert function_exported?(module, :get_prompts, 0)
      assert function_exported?(module, :get_server_info_from_opts, 0)
      assert function_exported?(module, :get_capabilities, 0)

      # Verify they return expected data
      tools = module.get_tools()
      assert is_map(tools)
      assert Map.has_key?(tools, "test_tool")

      resources = module.get_resources()
      assert is_map(resources)
      assert Map.has_key?(resources, "test://resource")

      prompts = module.get_prompts()
      assert is_map(prompts)
      assert Map.has_key?(prompts, "test_prompt")
    end
  end

  describe "error handling integration" do
    test "errors are properly formatted through the pipeline", %{server: server} do
      # Test unknown method
      request = %{
        "jsonrpc" => "2.0",
        "method" => "unknown/method",
        "id" => "error-1"
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      assert response["error"]["code"] == -32601
      assert response["error"]["message"] =~ "Method not found"
      assert response["id"] == "error-1"
    end

    test "invalid requests are handled gracefully", %{server: server} do
      # Missing method
      request = %{
        "jsonrpc" => "2.0",
        "id" => "invalid-1"
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      assert response["error"]["code"] == -32600
      assert response["error"]["message"] == "Invalid Request"
    end
  end

  describe "notifications" do
    test "notifications don't generate responses", %{server: server} do
      # Send initialized notification
      notification = %{
        "jsonrpc" => "2.0",
        "method" => "notifications/initialized"
      }

      result = GenServer.call(server, {:process_request, notification})

      # Should return :ok for notifications
      assert result == :ok
    end
  end

  describe "concurrent operations" do
    test "multiple concurrent requests are handled correctly", %{server: server} do
      # Launch multiple concurrent requests
      tasks =
        for i <- 1..10 do
          Task.async(fn ->
            request = %{
              "jsonrpc" => "2.0",
              "method" => "tools/call",
              "id" => "concurrent-#{i}",
              "params" => %{
                "name" => "test_tool",
                "arguments" => %{"index" => i}
              }
            }

            GenServer.call(server, {:process_request, request}, 5000)
          end)
        end

      # Collect all responses
      responses =
        Enum.map(tasks, fn task ->
          {:ok, response} = Task.await(task)
          response
        end)

      # Verify all succeeded
      assert length(responses) == 10

      for response <- responses do
        assert response["result"]["content"] == [%{"type" => "text", "text" => "Tool executed"}]
      end
    end
  end

  describe "state management" do
    test "server state includes __module__ for dynamic dispatch", %{server: server} do
      state = :sys.get_state(server)
      assert state.__module__ == TestHelpers.RefactoredTestServer
    end

    test "protocol version is stored after initialization", %{server: server} do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "initialize",
        "id" => "init-1",
        "params" => %{
          "protocolVersion" => "2025-06-18",
          "clientInfo" => %{
            "name" => "test-client",
            "version" => "1.0.0"
          }
        }
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      assert response["result"]["protocolVersion"] == "2025-06-18"

      # Check state was updated
      state = :sys.get_state(server)
      assert state.protocol_version == "2025-06-18"
    end
  end
end
