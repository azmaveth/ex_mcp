defmodule ExMCP.Integration.ErrorHandlingTest do
  @moduledoc """
  Integration tests for error handling across the system.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Error
  alias ExMCP.Protocol.ResponseBuilder
  alias ExMCP.TestHelpers

  setup do
    {:ok, server} = TestHelpers.start_test_server(TestHelpers.ErrorTestServer)
    {:ok, server: server}
  end

  describe "error type handling" do
    test "protocol errors are properly formatted", %{server: server} do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 1,
        "params" => %{
          "name" => "protocol_error",
          "arguments" => %{}
        }
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      assert response["error"]["code"] == -32602
      assert response["error"]["message"] == "MCP Protocol Error (-32602): Invalid parameters"
      assert response["error"]["data"] == %{"field" => "name"}
    end

    test "transport errors are properly formatted", %{server: server} do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 2,
        "params" => %{
          "name" => "transport_error",
          "arguments" => %{}
        }
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      assert response["error"]["code"] == -32000
      assert response["error"]["message"] == "Transport error"
      assert response["error"]["data"]["transport"] == "stdio"
      assert response["error"]["data"]["reason"] == ":connection_lost"
      assert response["error"]["data"]["details"] == %{"attempts" => 3}
    end

    test "tool errors are properly formatted", %{server: server} do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 3,
        "params" => %{
          "name" => "tool_error",
          "arguments" => %{}
        }
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      assert response["error"]["code"] == -32000
      assert response["error"]["message"] == "Tool execution error"
      assert response["error"]["data"]["tool"] == "test_tool"
      assert response["error"]["data"]["reason"] == "\"Tool execution failed\""
    end

    test "resource errors are properly formatted", %{server: server} do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "resources/read",
        "id" => 4,
        "params" => %{
          "uri" => "error://test"
        }
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      assert response["error"]["code"] == -32000
      assert response["error"]["message"] == "Resource operation error"
      assert response["error"]["data"]["uri"] == "error://test"
      assert response["error"]["data"]["operation"] == "read"
      assert response["error"]["data"]["reason"] == "\"Resource not found\""
    end

    test "validation errors are properly formatted", %{server: server} do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 5,
        "params" => %{
          "name" => "validation_error",
          "arguments" => %{}
        }
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      assert response["error"]["code"] == -32000
      assert response["error"]["message"] == "Tool execution error"
      assert response["error"]["data"]["tool"] == "validation_error"
    end

    test "generic errors are handled gracefully", %{server: server} do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 6,
        "params" => %{
          "name" => "generic_error",
          "arguments" => %{}
        }
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      assert response["error"]["code"] == -32000
      assert response["error"]["message"] == "Tool execution error"
    end
  end

  describe "error propagation" do
    test "errors in request processing are caught and formatted", %{server: server} do
      # Send invalid request
      request = %{
        "jsonrpc" => "2.0",
        "method" => "invalid/method",
        "id" => 7
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      assert response["error"]["code"] == -32601
      assert response["error"]["message"] =~ "Method not found"
    end

    test "missing tool name results in proper error", %{server: server} do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 8,
        "params" => %{
          "arguments" => %{}
        }
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      assert response["error"]["code"] == -32000
      assert response["error"]["message"] == "Tool execution error"
      assert response["error"]["data"]["tool"] == "unknown"
    end
  end

  describe "ResponseBuilder error integration" do
    test "ResponseBuilder accepts Error structs" do
      error = Error.protocol_error(-32601, "Method not found")
      response = ResponseBuilder.build_error_response(error, 123)

      assert response["jsonrpc"] == "2.0"
      assert response["id"] == 123
      assert response["error"]["code"] == -32601
      assert response["error"]["message"] == "Method not found"
    end

    test "ResponseBuilder accepts all error types" do
      errors = [
        Error.protocol_error(-32600, "Invalid request"),
        Error.transport_error(:http, :timeout),
        Error.tool_error("test", "Failed"),
        Error.resource_error("test://uri", :read, "Not found"),
        Error.validation_error("field", "value", "Invalid")
      ]

      for error <- errors do
        response = ResponseBuilder.build_error_response(error, 456)
        assert response["jsonrpc"] == "2.0"
        assert response["id"] == 456
        assert is_integer(response["error"]["code"])
        assert is_binary(response["error"]["message"])
      end
    end
  end

  describe "error helper functions" do
    test "connection_error creates transport error" do
      error = Error.connection_error("Connection refused")
      assert %Error.TransportError{} = error
      assert error.transport == :connection
      assert error.reason == "Connection refused"
    end

    test "internal_error creates protocol error with correct code" do
      error = Error.internal_error("Unexpected state")
      assert %Error.ProtocolError{} = error
      assert error.code == -32603
      assert error.message == "Unexpected state"
    end

    test "from_json_rpc_error converts JSON-RPC errors" do
      json_error = %{
        "code" => -32700,
        "message" => "Parse error",
        "data" => %{"line" => 5}
      }

      error = Error.from_json_rpc_error(json_error)
      assert %Error.ProtocolError{} = error
      assert error.code == -32700
      assert error.message == "Parse error"
      assert error.data == %{"line" => 5}
    end
  end
end
