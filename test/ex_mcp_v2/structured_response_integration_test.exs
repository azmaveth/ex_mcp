defmodule ExMCP.StructuredResponseIntegrationTest do
  use ExUnit.Case, async: true

  alias ExMCP.{Response, Error}

  describe "Response integration" do
    test "from_raw_response creates structured response" do
      raw = %{
        "content" => [
          %{"type" => "text", "text" => "Hello World!"}
        ],
        "meta" => %{"timestamp" => "2024-01-01"}
      }

      response = Response.from_raw_response(raw, tool_name: "test_tool")

      assert %Response{} = response
      assert response.tool_name == "test_tool"
      assert response.is_error == false
      assert Response.text_content(response) == "Hello World!"
    end

    test "to_raw converts response back to raw format" do
      response = Response.text("Test message", "test_tool")
      raw = Response.to_raw(response)

      assert raw["content"] == [%{"type" => "text", "text" => "Test message"}]
      refute Map.has_key?(raw, "isError")
    end

    test "error responses are properly structured" do
      response = Response.error("Something failed", "test_tool")

      assert Response.error?(response) == true
      assert response.tool_name == "test_tool"
      assert Response.text_content(response) == "Error: Something failed"
    end
  end

  describe "Error integration" do
    test "from_json_rpc_error creates structured error" do
      json_error = %{
        "code" => -32601,
        "message" => "Method not found",
        "data" => %{"method" => "unknown"}
      }

      error = Error.from_json_rpc_error(json_error, request_id: "123")

      assert %Error{} = error
      assert error.code == -32601
      assert error.message == "Method not found"
      assert error.request_id == "123"
    end

    test "to_json_rpc converts error to JSON-RPC format" do
      error = Error.tool_error("Execution failed", "test_tool")
      json = Error.to_json_rpc(error)

      assert json["code"] == -32000
      assert json["message"] =~ "test_tool"
      assert json["message"] =~ "Execution failed"
    end

    test "error categorization works correctly" do
      tool_error = Error.tool_error("Failed", "tool")
      rpc_error = Error.parse_error("Invalid JSON")
      conn_error = Error.connection_error("Timeout")

      assert Error.mcp_error?(tool_error) == true
      assert Error.json_rpc_error?(rpc_error) == true
      assert Error.mcp_error?(rpc_error) == false
      assert Error.json_rpc_error?(conn_error) == false

      assert Error.category(tool_error) == "Tool Error"
      assert Error.category(rpc_error) == "Parse Error"
      assert Error.category(conn_error) == "Connection Error"
    end
  end
end
