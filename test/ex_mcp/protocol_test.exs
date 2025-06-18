defmodule ExMCP.ProtocolTest do
  use ExUnit.Case

  @moduletag :protocol
  @moduletag :unit

  alias ExMCP.Protocol

  describe "client request encoding" do
    test "encode_initialize/1" do
      client_info = %{name: "test-client", version: "1.0.0"}
      msg = Protocol.encode_initialize(client_info)

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "initialize"
      assert msg["params"]["protocolVersion"] == "2025-03-26"
      assert msg["params"]["clientInfo"] == client_info
      assert is_integer(msg["id"])
    end

    test "encode_initialized/0" do
      msg = Protocol.encode_initialized()

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "notifications/initialized"
      assert msg["params"] == %{}
      refute Map.has_key?(msg, "id")
    end

    test "encode_list_tools/0" do
      msg = Protocol.encode_list_tools()

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "tools/list"
      assert msg["params"] == %{}
      assert is_integer(msg["id"])
    end

    test "encode_call_tool/2" do
      msg = Protocol.encode_call_tool("calculator", %{"expression" => "2 + 2"})

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "tools/call"
      assert msg["params"]["name"] == "calculator"
      assert msg["params"]["arguments"] == %{"expression" => "2 + 2"}
      assert is_integer(msg["id"])
    end
  end

  describe "server response encoding" do
    test "encode_response/2" do
      result = %{tools: []}
      id = 123
      msg = Protocol.encode_response(result, id)

      assert msg["jsonrpc"] == "2.0"
      assert msg["result"] == result
      assert msg["id"] == id
    end

    test "encode_error/4" do
      msg = Protocol.encode_error(-32601, "Method not found", nil, 123)

      assert msg["jsonrpc"] == "2.0"
      assert msg["error"]["code"] == -32601
      assert msg["error"]["message"] == "Method not found"
      assert msg["id"] == 123
      refute Map.has_key?(msg["error"], "data")
    end

    test "encode_error/4 with data" do
      data = %{details: "Additional info"}
      msg = Protocol.encode_error(-32602, "Invalid params", data, 123)

      assert msg["error"]["data"] == data
    end

    test "encode_notification/2" do
      msg = Protocol.encode_notification("test/event", %{foo: "bar"})

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "test/event"
      assert msg["params"] == %{foo: "bar"}
      refute Map.has_key?(msg, "id")
    end
  end

  describe "message parsing" do
    test "parse_message/1 with request" do
      json = ~s({"jsonrpc":"2.0","method":"tools/list","params":{},"id":1})

      assert {:request, "tools/list", %{}, 1} = Protocol.parse_message(json)
    end

    test "parse_message/1 with notification" do
      json = ~s({"jsonrpc":"2.0","method":"notifications/initialized","params":{}})

      assert {:notification, "notifications/initialized", %{}} = Protocol.parse_message(json)
    end

    test "parse_message/1 with result" do
      json = ~s({"jsonrpc":"2.0","result":{"tools":[]},"id":1})

      assert {:result, %{"tools" => []}, 1} = Protocol.parse_message(json)
    end

    test "parse_message/1 with error" do
      json = ~s({"jsonrpc":"2.0","error":{"code":-32601,"message":"Not found"},"id":1})

      assert {:error, %{"code" => -32601, "message" => "Not found"}, 1} =
               Protocol.parse_message(json)
    end

    test "parse_message/1 with invalid JSON" do
      assert {:error, :invalid_message} = Protocol.parse_message("not json")
    end

    test "parse_message/1 with invalid structure" do
      assert {:error, :invalid_message} = Protocol.parse_message(%{})
    end
  end

  describe "utilities" do
    test "generate_id/0 generates unique IDs" do
      ids = for _ <- 1..100, do: Protocol.generate_id()
      assert length(Enum.uniq(ids)) == 100
    end

    test "encode_to_string/1" do
      msg = %{test: "value"}
      assert {:ok, json} = Protocol.encode_to_string(msg)
      assert is_binary(json)
      assert {:ok, decoded} = Jason.decode(json)
      assert decoded == %{"test" => "value"}
    end

    test "error code constants" do
      assert Protocol.parse_error() == -32700
      assert Protocol.invalid_request() == -32600
      assert Protocol.method_not_found() == -32601
      assert Protocol.invalid_params() == -32602
      assert Protocol.internal_error() == -32603
    end
  end

  describe "roots encoding" do
    test "encode_list_roots/0" do
      msg = Protocol.encode_list_roots()

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "roots/list"
      assert msg["params"] == %{}
      assert is_integer(msg["id"])
    end

    test "encode_roots_changed/0" do
      msg = Protocol.encode_roots_changed()

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "notifications/roots/list_changed"
      assert msg["params"] == %{}
      refute Map.has_key?(msg, "id")
    end
  end

  describe "resource subscriptions encoding" do
    test "encode_subscribe_resource/1" do
      msg = Protocol.encode_subscribe_resource("file:///test.txt")

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "resources/subscribe"
      assert msg["params"]["uri"] == "file:///test.txt"
      assert is_integer(msg["id"])
    end

    test "encode_unsubscribe_resource/1" do
      msg = Protocol.encode_unsubscribe_resource("file:///test.txt")

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "resources/unsubscribe"
      assert msg["params"]["uri"] == "file:///test.txt"
      assert is_integer(msg["id"])
    end
  end

  describe "sampling encoding" do
    test "encode_create_message/1" do
      params = %{
        messages: [
          %{role: "user", content: %{type: "text", text: "Hello"}}
        ],
        max_tokens: 100
      }

      msg = Protocol.encode_create_message(params)

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "sampling/createMessage"
      assert msg["params"] == params
      assert is_integer(msg["id"])
    end
  end
end
