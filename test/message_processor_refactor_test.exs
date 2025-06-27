defmodule ExMCP.MessageProcessorRefactorTest do
  use ExUnit.Case, async: true

  @moduletag :skip
  @moduletag skip: """
             Incomplete MessageProcessor refactoring. To remove this skip tag, implement:
             1. Complete all handler functions in lib/ex_mcp/message_processor/handlers.ex (currently all have TODO comments)
             2. Implement proper response setting in Conn struct
             3. Complete Dispatcher.dispatch/4 to properly route requests to handlers
             4. Ensure handlers set conn.response field with proper JSON-RPC responses
             """

  alias ExMCP.MessageProcessor.Conn
  alias ExMCP.MessageProcessor.{Dispatcher, Handlers}

  describe "Dispatcher.dispatch/4" do
    test "routes ping requests correctly" do
      conn = %Conn{
        request: %{"method" => "ping", "id" => 123},
        state: %{}
      }

      result = Dispatcher.dispatch(conn, nil, :direct)

      assert result.response == %{
               "jsonrpc" => "2.0",
               "id" => 123,
               "result" => %{}
             }
    end

    test "routes initialize requests with server info" do
      conn = %Conn{
        request: %{
          "method" => "initialize",
          "params" => %{"protocolVersion" => "1.0"},
          "id" => 456
        },
        state: %{}
      }

      handler = TestHandler
      server_info = %{"name" => "test-server", "version" => "1.0"}

      # Mock the handler module
      defmodule TestHandler do
        def handle_initialize(_params, state) do
          {:ok, %{"capabilities" => %{}}, state}
        end
      end

      result = Dispatcher.dispatch(conn, handler, :direct, server_info)

      assert result.response["id"] == 456
      assert result.response["result"]["name"] == "test-server"
    end

    test "routes unknown methods to custom handler" do
      conn = %Conn{
        request: %{
          "method" => "custom/method",
          "params" => %{"foo" => "bar"},
          "id" => 789
        },
        state: %{}
      }

      handler = CustomHandler

      defmodule CustomHandler do
        def handle_custom_request(%{method: "custom/method", params: params}, state) do
          {:ok, %{"received" => params}, state}
        end
      end

      result = Dispatcher.dispatch(conn, handler, :direct)

      assert result.response["result"]["received"]["foo"] == "bar"
    end
  end

  describe "Handlers business logic" do
    test "handle_ping returns empty result" do
      conn = %Conn{response: nil}
      result = Handlers.handle_ping(conn, 123)

      assert result.response == %{
               "jsonrpc" => "2.0",
               "id" => 123,
               "result" => %{}
             }
    end

    test "handle_tools_call atomizes params correctly" do
      conn = %Conn{
        request: %{"method" => "tools/call", "params" => %{"name" => "test"}},
        state: %{}
      }

      handler = ToolHandler

      defmodule ToolHandler do
        def handle_call_tool(%{name: name}, state) do
          # Note: params should be atomized
          assert name == "test"
          {:ok, %{"result" => "success"}, state}
        end
      end

      result = Handlers.handle_tools_call(conn, handler, :direct, %{"name" => "test"}, 123)

      assert result.response["result"]["result"] == "success"
    end
  end

  describe "Backward compatibility" do
    test "all MCP methods are handled" do
      methods = [
        "ping",
        "initialize",
        "tools/list",
        "tools/call",
        "resources/list",
        "resources/read",
        "resources/subscribe",
        "resources/unsubscribe",
        "prompts/list",
        "prompts/get",
        "completion/complete"
      ]

      Enum.each(methods, fn method ->
        conn = %Conn{
          request: %{"method" => method, "params" => %{}, "id" => 1},
          state: %{}
        }

        # Should not raise
        assert %Conn{} = Dispatcher.dispatch(conn, nil, :direct)
      end)
    end
  end
end
