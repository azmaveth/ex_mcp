defmodule ExMCP.Protocol.RequestProcessorTest do
  use ExUnit.Case, async: true

  alias ExMCP.Protocol.RequestProcessor

  defmodule TestHandler do
    def get_server_info_from_opts, do: %{"name" => "test-server", "version" => "1.0"}
    def get_capabilities, do: %{"tools" => true}
    def get_tools, do: %{"test_tool" => %{"name" => "test_tool", "description" => "Test"}}
    def get_resources, do: %{"test://resource" => %{"uri" => "test://resource", "name" => "Test"}}
    def get_prompts, do: %{"test_prompt" => %{"name" => "test_prompt", "description" => "Test"}}

    def handle_initialize(_params, state) do
      {:ok, %{"custom" => true}, state}
    end

    def handle_tool_call("test_tool", args, state) do
      {:ok, %{content: [%{"type" => "text", "text" => "Result: #{inspect(args)}"}]}, state}
    end

    def handle_tool_call("error_tool", _args, state) do
      {:error, "Tool error", state}
    end

    def handle_tool_call(nil, _args, state) do
      {:error, "No tool name provided", state}
    end

    def handle_tool_call(_other, _args, state) do
      {:error, "Unknown tool", state}
    end

    def handle_resource_read("test://resource", _uri, state) do
      {:ok, %{"type" => "text", "text" => "Resource content"}, state}
    end

    def handle_resource_read(_uri, _full_uri, state) do
      {:error, "Unknown resource", state}
    end

    def handle_prompt_get("test_prompt", args, state) do
      {:ok, %{messages: [%{"role" => "user", "content" => "Prompt: #{inspect(args)}"}]}, state}
    end

    def handle_prompt_get(_name, _args, state) do
      {:error, "Unknown prompt", state}
    end
  end

  defmodule MinimalHandler do
    # No custom implementations
  end

  setup do
    state = %{__module__: TestHandler}
    minimal_state = %{__module__: MinimalHandler}
    {:ok, state: state, minimal_state: minimal_state}
  end

  describe "process/2" do
    test "handles invalid request format", %{state: state} do
      {:response, response, ^state} = RequestProcessor.process(%{}, state)

      assert response["error"]["code"] == -32600
      assert response["error"]["message"] =~ "Invalid Request"
      assert response["id"] == nil
    end

    test "routes to correct method handlers", %{state: state} do
      methods = [
        "initialize",
        "tools/list",
        "tools/call",
        "resources/list",
        "resources/read",
        "prompts/list",
        "prompts/get",
        "notifications/initialized"
      ]

      for method <- methods do
        request = %{"method" => method, "id" => 123}
        result = RequestProcessor.process(request, state)

        case result do
          {:response, _, _} -> :ok
          {:notification, _} -> :ok
          other -> flunk("Unexpected result for #{method}: #{inspect(other)}")
        end
      end
    end

    test "handles unknown method", %{state: state} do
      request = %{"method" => "unknown/method", "id" => 123}
      {:response, response, ^state} = RequestProcessor.process(request, state)

      assert response["error"]["code"] == -32601
      assert response["error"]["message"] =~ "Method not found: unknown/method"
    end
  end

  describe "initialize" do
    test "uses custom handler when available", %{state: state} do
      request = %{"method" => "initialize", "id" => 123, "params" => %{}}
      {:response, response, ^state} = RequestProcessor.process(request, state)

      assert response["result"]["custom"] == true
      assert response["id"] == 123
    end

    test "uses default implementation when no custom handler", %{minimal_state: state} do
      request = %{
        "method" => "initialize",
        "id" => 123,
        "params" => %{"protocolVersion" => "2025-06-18"}
      }

      {:response, response, new_state} = RequestProcessor.process(request, state)

      assert response["result"]["protocolVersion"] == "2025-06-18"
      assert response["result"]["serverInfo"]
      assert response["result"]["capabilities"]
      assert new_state.protocol_version == "2025-06-18"
    end

    test "rejects unsupported protocol version", %{minimal_state: state} do
      request = %{
        "method" => "initialize",
        "id" => 123,
        "params" => %{"protocolVersion" => "1999-01-01"}
      }

      {:response, response, ^state} = RequestProcessor.process(request, state)

      assert response["error"]["code"] == -32600
      assert response["error"]["message"] =~ "Unsupported protocol version"
      assert response["error"]["data"]["supported_versions"]
    end
  end

  describe "tools/list" do
    test "returns tools from handler", %{state: state} do
      request = %{"method" => "tools/list", "id" => 123}
      {:response, response, ^state} = RequestProcessor.process(request, state)

      assert [tool] = response["result"]["tools"]
      assert tool["name"] == "test_tool"
    end

    test "returns empty list when no tools", %{minimal_state: state} do
      request = %{"method" => "tools/list", "id" => 123}
      {:response, response, ^state} = RequestProcessor.process(request, state)

      assert response["result"]["tools"] == []
    end
  end

  describe "tools/call" do
    test "calls custom handler successfully", %{state: state} do
      request = %{
        "method" => "tools/call",
        "id" => 123,
        "params" => %{
          "name" => "test_tool",
          "arguments" => %{"key" => "value"}
        }
      }

      {:response, response, ^state} = RequestProcessor.process(request, state)

      assert [content] = response["result"]["content"]
      assert content["text"] =~ "Result:"
      assert content["text"] =~ "key"
      refute response["result"]["isError"]
    end

    test "handles tool error", %{state: state} do
      request = %{
        "method" => "tools/call",
        "id" => 123,
        "params" => %{"name" => "error_tool"}
      }

      {:response, response, ^state} = RequestProcessor.process(request, state)

      assert response["error"]["code"] == -32000
      assert response["error"]["message"] == "Tool execution error"
      assert response["error"]["data"]["tool"] == "error_tool"
    end

    test "returns error when no handler", %{minimal_state: state} do
      request = %{
        "method" => "tools/call",
        "id" => 123,
        "params" => %{"name" => "any_tool"}
      }

      {:response, response, ^state} = RequestProcessor.process(request, state)

      assert response["error"]["code"] == -32000
      assert response["error"]["message"] == "Tool execution error"
      assert response["error"]["data"]["tool"] == "any_tool"
    end
  end

  describe "resources/list" do
    test "returns resources from handler", %{state: state} do
      request = %{"method" => "resources/list", "id" => 123}
      {:response, response, ^state} = RequestProcessor.process(request, state)

      assert [resource] = response["result"]["resources"]
      assert resource["uri"] == "test://resource"
    end
  end

  describe "resources/read" do
    test "reads resource successfully", %{state: state} do
      request = %{
        "method" => "resources/read",
        "id" => 123,
        "params" => %{"uri" => "test://resource"}
      }

      {:response, response, ^state} = RequestProcessor.process(request, state)

      assert [content] = response["result"]["contents"]
      assert content["text"] == "Resource content"
    end

    test "returns error when no handler", %{minimal_state: state} do
      request = %{
        "method" => "resources/read",
        "id" => 123,
        "params" => %{"uri" => "any://resource"}
      }

      {:response, response, ^state} = RequestProcessor.process(request, state)

      assert response["error"]["code"] == -32000
      assert response["error"]["message"] == "Resource operation error"
      assert response["error"]["data"]["uri"] == "any://resource"
    end
  end

  describe "prompts/list" do
    test "returns prompts from handler", %{state: state} do
      request = %{"method" => "prompts/list", "id" => 123}
      {:response, response, ^state} = RequestProcessor.process(request, state)

      assert [prompt] = response["result"]["prompts"]
      assert prompt["name"] == "test_prompt"
    end
  end

  describe "prompts/get" do
    test "gets prompt successfully", %{state: state} do
      request = %{
        "method" => "prompts/get",
        "id" => 123,
        "params" => %{
          "name" => "test_prompt",
          "arguments" => %{"style" => "formal"}
        }
      }

      {:response, response, ^state} = RequestProcessor.process(request, state)

      # The handler returns a map with atom keys
      assert response["result"][:messages]
      assert [message] = response["result"][:messages]
      assert message["content"] =~ "Prompt:"
      assert message["content"] =~ "style"
    end

    test "returns error when no handler", %{minimal_state: state} do
      request = %{
        "method" => "prompts/get",
        "id" => 123,
        "params" => %{"name" => "any_prompt"}
      }

      {:response, response, ^state} = RequestProcessor.process(request, state)

      assert response["error"]["message"] =~ "Prompt retrieval not implemented"
    end
  end

  describe "notifications/initialized" do
    test "handles initialized notification", %{state: state} do
      request = %{"method" => "notifications/initialized"}
      {:notification, ^state} = RequestProcessor.process(request, state)
    end
  end
end
