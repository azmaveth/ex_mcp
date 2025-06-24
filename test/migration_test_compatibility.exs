defmodule ExMCP.MigrationCompatibilityTest do
  @moduledoc """
  Transport compatibility test for DSL server migration.

  This test validates that modern DSL-based servers can work with
  the test infrastructure used by legacy handler-based tests.
  """
  use ExUnit.Case, async: true

  alias ExMCP.{Client, Server}

  # Simple DSL server for compatibility testing
  defmodule TestDSLServer do
    use ExMCP.Server

    deftool "echo" do
      meta do
        description("Simple echo tool for testing")
      end

      input_schema(%{
        type: "object",
        properties: %{
          message: %{type: "string", description: "Message to echo"}
        },
        required: ["message"]
      })
    end

    deftool "add" do
      meta do
        description("Adds two numbers")
      end

      input_schema(%{
        type: "object",
        properties: %{
          a: %{type: "number", description: "First number"},
          b: %{type: "number", description: "Second number"}
        },
        required: ["a", "b"]
      })
    end

    defresource "config://test" do
      meta do
        name("Test Config")
        description("Test configuration resource")
      end

      mime_type("application/json")
    end

    # Handler implementations
    @impl true
    def handle_tool_call("echo", %{"message" => message}, state) do
      result = %{content: [%{"type" => "text", "text" => "Echo: #{message}"}]}
      {:ok, result, state}
    end

    def handle_tool_call("add", %{"a" => a, "b" => b}, state) do
      result = %{content: [%{"type" => "text", "text" => "Result: #{a + b}"}]}
      {:ok, result, state}
    end

    @impl true
    def handle_resource_read("config://test", _uri, state) do
      content = [%{"type" => "text", "text" => ~s({"test": true, "mode": "dsl"})}]
      {:ok, content, state}
    end
  end

  describe "DSL server transport compatibility" do
    test "DSL server starts with native transport" do
      {:ok, pid} = TestDSLServer.start_link(transport: :native)
      assert Process.alive?(pid)
      GenServer.stop(pid)
    end

    test "DSL server exposes capabilities correctly" do
      capabilities = TestDSLServer.get_capabilities()

      # Should have tools capability
      assert Map.has_key?(capabilities, "tools")
      assert capabilities["tools"]["listChanged"] == true

      # Should have resources capability  
      assert Map.has_key?(capabilities, "resources")
      assert capabilities["resources"]["listChanged"] == true
    end

    test "DSL server exposes tools correctly" do
      tools = TestDSLServer.get_tools()

      assert Map.has_key?(tools, "echo")
      assert Map.has_key?(tools, "add")

      echo_tool = tools["echo"]
      assert echo_tool.name == "echo"
      assert echo_tool.description == "Simple echo tool for testing"
      assert echo_tool.input_schema[:type] == "object"
    end

    test "DSL server exposes resources correctly" do
      resources = TestDSLServer.get_resources()

      assert Map.has_key?(resources, "config://test")

      config_resource = resources["config://test"]
      assert config_resource.name == "Test Config"
      assert config_resource.mime_type == "application/json"
    end
  end

  describe "DSL server with client integration" do
    setup do
      {:ok, server} = TestDSLServer.start_link(transport: :native)

      on_exit(fn ->
        if Process.alive?(server), do: GenServer.stop(server)
      end)

      %{server: server}
    end

    test "can call tool handlers directly", %{server: server} do
      state = :sys.get_state(server)

      {:ok, result, _new_state} =
        TestDSLServer.handle_tool_call("echo", %{"message" => "hello"}, state)

      assert %{content: [content]} = result
      assert content["type"] == "text"
      assert content["text"] == "Echo: hello"
    end

    test "can call resource handlers directly", %{server: server} do
      state = :sys.get_state(server)

      {:ok, content, _new_state} =
        TestDSLServer.handle_resource_read("config://test", "config://test", state)

      assert is_list(content)
      assert length(content) == 1
      [item] = content
      assert item["type"] == "text"
      assert String.contains?(item["text"], "dsl")
    end
  end
end
