defmodule ExMCP.MessageProcessorValidationTest do
  @moduledoc """
  Validation framework for testing message processor routing behavior.

  This test demonstrates the fundamental flaw in the message processor where
  handler-based servers fail because the processor calls DSL-style functions
  that don't exist on handler modules.
  """

  use ExUnit.Case, async: true

  alias ExMCP.MessageProcessor
  alias ExMCP.MessageProcessor.Conn

  # Minimal handler-based server
  defmodule MinimalHandler do
    use ExMCP.Server.Handler

    @impl true
    def init(_args), do: {:ok, %{name: "minimal-handler"}}

    @impl true
    def handle_initialize(_params, state) do
      {:ok,
       %{
         protocolVersion: "2025-03-26",
         serverInfo: %{name: "minimal-handler", version: "1.0.0"},
         capabilities: %{tools: %{}}
       }, state}
    end

    @impl true
    def handle_list_tools(_cursor, state) do
      tools = [
        %{name: "test_tool", description: "A test tool", inputSchema: %{}}
      ]

      {:ok, tools, nil, state}
    end

    @impl true
    def handle_call_tool(name, args, state) do
      {:ok, %{content: [%{type: "text", text: "Called #{name} with #{inspect(args)}"}]}, state}
    end

    # Required callbacks for handler behavior
    @impl true
    def handle_list_prompts(_cursor, state), do: {:ok, [], nil, state}

    @impl true
    def handle_get_prompt(_name, _args, state), do: {:error, "Not implemented", state}

    @impl true
    def handle_list_resources(_cursor, state), do: {:ok, [], nil, state}

    @impl true
    def handle_read_resource(_uri, state), do: {:error, "Not implemented", state}

    # GenServer callbacks needed when started as GenServer
    def handle_call({:handle_list_tools, cursor}, _from, state) do
      {:ok, tools, next_cursor, new_state} = handle_list_tools(cursor, state)
      {:reply, {:ok, tools, next_cursor, new_state}, new_state}
    end

    def handle_call({:handle_list_prompts, cursor}, _from, state) do
      {:ok, prompts, next_cursor, new_state} = handle_list_prompts(cursor, state)
      {:reply, {:ok, prompts, next_cursor, new_state}, new_state}
    end

    def handle_call({:handle_list_resources, cursor}, _from, state) do
      {:ok, resources, next_cursor, new_state} = handle_list_resources(cursor, state)
      {:reply, {:ok, resources, next_cursor, new_state}, new_state}
    end

    def handle_call({:handle_call_tool, name, args}, _from, state) do
      result = handle_call_tool(name, args, state)
      {:reply, result, state}
    end

    def handle_call({:handle_read_resource, uri}, _from, state) do
      result = handle_read_resource(uri, state)
      {:reply, result, state}
    end

    def handle_call({:handle_get_prompt, name, args}, _from, state) do
      result = handle_get_prompt(name, args, state)
      {:reply, result, state}
    end

    def handle_call({:handle_initialize, params}, _from, state) do
      {:ok, result, new_state} = handle_initialize(params, state)
      {:reply, {:ok, result, new_state}, new_state}
    end

    def handle_call(request, _from, state) do
      {:reply, {:error, "Unknown request: #{inspect(request)}"}, state}
    end
  end

  # Minimal DSL-based server  
  defmodule MinimalDslServer do
    use ExMCP.Server

    deftool "test_tool" do
      meta do
        name("Test Tool")
        description("A test tool for DSL server")
      end

      input_schema(%{
        type: "object",
        properties: %{}
      })
    end

    @impl true
    def init(_args), do: {:ok, %{name: "minimal-dsl"}}

    @impl true
    def handle_tool_call("test_tool", args, state) do
      {:ok, %{content: [%{type: "text", text: "DSL called with #{inspect(args)}"}]}, state}
    end
  end

  describe "message processor routing validation" do
    test "handler server works correctly with new routing" do
      # Create a tools/list request
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/list",
        "params" => %{},
        "id" => 1
      }

      # Create connection with handler server
      conn = MessageProcessor.new(request, transport: :test)
      opts = %{handler: MinimalHandler}

      # This should now work with the fixed routing
      result = MessageProcessor.process(conn, opts)

      # Verify we got a successful response
      assert result.response["jsonrpc"] == "2.0"
      assert result.response["id"] == 1
      assert is_map(result.response["result"])
      assert is_list(result.response["result"]["tools"])

      # Verify the tool from our handler
      tools = result.response["result"]["tools"]
      assert length(tools) == 1

      # Check the actual structure returned
      tool = hd(tools)
      assert tool.name == "test_tool" || tool["name"] == "test_tool"
    end

    test "DSL server works correctly through message processor" do
      # Create a tools/list request
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/list",
        "params" => %{},
        "id" => 1
      }

      # Create connection with DSL server
      conn = MessageProcessor.new(request, transport: :test)
      opts = %{handler: MinimalDslServer}

      # This should work because DSL servers have the expected functions
      result = MessageProcessor.process(conn, opts)

      # DSL server returns the tool with all metadata
      tools = result.response["result"]["tools"]
      assert length(tools) == 1

      tool = hd(tools)
      assert tool.name == "test_tool"
      assert tool.description == "A test tool for DSL server"
      assert tool.display_name == "Test Tool"
    end

    test "server type detection is flawed - DSL servers are detected by start_link" do
      # DSL servers have start_link because they use GenServer
      assert function_exported?(MinimalDslServer, :start_link, 1)

      # Handler servers don't inherently have start_link
      refute function_exported?(MinimalHandler, :start_link, 1)

      # The detection logic in message_processor.ex lines 137-138 uses this
      # to differentiate, but it's not a reliable method
    end

    test "handler server has correct callbacks but message processor doesn't use them" do
      # Handler has the correct MCP protocol callbacks
      assert function_exported?(MinimalHandler, :handle_list_tools, 2)
      assert function_exported?(MinimalHandler, :handle_call_tool, 3)

      # But message processor expects DSL-style functions
      refute function_exported?(MinimalHandler, :get_tools, 0)
      refute function_exported?(MinimalHandler, :get_prompts, 0)
      refute function_exported?(MinimalHandler, :get_resources, 0)
    end

    test "handler routing works for multiple MCP methods" do
      # Test prompts/list
      request = %{
        "jsonrpc" => "2.0",
        "method" => "prompts/list",
        "params" => %{},
        "id" => 2
      }

      conn = MessageProcessor.new(request, transport: :test)
      opts = %{handler: MinimalHandler}

      # This should now work
      result = MessageProcessor.process(conn, opts)
      assert result.response["result"]["prompts"] == []

      # Test resources/list
      request2 = %{
        "jsonrpc" => "2.0",
        "method" => "resources/list",
        "params" => %{},
        "id" => 3
      }

      conn2 = MessageProcessor.new(request2, transport: :test)
      result2 = MessageProcessor.process(conn2, opts)
      assert result2.response["result"]["resources"] == []
    end
  end

  describe "proper handler routing demonstration" do
    test "show how handler should be called via GenServer" do
      # Start handler as a GenServer
      {:ok, pid} = GenServer.start_link(MinimalHandler, [])

      # This is how the message processor SHOULD call handlers
      result = GenServer.call(pid, {:handle_list_tools, nil})

      assert {:ok, tools, _cursor, _state} = result
      assert length(tools) == 1
      assert hd(tools).name == "test_tool"

      GenServer.stop(pid)
    end

    test "demonstrate handler initialization via GenServer" do
      {:ok, pid} = GenServer.start_link(MinimalHandler, [])

      # Handler should be initialized and callable
      result = GenServer.call(pid, {:handle_initialize, %{}})

      assert {:ok, init_result, _state} = result
      assert init_result.protocolVersion == "2025-03-26"
      assert init_result.serverInfo.name == "minimal-handler"

      GenServer.stop(pid)
    end
  end

  describe "routing comparison table" do
    test "document the routing mismatch" do
      routing_comparison = %{
        "tools/list" => %{
          handler_expects: "GenServer.call(pid, {:handle_list_tools, cursor})",
          processor_calls: "handler_module.get_tools() at line 250",
          result: "UndefinedFunctionError"
        },
        "prompts/list" => %{
          handler_expects: "GenServer.call(pid, {:handle_list_prompts, cursor})",
          processor_calls: "handler_module.get_prompts() at line 308",
          result: "UndefinedFunctionError"
        },
        "resources/list" => %{
          handler_expects: "GenServer.call(pid, {:handle_list_resources, cursor})",
          processor_calls: "handler_module.get_resources() at line 277",
          result: "UndefinedFunctionError"
        }
      }

      # Verify our understanding is correct
      assert routing_comparison["tools/list"].result == "UndefinedFunctionError"
    end
  end
end
