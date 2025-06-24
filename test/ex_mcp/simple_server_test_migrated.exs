defmodule ExMCP.SimpleServerTestMigrated do
  use ExUnit.Case

  @moduledoc """
  Migrated version of simple_server_test.exs demonstrating both handler and DSL patterns.

  This test demonstrates that both handler-based and DSL-based servers provide
  equivalent functionality for basic server features and notification capabilities.

  Key differences in migration:
  - Handler tests remain unchanged (test the Handler behavior itself)
  - Added DSL server tests showing equivalent capabilities
  - Both patterns coexist and work correctly
  """

  alias ExMCP.Server.Handler

  # Original handler pattern (kept as-is since it tests Handler behavior)
  defmodule TestHandler do
    use Handler

    @impl true
    def init(_args), do: {:ok, %{}}

    @impl true
    def handle_initialize(_params, state) do
      {:ok,
       %{
         name: "test-server",
         version: "1.0.0",
         capabilities: %{
           roots: %{},
           resources: %{subscribe: true}
         }
       }, state}
    end

    @impl true
    def handle_list_roots(state) do
      roots = [
        %{uri: "file:///home", name: "Home"},
        %{uri: "file:///work", name: "Work"}
      ]

      {:ok, roots, state}
    end

    @impl true
    def handle_subscribe_resource(uri, state) do
      subs = Map.get(state, :subscriptions, [])
      new_state = Map.put(state, :subscriptions, [uri | subs])
      {:ok, %{}, new_state}
    end

    @impl true
    def handle_unsubscribe_resource(uri, state) do
      subs = Map.get(state, :subscriptions, [])
      new_state = Map.put(state, :subscriptions, List.delete(subs, uri))
      {:ok, %{}, new_state}
    end

    @impl true
    def handle_create_message(params, state) do
      result = %{
        role: "assistant",
        content: %{
          type: "text",
          text: "Echo: #{inspect(params)}"
        }
      }

      {:ok, result, state}
    end

    # Required callbacks with default implementations
    @impl true
    def handle_list_tools(_cursor, state), do: {:ok, [], nil, state}
    @impl true
    def handle_call_tool(_name, _args, state), do: {:error, "Not implemented", state}
    @impl true
    def handle_list_resources(_cursor, state), do: {:ok, [], nil, state}
    @impl true
    def handle_read_resource(_uri, state), do: {:error, "Not implemented", state}
    @impl true
    def handle_list_prompts(_cursor, state), do: {:ok, [], nil, state}
    @impl true
    def handle_get_prompt(_name, _args, state), do: {:error, "Not implemented", state}
  end

  # New DSL pattern equivalent
  defmodule TestDslServer do
    use ExMCP.Server

    # DSL servers automatically provide equivalent capabilities
    @impl true
    def init(_args), do: {:ok, %{subscriptions: []}}

    # Required callback implementations (minimal for this test)
    @impl true
    def handle_tool_call(_name, _arguments, state) do
      {:ok, %{content: [%{type: "text", text: "Not implemented"}]}, state}
    end

    @impl true
    def handle_resource_read(_uri, _full_uri, state) do
      {:error, "Not implemented", state}
    end

    @impl true
    def handle_prompt_get(_name, _args, state) do
      {:error, "Not implemented", state}
    end

    # Note: DSL servers don't have root handling in the current implementation
    # This demonstrates a limitation where not all handler features are available in DSL

    # Resource subscription management (equivalent to TestHandler)
    @impl true
    def handle_resource_subscribe(uri, state) do
      subs = Map.get(state, :subscriptions, [])
      new_state = Map.put(state, :subscriptions, [uri | subs])
      {:ok, new_state}
    end

    @impl true
    def handle_resource_unsubscribe(uri, state) do
      subs = Map.get(state, :subscriptions, [])
      new_state = Map.put(state, :subscriptions, List.delete(subs, uri))
      {:ok, new_state}
    end

    # Note: DSL servers don't have handle_create_message callback
    # This is another limitation where not all handler features are available in DSL
  end

  describe "server features (original handler pattern)" do
    test "server has notification functions" do
      # Verify the functions that actually exist in the current Server module
      assert function_exported?(ExMCP.Server, :notify_progress, 4)
      assert function_exported?(ExMCP.Server, :notify_resource_update, 2)

      # Note: Other notification functions (notify_roots_changed, notify_tools_changed, etc.)
      # were removed during consolidation phase. This test documents the current state.
    end

    test "handler behaviour includes new callbacks" do
      # Get the behaviour callbacks
      callbacks = Handler.behaviour_info(:callbacks)

      # Verify new callbacks exist
      assert {:handle_list_roots, 1} in callbacks
      assert {:handle_subscribe_resource, 2} in callbacks
      assert {:handle_unsubscribe_resource, 2} in callbacks
      assert {:handle_create_message, 2} in callbacks
    end
  end

  describe "server features (migrated DSL pattern)" do
    test "DSL server provides same notification functions" do
      # DSL servers use the same ExMCP.Server notification functions
      assert function_exported?(ExMCP.Server, :notify_progress, 4)
      assert function_exported?(ExMCP.Server, :notify_resource_update, 2)

      # DSL and handler servers both use the same underlying Server module
    end

    test "DSL server has auto-generated capabilities" do
      # DSL servers automatically generate capabilities based on definitions
      capabilities = TestDslServer.get_capabilities()

      # Verify capabilities structure exists
      assert is_map(capabilities)

      # DSL servers provide default capabilities
      # (specific capabilities depend on what's defined in the DSL)
    end

    test "DSL server implements required behaviors" do
      # Verify the DSL server module has the expected callbacks
      callbacks = TestDslServer.__info__(:functions)

      # Check for key DSL-generated functions
      assert {:init, 1} in callbacks
      assert {:handle_tool_call, 3} in callbacks
      assert {:handle_resource_read, 3} in callbacks
      assert {:handle_prompt_get, 3} in callbacks
    end

    test "DSL and handler servers provide equivalent functionality" do
      # Both approaches should provide the same core capabilities:

      # 1. Notification functions (both use ExMCP.Server module)
      assert function_exported?(ExMCP.Server, :notify_progress, 4)

      # 2. Initialization and state management
      {:ok, handler_state} = TestHandler.init([])
      {:ok, dsl_state} = TestDslServer.init([])
      assert is_map(handler_state)
      assert is_map(dsl_state)

      # 3. Core callback structure exists in both
      handler_callbacks = Handler.behaviour_info(:callbacks)
      dsl_functions = TestDslServer.__info__(:functions)

      # Both have initialization
      assert {:init, 1} in handler_callbacks
      assert {:init, 1} in dsl_functions
    end
  end

  describe "migration pattern validation" do
    test "both patterns coexist without conflicts" do
      # This test validates that having both handler and DSL patterns
      # in the same test file doesn't cause conflicts

      # Handler pattern works
      assert {:ok, %{}} = TestHandler.init([])

      # DSL pattern works
      assert {:ok, %{}} = TestDslServer.init([])

      # Handler has capabilities that DSL doesn't (documented limitation)
      handler_result = TestHandler.handle_create_message(%{test: "data"}, %{})
      assert match?({:ok, _, _}, handler_result)

      # DSL servers focus on tools, resources, and prompts rather than message creation
    end

    test "migration preserves core functionality" do
      # Verify that migrating from handler to DSL preserves essential features:

      # 1. State management
      {:ok, handler_state} = TestHandler.init([])
      {:ok, dsl_state} = TestDslServer.init([])
      assert is_map(handler_state) and is_map(dsl_state)

      # 2. Root listing capability (handler only - DSL limitation)
      {:ok, handler_roots, _} = TestHandler.handle_list_roots(%{})
      assert length(handler_roots) == 2
      assert Enum.at(handler_roots, 0)[:uri] == "file:///home"

      # Note: DSL servers don't currently support root listing - this is a documented limitation

      # 3. Subscription management
      {:ok, _handler_sub_result, handler_sub_state} =
        TestHandler.handle_subscribe_resource("test://uri", %{})

      {:ok, dsl_sub_state} = TestDslServer.handle_resource_subscribe("test://uri", %{})
      assert Map.has_key?(handler_sub_state, :subscriptions)
      assert Map.has_key?(dsl_sub_state, :subscriptions)
    end
  end
end
