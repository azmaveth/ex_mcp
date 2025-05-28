defmodule ExMCP.SimpleServerTest do
  use ExUnit.Case

  alias ExMCP.Server.Handler

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

  describe "server features" do
    test "server has notification functions" do
      # Verify the functions exist
      assert function_exported?(ExMCP.Server, :notify_roots_changed, 1)
      assert function_exported?(ExMCP.Server, :notify_resources_changed, 1)
      assert function_exported?(ExMCP.Server, :notify_resource_updated, 2)
      assert function_exported?(ExMCP.Server, :notify_tools_changed, 1)
      assert function_exported?(ExMCP.Server, :notify_prompts_changed, 1)
      assert function_exported?(ExMCP.Server, :notify_progress, 3)
      assert function_exported?(ExMCP.Server, :notify_progress, 4)
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
end
