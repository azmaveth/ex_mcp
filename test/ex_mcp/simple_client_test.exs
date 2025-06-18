defmodule ExMCP.V1SimpleClientTest do
  use ExUnit.Case, async: true

  alias ExMCP.{Client, Server}

  defmodule TestHandler do
    use ExMCP.Server.Handler

    @impl true
    def init(_args) do
      {:ok, %{subscriptions: []}}
    end

    @impl true
    def handle_initialize(_params, state) do
      {:ok,
       %{
         name: "test-server",
         version: "1.0.0",
         protocolVersion: "2025-03-26",
         capabilities: %{
           roots: %{},
           resources: %{subscribe: true},
           sampling: %{}
         }
       }, state}
    end

    @impl true
    def handle_list_roots(state) do
      roots = [
        %{uri: "file:///home", name: "Home"},
        %{uri: "file:///projects", name: "Projects"}
      ]

      {:ok, roots, state}
    end

    @impl true
    def handle_subscribe_resource(uri, state) do
      subscriptions = [uri | state.subscriptions]
      {:ok, %{}, %{state | subscriptions: subscriptions}}
    end

    @impl true
    def handle_unsubscribe_resource(uri, state) do
      subscriptions = List.delete(state.subscriptions, uri)
      {:ok, %{}, %{state | subscriptions: subscriptions}}
    end

    @impl true
    def handle_create_message(params, state) do
      # Echo back the request with a simple response
      result = %{
        role: "assistant",
        content: %{
          type: "text",
          text: "Received #{length(params["messages"])} messages"
        }
      }

      {:ok, result, state}
    end

    # Required callbacks
    @impl true
    def handle_list_tools(_cursor, state), do: {:ok, [], nil, state}
    @impl true
    def handle_call_tool(_name, _params, state), do: {:error, "Not implemented", state}
    @impl true
    def handle_list_resources(_cursor, state), do: {:ok, [], nil, state}
    @impl true
    def handle_read_resource(_uri, state), do: {:error, "Not implemented", state}
    @impl true
    def handle_list_prompts(_cursor, state), do: {:ok, [], nil, state}
    @impl true
    def handle_get_prompt(_name, _args, state), do: {:error, "Not implemented", state}
  end

  describe "new protocol features via public API" do
    setup do
      # Start server and client
      {:ok, server} =
        Server.start_link(
          transport: :test,
          handler: TestHandler
        )

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server
        )

      # Wait for connection
      Process.sleep(50)

      {:ok, %{client: client, server: server}}
    end

    test "protocol version is negotiated correctly", %{client: client} do
      # The protocol version is negotiated during initialization
      # We can verify the server capabilities were received
      state = :sys.get_state(client)

      # Server info should be stored in the client state
      assert state.initialized == true

      # Use the public API to get the negotiated version
      {:ok, version} = Client.negotiated_version(client)
      assert version == "2025-03-26"
    end

    test "roots operations work through client API", %{client: client} do
      # Test list roots
      {:ok, result} = Client.list_roots(client)

      assert length(result.roots) == 2
      assert hd(result.roots).uri == "file:///home"
      assert hd(result.roots).name == "Home"
    end

    test "resource subscription works through client API", %{client: client} do
      uri = "file:///test.txt"

      # Test subscribe
      {:ok, _result} = Client.subscribe_resource(client, uri)

      # Test unsubscribe
      {:ok, _result} = Client.unsubscribe_resource(client, uri)
    end

    test "sampling/createMessage works through client API", %{client: client} do
      params = %{
        messages: [%{role: "user", content: %{type: "text", text: "Hello"}}],
        max_tokens: 100
      }

      {:ok, result} = Client.create_message(client, params)

      assert result.role == "assistant"
      assert result.content.type == "text"
      assert String.contains?(result.content.text, "1 messages")
    end

    test "client has new methods available" do
      # Just verify the functions exist and have correct arity
      # Functions with default args are exported with both arities
      assert function_exported?(ExMCP.Client, :list_roots, 1) ||
               function_exported?(ExMCP.Client, :list_roots, 2)

      assert function_exported?(ExMCP.Client, :subscribe_resource, 2) ||
               function_exported?(ExMCP.Client, :subscribe_resource, 3)

      assert function_exported?(ExMCP.Client, :unsubscribe_resource, 2) ||
               function_exported?(ExMCP.Client, :unsubscribe_resource, 3)

      assert function_exported?(ExMCP.Client, :create_message, 2) ||
               function_exported?(ExMCP.Client, :create_message, 3)
    end
  end
end
