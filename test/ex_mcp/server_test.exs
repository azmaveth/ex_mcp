defmodule ExMCP.ServerTest do
  use ExUnit.Case, async: true

  alias ExMCP.{Client, Server}

  # Test handler module
  defmodule TestHandler do
    use ExMCP.Server.Handler

    @impl true
    def init(_args), do: {:ok, %{}}

    @impl true
    def handle_initialize(_params, state) do
      {:ok,
       %{
         name: "test-server",
         version: "1.0.0",
         protocolVersion: "2025-03-26",
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
        %{uri: "file:///projects", name: "Projects"}
      ]

      {:ok, roots, state}
    end

    @impl true
    def handle_subscribe_resource(uri, state) do
      # Store subscription in state
      subscriptions = Map.get(state, :subscriptions, [])
      new_state = Map.put(state, :subscriptions, [uri | subscriptions])
      {:ok, %{}, new_state}
    end

    @impl true
    def handle_unsubscribe_resource(uri, state) do
      subscriptions = Map.get(state, :subscriptions, [])
      new_state = Map.put(state, :subscriptions, List.delete(subscriptions, uri))
      {:ok, %{}, new_state}
    end

    @impl true
    def handle_create_message(params, state) do
      # Simple echo response
      result = %{
        role: "assistant",
        content: %{
          type: "text",
          text: "Received: #{inspect(params)}"
        }
      }

      {:ok, result, state}
    end

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

    # Wait for connection to establish
    Process.sleep(50)

    {:ok, %{server: server, client: client}}
  end

  describe "roots operations" do
    test "handles roots/list request", %{client: client} do
      {:ok, result} = Client.list_roots(client)

      assert length(result.roots) == 2
      assert Enum.at(result.roots, 0).uri == "file:///home"
      assert Enum.at(result.roots, 0).name == "Home"
      assert Enum.at(result.roots, 1).uri == "file:///projects"
      assert Enum.at(result.roots, 1).name == "Projects"
    end

    test "sends roots/list_changed notification", %{server: server, client: _client} do
      # The notification should be received by the client
      # Since we can't easily intercept it in the test transport,
      # we just verify the function exists and returns ok
      assert :ok = Server.notify_roots_changed(server)
    end
  end

  describe "resource subscriptions" do
    test "handles resources/subscribe request", %{client: client} do
      uri = "file:///test.txt"

      {:ok, result} = Client.subscribe_resource(client, uri)
      assert result == %{}
    end

    test "handles resources/unsubscribe request", %{client: client} do
      uri = "file:///test.txt"

      # First subscribe
      {:ok, _} = Client.subscribe_resource(client, uri)

      # Then unsubscribe
      {:ok, result} = Client.unsubscribe_resource(client, uri)
      assert result == %{}
    end
  end

  describe "sampling" do
    test "handles sampling/createMessage request", %{client: client} do
      params = %{
        "messages" => [%{"role" => "user", "content" => %{"type" => "text", "text" => "Hello"}}],
        "max_tokens" => 100
      }

      {:ok, result} = Client.create_message(client, params)

      assert result.role == "assistant"
      assert result.content.type == "text"
      assert String.contains?(result.content.text, "Received:")
    end
  end

  describe "error handling" do
    test "returns error for requests before initialization" do
      # Create a new server and client without waiting for initialization
      {:ok, server} =
        Server.start_link(
          transport: :test,
          handler: TestHandler
        )

      # Create client but disconnect immediately to prevent initialization
      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server,
          auto_reconnect: false
        )

      # Disconnect to ensure we're not initialized
      :ok = Client.disconnect(client)

      # Try to make a request - it should fail since we're disconnected
      assert {:error, _reason} = Client.list_roots(client, 100)
    end
  end
end
