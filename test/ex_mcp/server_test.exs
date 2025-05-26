defmodule ExMCP.ServerTest do
  use ExUnit.Case
  import Mox

  alias ExMCP.Protocol
  alias ExMCP.Server
  alias ExMCP.Transport.Mock, as: MockTransport

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
    def handle_list_tools(state), do: {:ok, [], state}
    @impl true
    def handle_call_tool(_name, _params, state), do: {:error, "Not implemented", state}
    @impl true
    def handle_list_resources(state), do: {:ok, [], state}
    @impl true
    def handle_read_resource(_uri, state), do: {:error, "Not implemented", state}
    @impl true
    def handle_list_prompts(state), do: {:ok, [], state}
    @impl true
    def handle_get_prompt(_name, _args, state), do: {:error, "Not implemented", state}
  end

  setup :verify_on_exit!

  setup do
    # Set Mox to global mode for cross-process mocking
    Mox.set_mox_global()

    # Mock transport - servers may use connect or accept depending on transport type
    MockTransport
    |> expect(:connect, fn _opts -> {:ok, :mock_state} end)
    |> stub(:send_message, fn _msg, state -> {:ok, state} end)
    |> stub(:receive_message, fn state ->
      # Block forever
      Process.sleep(:infinity)
      {:ok, nil, state}
    end)

    {:ok, server} =
      Server.start_link(
        handler: TestHandler,
        transport: MockTransport
      )

    # Initialize the server
    init_request = Protocol.encode_initialize(%{name: "test-client", version: "1.0"})
    {:ok, json} = Protocol.encode_to_string(init_request)
    send(server, {:transport_message, json})
    Process.sleep(10)

    {:ok, %{server: server}}
  end

  describe "roots operations" do
    test "handles roots/list request", %{server: server} do
      MockTransport
      |> expect(:send_message, fn msg, state ->
        {:ok, msg_data} = Jason.decode(msg)

        assert msg_data["result"]["roots"] == [
                 %{"uri" => "file:///home", "name" => "Home"},
                 %{"uri" => "file:///projects", "name" => "Projects"}
               ]

        {:ok, state}
      end)

      request = %{
        "jsonrpc" => "2.0",
        "method" => "roots/list",
        "params" => %{},
        "id" => 1
      }

      {:ok, json} = Jason.encode(request)
      send(server, {:transport_message, json})
      Process.sleep(10)
    end

    test "sends roots/list_changed notification", %{server: server} do
      MockTransport
      |> expect(:send_message, fn msg, state ->
        {:ok, msg_data} = Jason.decode(msg)
        assert msg_data["method"] == "notifications/roots/list_changed"
        assert msg_data["params"] == %{}
        refute Map.has_key?(msg_data, "id")
        {:ok, state}
      end)

      Server.notify_roots_changed(server)
      Process.sleep(10)
    end
  end

  describe "resource subscriptions" do
    test "handles resources/subscribe request", %{server: server} do
      uri = "file:///test.txt"

      MockTransport
      |> expect(:send_message, fn msg, state ->
        {:ok, msg_data} = Jason.decode(msg)
        assert msg_data["result"] == %{}
        {:ok, state}
      end)

      request = %{
        "jsonrpc" => "2.0",
        "method" => "resources/subscribe",
        "params" => %{"uri" => uri},
        "id" => 1
      }

      {:ok, json} = Jason.encode(request)
      send(server, {:transport_message, json})
      Process.sleep(10)
    end

    test "handles resources/unsubscribe request", %{server: server} do
      uri = "file:///test.txt"

      MockTransport
      |> expect(:send_message, fn msg, state ->
        {:ok, msg_data} = Jason.decode(msg)
        assert msg_data["result"] == %{}
        {:ok, state}
      end)

      request = %{
        "jsonrpc" => "2.0",
        "method" => "resources/unsubscribe",
        "params" => %{"uri" => uri},
        "id" => 1
      }

      {:ok, json} = Jason.encode(request)
      send(server, {:transport_message, json})
      Process.sleep(10)
    end
  end

  describe "sampling" do
    test "handles sampling/createMessage request", %{server: server} do
      MockTransport
      |> expect(:send_message, fn msg, state ->
        {:ok, msg_data} = Jason.decode(msg)
        result = msg_data["result"]
        assert result["role"] == "assistant"
        assert result["content"]["type"] == "text"
        assert String.contains?(result["content"]["text"], "Received:")
        {:ok, state}
      end)

      request = %{
        "jsonrpc" => "2.0",
        "method" => "sampling/createMessage",
        "params" => %{
          "messages" => [%{"role" => "user", "content" => %{"type" => "text", "text" => "Hello"}}],
          "max_tokens" => 100
        },
        "id" => 1
      }

      {:ok, json} = Jason.encode(request)
      send(server, {:transport_message, json})
      Process.sleep(10)
    end
  end

  describe "error handling" do
    test "returns error for requests before initialization", %{server: _server} do
      # Create a new server without initializing
      MockTransport
      |> expect(:connect, fn _opts -> {:ok, :mock_state} end)
      |> stub(:receive_message, fn state ->
        # Block forever
        Process.sleep(:infinity)
        {:ok, nil, state}
      end)
      |> expect(:send_message, fn msg, state ->
        {:ok, msg_data} = Jason.decode(msg)
        assert msg_data["error"]["message"] == "Not initialized"
        {:ok, state}
      end)

      {:ok, server} =
        Server.start_link(
          handler: TestHandler,
          transport: MockTransport
        )

      request = %{
        "jsonrpc" => "2.0",
        "method" => "roots/list",
        "params" => %{},
        "id" => 1
      }

      {:ok, json} = Jason.encode(request)
      send(server, {:transport_message, json})
      Process.sleep(10)
    end
  end
end
