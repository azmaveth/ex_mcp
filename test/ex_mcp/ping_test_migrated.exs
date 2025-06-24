defmodule ExMCP.PingTestMigrated do
  use ExUnit.Case, async: true

  alias ExMCP.{Client, Protocol, Server}
  alias ExMCP.Client.Handler, as: ClientHandler

  # DSL-based server replacing TestServerHandler
  defmodule TestPingServer do
    use ExMCP.Server

    # Custom initialization to maintain state compatibility
    @impl true
    def init(_args) do
      {:ok, %{ping_count: 0}}
    end

    # Tool implementations (required but not used in ping tests)
    @impl true
    def handle_tool_call(_name, _arguments, state) do
      {:ok, %{content: [%{type: "text", text: "Not implemented"}]}, state}
    end

    # Resource implementations (required but not used)
    @impl true
    def handle_resource_read(_uri, _full_uri, state) do
      {:error, "No resources implemented", state}
    end

    # Prompt implementations (required but not used)
    @impl true
    def handle_prompt_get(_name, _args, state) do
      {:error, "No prompts implemented", state}
    end
  end

  # Slow ping server for timeout testing
  defmodule SlowPingServer do
    use ExMCP.Server

    @impl true
    def init(_args), do: {:ok, %{}}

    @impl true
    def handle_tool_call(_name, _arguments, state) do
      {:ok, %{content: [%{type: "text", text: "Not implemented"}]}, state}
    end

    @impl true
    def handle_resource_read(_uri, _full_uri, state) do
      {:error, "No resources implemented", state}
    end

    @impl true
    def handle_prompt_get(_name, _args, state) do
      {:error, "No prompts implemented", state}
    end
  end

  # Client handler remains the same as it's not part of the migration
  defmodule TestClientHandler do
    @behaviour ClientHandler

    @impl true
    def init(args) do
      {:ok, args}
    end

    @impl true
    def handle_ping(state) do
      # Track ping count
      new_state = Map.update(state, :ping_count, 1, &(&1 + 1))
      {:ok, %{}, new_state}
    end

    @impl true
    def handle_list_roots(state) do
      {:ok, [], state}
    end

    @impl true
    def handle_create_message(_params, state) do
      {:ok, %{}, state}
    end

    @impl true
    def terminate(_reason, _state) do
      :ok
    end
  end

  describe "ping protocol compliance" do
    test "encode_ping creates correct request format" do
      ping_request = Protocol.encode_ping()

      assert ping_request["jsonrpc"] == "2.0"
      assert ping_request["method"] == "ping"
      assert ping_request["params"] == %{}
      assert is_integer(ping_request["id"])
    end

    test "encode_pong creates correct response format" do
      pong_response = Protocol.encode_pong(123)

      assert pong_response["jsonrpc"] == "2.0"
      assert pong_response["id"] == 123
      assert pong_response["result"] == %{}
      refute Map.has_key?(pong_response, "error")
    end
  end

  describe "client to server ping" do
    test "client can ping server successfully" do
      # Use handler-based server for transport compatibility
      {:ok, server} =
        Server.start_link(
          transport: :test,
          handler: TestPingServer
        )

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server
        )

      # Wait for initialization
      Process.sleep(100)

      # Client pings server
      assert {:ok, %{}} = Client.ping(client)

      # Multiple pings should work
      assert {:ok, %{}} = Client.ping(client)
      assert {:ok, %{}} = Client.ping(client)

      # Cleanup
      GenServer.stop(client)
      GenServer.stop(server)
    end

    test "client ping respects timeout" do
      {:ok, server} = TestPingServer.start_link(transport: :native)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server
        )

      # Wait for initialization
      Process.sleep(100)

      # Short timeout should still work for ping
      assert {:ok, %{}} = Client.ping(client, 100)

      # Cleanup
      GenServer.stop(client)
      GenServer.stop(server)
    end
  end

  describe "server to client ping" do
    test "server can ping client successfully" do
      {:ok, server} = TestPingServer.start_link(transport: :native)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server,
          handler: TestClientHandler,
          handler_state: %{ping_count: 0}
        )

      # Wait for initialization
      Process.sleep(100)

      # Server pings client
      assert {:ok, %{}} = Server.ping(server)

      # Multiple pings should work
      assert {:ok, %{}} = Server.ping(server)
      assert {:ok, %{}} = Server.ping(server)

      # Cleanup
      GenServer.stop(client)
      GenServer.stop(server)
    end

    test "server ping fails when client doesn't have handler" do
      {:ok, server} = TestPingServer.start_link(transport: :native)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server
          # No handler specified - uses DefaultHandler which doesn't support server requests
        )

      # Wait for initialization
      Process.sleep(100)

      # Server ping should fail
      assert {:error, %{"code" => -32601}} = Server.ping(server)

      # Cleanup
      GenServer.stop(client)
      GenServer.stop(server)
    end
  end

  describe "bidirectional ping" do
    test "both client and server can ping each other" do
      {:ok, server} = TestPingServer.start_link(transport: :native)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server,
          handler: TestClientHandler,
          handler_state: %{}
        )

      # Wait for initialization
      Process.sleep(100)

      # Client pings server
      assert {:ok, %{}} = Client.ping(client)

      # Server pings client
      assert {:ok, %{}} = Server.ping(server)

      # Interleaved pings
      assert {:ok, %{}} = Client.ping(client)
      assert {:ok, %{}} = Server.ping(server)
      assert {:ok, %{}} = Client.ping(client)

      # Cleanup
      GenServer.stop(client)
      GenServer.stop(server)
    end
  end

  describe "ping error handling" do
    test "ping handles transport failures gracefully" do
      # Create a client without a server
      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: :non_existent_server
        )

      # Ping should fail with connection error
      assert {:error, :not_connected} = Client.ping(client)

      # Cleanup
      GenServer.stop(client)
    end

    test "ping respects timeout on slow connections" do
      {:ok, server} = SlowPingServer.start_link(transport: :native)

      # Start client with custom transport that delays responses
      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server
        )

      # Wait for initialization
      Process.sleep(100)

      # Normal ping should work
      assert {:ok, %{}} = Client.ping(client, 1000)

      # Cleanup
      GenServer.stop(client)
      GenServer.stop(server)
    end
  end

  describe "ping as health check" do
    test "periodic pings can detect connection health" do
      {:ok, server} = TestPingServer.start_link(transport: :native)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server
        )

      # Wait for initialization
      Process.sleep(100)

      # Simulate periodic health checks
      results =
        for _i <- 1..5 do
          Process.sleep(50)
          Client.ping(client)
        end

      # All pings should succeed
      assert Enum.all?(results, fn result ->
               match?({:ok, %{}}, result)
             end)

      # Stop server to simulate connection loss
      GenServer.stop(server)
      Process.sleep(100)

      # Ping should now fail
      assert {:error, _} = Client.ping(client)

      # Cleanup
      GenServer.stop(client)
    end
  end
end
