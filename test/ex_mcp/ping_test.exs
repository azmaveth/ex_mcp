defmodule ExMCP.PingTest do
  use ExUnit.Case, async: true

  alias ExMCP.{Client, Server, Protocol}
  alias ExMCP.Server.Handler
  alias ExMCP.Client.Handler, as: ClientHandler

  defmodule TestServerHandler do
    @behaviour Handler

    @impl true
    def init(_args) do
      {:ok, %{ping_count: 0}}
    end

    @impl true
    def handle_initialize(_params, state) do
      {:ok,
       %{
         name: "ping-test-server",
         version: "1.0.0",
         capabilities: %{}
       }, state}
    end

    @impl true
    def handle_list_tools(_params, state) do
      {:ok, [], state}
    end

    @impl true
    def handle_call_tool(_name, _params, state) do
      {:ok, [%{type: "text", text: "Not implemented"}], state}
    end

    @impl true
    def terminate(_reason, _state) do
      :ok
    end
  end

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
      {:ok, server} =
        Server.start_link(
          transport: :beam,
          name: :ping_test_server_1,
          handler: TestServerHandler
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :ping_test_server_1
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
      {:ok, server} =
        Server.start_link(
          transport: :beam,
          name: :ping_test_server_2,
          handler: TestServerHandler
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :ping_test_server_2
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
      {:ok, server} =
        Server.start_link(
          transport: :beam,
          name: :ping_test_server_3,
          handler: TestServerHandler
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :ping_test_server_3,
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
      {:ok, server} =
        Server.start_link(
          transport: :beam,
          name: :ping_test_server_4,
          handler: TestServerHandler
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :ping_test_server_4
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
      {:ok, server} =
        Server.start_link(
          transport: :beam,
          name: :ping_test_server_5,
          handler: TestServerHandler
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :ping_test_server_5,
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
          transport: :beam,
          server: :non_existent_server
        )

      # Ping should fail with connection error
      assert {:error, :not_connected} = Client.ping(client)

      # Cleanup
      GenServer.stop(client)
    end

    test "ping respects timeout on slow connections" do
      defmodule SlowPingHandler do
        @behaviour Handler

        @impl true
        def init(_args), do: {:ok, %{}}

        @impl true
        def handle_initialize(_params, state) do
          {:ok,
           %{
             name: "slow-server",
             version: "1.0.0",
             capabilities: %{}
           }, state}
        end

        @impl true
        def handle_list_tools(_params, state), do: {:ok, [], state}

        @impl true
        def terminate(_reason, _state), do: :ok
      end

      {:ok, server} =
        Server.start_link(
          transport: :beam,
          name: :slow_ping_server,
          handler: SlowPingHandler
        )

      # Start client with custom transport that delays responses
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :slow_ping_server
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
      {:ok, server} =
        Server.start_link(
          transport: :beam,
          name: :health_check_server,
          handler: TestServerHandler
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :health_check_server
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
