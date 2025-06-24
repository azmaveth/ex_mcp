defmodule ExMCP.PingTestMigrated do
  use ExUnit.Case, async: false
  import ExMCP.TestHelpers

  alias ExMCP.{Client, Protocol, Server}
  alias ExMCP.Client.Handler, as: ClientHandler

  setup_all do
    # Ensure ExMCP application is started for service registry
    {:ok, _} = Application.ensure_all_started(:ex_mcp)

    on_exit(fn ->
      Application.stop(:ex_mcp)
    end)

    :ok
  end

  # DSL-based server replacing TestServerHandler
  defmodule TestPingServer do
    use ExMCP.Service, name: :test_ping_server

    # Custom initialization to maintain state compatibility
    @impl true
    def init(args) do
      # Call parent to register the service
      {:ok, _} = super(args)
      # Then set our custom state
      {:ok, %{ping_count: 0}}
    end

    # MCP request implementations (required for ExMCP.Service)
    @impl true
    def handle_mcp_request("initialize", _params, state) do
      {:ok,
       %{
         "protocolVersion" => "2025-06-18",
         "capabilities" => %{
           "tools" => %{},
           "resources" => %{},
           "prompts" => %{}
         },
         "serverInfo" => %{
           "name" => "TestPingServer",
           "version" => "1.0.0"
         }
       }, state}
    end

    def handle_mcp_request("ping", _params, state) do
      {:ok, %{}, state}
    end

    def handle_mcp_request(_method, _params, state) do
      {:error, "Method not implemented", state}
    end

    @impl true
    def handle_call(:ping, _from, state) do
      {:reply, {:ok, %{}}, state}
    end
  end

  # Slow ping server for timeout testing
  defmodule SlowPingServer do
    use ExMCP.Service, name: :slow_ping_server

    @impl true
    def init(args) do
      # Call parent to register the service
      {:ok, _} = super(args)
      # Then set our custom state
      {:ok, %{}}
    end

    @impl true
    def handle_mcp_request("initialize", _params, state) do
      {:ok,
       %{
         "protocolVersion" => "2025-06-18",
         "capabilities" => %{
           "tools" => %{},
           "resources" => %{},
           "prompts" => %{}
         },
         "serverInfo" => %{
           "name" => "SlowPingServer",
           "version" => "1.0.0"
         }
       }, state}
    end

    def handle_mcp_request("ping", _params, state) do
      # Add delay for slow ping testing
      Process.sleep(50)
      {:ok, %{}, state}
    end

    def handle_mcp_request(_method, _params, state) do
      {:error, "Method not implemented", state}
    end

    @impl true
    def handle_call(:ping, _from, state) do
      {:reply, {:error, :ping_not_implemented}, state}
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

  # Handler that does not implement ping for testing error cases
  defmodule NoPingHandler do
    @behaviour ClientHandler

    @impl true
    def init(args), do: {:ok, args}

    @impl true
    def handle_list_roots(state), do: {:ok, [], state}

    @impl true
    def handle_create_message(_params, state), do: {:ok, %{}, state}

    @impl true
    def terminate(_reason, _state), do: :ok
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
      # Use Service with beam transport
      {:ok, server} = TestPingServer.start_link(%{})

      # Wait for service registration
      {:ok, _} = wait_for_service_registration(:test_ping_server)

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          service_name: :test_ping_server
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
      ExMCP.Native.unregister_service(:test_ping_server)
    end

    test "client ping respects timeout" do
      {:ok, server} = TestPingServer.start_link(%{})
      {:ok, _} = wait_for_service_registration(:test_ping_server)

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          service_name: :test_ping_server
        )

      # Wait for initialization
      Process.sleep(100)

      # Short timeout should still work for ping
      assert {:ok, %{}} = Client.ping(client, 100)

      # Cleanup
      GenServer.stop(client)
      GenServer.stop(server)
      ExMCP.Native.unregister_service(:test_ping_server)
    end
  end

  describe "server to client ping" do
    test "server can ping client successfully" do
      {:ok, server} = TestPingServer.start_link(%{})
      {:ok, _} = wait_for_service_registration(:test_ping_server)

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          service_name: :test_ping_server,
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
      ExMCP.Native.unregister_service(:test_ping_server)
    end

    test "server ping fails when client doesn't have handler" do
      {:ok, server} = SlowPingServer.start_link(%{})
      {:ok, _} = wait_for_service_registration(:slow_ping_server)

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          service_name: :slow_ping_server,
          handler: NoPingHandler
        )

      # Wait for initialization
      Process.sleep(100)

      # Server ping should fail
      assert {:error, :ping_not_implemented} = Server.ping(server)

      # Cleanup
      GenServer.stop(client)
      GenServer.stop(server)
      ExMCP.Native.unregister_service(:slow_ping_server)
    end
  end

  describe "bidirectional ping" do
    test "both client and server can ping each other" do
      {:ok, server} = TestPingServer.start_link(%{})
      {:ok, _} = wait_for_service_registration(:test_ping_server)

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          service_name: :test_ping_server,
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
      ExMCP.Native.unregister_service(:test_ping_server)
    end
  end

  describe "ping error handling" do
    test "ping handles transport failures gracefully" do
      # Create a client without a server, skipping the connection attempt
      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: :non_existent_server,
          _skip_connect: true
        )

      # Ping should fail with connection error
      assert {:error, :not_connected} = Client.ping(client)

      # Cleanup
      GenServer.stop(client)
    end

    test "ping respects timeout on slow connections" do
      {:ok, server} = SlowPingServer.start_link(%{})
      {:ok, _} = wait_for_service_registration(:slow_ping_server)

      # Start client with custom transport that delays responses
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          service_name: :slow_ping_server
        )

      # Wait for initialization
      Process.sleep(100)

      # Normal ping should work
      assert {:ok, %{}} = Client.ping(client, 1000)

      # Cleanup
      GenServer.stop(client)
      GenServer.stop(server)
      ExMCP.Native.unregister_service(:slow_ping_server)
    end
  end

  describe "ping as health check" do
    test "periodic pings can detect connection health" do
      {:ok, server} = TestPingServer.start_link(%{})
      {:ok, _} = wait_for_service_registration(:test_ping_server)

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          service_name: :test_ping_server
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
      ExMCP.Native.unregister_service(:test_ping_server)
    end
  end
end
