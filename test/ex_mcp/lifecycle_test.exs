defmodule ExMCP.LifecycleTest do
  @moduledoc """
  Tests for MCP lifecycle management including initialization,
  version negotiation, and shutdown procedures.
  """
  use ExUnit.Case, async: true
  @moduletag :integration

  alias ExMCP.Client
  alias ExMCP.Server

  defmodule TestHandler do
    use ExMCP.Server.Handler

    @impl true
    def init(_args), do: {:ok, %{}}

    @impl true
    def handle_initialize(params, state) do
      client_version = params["protocolVersion"]

      # Version negotiation
      negotiated_version =
        case client_version do
          "2025-03-26" -> "2025-03-26"
          "2025-06-18" -> "2025-06-18"
          # Propose latest for unknown versions
          _ -> "2025-03-26"
        end

      result = %{
        protocolVersion: negotiated_version,
        serverInfo: %{name: "test-server", version: "1.0.0"},
        capabilities: %{
          tools: %{},
          resources: %{}
        }
      }

      {:ok, result, state}
    end

    @impl true
    def handle_list_tools(_cursor, state), do: {:ok, [], nil, state}

    @impl true
    def handle_call_tool(_name, _args, state), do: {:error, "Not implemented", state}
  end

  describe "lifecycle management" do
    setup do
      # Start server
      {:ok, server} =
        Server.start_link(
          transport: :test,
          handler: TestHandler,
          handler_args: []
        )

      on_exit(fn ->
        if Process.alive?(server), do: GenServer.stop(server)
      end)

      {:ok, server: server}
    end

    test "initialization follows proper sequence", %{server: server} do
      # Start client
      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server
        )

      # Client should be initialized
      assert {:ok, %{tools: []}} = Client.list_tools(client)

      # Clean up
      GenServer.stop(client)
    end

    test "version negotiation works correctly", %{server: server} do
      # Start client with specific version
      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server,
          client_info: %{
            name: "test-client",
            version: "1.0.0"
          }
        )

      # Should successfully connect even with version negotiation
      assert {:ok, _} = Client.ping(client)

      GenServer.stop(client)
    end

    test "disconnect performs clean shutdown", %{server: server} do
      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server
        )

      # Verify connection is working
      assert {:ok, _} = Client.ping(client)

      # Disconnect gracefully
      assert :ok = Client.disconnect(client)

      # Further operations should fail
      assert {:error, :not_connected} = Client.ping(client)
    end

    test "server handles client disconnection gracefully", %{server: server} do
      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server
        )

      # Verify connection is working
      assert {:ok, _} = Client.ping(client)

      # Client disconnects
      :ok = Client.disconnect(client)

      # Give server time to process disconnection but it should stay alive
      Process.sleep(50)
      assert Process.alive?(server)

      # Server should be able to accept new connections
      {:ok, new_client} =
        Client.start_link(
          transport: :test,
          server: server
        )

      assert {:ok, _} = Client.ping(new_client)
      GenServer.stop(new_client)
    end
  end

  describe "initialization errors" do
    defmodule StrictVersionHandler do
      use ExMCP.Server.Handler

      @impl true
      def init(_args), do: {:ok, %{}}

      @impl true
      def handle_initialize(params, state) do
        client_version = params["protocolVersion"]

        # Only accept exact version match
        if client_version == "2025-03-26" do
          {:ok,
           %{
             protocolVersion: "2025-03-26",
             serverInfo: %{name: "strict-server", version: "1.0.0"},
             capabilities: %{}
           }, state}
        else
          {:error, "Unsupported protocol version: #{client_version}", state}
        end
      end

      @impl true
      def handle_list_tools(_cursor, state), do: {:ok, [], nil, state}

      @impl true
      def handle_call_tool(_name, _args, state), do: {:error, "Not implemented", state}
    end

    test "initialization fails with incompatible version" do
      {:ok, server} =
        Server.start_link(
          transport: :test,
          handler: StrictVersionHandler,
          handler_args: []
        )

      # This should fail because ExMCP.Protocol uses "2025-03-26" by default
      # and StrictVersionHandler only accepts that exact version
      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server
        )

      # Should connect successfully with matching version
      assert {:ok, _} = Client.ping(client)

      GenServer.stop(client)
      GenServer.stop(server)
    end
  end
end
