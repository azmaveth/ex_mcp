defmodule ExMCP.ClientDisconnectTest do
  @moduledoc """
  Tests for the Client.disconnect/1 function added in Phase 2.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Client
  alias ExMCP.Transport.Test, as: TestTransport

  describe "disconnect/1" do
    test "disconnect cleans up resources and updates state" do
      # Create a mock transport
      transport_state = %TestTransport{role: :client, peer_pid: self()}

      # Start client with mock setup
      {:ok, client} =
        GenServer.start_link(Client,
          transport: :test,
          # Skip the connection process
          _skip_connect: true
        )

      # Create a mock task that won't cause issues when killed
      mock_task = %Task{
        mfa: {:erlang, :apply, 2},
        owner: self(),
        pid: spawn(fn -> :timer.sleep(:infinity) end),
        ref: make_ref()
      }

      # Manually set up the client state to simulate a connected client
      state = %Client{
        transport_mod: TestTransport,
        transport_state: transport_state,
        server_info: %{"name" => "test-server"},
        connection_status: :connected,
        pending_requests: %{},
        receiver_task: mock_task,
        health_check_ref: Process.send_after(self(), :health_check, 30_000),
        health_check_interval: 30_000,
        last_activity: System.system_time(:second),
        reconnect_attempts: 0,
        client_info: %{"name" => "ExMCP", "version" => "0.8.0"}
      }

      :sys.replace_state(client, fn _ -> state end)

      # Verify initial state
      {:ok, status} = Client.get_status(client)
      assert status.connection_status == :connected

      # Call disconnect
      assert :ok = Client.disconnect(client)

      # Verify state after disconnect
      {:ok, status} = Client.get_status(client)
      assert status.connection_status == :disconnected
      assert status.pending_requests == 0

      # Verify resources were cleaned up
      final_state = :sys.get_state(client)
      assert final_state.receiver_task == nil
      assert final_state.health_check_ref == nil

      # Stop the client
      GenServer.stop(client)
    end

    test "disconnect handles pending requests" do
      # Create a mock transport
      transport_state = %TestTransport{role: :client, peer_pid: self()}

      # Start client
      {:ok, client} =
        GenServer.start_link(Client,
          transport: :test,
          _skip_connect: true
        )

      # Create a pending request with our pid and a ref
      ref = make_ref()
      from = {self(), ref}
      pending_requests = %{1 => from}

      # Set up state with pending request
      state = %Client{
        transport_mod: TestTransport,
        transport_state: transport_state,
        server_info: %{"name" => "test-server"},
        connection_status: :connected,
        pending_requests: pending_requests,
        receiver_task: nil,
        health_check_ref: nil,
        health_check_interval: 30_000,
        last_activity: System.system_time(:second),
        reconnect_attempts: 0,
        client_info: %{"name" => "ExMCP", "version" => "0.8.0"}
      }

      :sys.replace_state(client, fn _ -> state end)

      # Call disconnect
      assert :ok = Client.disconnect(client)

      # We should receive the error reply for the pending request
      # GenServer.reply sends {ref, response}
      assert_receive {^ref,
                      {:error,
                       %ExMCP.Error.TransportError{
                         transport: :connection,
                         reason: "Client disconnected"
                       }}},
                     1_000

      # Stop the client
      GenServer.stop(client)
    end

    test "disconnect is idempotent" do
      # Start a client in disconnected state
      {:ok, client} =
        GenServer.start_link(Client,
          transport: :test,
          _skip_connect: true
        )

      # Set disconnected state
      state = %Client{
        transport_mod: nil,
        transport_state: nil,
        server_info: nil,
        transport_opts: [],
        connection_status: :disconnected,
        pending_requests: %{},
        pending_batches: %{},
        receiver_task: nil,
        health_check_ref: nil,
        health_check_interval: 30_000,
        last_activity: System.system_time(:second),
        reconnect_attempts: 0,
        client_info: %{"name" => "ExMCP", "version" => "0.8.0"},
        raw_terms_enabled: false,
        server_capabilities: %{},
        initialized: false,
        default_retry_policy: [],
        protocol_version: "2025-06-18",
        default_timeout: 5000
      }

      :sys.replace_state(client, fn _ -> state end)

      # First disconnect
      assert :ok = Client.disconnect(client)

      # Second disconnect should also succeed
      assert :ok = Client.disconnect(client)

      # Status should remain disconnected
      {:ok, status} = Client.get_status(client)
      assert status.connection_status == :disconnected

      # Stop the client
      GenServer.stop(client)
    end

    test "requests fail after disconnect" do
      # Start client in disconnected state
      {:ok, client} =
        GenServer.start_link(Client,
          transport: :test,
          _skip_connect: true
        )

      # Set disconnected state
      state = %Client{
        transport_mod: nil,
        transport_state: nil,
        server_info: nil,
        transport_opts: [],
        connection_status: :disconnected,
        pending_requests: %{},
        pending_batches: %{},
        receiver_task: nil,
        health_check_ref: nil,
        health_check_interval: 30_000,
        last_activity: System.system_time(:second),
        reconnect_attempts: 0,
        client_info: %{"name" => "ExMCP", "version" => "0.8.0"},
        raw_terms_enabled: false,
        server_capabilities: %{},
        initialized: false,
        default_retry_policy: [],
        protocol_version: "2025-06-18",
        default_timeout: 5000
      }

      :sys.replace_state(client, fn _ -> state end)

      # Try to make requests - they should fail
      assert {:error, :not_connected} = Client.list_tools(client)
      assert {:error, :not_connected} = Client.call_tool(client, "test", %{})
      assert {:error, :not_connected} = Client.list_prompts(client)
      assert {:error, :not_connected} = Client.list_resources(client)

      # Stop the client
      GenServer.stop(client)
    end
  end
end
