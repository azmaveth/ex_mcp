defmodule ExMCP.Client.StateMachineTest do
  use ExUnit.Case, async: true

  alias ExMCP.Client.StateMachine
  alias ExMCP.Client.States
  alias ExMCP.Transport.Test, as: TestTransport

  describe "initialization" do
    test "starts in disconnected state" do
      config = %{transport: TestTransport, server: self()}
      {:ok, client} = StateMachine.start_link(config)

      state_info = StateMachine.get_state(client)
      assert state_info.state == :disconnected
      assert state_info.data_type == States.Disconnected
      assert state_info.connected == false
    end

    test "accepts initialization options" do
      config = %{transport: TestTransport, server: self()}
      opts = [name: :test_client, on_error: fn _ -> :ok end]

      {:ok, client} = StateMachine.start_link(config, opts)
      assert is_pid(client)
    end
  end

  describe "state transitions" do
    setup do
      config = %{transport: TestTransport, server: self()}
      {:ok, client} = StateMachine.start_link(config)
      %{client: client, config: config}
    end

    test "disconnected -> connecting on connect", %{client: client} do
      assert :ok = StateMachine.connect(client)

      # The connection will fail because test server doesn't respond
      # Give it a moment to process
      Process.sleep(20)

      state_info = StateMachine.get_state(client)
      # Should be back to disconnected after failed handshake
      assert state_info.state == :disconnected
    end

    test "cannot connect when already connected", %{client: client} do
      assert :ok = StateMachine.connect(client)
      assert {:error, :already_connected} = StateMachine.connect(client)
    end

    test "disconnect returns to disconnected state", %{client: client} do
      # First connect
      assert :ok = StateMachine.connect(client)

      # Then disconnect
      assert :ok = StateMachine.disconnect(client)

      state_info = StateMachine.get_state(client)
      assert state_info.state == :disconnected
    end

    test "disconnect is idempotent", %{client: client} do
      assert :ok = StateMachine.disconnect(client)
      assert :ok = StateMachine.disconnect(client)
    end
  end

  describe "request handling" do
    setup do
      config = %{transport: TestTransport, server: self()}
      {:ok, client} = StateMachine.start_link(config)
      %{client: client}
    end

    test "cannot send request when disconnected", %{client: client} do
      assert {:error, {:not_connected, :disconnected}} =
               StateMachine.request(client, "test/method", %{})
    end

    test "cannot send request when connecting", %{client: client} do
      # Start connection
      assert :ok = StateMachine.connect(client)

      # Immediately try to send request (should still be connecting)
      assert {:error, {:not_connected, state}} =
               StateMachine.request(client, "test/method", %{})

      assert state in [:connecting, :handshaking]
    end
  end

  describe "error handling" do
    setup do
      config = %{transport: TestTransport, server: self()}
      {:ok, client} = StateMachine.start_link(config)
      %{client: client}
    end

    test "transitions to disconnected on transport error", %{client: client} do
      # Connect first
      assert :ok = StateMachine.connect(client)

      # Simulate transport error
      send(client, {:transport_error, :connection_lost})

      # Give it a moment to process
      Process.sleep(10)

      state_info = StateMachine.get_state(client)
      assert state_info.state == :disconnected
    end
  end

  describe "state data integrity" do
    test "each state has appropriate data structure" do
      config = %{transport: TestTransport, server: self()}

      # Test creating each state type
      common = States.Common.new(config, [])

      disconnected = States.Disconnected.new(config, [])
      assert disconnected.common.config == config
      assert disconnected.retry_count == 0

      connecting = %States.Connecting{
        common: common,
        start_time: System.monotonic_time(),
        attempt_number: 1
      }

      assert connecting.common == common

      handshaking = %States.Handshaking{
        common: common,
        transport: {TestTransport, %{}, self()},
        client_info: %{},
        handshake_start_time: System.monotonic_time()
      }

      assert handshaking.transport == {TestTransport, %{}, self()}

      ready = %States.Ready{
        common: common,
        transport: {TestTransport, %{}, self()},
        server_info: %{"name" => "test"},
        capabilities: %{},
        pending_requests: %{},
        next_request_id: 1,
        progress_callbacks: %{},
        initialized_capability: nil
      }

      assert ready.server_info["name"] == "test"
      assert ready.pending_requests == %{}
      assert ready.next_request_id == 1
    end
  end
end
