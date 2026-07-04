defmodule ExMCP.Client.StateMachineSimpleTest do
  use ExUnit.Case, async: true

  alias ExMCP.Client.StateMachine

  defmodule SimpleTransport do
    @behaviour ExMCP.Transport

    defstruct [:test_pid, :receive_timeout]

    @impl true
    def connect(opts) do
      test_pid = Keyword.get(opts, :test_pid)
      receive_timeout = Keyword.get(opts, :receive_timeout, 5_000)
      {:ok, %__MODULE__{test_pid: test_pid, receive_timeout: receive_timeout}}
    end

    @impl true
    def send_message(message, %__MODULE__{test_pid: test_pid} = state) do
      send(test_pid, {:sent_message, message})
      {:ok, state}
    end

    @impl true
    def receive_message(%__MODULE__{receive_timeout: receive_timeout} = state) do
      receive do
        {:mock_message, message} ->
          {:ok, message, state}
      after
        receive_timeout ->
          {:error, :timeout}
      end
    end

    @impl true
    def close(_state), do: :ok

    @impl true
    def connected?(_state), do: true
  end

  describe "state machine transport integration" do
    test "connects and performs handshake" do
      test_pid = self()

      config = %{
        transport: SimpleTransport,
        test_pid: test_pid
      }

      {:ok, client} = StateMachine.start_link(config)

      # Start connection
      assert :ok = StateMachine.connect(client)

      # Should receive initialize request
      assert_receive {:sent_message, init_msg}, 1000
      {:ok, init_request} = Jason.decode(init_msg)

      assert init_request["method"] == "initialize"
      assert init_request["params"]["protocolVersion"] == "2025-03-26"
      assert is_map(init_request["params"]["capabilities"])

      # Send initialize response
      init_response = %{
        "jsonrpc" => "2.0",
        "result" => %{
          "protocolVersion" => "2025-03-26",
          "capabilities" => %{},
          "serverInfo" => %{"name" => "test-server"}
        },
        "id" => init_request["id"]
      }

      send(client, {:transport_message, Jason.encode!(init_response)})

      # Should receive initialized notification
      assert_receive {:sent_message, initialized_msg}, 1000
      {:ok, initialized} = Jason.decode(initialized_msg)

      assert initialized["method"] == "notifications/initialized"
      assert initialized["params"] == %{}

      # Should be in ready state
      state_info = assert_state(client, :ready)
      assert state_info.state == :ready
      assert state_info.connected == true
    end

    test "handles request/response" do
      test_pid = self()

      # Set up connected client
      config = %{
        transport: SimpleTransport,
        test_pid: test_pid
      }

      {:ok, client} = StateMachine.start_link(config)
      :ok = StateMachine.connect(client)

      # Handle handshake
      assert_receive {:sent_message, init_msg}, 1000
      {:ok, init_request} = Jason.decode(init_msg)

      init_response = %{
        "jsonrpc" => "2.0",
        "result" => %{
          "protocolVersion" => "2025-03-26",
          "capabilities" => %{},
          "serverInfo" => %{"name" => "test-server"}
        },
        "id" => init_request["id"]
      }

      send(client, {:transport_message, Jason.encode!(init_response)})
      assert_receive {:sent_message, _initialized_msg}, 1000
      assert_state(client, :ready)

      # Now send a request
      task =
        Task.async(fn ->
          StateMachine.request(client, "test/method", %{foo: "bar"})
        end)

      # Should receive the request
      assert_receive {:sent_message, request_msg}, 1000
      {:ok, request} = Jason.decode(request_msg)

      assert request["method"] == "test/method"
      assert request["params"] == %{"foo" => "bar"}
      assert is_integer(request["id"])

      # Send response
      response = %{
        "jsonrpc" => "2.0",
        "result" => %{"success" => true},
        "id" => request["id"]
      }

      send(client, {:transport_message, Jason.encode!(response)})

      # Should get the result
      assert {:ok, %{"success" => true}} = Task.await(task)
    end

    test "handles transport errors" do
      test_pid = self()

      config = %{
        transport: SimpleTransport,
        test_pid: test_pid
      }

      {:ok, client} = StateMachine.start_link(config)
      :ok = StateMachine.connect(client)

      # Handle handshake
      assert_receive {:sent_message, init_msg}, 1000
      {:ok, init_request} = Jason.decode(init_msg)

      init_response = %{
        "jsonrpc" => "2.0",
        "result" => %{
          "protocolVersion" => "2025-03-26",
          "capabilities" => %{},
          "serverInfo" => %{"name" => "test-server"}
        },
        "id" => init_request["id"]
      }

      send(client, {:transport_message, Jason.encode!(init_response)})
      assert_receive {:sent_message, _initialized_msg}, 1000

      # Verify we're in ready state
      state_info = assert_state(client, :ready)
      assert state_info.state == :ready

      # Send transport error
      send(client, {:transport_error, :connection_lost})

      # Should be disconnected
      state_info = assert_state(client, :disconnected)
      assert state_info.state == :disconnected
    end

    test "handles reconnection" do
      test_pid = self()

      config = %{
        transport: SimpleTransport,
        test_pid: test_pid,
        max_reconnect_attempts: 1
      }

      {:ok, client} = StateMachine.start_link(config)
      :ok = StateMachine.connect(client)

      # Handle handshake
      assert_receive {:sent_message, init_msg}, 1000
      {:ok, init_request} = Jason.decode(init_msg)

      init_response = %{
        "jsonrpc" => "2.0",
        "result" => %{
          "protocolVersion" => "2025-03-26",
          "capabilities" => %{},
          "serverInfo" => %{"name" => "test-server"}
        },
        "id" => init_request["id"]
      }

      send(client, {:transport_message, Jason.encode!(init_response)})
      assert_receive {:sent_message, _initialized_msg}, 1000

      # Verify we're in ready state
      state_info = assert_state(client, :ready)
      assert state_info.state == :ready

      # Send transport closed (should trigger reconnection)
      send(client, {:transport_closed, :connection_lost})

      # Should be reconnecting
      state_info = assert_state(client, :reconnecting)
      assert state_info.state == :reconnecting

      # Wait for the reconnection backoff and observe the retry handshake.
      assert_receive {:sent_message, reconnect_init_msg}, 1500
      {:ok, reconnect_init_request} = Jason.decode(reconnect_init_msg)
      assert reconnect_init_request["method"] == "initialize"

      send(client, {:transport_error, :reconnect_failed})

      # With max_reconnect_attempts set to 1, the failed retry should give up.
      state_info = assert_state(client, :disconnected)
      assert state_info.state == :disconnected

      # This test demonstrates that reconnection is triggered properly
      # More comprehensive reconnection testing is done in integration tests
    end
  end

  defp assert_state(client, expected_state, timeout \\ 1_000) do
    deadline = System.monotonic_time(:millisecond) + timeout
    do_assert_state(client, expected_state, deadline)
  end

  defp do_assert_state(client, expected_state, deadline) do
    state_info = StateMachine.get_state(client)

    if state_info.state == expected_state do
      state_info
    else
      remaining = deadline - System.monotonic_time(:millisecond)

      if remaining <= 0 do
        flunk("expected #{inspect(expected_state)}, got #{inspect(state_info.state)}")
      else
        Process.sleep(min(10, remaining))
        do_assert_state(client, expected_state, deadline)
      end
    end
  end
end
