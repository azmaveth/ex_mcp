defmodule ExMCP.Client.ErrorScenariosTest do
  use ExUnit.Case, async: true

  alias ExMCP.Client.StateMachine
  alias ExMCP.TestHelpers.TestTransport

  describe "connection error scenarios" do
    test "handles transport connection failure" do
      config = %{transport: TestTransport, test_mode: :fail_connect}
      {:ok, client} = StateMachine.start_link(config)

      # Connection attempt returns :ok immediately (async)
      assert :ok = StateMachine.connect(client)

      # Wait for async connection to fail
      Process.sleep(100)

      # Should be back in disconnected state after failure
      state = StateMachine.get_state(client)
      assert state.state == :disconnected
    end

    test "handles handshake timeout" do
      config = %{transport: TestTransport, test_mode: :no_response}
      {:ok, client} = StateMachine.start_link(config)

      assert :ok = StateMachine.connect(client)

      # Wait for handshake timeout (5 seconds default)
      Process.sleep(6000)

      # Should be back to disconnected
      state = StateMachine.get_state(client)
      assert state.state == :disconnected
    end

    test "handles handshake error response" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      :ok = StateMachine.connect(client)
      Process.sleep(50)

      # Send error response to handshake with correct dynamic ID
      case TestTransport.get_last_sent_id(client) do
        {:ok, id} ->
          TestTransport.send_to_client(client, %{
            "id" => id,
            "error" => %{
              "code" => -32602,
              "message" => "Invalid params"
            }
          })

        {:error, _} ->
          # Fallback if unable to get ID
          TestTransport.send_to_client(client, %{
            "id" => "init_1",
            "error" => %{
              "code" => -32602,
              "message" => "Invalid params"
            }
          })
      end

      Process.sleep(50)

      # Should transition to disconnected
      state = StateMachine.get_state(client)
      assert state.state == :disconnected
    end

    test "handles malformed handshake response" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      :ok = StateMachine.connect(client)
      Process.sleep(50)

      # Send malformed response
      TestTransport.send_raw_to_client(client, "not json")
      Process.sleep(50)

      # Should still be alive and eventually timeout
      assert Process.alive?(client)
    end
  end

  describe "request error scenarios" do
    setup do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      # Connect and complete handshake
      :ok = StateMachine.connect(client)
      Process.sleep(50)

      TestTransport.respond_to_handshake(client)

      Process.sleep(50)

      %{client: client}
    end

    test "handles request timeout", %{client: client} do
      # Make request with short timeout
      result = StateMachine.request(client, "slow/method", %{}, timeout: 100)

      # Should timeout since no response is sent
      assert {:error, :timeout} = result
    end

    test "handles server error response", %{client: client} do
      task =
        Task.async(fn ->
          StateMachine.request(client, "error/method", %{})
        end)

      Process.sleep(50)

      # Send error response
      TestTransport.send_to_client(client, %{
        "id" => 1,
        "error" => %{
          "code" => -32000,
          "message" => "Internal server error",
          "data" => %{"detail" => "Something went wrong"}
        }
      })

      assert {:error, error} = Task.await(task)
      assert error["code"] == -32000
      assert error["message"] == "Internal server error"
    end

    test "handles malformed response", %{client: client} do
      task =
        Task.async(fn ->
          # Use shorter timeout for testing
          StateMachine.request(client, "malformed/method", %{}, timeout: 500)
        end)

      Process.sleep(50)

      # Send malformed JSON
      TestTransport.send_raw_to_client(client, "{invalid json")

      # Should timeout since malformed response is ignored
      assert {:error, :timeout} = Task.await(task, 1000)
    end

    test "handles response with wrong ID", %{client: client} do
      task =
        Task.async(fn ->
          # Use shorter timeout for testing
          StateMachine.request(client, "wrong_id/method", %{}, timeout: 500)
        end)

      Process.sleep(50)

      # Send response with wrong ID
      TestTransport.send_to_client(client, %{
        # Wrong ID
        "id" => 999,
        "result" => %{"data" => "test"}
      })

      # Should timeout since response doesn't match
      assert {:error, :timeout} = Task.await(task, 1000)
    end

    test "handles concurrent request failures", %{client: client} do
      # Start multiple requests
      tasks =
        for i <- 1..5 do
          Task.async(fn ->
            StateMachine.request(client, "concurrent_error_#{i}", %{})
          end)
        end

      Process.sleep(50)

      # Send error responses for all
      for i <- 1..5 do
        TestTransport.send_to_client(client, %{
          "id" => i,
          "error" => %{
            "code" => -32000,
            "message" => "Error #{i}"
          }
        })
      end

      # All should receive their respective errors
      results = Enum.map(tasks, &Task.await/1)

      for {result, i} <- Enum.with_index(results, 1) do
        assert {:error, error} = result
        assert error["message"] == "Error #{i}"
      end
    end
  end

  describe "transport error scenarios" do
    setup do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)
      %{client: client}
    end

    test "handles transport error during connection", %{client: client} do
      :ok = StateMachine.connect(client)

      # Simulate transport error during handshake
      send(client, {:transport_error, :connection_lost})
      Process.sleep(50)

      # Should be disconnected
      state = StateMachine.get_state(client)
      assert state.state == :disconnected
    end

    test "handles transport closure in ready state", %{client: client} do
      # Connect and complete handshake
      :ok = StateMachine.connect(client)
      Process.sleep(50)

      TestTransport.respond_to_handshake(client)

      Process.sleep(50)

      # Verify ready state
      state = StateMachine.get_state(client)
      assert state.state == :ready

      # Simulate transport closure
      send(client, {:transport_closed, :normal})
      Process.sleep(50)

      # Should attempt reconnection (may be in reconnecting or already moved to handshaking)
      state = StateMachine.get_state(client)
      assert state.state in [:reconnecting, :handshaking]
    end

    test "handles transport closure in non-ready states", %{client: client} do
      :ok = StateMachine.connect(client)

      # Close transport while connecting/handshaking
      send(client, {:transport_closed, :connection_reset})
      Process.sleep(50)

      # Should go to disconnected (not reconnecting)
      state = StateMachine.get_state(client)
      assert state.state == :disconnected
    end
  end

  describe "reconnection error scenarios" do
    test "gives up after max reconnect attempts" do
      config = %{
        transport: TestTransport,
        test_mode: :always_fail,
        max_reconnect_attempts: 2,
        reconnect_backoff_ms: 50
      }

      {:ok, client} = StateMachine.start_link(config)

      # Connection attempt returns :ok immediately but fails asynchronously
      assert :ok = StateMachine.connect(client)

      # Wait for connection attempt to fail and retries to be exhausted
      Process.sleep(300)

      # Should be disconnected after failing connection attempts
      state = StateMachine.get_state(client)
      assert state.state == :disconnected
    end

    test "handles reconnection timeout" do
      config = %{
        transport: TestTransport,
        test_mode: :reconnect_timeout,
        max_reconnect_attempts: 1,
        reconnect_backoff_ms: 50
      }

      {:ok, client} = StateMachine.start_link(config)

      # Attempt connection
      StateMachine.connect(client)

      # Simulate reconnection timeout
      send(client, :reconnect_timeout)
      Process.sleep(50)

      # Should be disconnected
      state = StateMachine.get_state(client)
      assert state.state == :disconnected
    end

    test "handles errors during reconnection transport setup" do
      # This test verifies that when reconnection attempts fail,
      # the client eventually gives up after max attempts
      config = %{
        transport: TestTransport,
        # Use flaky mode which fails on reconnection
        test_mode: :flaky,
        # Small number for faster test
        max_reconnect_attempts: 2,
        reconnect_backoff_ms: 50
      }

      {:ok, client} = StateMachine.start_link(config)

      # Connect successfully first (flaky mode allows first connection)
      :ok = StateMachine.connect(client)
      Process.sleep(50)

      # Complete handshake to get to ready
      TestTransport.respond_to_handshake(client)
      Process.sleep(50)

      # Verify ready state
      state = StateMachine.get_state(client)
      assert state.state == :ready

      # Simulate disconnection to trigger reconnection
      # Don't use simulate_disconnect as it unregisters the transport
      # Just send the transport_closed message
      send(client, {:transport_closed, :connection_lost})
      Process.sleep(50)

      # Should be in reconnecting state
      state = StateMachine.get_state(client)
      assert state.state == :reconnecting

      # Wait for reconnection attempts to be exhausted
      # With 2 attempts:
      # Attempt 1: immediate (fails), backoff becomes 2000ms
      # Attempt 2: after 2000ms (fails), then disconnects
      # Total wait time should be > 2000ms
      Process.sleep(2500)

      # Should be disconnected after max attempts
      state = StateMachine.get_state(client)
      assert state.state == :disconnected
    end
  end

  describe "edge case error scenarios" do
    test "handles server sending unknown notifications" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      :ok = StateMachine.connect(client)
      Process.sleep(50)

      # Complete handshake
      TestTransport.respond_to_handshake(client)

      Process.sleep(50)

      # Send unknown notification
      TestTransport.send_to_client(client, %{
        "method" => "notifications/unknown_notification",
        "params" => %{"data" => "test"}
      })

      # Should not crash
      Process.sleep(50)
      assert Process.alive?(client)
    end

    test "handles server sending unexpected requests" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      :ok = StateMachine.connect(client)
      Process.sleep(50)

      # Complete handshake
      TestTransport.respond_to_handshake(client)

      Process.sleep(50)

      # Send unexpected server request
      TestTransport.send_to_client(client, %{
        "id" => "server_request_1",
        "method" => "unknown_server_request",
        "params" => %{}
      })

      # Should not crash (though no response will be sent)
      Process.sleep(50)
      assert Process.alive?(client)
    end

    test "handles extremely rapid state transitions" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      # Rapidly connect and disconnect
      for _i <- 1..10 do
        spawn(fn -> StateMachine.connect(client) end)
        spawn(fn -> StateMachine.disconnect(client) end)
      end

      Process.sleep(100)

      # Should still be alive and in a valid state
      assert Process.alive?(client)
      state = StateMachine.get_state(client)
      assert state.state in [:disconnected, :connecting, :handshaking, :ready, :reconnecting]
    end

    test "handles message queue overflow gracefully" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      # Send many messages rapidly
      for i <- 1..1000 do
        send(client, {:test_message, i})
      end

      Process.sleep(100)

      # Should still be alive
      assert Process.alive?(client)
    end
  end

  describe "process lifecycle error scenarios" do
    test "handles client process crash gracefully" do
      config = %{transport: TestTransport, test_mode: :interactive}

      # Start unlinked to avoid test process crash
      {:ok, client} = GenStateMachine.start(StateMachine, {config, []})

      # Monitor the process
      ref = Process.monitor(client)

      # Force crash the process
      Process.exit(client, :kill)

      # Should receive DOWN message
      assert_receive {:DOWN, ^ref, :process, ^client, :killed}
    end

    test "handles invalid state data gracefully" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      # Try to send invalid internal state message
      send(client, {:invalid_state_message, "bad data"})

      Process.sleep(50)

      # Should still be alive
      assert Process.alive?(client)
    end
  end
end
