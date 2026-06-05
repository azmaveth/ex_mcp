defmodule ExMCP.Client.ErrorScenariosTest do
  use ExUnit.Case, async: true

  alias ExMCP.Client.StateMachine
  alias ExMCP.TestHelpers.TestTransport

  describe "connection error scenarios" do
    test "handles transport connection failure" do
      config = %{transport: TestTransport, test_mode: :fail_connect}
      {:ok, client} = StateMachine.start_link(config)
      attach_state_transition_listener(client)

      # Connection attempt returns :ok immediately (async)
      assert :ok = StateMachine.connect(client)

      # Wait for the failure to drive the state back to :disconnected.
      # `from_state` filter skips the initial :disconnected → :connecting
      # event so we get the post-failure transition.
      assert_receive {:state_transition, %{from_state: :connecting, to_state: :disconnected}},
                     2_000
    end

    test "handles handshake timeout" do
      config = %{transport: TestTransport, test_mode: :no_response}
      {:ok, client} = StateMachine.start_link(config)
      attach_state_transition_listener(client)

      assert :ok = StateMachine.connect(client)

      # Default handshake timeout is 5s; wait up to 7s for the resulting
      # :handshaking → :disconnected transition. Using assert_receive
      # lets the test pass the moment the timeout fires rather than
      # padding wall-clock with a fixed 6s sleep.
      assert_receive {:state_transition, %{from_state: :handshaking, to_state: :disconnected}},
                     7_000
    end

    test "handles handshake error response" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)
      attach_state_transition_listener(client)

      :ok = StateMachine.connect(client)
      assert_receive {:state_transition, %{to_state: :handshaking}}, 2_000

      # Send error response to handshake. respond_to_handshake's polling
      # mechanism is for the success path; here we want to send an error,
      # so we poll get_last_sent_id ourselves via wait_for_sent_id.
      case TestTransport.wait_for_sent_id(client, 2_000) do
        {:ok, id} ->
          TestTransport.send_to_client(client, %{
            "id" => id,
            "error" => %{"code" => -32602, "message" => "Invalid params"}
          })

        {:error, _} ->
          # Fallback if unable to get ID
          TestTransport.send_to_client(client, %{
            "id" => "init_1",
            "error" => %{"code" => -32602, "message" => "Invalid params"}
          })
      end

      assert_receive {:state_transition, %{from_state: :handshaking, to_state: :disconnected}},
                     2_000
    end

    test "handles malformed handshake response" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)
      attach_state_transition_listener(client)

      :ok = StateMachine.connect(client)
      assert_receive {:state_transition, %{to_state: :handshaking}}, 2_000

      # Send malformed response — the state machine should ignore it
      # and stay alive (eventually timing out the handshake, but that's
      # not what we're asserting here).
      TestTransport.send_raw_to_client(client, "not json")
      Process.sleep(50)

      assert Process.alive?(client)
    end
  end

  describe "request error scenarios" do
    setup do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)
      attach_state_transition_listener(client)

      # Connect and complete handshake using telemetry-driven waits so
      # the setup doesn't race on CPU-busy CI runners.
      :ok = StateMachine.connect(client)
      assert_receive {:state_transition, %{to_state: :handshaking}}, 2_000

      TestTransport.respond_to_handshake(client)
      assert_receive {:state_transition, %{to_state: :ready}}, 2_000

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

      error_messages =
        for result <- results do
          assert {:error, error} = result
          error["message"]
        end

      # All 5 error messages should be present (order may vary due to concurrency)
      for i <- 1..5 do
        assert "Error #{i}" in error_messages
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
      attach_state_transition_listener(client)
      :ok = StateMachine.connect(client)
      assert_receive {:state_transition, %{to_state: :connecting}}, 2_000

      # Simulate transport error during handshake. The transition back
      # to :disconnected may go from any in-flight state (:connecting
      # or :handshaking) depending on what the receive consumed before
      # the transport_error message arrived.
      send(client, {:transport_error, :connection_lost})

      assert_receive {:state_transition, %{to_state: :disconnected}}, 2_000
    end

    test "handles transport closure in ready state", %{client: client} do
      # Subscribe to state transitions for this client BEFORE triggering
      # them. The previous fixed-sleep version (50ms between connect
      # and respond_to_handshake) flaked on CPU-busy CI runners — the
      # client hadn't transitioned to :handshaking yet when the
      # response landed, so the state machine ended up :disconnected
      # instead of :ready. Telemetry-driven waiting eliminates the
      # race and the assertion fires the instant the transition lands,
      # not on the next sleep boundary.
      attach_state_transition_listener(client)

      :ok = StateMachine.connect(client)
      assert_receive {:state_transition, %{to_state: :handshaking}}, 2_000

      # respond_to_handshake polls internally for the handshake request
      # to be buffered at the transport — it can race the :handshaking
      # state transition since the send is scheduled asynchronously.
      TestTransport.respond_to_handshake(client)
      assert_receive {:state_transition, %{to_state: :ready}}, 2_000

      # Simulate transport closure
      send(client, {:transport_closed, :normal})

      # Should attempt reconnection. Match on `from_state: :ready` so
      # we look at the transition triggered BY the closure — earlier
      # transitions (e.g. the initial :disconnected → :connecting that
      # the first assert_receive skipped past on its way to :handshaking)
      # are still in the mailbox.
      assert_receive {:state_transition, %{from_state: :ready, to_state: to}}, 2_000
      assert to in [:reconnecting, :handshaking]
    end

    test "handles transport closure in non-ready states", %{client: client} do
      attach_state_transition_listener(client)
      :ok = StateMachine.connect(client)
      assert_receive {:state_transition, %{to_state: :connecting}}, 2_000

      # Close transport while connecting/handshaking — should go to
      # :disconnected, not :reconnecting (reconnection only triggers
      # from :ready).
      send(client, {:transport_closed, :connection_reset})

      assert_receive {:state_transition, %{to_state: :disconnected}}, 2_000
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
      attach_state_transition_listener(client)

      assert :ok = StateMachine.connect(client)

      # Wait for the state machine to settle back at :disconnected
      # after all retries exhaust. Match on `to_state: :disconnected`
      # — multiple transitions may queue before this one (connecting,
      # back-and-forth on retries), but the final stable state is the
      # one we care about. 5s is comfortably above the retry budget.
      wait_for_final_state(:disconnected, 5_000)
    end

    test "handles reconnection timeout" do
      config = %{
        transport: TestTransport,
        test_mode: :reconnect_timeout,
        max_reconnect_attempts: 1,
        reconnect_backoff_ms: 50
      }

      {:ok, client} = StateMachine.start_link(config)
      attach_state_transition_listener(client)

      StateMachine.connect(client)

      # `:reconnect_timeout` is only handled while in `:reconnecting`
      # state. Drive the client into :reconnecting first by completing
      # handshake → ready → transport_closed, THEN inject the timeout.
      # The original test sent :reconnect_timeout immediately after
      # connect/1, which the state machine logs as "Unhandled event in
      # state handshaking" and silently ignores — the original test
      # passed by accident, not by design.
      assert_receive {:state_transition, %{to_state: :handshaking}}, 2_000

      TestTransport.respond_to_handshake(client)
      assert_receive {:state_transition, %{to_state: :ready}}, 2_000

      send(client, {:transport_closed, :connection_lost})
      assert_receive {:state_transition, %{from_state: :ready, to_state: :reconnecting}}, 2_000

      # Now inject the timeout — handled correctly in :reconnecting,
      # should drive the state machine to :disconnected.
      send(client, :reconnect_timeout)

      wait_for_final_state(:disconnected, 2_000)
    end

    test "handles errors during reconnection transport setup" do
      # This test verifies that when reconnection attempts fail,
      # the client eventually gives up after max attempts.
      config = %{
        transport: TestTransport,
        test_mode: :flaky,
        max_reconnect_attempts: 2,
        reconnect_backoff_ms: 50
      }

      {:ok, client} = StateMachine.start_link(config)
      attach_state_transition_listener(client)

      # Connect successfully first (flaky mode allows first connection)
      :ok = StateMachine.connect(client)
      assert_receive {:state_transition, %{to_state: :handshaking}}, 2_000

      TestTransport.respond_to_handshake(client)
      assert_receive {:state_transition, %{to_state: :ready}}, 2_000

      # Simulate disconnection to trigger reconnection.
      # Don't use simulate_disconnect as it unregisters the transport;
      # just send the transport_closed message.
      send(client, {:transport_closed, :connection_lost})
      assert_receive {:state_transition, %{from_state: :ready, to_state: :reconnecting}}, 2_000

      # Wait for retry budget to exhaust. With 2 attempts × 50ms
      # initial backoff + exponential (up to ~2s on attempt 2), 5s is
      # comfortably above the budget.
      wait_for_final_state(:disconnected, 5_000)
    end
  end

  describe "edge case error scenarios" do
    test "handles server sending unknown notifications" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)
      attach_state_transition_listener(client)

      :ok = StateMachine.connect(client)
      assert_receive {:state_transition, %{to_state: :handshaking}}, 2_000

      TestTransport.respond_to_handshake(client)
      assert_receive {:state_transition, %{to_state: :ready}}, 2_000

      # Send unknown notification
      TestTransport.send_to_client(client, %{
        "method" => "notifications/unknown_notification",
        "params" => %{"data" => "test"}
      })

      # Should not crash. Brief sleep is fine here — we're not waiting
      # for a state transition, just verifying the process survives.
      Process.sleep(50)
      assert Process.alive?(client)
    end

    test "handles server sending unexpected requests" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)
      attach_state_transition_listener(client)

      :ok = StateMachine.connect(client)
      assert_receive {:state_transition, %{to_state: :handshaking}}, 2_000

      TestTransport.respond_to_handshake(client)
      assert_receive {:state_transition, %{to_state: :ready}}, 2_000

      # Send unexpected server request
      TestTransport.send_to_client(client, %{
        "id" => "server_request_1",
        "method" => "unknown_server_request",
        "params" => %{}
      })

      # Should not crash (no response will be sent).
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

  # ── Helpers ────────────────────────────────────────────────────────

  # Subscribe the test process to state transitions emitted by `client`.
  # The handler filters on `metadata.pid` so async tests with multiple
  # state machines don't see each other's transitions.
  #
  # Prefer `assert_receive {:state_transition, %{to_state: X}}, timeout`
  # over `Process.sleep(N) + assert state.state == X` — the latter is
  # timing-sensitive on CPU-busy CI runners and has caused flakes.
  #
  # The handler is auto-detached on test exit.
  defp attach_state_transition_listener(client) do
    handler_id =
      "state-transition-listener-#{:erlang.unique_integer([:positive])}"

    test_pid = self()

    :ok =
      :telemetry.attach(
        handler_id,
        [:ex_mcp, :client, :state_transition],
        fn _event, _measurements, metadata, _config ->
          if metadata[:pid] == client do
            send(test_pid, {:state_transition, metadata})
          end
        end,
        nil
      )

    ExUnit.Callbacks.on_exit(fn -> :telemetry.detach(handler_id) end)
    :ok
  end

  # Drain state-transition messages until we see `target_state` or
  # the timeout elapses. Used for tests that need the FINAL state after
  # multiple transitions (retries, reconnect-then-give-up flows) rather
  # than a specific from/to pair.
  defp wait_for_final_state(target_state, timeout_ms) do
    deadline = System.monotonic_time(:millisecond) + timeout_ms
    do_wait_for_final_state(target_state, deadline)
  end

  defp do_wait_for_final_state(target_state, deadline) do
    remaining = max(0, deadline - System.monotonic_time(:millisecond))

    receive do
      {:state_transition, %{to_state: ^target_state}} ->
        # Saw the target — drain any further transitions briefly to
        # confirm we've settled, then return.
        receive do
          {:state_transition, %{to_state: other}} when other != target_state ->
            do_wait_for_final_state(target_state, deadline)
        after
          50 -> :ok
        end

      {:state_transition, _other} ->
        do_wait_for_final_state(target_state, deadline)
    after
      remaining ->
        ExUnit.Assertions.flunk(
          "wait_for_final_state: did not reach #{inspect(target_state)} within timeout"
        )
    end
  end
end
