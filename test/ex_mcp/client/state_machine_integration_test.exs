defmodule ExMCP.Client.StateMachineIntegrationTest do
  use ExUnit.Case, async: true

  alias ExMCP.Client.StateMachine
  alias ExMCP.TestHelpers.TestTransport

  describe "state transitions" do
    test "complete lifecycle: disconnected -> connecting -> handshaking -> ready -> disconnected" do
      config = %{transport: TestTransport, test_mode: :controlled}
      {:ok, client} = StateMachine.start_link(config)

      # Start disconnected
      assert %{state: :disconnected} = StateMachine.get_state(client)

      # Connect transitions to connecting then handshaking
      assert :ok = StateMachine.connect(client)

      # Should transition through handshaking to ready automatically
      # (TestTransport in :controlled mode auto-responds to initialize)
      Process.sleep(200)
      assert %{state: :ready} = StateMachine.get_state(client)

      # Disconnect
      assert :ok = StateMachine.disconnect(client)
      assert %{state: :disconnected} = StateMachine.get_state(client)
    end

    test "handles connection failure" do
      config = %{transport: TestTransport, test_mode: :fail_connect}
      {:ok, client} = StateMachine.start_link(config)

      # Connect returns :ok immediately (async)
      assert :ok = StateMachine.connect(client)

      # Wait for async connection to fail
      Process.sleep(100)

      # Should be back in disconnected state after failure
      assert %{state: :disconnected} = StateMachine.get_state(client)
    end

    test "handles handshake timeout" do
      config = %{transport: TestTransport, test_mode: :no_response, handshake_timeout: 100}
      {:ok, client} = StateMachine.start_link(config)

      assert :ok = StateMachine.connect(client)

      # Wait for handshake timeout
      # Need to wait longer than the 5s timeout
      Process.sleep(6000)
      assert %{state: :disconnected} = StateMachine.get_state(client)
    end
  end

  describe "request handling" do
    setup do
      config = %{transport: TestTransport, test_mode: :echo}
      {:ok, client} = StateMachine.start_link(config)

      # Connect and complete handshake
      :ok = StateMachine.connect(client)
      Process.sleep(50)

      TestTransport.respond_to_handshake(client)

      Process.sleep(50)

      %{client: client}
    end

    test "handles successful request", %{client: client} do
      # The echo mode automatically responds, so we just need to wait
      result = StateMachine.request(client, "test/method", %{foo: "bar"})

      assert {:ok, %{"echo" => %{"method" => "test/method", "params" => %{"foo" => "bar"}}}} =
               result
    end

    test "handles request error" do
      # Use interactive mode to control responses
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      # Connect and complete handshake
      :ok = StateMachine.connect(client)
      Process.sleep(50)
      TestTransport.respond_to_handshake(client)
      Process.sleep(50)

      task =
        Task.async(fn ->
          StateMachine.request(client, "error/method", %{})
        end)

      Process.sleep(50)

      TestTransport.send_to_client(client, %{
        "id" => 1,
        "error" => %{
          "code" => -32000,
          "message" => "Method not found"
        }
      })

      assert {:error, %{"code" => -32000}} = Task.await(task)
    end

    test "handles concurrent requests" do
      # Use interactive mode to control responses
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      # Connect and complete handshake
      :ok = StateMachine.connect(client)
      Process.sleep(50)
      TestTransport.respond_to_handshake(client)
      Process.sleep(50)
      # Start 5 concurrent requests
      tasks =
        for i <- 1..5 do
          Task.async(fn ->
            StateMachine.request(client, "test/method#{i}", %{n: i})
          end)
        end

      # Send responses in reverse order
      for i <- 5..1//-1 do
        Process.sleep(10)

        TestTransport.send_to_client(client, %{
          "id" => i,
          "result" => %{"n" => i}
        })
      end

      # All should complete successfully
      results = Enum.map(tasks, &Task.await/1)

      # Sort results by the "n" value since tasks may complete out of order
      sorted_results =
        results
        |> Enum.sort_by(fn {:ok, %{"n" => n}} -> n end)

      for i <- 1..5 do
        assert {:ok, %{"n" => ^i}} = Enum.at(sorted_results, i - 1)
      end
    end

    test "handles request timeout" do
      # Use interactive mode to control responses manually
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      # Connect and complete handshake
      :ok = StateMachine.connect(client)
      Process.sleep(50)
      TestTransport.respond_to_handshake(client)
      Process.sleep(50)

      # Request with short timeout
      result = StateMachine.request(client, "slow/method", %{}, timeout: 100)

      # No response sent - should timeout
      assert {:error, :timeout} = result
    end
  end

  describe "reconnection" do
    test "automatically reconnects on transport failure" do
      config = %{
        transport: TestTransport,
        # Use interactive mode for successful reconnection
        test_mode: :interactive,
        max_reconnect_attempts: 3,
        reconnect_backoff_ms: 50
      }

      {:ok, client} = StateMachine.start_link(config)

      # Connect successfully
      :ok = StateMachine.connect(client)
      Process.sleep(50)

      # Complete handshake
      TestTransport.respond_to_handshake(client)

      Process.sleep(50)
      assert %{state: :ready} = StateMachine.get_state(client)

      # Start a task that will respond to the handshake as soon as it arrives
      responder_task =
        Task.async(fn ->
          # Poll for handshake request
          Enum.reduce_while(1..100, nil, fn _i, _acc ->
            case TestTransport.get_last_message(client) do
              {:ok, %{"method" => "initialize"}} ->
                # Handshake request found, respond immediately
                TestTransport.respond_to_handshake(client)
                {:halt, :ok}

              _ ->
                Process.sleep(10)
                {:cont, nil}
            end
          end)
        end)

      # Simulate transport failure
      TestTransport.simulate_disconnect(client)

      # Should transition to reconnecting immediately
      Process.sleep(10)
      state = StateMachine.get_state(client)
      assert state.state == :reconnecting

      # Wait for the responder task to complete (it will respond to handshake)
      Task.await(responder_task, 2000)

      # Wait for handshake to complete
      Process.sleep(100)

      # Should be back to ready state
      state = StateMachine.get_state(client)
      assert state.state == :ready
    end

    test "gives up after max reconnect attempts" do
      config = %{
        transport: TestTransport,
        test_mode: :always_fail,
        max_reconnect_attempts: 2,
        reconnect_backoff_ms: 50
      }

      {:ok, client} = StateMachine.start_link(config)

      # This will fail but that's ok for this test
      StateMachine.connect(client)

      # Wait for max attempts
      Process.sleep(200)

      # Should be disconnected
      assert %{state: :disconnected} = StateMachine.get_state(client)
    end
  end

  describe "server notifications" do
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

    test "handles server notifications", %{client: client} do
      # Send various notifications
      TestTransport.send_to_client(client, %{
        "method" => "notifications/resources/changed",
        "params" => %{}
      })

      TestTransport.send_to_client(client, %{
        "method" => "notifications/tools/changed",
        "params" => %{}
      })

      TestTransport.send_to_client(client, %{
        "method" => "notifications/prompts/changed",
        "params" => %{}
      })

      # Client should remain ready
      Process.sleep(50)
      assert %{state: :ready} = StateMachine.get_state(client)
    end

    test "handles server requests (ping)", %{client: client} do
      # Server sends ping request
      TestTransport.send_to_client(client, %{
        "id" => "ping_1",
        "method" => "ping",
        "params" => %{}
      })

      # Should receive pong response
      Process.sleep(50)
      assert {:ok, response} = TestTransport.get_last_message(client)
      assert response["id"] == "ping_1"
      assert response["result"] == %{}
    end
  end

  describe "edge cases" do
    test "handles malformed messages gracefully" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      :ok = StateMachine.connect(client)
      Process.sleep(50)

      # Send malformed message
      TestTransport.send_raw_to_client(client, "not json")

      # Should not crash
      Process.sleep(50)
      assert Process.alive?(client)
    end

    test "prevents requests when not connected" do
      config = %{transport: TestTransport}
      {:ok, client} = StateMachine.start_link(config)

      # Try to make request while disconnected
      assert {:error, {:not_connected, :disconnected}} =
               StateMachine.request(client, "test", %{})
    end

    test "handles duplicate response IDs" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      :ok = StateMachine.connect(client)
      Process.sleep(50)

      TestTransport.respond_to_handshake(client)

      Process.sleep(50)

      # Make a request
      task =
        Task.async(fn ->
          StateMachine.request(client, "test", %{})
        end)

      # Send response with correct ID
      Process.sleep(50)

      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"success" => true}
      })

      assert {:ok, %{"success" => true}} = Task.await(task)

      # Send another response with same ID (should be ignored)
      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"duplicate" => true}
      })

      # Should not affect anything
      Process.sleep(50)
      assert %{state: :ready} = StateMachine.get_state(client)
    end
  end

  describe "progress tracking" do
    setup do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      :ok = StateMachine.connect(client)
      Process.sleep(50)

      TestTransport.respond_to_handshake(client)

      Process.sleep(50)

      %{client: client}
    end

    test "handles progress notifications", %{client: client} do
      progress_pid = self()
      progress_token = "progress_123"

      # Make request with progress callback
      task =
        Task.async(fn ->
          StateMachine.request(client, "long/operation", %{},
            progress_callback: fn progress ->
              send(progress_pid, {:progress, progress})
            end,
            progress_token: progress_token
          )
        end)

      # Wait a moment for the request to be registered
      Process.sleep(50)

      # Send progress updates
      TestTransport.send_to_client(client, %{
        "method" => "notifications/progress",
        "params" => %{
          "progressToken" => progress_token,
          "progress" => 25,
          "message" => "Processing..."
        }
      })

      assert_receive {:progress, %{"progress" => 25}}, 100

      TestTransport.send_to_client(client, %{
        "method" => "notifications/progress",
        "params" => %{
          "progressToken" => progress_token,
          "progress" => 75,
          "message" => "Almost done..."
        }
      })

      assert_receive {:progress, %{"progress" => 75}}, 100

      # Complete the request
      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"status" => "completed"}
      })

      assert {:ok, %{"status" => "completed"}} = Task.await(task)
    end
  end
end
