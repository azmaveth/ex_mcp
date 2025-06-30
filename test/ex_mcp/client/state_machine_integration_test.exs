defmodule ExMCP.Client.StateMachineIntegrationTest do
  use ExUnit.Case, async: true

  alias ExMCP.Client.StateMachine
  alias ExMCP.Client.StateMachineAdapter
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

      assert {:error, _} = StateMachine.connect(client)
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

      TestTransport.send_to_client(client, %{
        "id" => "init_1",
        "result" => %{
          "protocolVersion" => "2025-03-26",
          "capabilities" => %{},
          "serverInfo" => %{"name" => "test", "version" => "1.0"}
        }
      })

      Process.sleep(50)

      %{client: client}
    end

    test "handles successful request", %{client: client} do
      task =
        Task.async(fn ->
          StateMachine.request(client, "test/method", %{foo: "bar"})
        end)

      # Server echoes back the request
      Process.sleep(50)

      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"echo" => %{"method" => "test/method", "params" => %{"foo" => "bar"}}}
      })

      assert {:ok, %{"echo" => %{"method" => "test/method"}}} = Task.await(task)
    end

    test "handles request error", %{client: client} do
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

    test "handles concurrent requests", %{client: client} do
      # Start 5 concurrent requests
      tasks =
        for i <- 1..5 do
          Task.async(fn ->
            StateMachine.request(client, "test/method#{i}", %{n: i})
          end)
        end

      # Send responses in reverse order
      for i <- 5..1 do
        Process.sleep(10)

        TestTransport.send_to_client(client, %{
          "id" => i,
          "result" => %{"n" => i}
        })
      end

      # All should complete successfully
      results = Enum.map(tasks, &Task.await/1)

      for i <- 1..5 do
        assert {:ok, %{"n" => ^i}} = Enum.at(results, i - 1)
      end
    end

    test "handles request timeout", %{client: client} do
      # Request with short timeout
      result = StateMachine.request(client, "slow/method", %{}, timeout: 100)

      # No response sent
      assert {:error, :timeout} = result
    end
  end

  describe "reconnection" do
    test "automatically reconnects on transport failure" do
      config = %{
        transport: TestTransport,
        test_mode: :flaky,
        max_reconnect_attempts: 3,
        reconnect_backoff_ms: 50
      }

      {:ok, client} = StateMachine.start_link(config)

      # Connect successfully
      :ok = StateMachine.connect(client)
      Process.sleep(50)

      # Complete handshake
      TestTransport.send_to_client(client, %{
        "id" => "init_1",
        "result" => %{
          "protocolVersion" => "2025-03-26",
          "capabilities" => %{},
          "serverInfo" => %{"name" => "test", "version" => "1.0"}
        }
      })

      Process.sleep(50)
      assert %{state: :ready} = StateMachine.get_state(client)

      # Simulate transport failure
      TestTransport.simulate_disconnect(client)

      # Should transition to reconnecting
      Process.sleep(50)
      assert %{state: :reconnecting} = StateMachine.get_state(client)

      # Wait for reconnection
      Process.sleep(100)

      # Should reconnect and complete handshake again
      TestTransport.send_to_client(client, %{
        "id" => "init_2",
        "result" => %{
          "protocolVersion" => "2025-03-26",
          "capabilities" => %{},
          "serverInfo" => %{"name" => "test", "version" => "1.0"}
        }
      })

      Process.sleep(50)
      assert %{state: :ready} = StateMachine.get_state(client)
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

      TestTransport.send_to_client(client, %{
        "id" => "init_1",
        "result" => %{
          "protocolVersion" => "2025-03-26",
          "capabilities" => %{},
          "serverInfo" => %{"name" => "test", "version" => "1.0"}
        }
      })

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

      TestTransport.send_to_client(client, %{
        "id" => "init_1",
        "result" => %{
          "protocolVersion" => "2025-03-26",
          "capabilities" => %{},
          "serverInfo" => %{"name" => "test", "version" => "1.0"}
        }
      })

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

      TestTransport.send_to_client(client, %{
        "id" => "init_1",
        "result" => %{
          "protocolVersion" => "2025-03-26",
          "capabilities" => %{"progress" => true},
          "serverInfo" => %{"name" => "test", "version" => "1.0"}
        }
      })

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
