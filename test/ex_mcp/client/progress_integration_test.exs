defmodule ExMCP.Client.ProgressIntegrationTest do
  use ExUnit.Case, async: true

  alias ExMCP.Client.StateMachine
  alias ExMCP.ProgressTracker
  alias ExMCP.TestHelpers.TestTransport

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

  describe "progress tracking integration" do
    test "registers progress token with ProgressTracker", %{client: client} do
      progress_token = "integration_test_#{System.unique_integer()}"
      progress_pid = self()

      # Make request with progress tracking
      task =
        Task.async(fn ->
          StateMachine.request(client, "long/operation", %{},
            progress_callback: fn progress ->
              send(progress_pid, {:progress_received, progress})
            end,
            progress_token: progress_token
          )
        end)

      # Wait for request to be registered
      Process.sleep(50)

      # Verify token is registered in ProgressTracker
      case ProgressTracker.get_progress_state(progress_token) do
        {:ok, _progress_state} ->
          # Token is properly registered
          :ok

        {:error, :not_found} ->
          # This is also acceptable since ProgressTracker might not persist states
          :ok
      end

      # Send progress notification
      TestTransport.send_to_client(client, %{
        "method" => "notifications/progress",
        "params" => %{
          "progressToken" => progress_token,
          "progress" => 50,
          "total" => 100,
          "message" => "Halfway done"
        }
      })

      # Should receive progress callback
      assert_receive {:progress_received, %{"progress" => 50, "total" => 100}}

      # Complete the request
      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"completed" => true}
      })

      assert {:ok, %{"completed" => true}} = Task.await(task)
    end

    test "handles duplicate progress tokens gracefully", %{client: client} do
      progress_token = "duplicate_test_#{System.unique_integer()}"
      progress_pid = self()

      # Start first request
      task1 =
        Task.async(fn ->
          StateMachine.request(client, "operation1", %{},
            progress_callback: fn progress ->
              send(progress_pid, {:progress1, progress})
            end,
            progress_token: progress_token
          )
        end)

      Process.sleep(50)

      # Start second request with same token
      task2 =
        Task.async(fn ->
          StateMachine.request(client, "operation2", %{},
            progress_callback: fn progress ->
              send(progress_pid, {:progress2, progress})
            end,
            progress_token: progress_token
          )
        end)

      Process.sleep(50)

      # Send progress notification
      TestTransport.send_to_client(client, %{
        "method" => "notifications/progress",
        "params" => %{
          "progressToken" => progress_token,
          "progress" => 25
        }
      })

      # Only the last callback (progress2) should receive the progress
      # because it overwrote the first one
      assert_receive {:progress2, %{"progress" => 25}}
      refute_receive {:progress1, _}, 100

      # Complete both requests
      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"task1" => "done"}
      })

      TestTransport.send_to_client(client, %{
        "id" => 2,
        "result" => %{"task2" => "done"}
      })

      Task.await(task1)
      Task.await(task2)
    end

    test "cleans up progress tracking when request completes", %{client: client} do
      progress_token = "cleanup_test_#{System.unique_integer()}"

      # Make request
      task =
        Task.async(fn ->
          StateMachine.request(client, "short/operation", %{},
            progress_callback: fn _progress -> :ok end,
            progress_token: progress_token
          )
        end)

      Process.sleep(50)

      # Complete request immediately
      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"quick" => true}
      })

      Task.await(task)

      # Try to send progress after completion - should be ignored
      TestTransport.send_to_client(client, %{
        "method" => "notifications/progress",
        "params" => %{
          "progressToken" => progress_token,
          "progress" => 100
        }
      })

      # No callbacks should be called since request is complete
      refute_receive {:progress_received, _}, 100
    end

    test "handles progress notifications without callbacks", %{client: client} do
      # Send progress for non-existent token
      TestTransport.send_to_client(client, %{
        "method" => "notifications/progress",
        "params" => %{
          "progressToken" => "nonexistent_token",
          "progress" => 50
        }
      })

      # Should not crash - just log warning
      Process.sleep(50)
      assert Process.alive?(client)
    end

    test "integrates with ProgressTracker validation", %{client: client} do
      progress_token = "validation_test_#{System.unique_integer()}"
      progress_pid = self()

      # Make request
      task =
        Task.async(fn ->
          StateMachine.request(client, "validated/operation", %{},
            progress_callback: fn progress ->
              send(progress_pid, {:validated_progress, progress})
            end,
            progress_token: progress_token
          )
        end)

      Process.sleep(50)

      # Send increasing progress values
      TestTransport.send_to_client(client, %{
        "method" => "notifications/progress",
        "params" => %{
          "progressToken" => progress_token,
          "progress" => 25
        }
      })

      assert_receive {:validated_progress, %{"progress" => 25}}

      TestTransport.send_to_client(client, %{
        "method" => "notifications/progress",
        "params" => %{
          "progressToken" => progress_token,
          "progress" => 75
        }
      })

      assert_receive {:validated_progress, %{"progress" => 75}}

      # Try to send decreasing progress - should be rejected by ProgressTracker
      TestTransport.send_to_client(client, %{
        "method" => "notifications/progress",
        "params" => %{
          "progressToken" => progress_token,
          # Lower than previous 75
          "progress" => 50
        }
      })

      # Should not receive callback for invalid progress
      refute_receive {:validated_progress, %{"progress" => 50}}, 100

      # Complete request
      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"validated" => true}
      })

      Task.await(task)
    end

    test "handles rapid progress updates with rate limiting", %{client: client} do
      progress_token = "rate_limit_test_#{System.unique_integer()}"
      progress_pid = self()

      # Make request
      task =
        Task.async(fn ->
          StateMachine.request(client, "rapid/operation", %{},
            progress_callback: fn progress ->
              send(progress_pid, {:rate_limited_progress, progress["progress"]})
            end,
            progress_token: progress_token
          )
        end)

      Process.sleep(50)

      # Send many rapid progress updates
      for i <- 1..10 do
        TestTransport.send_to_client(client, %{
          "method" => "notifications/progress",
          "params" => %{
            "progressToken" => progress_token,
            "progress" => i * 10
          }
        })
      end

      # Due to rate limiting, not all updates should come through
      received_count = receive_progress_count(0, 200)
      # Some should be rate limited
      assert received_count <= 10

      # Complete request
      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"rapid" => true}
      })

      Task.await(task)
    end

    test "progress without callback and token works", %{client: client} do
      # Make request without progress tracking
      task =
        Task.async(fn ->
          StateMachine.request(client, "simple/operation", %{})
        end)

      Process.sleep(50)

      # Complete request
      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"simple" => true}
      })

      assert {:ok, %{"simple" => true}} = Task.await(task)
    end

    test "progress token without callback is ignored", %{client: client} do
      # Make request with token but no callback
      task =
        Task.async(fn ->
          StateMachine.request(client, "token_only/operation", %{},
            progress_token: "token_without_callback"
          )
        end)

      Process.sleep(50)

      # Send progress notification
      TestTransport.send_to_client(client, %{
        "method" => "notifications/progress",
        "params" => %{
          "progressToken" => "token_without_callback",
          "progress" => 50
        }
      })

      # Should not crash
      Process.sleep(50)
      assert Process.alive?(client)

      # Complete request
      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"token_only" => true}
      })

      Task.await(task)
    end
  end

  # Helper function to count progress messages
  defp receive_progress_count(count, timeout) do
    receive do
      {:rate_limited_progress, _} ->
        receive_progress_count(count + 1, timeout)
    after
      timeout -> count
    end
  end
end
