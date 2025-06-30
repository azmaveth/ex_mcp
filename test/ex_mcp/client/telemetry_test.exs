defmodule ExMCP.Client.TelemetryTest do
  use ExUnit.Case, async: false

  alias ExMCP.Client.StateMachine
  alias ExMCP.TestHelpers.TestTransport

  setup do
    # Capture telemetry events
    test_pid = self()

    telemetry_events = [
      [:ex_mcp, :client, :state_transition],
      [:ex_mcp, :client, :request, :start],
      [:ex_mcp, :client, :request, :success],
      [:ex_mcp, :client, :request, :error],
      [:ex_mcp, :client, :connection, :success],
      [:ex_mcp, :client, :transport, :error],
      [:ex_mcp, :client, :transport, :closed],
      [:ex_mcp, :client, :handshake, :start],
      [:ex_mcp, :client, :handshake, :success],
      [:ex_mcp, :client, :handshake, :error],
      [:ex_mcp, :client, :reconnect, :attempt],
      [:ex_mcp, :client, :reconnect, :success],
      [:ex_mcp, :client, :reconnect, :error],
      [:ex_mcp, :client, :reconnect, :timeout],
      [:ex_mcp, :client, :progress, :update],
      [:ex_mcp, :client, :progress, :unknown_token],
      [:ex_mcp, :client, :progress, :rate_limited],
      [:ex_mcp, :client, :progress, :not_increasing],
      [:ex_mcp, :client, :progress, :untracked]
    ]

    handler_id = "test_telemetry_#{System.unique_integer()}"

    :telemetry.attach_many(
      handler_id,
      telemetry_events,
      fn event, measurements, metadata, _config ->
        send(test_pid, {:telemetry, event, measurements, metadata})
      end,
      %{}
    )

    on_exit(fn ->
      :telemetry.detach(handler_id)
    end)

    %{test_pid: test_pid}
  end

  describe "state transition telemetry" do
    test "emits state_transition events" do
      config = %{transport: TestTransport, test_mode: :controlled}
      {:ok, client} = StateMachine.start_link(config)

      # Should receive initial state transition event
      assert_receive {:telemetry, [:ex_mcp, :client, :state_transition], %{count: 1},
                      %{from_state: :disconnected, to_state: :disconnected}}

      # Connect to trigger transition
      :ok = StateMachine.connect(client)

      # Should receive multiple transition events
      assert_receive {:telemetry, [:ex_mcp, :client, :state_transition], %{count: 1},
                      %{from_state: :disconnected, to_state: :connecting}}

      assert_receive {:telemetry, [:ex_mcp, :client, :state_transition], %{count: 1},
                      %{from_state: :connecting, to_state: :handshaking}}
    end
  end

  describe "request lifecycle telemetry" do
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

    test "emits request start telemetry", %{client: client} do
      task =
        Task.async(fn ->
          StateMachine.request(client, "test/method", %{foo: "bar"})
        end)

      # Should receive request start event
      assert_receive {:telemetry, [:ex_mcp, :client, :request, :start], %{count: 1},
                      %{method: "test/method", state: :ready}}

      # Complete the request to avoid timeout
      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"success" => true}
      })

      Task.await(task)
    end

    test "emits request success telemetry with duration", %{client: client} do
      task =
        Task.async(fn ->
          StateMachine.request(client, "test/method", %{})
        end)

      Process.sleep(50)

      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"data" => "test"}
      })

      Task.await(task)

      # Should receive success event with duration
      assert_receive {:telemetry, [:ex_mcp, :client, :request, :success],
                      %{count: 1, duration: duration}, %{method: "test/method", request_id: 1}}

      assert is_integer(duration)
      assert duration >= 0
    end

    test "emits request error telemetry", %{client: client} do
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

      Task.await(task)

      # Should receive error event with duration
      assert_receive {:telemetry, [:ex_mcp, :client, :request, :error],
                      %{count: 1, duration: duration},
                      %{method: "error/method", request_id: 1, reason: error}}

      assert is_integer(duration)
      assert error["code"] == -32000
    end
  end

  describe "handshake telemetry" do
    test "emits handshake events" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      :ok = StateMachine.connect(client)

      # Should receive handshake start event
      assert_receive {:telemetry, [:ex_mcp, :client, :handshake, :start], %{count: 1},
                      %{transport: TestTransport}}

      # Give time for the handshake request to be stored in transport
      Process.sleep(10)

      # Complete handshake with correct dynamic ID
      TestTransport.respond_to_handshake(client)

      # Should receive handshake success event
      assert_receive {:telemetry, [:ex_mcp, :client, :handshake, :success], %{count: 1},
                      %{transport: TestTransport}}
    end

    test "emits handshake error on failure" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      :ok = StateMachine.connect(client)

      # Give time for the handshake request to be stored in transport
      Process.sleep(10)

      # Send error response with correct dynamic ID
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

      # Should receive handshake error event
      assert_receive {:telemetry, [:ex_mcp, :client, :handshake, :error], %{count: 1}, metadata}

      assert metadata.transport == TestTransport
      assert match?({:initialize_failed, _}, metadata.reason)
    end
  end

  describe "progress tracking telemetry" do
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

    test "emits progress update telemetry", %{client: client} do
      progress_token = "test_progress_#{System.unique_integer()}"

      # Make request with progress callback
      task =
        Task.async(fn ->
          StateMachine.request(client, "long/operation", %{},
            progress_callback: fn _progress -> :ok end,
            progress_token: progress_token
          )
        end)

      Process.sleep(50)

      # Send progress notification
      TestTransport.send_to_client(client, %{
        "method" => "notifications/progress",
        "params" => %{
          "progressToken" => progress_token,
          "progress" => 50,
          "total" => 100,
          "message" => "Half done"
        }
      })

      # Should receive progress telemetry
      assert_receive {:telemetry, [:ex_mcp, :client, :progress, :update],
                      %{count: 1, progress: 50, total: 100},
                      %{token: ^progress_token, message: "Half done"}}

      # Complete request
      TestTransport.send_to_client(client, %{
        "id" => 1,
        "result" => %{"done" => true}
      })

      Task.await(task)
    end

    test "emits unknown token telemetry", %{client: client} do
      # Send progress for unknown token
      TestTransport.send_to_client(client, %{
        "method" => "notifications/progress",
        "params" => %{
          "progressToken" => "unknown_token",
          "progress" => 25
        }
      })

      # Should receive unknown token telemetry
      assert_receive {:telemetry, [:ex_mcp, :client, :progress, :unknown_token], %{count: 1},
                      %{token: "unknown_token"}}
    end
  end

  describe "transport telemetry" do
    test "emits transport error telemetry" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      :ok = StateMachine.connect(client)
      # Wait longer for state machine to reach handshaking
      Process.sleep(50)

      # Simulate transport error during handshaking
      send(client, {:transport_error, :connection_lost})
      Process.sleep(50)

      # Should receive transport error telemetry
      assert_receive {:telemetry, [:ex_mcp, :client, :transport, :error], %{count: 1},
                      %{reason: :connection_lost, state: state}}

      assert state in [:handshaking, :connecting]
    end

    test "emits transport closed telemetry" do
      config = %{transport: TestTransport, test_mode: :interactive}
      {:ok, client} = StateMachine.start_link(config)

      :ok = StateMachine.connect(client)
      Process.sleep(10)

      # Complete handshake to get to ready state
      TestTransport.respond_to_handshake(client)

      Process.sleep(50)

      # Simulate transport closed
      send(client, {:transport_closed, :normal})
      Process.sleep(50)

      # Should receive transport closed telemetry with reconnect action
      assert_receive {:telemetry, [:ex_mcp, :client, :transport, :closed], %{count: 1},
                      %{reason: :normal, state: :ready, action: :reconnect}}
    end
  end

  describe "telemetry configuration" do
    test "can retrieve all telemetry events" do
      events = ExMCP.Client.Configuration.telemetry_events()

      # Verify we have all expected event types
      assert [:ex_mcp, :client, :state_transition] in events
      assert [:ex_mcp, :client, :request, :start] in events
      assert [:ex_mcp, :client, :request, :success] in events
      assert [:ex_mcp, :client, :request, :error] in events
      assert [:ex_mcp, :client, :handshake, :start] in events
      assert [:ex_mcp, :client, :progress, :update] in events
      assert [:ex_mcp, :client, :reconnect, :attempt] in events

      # Should have at least 18 different events
      assert length(events) >= 18
    end
  end
end
