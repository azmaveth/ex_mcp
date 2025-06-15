defmodule ExMCP.Transport.Beam.BatchTest do
  @moduledoc """
  Tests for message batching in BEAM transport.

  These tests verify that message batching works correctly for high-throughput
  scenarios, including proper triggering, ordering, and performance benefits.
  """

  use ExUnit.Case, async: true

  @moduletag :transport
  @moduletag :beam
  @moduletag :requires_beam

  alias ExMCP.Transport.Beam.Batch

  # Mock connection process for testing
  defmodule MockConnection do
    use GenServer

    def start_link(opts \\ []) do
      GenServer.start_link(__MODULE__, opts)
    end

    def get_sent_frames(pid) do
      GenServer.call(pid, :get_sent_frames)
    end

    def init(_opts) do
      {:ok, %{sent_frames: []}}
    end

    def handle_call({:send_raw_frame, frame_data}, _from, state) do
      new_frames = [frame_data | state.sent_frames]
      {:reply, :ok, %{state | sent_frames: new_frames}}
    end

    def handle_call(:get_sent_frames, _from, state) do
      # Return in reverse order (most recent first -> chronological order)
      {:reply, Enum.reverse(state.sent_frames), state}
    end
  end

  describe "batch analysis" do
    test "recommends batching for many small messages" do
      messages =
        for i <- 1..20 do
          %{"method" => "test", "params" => %{"index" => i}}
        end

      result = Batch.analyze_batching_benefit(messages)

      assert result.recommended == true
      # Significant reduction expected
      assert result.expected_reduction > 0.5
      assert result.reasoning =~ "small messages"
    end

    test "does not recommend batching for few messages" do
      messages = [%{"method" => "test"}]

      result = Batch.analyze_batching_benefit(messages)

      assert result.recommended == false
      assert result.reasoning =~ "Too few messages"
    end

    test "does not recommend batching for large messages" do
      # Create messages with large payloads
      large_data = String.duplicate("x", 15_000)

      messages =
        for _i <- 1..5 do
          %{"method" => "test", "params" => %{"data" => large_data}}
        end

      result = Batch.analyze_batching_benefit(messages)

      assert result.recommended == false
      assert result.reasoning =~ "Large messages"
    end
  end

  describe "batching lifecycle" do
    setup do
      {:ok, connection} = MockConnection.start_link()

      batch_opts = [
        connection: connection,
        max_batch_size: 3,
        max_batch_bytes: 1000,
        # 50ms for testing
        batch_timeout: 50
      ]

      {:ok, batcher} = start_supervised({Batch, batch_opts})

      %{batcher: batcher, connection: connection}
    end

    test "batches messages until size limit", %{batcher: batcher, connection: connection} do
      # Send 3 messages (should trigger size-based flush)
      message1 = %{"method" => "test1", "params" => %{}}
      message2 = %{"method" => "test2", "params" => %{}}
      message3 = %{"method" => "test3", "params" => %{}}

      assert :ok = Batch.send_message(batcher, message1)
      assert :ok = Batch.send_message(batcher, message2)
      assert :ok = Batch.send_message(batcher, message3)

      # Should have sent one batch
      frames = MockConnection.get_sent_frames(connection)
      assert length(frames) == 1

      # Decode the frame to verify contents
      <<_length::32, _version::8, type_byte::8, payload::binary>> = hd(frames)
      # :batch message type
      assert type_byte == 5

      batch_data = :erlang.binary_to_term(payload, [:safe])
      messages = batch_data["messages"]

      assert length(messages) == 3
      # First message sent
      assert hd(messages)["method"] == "test1"
      # Last message sent
      assert Enum.at(messages, 2)["method"] == "test3"
    end

    test "flushes on timeout", %{batcher: batcher, connection: connection} do
      message = %{"method" => "test_timeout", "params" => %{}}

      assert :ok = Batch.send_message(batcher, message)

      # Wait for timeout flush
      :timer.sleep(100)

      frames = MockConnection.get_sent_frames(connection)
      assert length(frames) == 1
    end

    test "manual flush works", %{batcher: batcher, connection: connection} do
      message = %{"method" => "test_manual", "params" => %{}}

      assert :ok = Batch.send_message(batcher, message)
      assert :ok = Batch.flush(batcher)

      frames = MockConnection.get_sent_frames(connection)
      assert length(frames) == 1
    end

    test "priority messages bypass batching", %{batcher: batcher, connection: connection} do
      # Send a regular message first
      regular = %{"method" => "regular", "params" => %{}}
      assert :ok = Batch.send_message(batcher, regular)

      # Send priority message
      priority = %{"method" => "priority", "params" => %{}}
      assert :ok = Batch.send_priority_message(batcher, priority)

      frames = MockConnection.get_sent_frames(connection)
      # Only priority message sent immediately
      assert length(frames) == 1

      # Verify the priority message was sent as individual frame
      <<_length::32, _version::8, type_byte::8, _payload::binary>> = hd(frames)
      # :rpc_request message type
      assert type_byte == 1
    end

    test "handles empty flush gracefully", %{batcher: batcher} do
      # Flush when no messages are pending
      assert :ok = Batch.flush(batcher)
    end
  end

  describe "statistics tracking" do
    setup do
      {:ok, connection} = MockConnection.start_link()

      batch_opts = [
        connection: connection,
        max_batch_size: 5,
        max_batch_bytes: 2000,
        batch_timeout: 100
      ]

      {:ok, batcher} = start_supervised({Batch, batch_opts})

      %{batcher: batcher, connection: connection}
    end

    test "tracks batch statistics", %{batcher: batcher} do
      initial_stats = Batch.get_stats(batcher)
      assert initial_stats.batches_sent == 0
      assert initial_stats.messages_batched == 0

      # Send enough messages to trigger a batch
      for i <- 1..5 do
        message = %{"method" => "test", "params" => %{"index" => i}}
        Batch.send_message(batcher, message)
      end

      final_stats = Batch.get_stats(batcher)
      assert final_stats.batches_sent == 1
      assert final_stats.messages_batched == 5
      assert final_stats.average_batch_size == 5.0
      assert final_stats.bytes_batched > 0
    end

    test "tracks different flush types", %{batcher: batcher} do
      # Trigger size-based flush
      for i <- 1..5 do
        message = %{"method" => "size_test", "params" => %{"index" => i}}
        Batch.send_message(batcher, message)
      end

      # Manual flush
      message = %{"method" => "manual_test", "params" => %{}}
      Batch.send_message(batcher, message)
      Batch.flush(batcher)

      # Timeout flush
      message = %{"method" => "timeout_test", "params" => %{}}
      Batch.send_message(batcher, message)
      # Wait for timeout
      :timer.sleep(150)

      stats = Batch.get_stats(batcher)
      assert stats.size_flushes >= 1
      assert stats.timeout_flushes >= 1
    end
  end

  describe "configuration" do
    setup do
      {:ok, connection} = MockConnection.start_link()

      batch_opts = [
        connection: connection,
        max_batch_size: 2,
        max_batch_bytes: 500,
        batch_timeout: 25
      ]

      {:ok, batcher} = start_supervised({Batch, batch_opts})

      %{batcher: batcher, connection: connection}
    end

    test "respects configuration limits", %{batcher: batcher, connection: connection} do
      # Should flush after 2 messages due to max_batch_size: 2
      message = %{"method" => "config_test", "params" => %{}}

      assert :ok = Batch.send_message(batcher, message)
      assert :ok = Batch.send_message(batcher, message)

      frames = MockConnection.get_sent_frames(connection)
      assert length(frames) == 1
    end

    test "allows runtime reconfiguration", %{batcher: batcher, connection: connection} do
      # Change configuration
      assert :ok = Batch.configure(batcher, max_batch_size: 5)

      # Should now allow 5 messages before flushing
      for _i <- 1..4 do
        message = %{"method" => "reconfig_test", "params" => %{}}
        Batch.send_message(batcher, message)
      end

      # Should not have flushed yet
      frames = MockConnection.get_sent_frames(connection)
      assert Enum.empty?(frames)

      # Fifth message should trigger flush
      message = %{"method" => "final_test", "params" => %{}}
      Batch.send_message(batcher, message)

      frames = MockConnection.get_sent_frames(connection)
      assert length(frames) == 1
    end
  end

  describe "error handling" do
    test "handles connection failure gracefully" do
      # Start with a connection that will fail
      {:ok, connection} = MockConnection.start_link()

      batch_opts = [
        connection: connection,
        max_batch_size: 2,
        batch_timeout: 10
      ]

      {:ok, batcher} = start_supervised({Batch, batch_opts})

      # Kill the connection
      GenServer.stop(connection)

      # Wait for batcher to detect connection down
      :timer.sleep(50)

      # Batcher should be terminated
      refute Process.alive?(batcher)
    end

    test "handles message size estimation edge cases" do
      # Test with various message types that might cause estimation issues
      messages = [
        %{"complex" => %{"nested" => [1, 2, 3]}},
        %{"binary" => :crypto.strong_rand_bytes(100)},
        %{"large_list" => Enum.to_list(1..100)},
        %{"atoms" => [:test, :message, :batching]}
      ]

      for message <- messages do
        result = Batch.analyze_batching_benefit([message])
        assert is_map(result)
        assert is_boolean(result.recommended)
        assert is_float(result.expected_reduction)
        assert is_binary(result.reasoning)
      end
    end
  end

  describe "integration scenarios" do
    setup do
      {:ok, connection} = MockConnection.start_link()

      batch_opts = [
        connection: connection,
        max_batch_size: 10,
        max_batch_bytes: 5000,
        batch_timeout: 20
      ]

      {:ok, batcher} = start_supervised({Batch, batch_opts})

      %{batcher: batcher, connection: connection}
    end

    test "handles mixed message types", %{batcher: batcher, connection: connection} do
      # Send different types of messages
      request = %{"id" => 1, "method" => "test", "params" => %{}}
      response = %{"id" => 1, "result" => "success"}
      notification = %{"method" => "notify", "params" => %{"event" => "update"}}

      Batch.send_message(batcher, request)
      Batch.send_message(batcher, response)
      Batch.send_message(batcher, notification)

      # Force flush
      Batch.flush(batcher)

      frames = MockConnection.get_sent_frames(connection)
      assert length(frames) == 1

      # Decode and verify all message types are present
      <<_length::32, _version::8, type_byte::8, payload::binary>> = hd(frames)
      # :batch
      assert type_byte == 5

      batch_data = :erlang.binary_to_term(payload, [:safe])
      messages = batch_data["messages"]

      assert length(messages) == 3

      # Verify all message types are preserved
      methods = Enum.map(messages, &Map.get(&1, "method"))
      assert "test" in methods
      assert "notify" in methods
    end

    test "maintains message ordering", %{batcher: batcher, connection: connection} do
      # Send messages with sequential IDs
      for i <- 1..5 do
        message = %{"id" => i, "method" => "sequence_test", "params" => %{}}
        Batch.send_message(batcher, message)
      end

      # Force flush
      Batch.flush(batcher)

      frames = MockConnection.get_sent_frames(connection)
      <<_length::32, _version::8, _type::8, payload::binary>> = hd(frames)

      batch_data = :erlang.binary_to_term(payload, [:safe])
      messages = batch_data["messages"]

      # Verify ordering is preserved (oldest to newest)
      ids = Enum.map(messages, &Map.get(&1, "id"))
      assert ids == [1, 2, 3, 4, 5]
    end
  end
end
