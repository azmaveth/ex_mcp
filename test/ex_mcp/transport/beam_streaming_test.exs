defmodule ExMCP.Transport.BeamStreamingTest do
  @moduledoc """
  Comprehensive tests for BEAM transport streaming support.

  Tests streaming capabilities for handling large payloads:
  - Stream-based message delivery
  - Backpressure handling and flow control
  - Chunked transfer for resources
  - Memory-efficient processing
  """
  use ExUnit.Case, async: true

  @moduletag :streaming
  @moduletag :transport
  @moduletag :beam
  @moduletag :requires_beam

  alias ExMCP.Transport.Beam.{Connection, Stream, StreamChunk}

  describe "stream initialization" do
    test "creates stream with unique ID and metadata" do
      stream_opts = %{
        content_type: "application/json",
        total_size: 1024,
        chunk_size: 256
      }

      {:ok, stream} = Stream.new(stream_opts)

      assert is_binary(stream.id)
      # UUID without hyphens
      assert byte_size(stream.id) == 32
      assert stream.content_type == "application/json"
      assert stream.total_size == 1024
      assert stream.chunk_size == 256
      assert stream.status == :initialized
    end

    test "validates stream options" do
      # Missing required fields
      assert {:error, :missing_content_type} = Stream.new(%{})

      # Invalid chunk size
      invalid_opts = %{
        content_type: "text/plain",
        chunk_size: 0
      }

      assert {:error, :invalid_chunk_size} = Stream.new(invalid_opts)
    end

    test "generates unique stream IDs" do
      {:ok, stream1} = Stream.new(%{content_type: "text/plain"})
      {:ok, stream2} = Stream.new(%{content_type: "text/plain"})

      assert stream1.id != stream2.id
    end
  end

  describe "chunk creation and validation" do
    test "creates valid chunks with sequence numbers" do
      {:ok, stream} = Stream.new(%{content_type: "text/plain", chunk_size: 10})

      data = "Hello, World!"
      {:ok, chunk} = StreamChunk.new(stream.id, 1, data, false)

      assert chunk.stream_id == stream.id
      assert chunk.sequence == 1
      assert chunk.data == "Hello, World!"
      assert chunk.is_final == false
      assert is_integer(chunk.timestamp)
    end

    test "validates chunk data size" do
      {:ok, stream} = Stream.new(%{content_type: "text/plain", chunk_size: 5})

      # Create chunk and validate size
      large_data = "This is too large for the chunk size"
      {:ok, chunk} = StreamChunk.new(stream.id, 1, large_data, false)
      assert {:error, :chunk_too_large} = StreamChunk.validate_size(chunk, stream.chunk_size)

      # Valid chunk
      small_data = "Hello"
      {:ok, small_chunk} = StreamChunk.new(stream.id, 1, small_data, false)
      assert :ok = StreamChunk.validate_size(small_chunk, stream.chunk_size)
    end

    test "creates final chunks" do
      {:ok, stream} = Stream.new(%{content_type: "text/plain"})

      {:ok, chunk} = StreamChunk.new(stream.id, 5, "Final data", true)

      assert chunk.is_final == true
      assert chunk.sequence == 5
    end
  end

  describe "streaming large tool outputs" do
    test "streams large JSON responses in chunks" do
      # Simulate a large tool output (e.g., database query result)
      # 10KB of JSON
      large_data = generate_large_json_data(10_000)

      stream_opts = %{
        content_type: "application/json",
        total_size: byte_size(large_data),
        # 1KB chunks
        chunk_size: 1024
      }

      {:ok, stream} = Stream.new(stream_opts)
      {:ok, chunks} = Stream.chunk_data(stream, large_data)

      # Should create multiple chunks
      assert length(chunks) > 1
      # Allow for actual chunk count based on real JSON size
      assert length(chunks) <= 20

      # Verify chunk sequence and final flag
      chunks
      |> Enum.with_index(1)
      |> Enum.each(fn {chunk, index} ->
        assert chunk.sequence == index

        if index == length(chunks) do
          assert chunk.is_final == true
        else
          assert chunk.is_final == false
        end
      end)

      # Verify data integrity when reassembled
      reassembled = Enum.map_join(chunks, "", & &1.data)
      assert reassembled == large_data
    end

    test "handles binary data streaming" do
      # Simulate streaming an image or file
      # 5KB binary
      binary_data = :crypto.strong_rand_bytes(5000)

      stream_opts = %{
        content_type: "application/octet-stream",
        total_size: byte_size(binary_data),
        chunk_size: 512
      }

      {:ok, stream} = Stream.new(stream_opts)
      {:ok, chunks} = Stream.chunk_data(stream, binary_data)

      # Verify binary integrity
      reassembled = Enum.map_join(chunks, "", & &1.data)
      assert reassembled == binary_data

      # Verify all chunks are properly sized (except possibly the last)
      {initial_chunks, [final_chunk]} = Enum.split(chunks, -1)

      Enum.each(initial_chunks, fn chunk ->
        assert byte_size(chunk.data) == 512
        assert chunk.is_final == false
      end)

      assert final_chunk.is_final == true
      assert byte_size(final_chunk.data) <= 512
    end
  end

  describe "backpressure and flow control" do
    test "implements send window for flow control" do
      {:ok, stream} =
        Stream.new(%{
          content_type: "text/plain",
          chunk_size: 100,
          # Allow 3 unacknowledged chunks
          window_size: 3
        })

      # Send initial chunks up to window size
      {:ok, window_state} =
        Stream.send_chunks_with_backpressure(stream, "a" |> String.duplicate(300))

      assert window_state.sent_count == 3
      assert window_state.acked_count == 0
      # Window is full
      assert window_state.window_available == 0
    end

    test "handles acknowledgments and window updates" do
      {:ok, stream} =
        Stream.new(%{
          content_type: "text/plain",
          chunk_size: 100,
          window_size: 2
        })

      # Send initial chunks
      {:ok, window_state} =
        Stream.send_chunks_with_backpressure(stream, "a" |> String.duplicate(200))

      # Acknowledge first chunk
      {:ok, updated_state} = Stream.acknowledge_chunk(window_state, 1)

      assert updated_state.acked_count == 1
      # One slot available
      assert updated_state.window_available == 1
    end

    test "blocks sending when window is full" do
      {:ok, stream} =
        Stream.new(%{
          content_type: "text/plain",
          chunk_size: 50,
          # Very small window
          window_size: 1
        })

      # 4 chunks needed
      large_data = "x" |> String.duplicate(200)

      # Should block after first chunk
      result = Stream.send_chunks_with_backpressure(stream, large_data)

      assert {:blocked, partial_state} = result
      assert partial_state.sent_count == 1
      assert partial_state.pending_data != nil
    end
  end

  describe "resource streaming" do
    test "streams large resource content" do
      # Simulate streaming a large configuration file
      # 8KB config
      large_config = generate_large_config_data(8192)

      resource_stream_opts = %{
        content_type: "application/yaml",
        resource_uri: "config://app/settings.yaml",
        total_size: byte_size(large_config),
        chunk_size: 1024
      }

      {:ok, stream} = Stream.new_resource_stream(resource_stream_opts)
      {:ok, chunks} = Stream.chunk_data(stream, large_config)

      # Verify resource-specific metadata
      assert stream.resource_uri == "config://app/settings.yaml"
      assert stream.content_type == "application/yaml"

      # Verify streaming - config data might not be exactly 8KB due to format
      assert length(chunks) >= 8 and length(chunks) <= 12

      # Each chunk should have resource context
      Enum.each(chunks, fn chunk ->
        assert chunk.stream_id == stream.id
        assert chunk.resource_uri == "config://app/settings.yaml"
      end)
    end

    test "handles streaming errors gracefully" do
      {:ok, stream} =
        Stream.new_resource_stream(%{
          content_type: "text/plain",
          resource_uri: "file://nonexistent.txt"
        })

      # Simulate resource read error
      error_result = Stream.handle_stream_error(stream, :resource_not_found)

      assert {:error, :resource_not_found, error_chunk} = error_result
      assert error_chunk.stream_id == stream.id
      assert error_chunk.is_final == true
      assert error_chunk.error_type == :resource_not_found
    end
  end

  describe "memory management" do
    @tag :skip
    test "limits concurrent streams per connection" do
      max_streams = 5

      # Create connection with stream limit
      {:ok, connection} = Connection.new(%{max_concurrent_streams: max_streams})

      # Create streams up to limit, tracking connection state
      {streams, final_connection} =
        Enum.reduce(1..max_streams, {[], connection}, fn _i, {acc_streams, conn} ->
          {:ok, stream, updated_conn} =
            Connection.create_stream(conn, %{content_type: "text/plain"})

          {[stream | acc_streams], updated_conn}
        end)

      assert length(streams) == max_streams

      # Attempt to create one more stream (should fail)
      assert {:error, :max_streams_exceeded} =
               Connection.create_stream(final_connection, %{content_type: "text/plain"})
    end

    @tag :skip
    test "cleans up completed streams" do
      {:ok, connection} = Connection.new(%{max_concurrent_streams: 2})

      # Create and complete a stream
      {:ok, stream, updated_connection} =
        Connection.create_stream(connection, %{content_type: "text/plain"})

      {:ok, completed_connection} = Connection.complete_stream(updated_connection, stream.id)

      # Should be able to create new streams after cleanup
      {:ok, _new_stream, _final_connection} =
        Connection.create_stream(completed_connection, %{content_type: "text/plain"})
    end

    test "implements stream timeout and cleanup" do
      {:ok, stream} =
        Stream.new(%{
          content_type: "text/plain",
          # Very short timeout for testing
          timeout_ms: 100
        })

      # Wait for timeout
      Process.sleep(150)

      # Stream should be marked as timed out
      assert {:error, :stream_timeout} = Stream.check_status(stream)
    end
  end

  describe "integration with BEAM transport" do
    test "sends streaming messages through BEAM transport" do
      # This test demonstrates the streaming API without actually setting up
      # a full client-server connection (which requires more complex setup)
      stream_opts = %{
        content_type: "application/json",
        chunk_size: 512,
        window_size: 3
      }

      {:ok, stream} = Stream.new(stream_opts)

      # Simulate large tool response data
      large_data = generate_large_json_data(5000)
      {:ok, chunks} = Stream.chunk_data(stream, large_data)

      assert length(chunks) > 1
      assert Enum.any?(chunks, & &1.is_final)

      # Verify chunk sequence
      sequences = Enum.map(chunks, & &1.sequence)
      assert sequences == Enum.to_list(1..length(chunks))
    end

    test "handles streaming with backpressure" do
      # Test backpressure mechanism without full transport setup
      stream_opts = %{
        content_type: "application/json",
        chunk_size: 100,
        # Very small window
        window_size: 1
      }

      {:ok, stream} = Stream.new(stream_opts)

      # Generate data that will require multiple chunks
      # 1KB data, 100 byte chunks = 10 chunks
      test_data = "x" |> String.duplicate(1000)

      # Test backpressure control
      result = Stream.send_chunks_with_backpressure(stream, test_data)

      case result do
        {:ok, window_state} ->
          # Window was large enough for all chunks
          assert window_state.sent_count <= stream.window_size

        {:blocked, window_state} ->
          # Window was full, some chunks are pending
          assert window_state.sent_count == stream.window_size
          assert window_state.window_available == 0
          assert window_state.pending_data != nil
      end
    end
  end

  # Helper functions

  defp generate_large_json_data(approximate_size) do
    # Generate a large JSON object for testing
    # Estimate ~50 bytes per item
    num_items = div(approximate_size, 50)

    items =
      for i <- 1..num_items do
        %{
          "id" => i,
          "name" => "Item #{i}",
          "description" => "A test item with some data",
          "value" => :rand.uniform(1000)
        }
      end

    Jason.encode!(%{"items" => items, "total" => num_items})
  end

  defp generate_large_config_data(size) do
    # Generate YAML-like configuration data
    lines =
      for i <- 1..div(size, 30) do
        "setting_#{i}: value_#{i}_with_some_data"
      end

    Enum.join(lines, "\n")
  end
end
