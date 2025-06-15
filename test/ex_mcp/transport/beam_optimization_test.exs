defmodule ExMCP.Transport.BeamOptimizationTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog

  alias ExMCP.Transport.Beam
  alias ExMCP.Transport.Beam.Mailbox

  describe "Local Connection Optimizations" do
    test "detects local connections and uses optimized path" do
      # Start a named server
      server_name = :test_opt_server
      {:ok, _server_state} = Beam.accept(name: server_name, format: :native)
      
      # Connect locally
      {:ok, state} = Beam.connect(server: server_name, format: :native)
      
      assert state.mode == :local
      assert state.optimized == true
    end

    test "uses zero-copy for large native messages in local mode" do
      {:ok, state} = setup_local_connection()
      
      # Create a large message (> 64KB)
      large_data = :binary.copy(<<"test">>, 20_000)  # 80KB
      message = %{data: large_data, type: "test"}
      
      {:ok, new_state} = Beam.send_message(message, state)
      
      # Should use zero-copy optimization
      assert new_state.stats.zero_copy_sends > 0
    end

    test "bypasses JSON encoding for local native format" do
      {:ok, state} = setup_local_connection(format: :native)
      
      message = %{test: "data", nested: %{value: 123}}
      
      # Measure time for native send
      {native_time, {:ok, _}} = :timer.tc(fn ->
        Beam.send_message(message, state)
      end)
      
      # Compare with JSON format
      {:ok, json_state} = setup_local_connection(format: :json)
      {json_time, {:ok, _}} = :timer.tc(fn ->
        Beam.send_message(message, json_state)
      end)
      
      # Native should be significantly faster (at least 2x)
      assert native_time < json_time / 2
    end

    test "uses direct process messaging for local connections" do
      {:ok, state} = setup_local_connection()
      
      # Send a message
      message = %{type: "ping"}
      {:ok, _} = Beam.send_message(message, state)
      
      # Should receive directly without intermediate processes
      assert_receive {:mcp_message, ^message}, 100
      refute_received {:transport_message, _}  # Old path
    end
  end

  describe "Message Batching" do
    test "batches multiple small messages for efficiency" do
      {:ok, state} = setup_local_connection(batch_enabled: true)
      
      # Send multiple small messages quickly
      messages = for i <- 1..10, do: %{id: i, data: "small"}
      
      for msg <- messages do
        {:ok, state} = Beam.send_message_async(msg, state)
      end
      
      # Force batch flush
      {:ok, state} = Beam.flush_batch(state)
      
      # Should have sent as a single batch
      assert state.stats.batched_sends == 1
      assert state.stats.messages_batched == 10
    end

    test "auto-flushes batch after timeout" do
      {:ok, state} = setup_local_connection(
        batch_enabled: true,
        batch_timeout: 50
      )
      
      # Send a message
      {:ok, state} = Beam.send_message_async(%{test: 1}, state)
      
      # Wait for auto-flush
      Process.sleep(100)
      
      # Should have received the message
      assert_receive {:mcp_message, %{test: 1}}
    end

    test "flushes batch when size limit reached" do
      {:ok, state} = setup_local_connection(
        batch_enabled: true,
        batch_size_limit: 3
      )
      
      # Send 3 messages (should trigger flush)
      for i <- 1..3 do
        {:ok, state} = Beam.send_message_async(%{id: i}, state)
      end
      
      # Should receive batch immediately
      assert_receive {:mcp_batch, messages}
      assert length(messages) == 3
    end
  end

  describe "Memory Pool Optimization" do
    test "reuses message buffers for similar-sized messages" do
      {:ok, state} = setup_local_connection(memory_pool: true)
      
      # Send multiple similar messages
      message_template = %{data: String.duplicate("x", 1000)}
      
      initial_memory = :erlang.memory(:binary)
      
      for _ <- 1..100 do
        {:ok, state} = Beam.send_message(message_template, state)
      end
      
      final_memory = :erlang.memory(:binary)
      
      # Memory growth should be minimal due to pooling
      memory_growth = final_memory - initial_memory
      assert memory_growth < 50_000  # Less than 50KB growth
    end

    test "returns buffers to pool after message delivery" do
      {:ok, state} = setup_local_connection(memory_pool: true)
      
      # Check initial pool stats
      initial_stats = Beam.get_pool_stats(state)
      
      # Send and receive a message
      {:ok, state} = Beam.send_message(%{data: "test"}, state)
      assert_receive {:mcp_message, _}
      
      # Buffer should be returned to pool
      final_stats = Beam.get_pool_stats(state)
      assert final_stats.available_buffers >= initial_stats.available_buffers
    end
  end

  describe "Shared Memory Optimization" do
    test "uses shared memory for very large messages" do
      {:ok, state} = setup_local_connection(shared_memory: true)
      
      # Create a very large message (> 1MB)
      huge_data = :binary.copy(<<0>>, 1_048_576)  # 1MB
      message = %{payload: huge_data}
      
      # Send the message
      {:ok, state} = Beam.send_message(message, state)
      
      # Should use shared memory reference
      assert_receive {:mcp_message_ref, ref}
      
      # Dereference should give original data
      {:ok, received_msg} = Beam.deref_message(ref)
      assert received_msg.payload == huge_data
    end

    test "cleans up shared memory after use" do
      {:ok, state} = setup_local_connection(shared_memory: true)
      
      # Send large message
      message = %{data: :binary.copy(<<1>>, 1_048_576)}
      {:ok, state} = Beam.send_message(message, state)
      
      assert_receive {:mcp_message_ref, ref}
      
      # Dereference and release
      {:ok, _} = Beam.deref_message(ref)
      :ok = Beam.release_message(ref)
      
      # Reference should be invalid now
      assert {:error, :invalid_ref} = Beam.deref_message(ref)
    end
  end

  describe "Process Affinity Optimization" do
    test "pins mailbox processes to same scheduler" do
      {:ok, state} = setup_local_connection(process_affinity: true)
      
      # Get scheduler IDs
      client_scheduler = :erlang.system_info(:scheduler_id)
      server_scheduler = get_mailbox_scheduler(state)
      
      # Should be on same scheduler for cache locality
      assert client_scheduler == server_scheduler
    end

    test "maintains affinity during high load" do
      {:ok, state} = setup_local_connection(process_affinity: true)
      
      # Send many messages
      for i <- 1..1000 do
        {:ok, state} = Beam.send_message(%{id: i}, state)
      end
      
      # Check scheduler didn't change
      assert get_mailbox_scheduler(state) == :erlang.system_info(:scheduler_id)
    end
  end

  describe "Benchmarks" do
    @tag :benchmark
    test "measures throughput improvement" do
      # Standard connection
      {:ok, standard_state} = setup_local_connection(optimized: false)
      
      # Optimized connection  
      {:ok, optimized_state} = setup_local_connection(optimized: true)
      
      message = %{data: String.duplicate("test", 100)}
      iterations = 10_000
      
      # Benchmark standard
      {standard_time, _} = :timer.tc(fn ->
        for _ <- 1..iterations do
          Beam.send_message(message, standard_state)
        end
      end)
      
      # Benchmark optimized
      {optimized_time, _} = :timer.tc(fn ->
        for _ <- 1..iterations do
          Beam.send_message(message, optimized_state)
        end  
      end)
      
      improvement = standard_time / optimized_time
      IO.puts("Performance improvement: #{Float.round(improvement, 2)}x")
      
      # Should see at least 2x improvement
      assert improvement > 2.0
    end

    @tag :benchmark
    test "measures latency improvement" do
      {:ok, standard_state} = setup_local_connection(optimized: false)
      {:ok, optimized_state} = setup_local_connection(optimized: true)
      
      # Measure round-trip latency
      standard_latencies = measure_latencies(standard_state, 100)
      optimized_latencies = measure_latencies(optimized_state, 100)
      
      standard_avg = Enum.sum(standard_latencies) / length(standard_latencies)
      optimized_avg = Enum.sum(optimized_latencies) / length(optimized_latencies)
      
      IO.puts("Average latency - Standard: #{standard_avg}μs, Optimized: #{optimized_avg}μs")
      
      # Optimized should have lower latency
      assert optimized_avg < standard_avg * 0.5
    end

    @tag :benchmark  
    test "measures memory usage" do
      {:ok, standard_state} = setup_local_connection(optimized: false)
      {:ok, optimized_state} = setup_local_connection(optimized: true)
      
      # Measure memory for sending many messages
      message = %{data: String.duplicate("x", 1000)}
      
      standard_memory = measure_memory_usage(fn ->
        for _ <- 1..1000, do: Beam.send_message(message, standard_state)
      end)
      
      optimized_memory = measure_memory_usage(fn ->
        for _ <- 1..1000, do: Beam.send_message(message, optimized_state)
      end)
      
      IO.puts("Memory - Standard: #{standard_memory}, Optimized: #{optimized_memory}")
      
      # Optimized should use less memory
      assert optimized_memory < standard_memory * 0.8
    end
  end

  describe "Compatibility" do
    test "maintains compatibility with non-optimized connections" do
      # Start optimized server
      {:ok, server_state} = Beam.accept(name: :opt_server, optimized: true)
      
      # Connect with non-optimized client
      {:ok, client_state} = Beam.connect(server: :opt_server, optimized: false)
      
      # Should still work
      message = %{test: "compatibility"}
      assert {:ok, _} = Beam.send_message(message, client_state)
    end

    test "falls back to standard path for distributed connections" do
      # Simulate distributed connection
      {:ok, state} = Beam.connect(
        server: {:remote_server, :"node@host"},
        optimized: true
      )
      
      # Should not use optimizations
      assert state.optimized == false
      assert state.mode == :distributed
    end

    test "handles optimization feature negotiation" do
      {:ok, server_state} = Beam.accept(
        name: :feature_server,
        supported_features: [:zero_copy, :batching]
      )
      
      {:ok, client_state} = Beam.connect(
        server: :feature_server,
        requested_features: [:zero_copy, :batching, :compression]
      )
      
      # Should negotiate common features
      assert :zero_copy in client_state.enabled_features
      assert :batching in client_state.enabled_features
      refute :compression in client_state.enabled_features
    end
  end

  # Helper functions

  defp setup_local_connection(opts \\ []) do
    name = String.to_atom("test_server_#{System.unique_integer([:positive])}")
    
    server_opts = Keyword.merge([name: name, format: :native], opts)
    {:ok, _server} = Beam.accept(server_opts)
    
    # Give server time to initialize
    Process.sleep(10)
    
    client_opts = Keyword.merge([server: name, format: :native], opts)
    Beam.connect(client_opts)
  end

  defp get_mailbox_scheduler(%{mailbox_pid: pid}) do
    {:dictionary, dict} = Process.info(pid, :dictionary)
    Keyword.get(dict, :"$scheduler_id", nil)
  end

  defp measure_latencies(state, count) do
    for _ <- 1..count do
      start = System.monotonic_time(:microsecond)
      {:ok, _} = Beam.send_message(%{type: "ping"}, state)
      assert_receive {:mcp_message, %{type: "ping"}}
      System.monotonic_time(:microsecond) - start
    end
  end

  defp measure_memory_usage(fun) do
    :erlang.garbage_collect()
    initial = :erlang.memory(:total)
    fun.()
    :erlang.garbage_collect()
    final = :erlang.memory(:total)
    final - initial
  end
end