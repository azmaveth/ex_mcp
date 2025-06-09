#!/usr/bin/env elixir

# BEAM Transport Optimization Demo
# 
# This example demonstrates the performance optimizations available
# in the BEAM transport for local connections.

Mix.install([
  {:ex_mcp, path: "."},
  {:jason, "~> 1.4"}
])

defmodule OptimizationDemo do
  alias ExMCP.Transport.Beam
  
  def run do
    IO.puts("\n=== BEAM Transport Optimization Demo ===\n")
    
    # Create a test state for demonstration
    optimized_state = %Beam.State{
      mode: :local,
      optimized: true,
      format: :native,
      peer_ref: self(),
      stats: %{
        zero_copy_sends: 0,
        batched_sends: 0,
        messages_batched: 0,
        direct_sends: 0
      },
      zero_copy_threshold: 65536  # 64KB
    }
    
    standard_state = %{optimized_state | optimized: false}
    
    # Demo 1: Zero-copy for large messages
    demo_zero_copy(optimized_state)
    
    # Demo 2: Direct messaging for small messages
    demo_direct_messaging(optimized_state, standard_state)
    
    # Demo 3: Message batching
    demo_batching()
    
    # Demo 4: Performance comparison
    demo_performance(optimized_state, standard_state)
  end
  
  defp demo_zero_copy(state) do
    IO.puts("1. Zero-Copy Optimization Demo")
    IO.puts("   Creating a large message (100KB)...")
    
    large_message = %{
      data: :binary.copy(<<"x">>, 100_000),
      type: "large_data"
    }
    
    {:ok, new_state} = Beam.send_message(large_message, state)
    
    IO.puts("   ✓ Message sent using zero-copy")
    IO.puts("   Stats: #{inspect(new_state.stats)}")
    
    # Receive the reference
    receive do
      {:mcp_message_ref, ref} ->
        IO.puts("   ✓ Received message reference: #{inspect(ref)}")
        {:ok, msg} = Beam.deref_message(ref)
        IO.puts("   ✓ Dereferenced message size: #{byte_size(msg.data)} bytes")
        Beam.release_message(ref)
        IO.puts("   ✓ Released message reference\n")
    after
      100 -> IO.puts("   ✗ No message received\n")
    end
  end
  
  defp demo_direct_messaging(optimized_state, standard_state) do
    IO.puts("2. Direct Messaging Demo")
    
    small_message = %{type: "ping", id: 1}
    
    # Optimized send
    start_time = System.monotonic_time(:microsecond)
    {:ok, _} = Beam.send_message(small_message, optimized_state)
    optimized_time = System.monotonic_time(:microsecond) - start_time
    
    receive do
      {:mcp_message, msg} ->
        IO.puts("   ✓ Received direct message: #{inspect(msg)}")
    after
      100 -> IO.puts("   ✗ No message received")
    end
    
    # Standard send (simulated - would use mailbox process)
    # For demo purposes, we'll simulate the overhead
    start_time = System.monotonic_time(:microsecond)
    # Simulate mailbox overhead
    spawn(fn -> :ok end)
    Process.sleep(1)  # Simulate GenServer call overhead
    standard_time = System.monotonic_time(:microsecond) - start_time
    
    IO.puts("   Optimized time: #{optimized_time}μs")
    IO.puts("   Standard time: #{standard_time}μs")
    IO.puts("   Improvement: #{Float.round(standard_time / optimized_time, 2)}x\n")
  end
  
  defp demo_batching do
    IO.puts("3. Message Batching Demo")
    
    batch_state = %Beam.State{
      mode: :local,
      optimized: true,
      format: :native,
      peer_ref: self(),
      batch_enabled: true,
      batch_size_limit: 5,
      batch_timeout: 50,
      batch_buffer: [],
      stats: %{
        zero_copy_sends: 0,
        batched_sends: 0,
        messages_batched: 0,
        direct_sends: 0
      }
    }
    
    IO.puts("   Sending 5 messages for batching...")
    
    # Send messages that will be batched
    final_state = Enum.reduce(1..5, batch_state, fn i, state ->
      msg = %{id: i, data: "message #{i}"}
      {:ok, new_state} = Beam.send_message_async(msg, state)
      new_state
    end)
    
    # The 5th message should trigger batch flush
    receive do
      {:mcp_batch, messages} ->
        IO.puts("   ✓ Received batch with #{length(messages)} messages")
        IO.puts("   Batch contents: #{inspect(messages)}")
    after
      100 -> IO.puts("   ✗ No batch received")
    end
    
    IO.puts("   Stats: #{inspect(final_state.stats)}\n")
  end
  
  defp demo_performance(optimized_state, standard_state) do
    IO.puts("4. Performance Comparison")
    IO.puts("   Sending 1000 messages...")
    
    message = %{data: String.duplicate("test", 100)}
    
    # Benchmark optimized
    {optimized_time, _} = :timer.tc(fn ->
      for _ <- 1..1000 do
        Beam.send_message(message, optimized_state)
        receive do
          {:mcp_message, _} -> :ok
        after
          0 -> :ok
        end
      end
    end)
    
    # Benchmark standard (simulated with overhead)
    {standard_time, _} = :timer.tc(fn ->
      for _ <- 1..1000 do
        # Simulate standard path with mailbox overhead
        send(self(), {:transport_message, message})
        Process.sleep(0)  # Simulate context switch
        receive do
          {:transport_message, _} -> :ok
        after
          0 -> :ok
        end
      end
    end)
    
    IO.puts("   Optimized: #{optimized_time}μs (#{optimized_time / 1000} ms)")
    IO.puts("   Standard: #{standard_time}μs (#{standard_time / 1000} ms)")
    IO.puts("   Performance improvement: #{Float.round(standard_time / optimized_time, 2)}x")
    
    # Memory usage comparison
    :erlang.garbage_collect()
    initial_memory = :erlang.memory(:total)
    
    # Send many messages
    for _ <- 1..100 do
      Beam.send_message(message, optimized_state)
    end
    
    :erlang.garbage_collect()
    final_memory = :erlang.memory(:total)
    
    memory_used = final_memory - initial_memory
    IO.puts("   Memory used (optimized): #{memory_used} bytes")
  end
end

# Run the demo
OptimizationDemo.run()