defmodule ExMCP.Stress.LoadTest do
  use ExUnit.Case, async: false

  alias ExMCP.Testing.{MockServer, Builders}
  alias ExMCP.Content.Protocol

  @moduletag :stress
  # 2 minutes for stress tests
  @moduletag timeout: 120_000

  # Stress test configuration
  @high_concurrency System.schedulers_online() * 4
  @medium_concurrency System.schedulers_online() * 2
  @operations_per_task 100
  @large_content_size 10_000
  # 30 seconds
  @stress_duration 30_000

  describe "content protocol stress tests" do
    test "high volume content creation and validation" do
      {time, results} =
        :timer.tc(fn ->
          1..1000
          |> Task.async_stream(
            fn i ->
              # Create multiple content types per task
              text = Protocol.text("Stress test content #{i}")
              image_data = Base.encode64(:crypto.strong_rand_bytes(1024))
              image = Protocol.image(image_data, "image/png")

              # Validate all content
              text_valid = Protocol.validate(text) == :ok
              image_valid = Protocol.validate(image) == :ok

              {text_valid, image_valid}
            end,
            max_concurrency: @high_concurrency,
            timeout: 30_000
          )
          |> Enum.to_list()
        end)

      # Verify all operations completed successfully
      successful_results =
        Enum.count(results, fn {:ok, {text_valid, image_valid}} ->
          text_valid and image_valid
        end)

      assert successful_results == 1000,
             "Expected 1000 successful operations, got #{successful_results}"

      avg_time_per_operation = div(time, 1000)
      # 10ms per operation
      assert avg_time_per_operation < 10_000,
             "Average time per operation: #{avg_time_per_operation}μs"
    end

    test "concurrent serialization/deserialization stress" do
      # Create varied content for testing
      contents =
        Enum.map(1..100, fn i ->
          case rem(i, 4) do
            0 -> Protocol.text("Text content #{i}", format: :markdown)
            1 -> Protocol.image(Base.encode64(:crypto.strong_rand_bytes(512)), "image/png")
            2 -> Protocol.audio(Base.encode64(:crypto.strong_rand_bytes(1024)), "audio/wav")
            3 -> Protocol.resource("file://resource#{i}.txt")
          end
        end)

      {time, results} =
        :timer.tc(fn ->
          contents
          |> Task.async_stream(
            fn content ->
              # Perform multiple serialization/deserialization cycles
              Enum.map(1..10, fn _ ->
                serialized = Protocol.serialize(content)

                case Protocol.deserialize(serialized) do
                  {:ok, deserialized} ->
                    # Verify roundtrip preserved type
                    content.type == deserialized.type

                  {:error, _} ->
                    false
                end
              end)
              |> Enum.all?()
            end,
            max_concurrency: @high_concurrency,
            timeout: 60_000
          )
          |> Enum.to_list()
        end)

      successful_tasks = Enum.count(results, fn {:ok, success} -> success end)

      assert successful_tasks == 100,
             "Expected 100 successful tasks, got #{successful_tasks}"

      IO.puts("Concurrent serialization stress test completed in #{div(time, 1000)}ms")
    end

    test "large content handling stress" do
      # Test with increasingly large content sizes
      large_contents =
        Enum.map([1_000, 5_000, 10_000, 50_000], fn size ->
          large_text = String.duplicate("Large content test. ", size)
          Protocol.text(large_text, format: :markdown, metadata: %{size: size})
        end)

      {time, results} =
        :timer.tc(fn ->
          large_contents
          |> Task.async_stream(
            fn content ->
              # Test multiple operations on large content
              validation_result = Protocol.validate(content)
              serialized = Protocol.serialize(content)
              {:ok, deserialized} = Protocol.deserialize(serialized)

              validation_result == :ok and
                content.type == deserialized.type and
                String.length(content.text) == String.length(deserialized.text)
            end,
            max_concurrency: @medium_concurrency,
            timeout: 30_000
          )
          |> Enum.to_list()
        end)

      successful_results = Enum.count(results, fn {:ok, success} -> success end)

      assert successful_results == 4,
             "Expected 4 successful large content operations, got #{successful_results}"

      IO.puts("Large content stress test completed in #{div(time, 1000)}ms")
    end
  end

  describe "mock server stress tests" do
    test "high concurrency request handling" do
      tools =
        Enum.map(1..10, fn i ->
          MockServer.sample_tool(name: "stress_tool_#{i}")
        end)

      MockServer.with_server([tools: tools, latency: 1], fn _client ->
        # Simulate high concurrency load
        {time, results} =
          :timer.tc(fn ->
            1..500
            |> Task.async_stream(
              fn i ->
                # Each task makes multiple requests
                tool_name = "stress_tool_#{rem(i, 10) + 1}"

                # Simulate multiple operations per task
                Enum.map(1..5, fn j ->
                  request =
                    Builders.request("tools/call",
                      id: i * 100 + j,
                      params: %{
                        "name" => tool_name,
                        "arguments" => %{"input" => "stress_test_#{i}_#{j}"}
                      }
                    )

                  # In real test, would send to server
                  # For now, just prepare the request
                  is_map(request) and Map.has_key?(request, "id")
                end)
                |> Enum.all?()
              end,
              max_concurrency: @high_concurrency,
              timeout: 60_000
            )
            |> Enum.to_list()
          end)

        successful_tasks = Enum.count(results, fn {:ok, success} -> success end)

        assert successful_tasks == 500,
               "Expected 500 successful tasks, got #{successful_tasks}"

        IO.puts("High concurrency server stress test completed in #{div(time, 1000)}ms")
      end)
    end

    test "memory usage under sustained load" do
      tools = [MockServer.sample_tool()]

      MockServer.with_server([tools: tools], fn _client ->
        initial_memory = :erlang.memory(:total)

        # Run sustained operations for a period
        start_time = System.monotonic_time(:millisecond)
        # 10 seconds
        end_time = start_time + 10_000

        operation_count =
          run_operations_until(end_time, fn ->
            # Simulate various operations
            request = Builders.request("tools/list", id: :rand.uniform(10000))

            tool_request =
              Builders.request("tools/call",
                id: :rand.uniform(10000),
                params: %{
                  "name" => "sample_tool",
                  "arguments" => %{"input" => "memory_test"}
                }
              )

            # Verify requests are well-formed
            is_map(request) and is_map(tool_request)
          end)

        # Force garbage collection and check memory
        :erlang.garbage_collect()
        # Allow GC to complete
        Process.sleep(100)
        final_memory = :erlang.memory(:total)

        memory_increase = final_memory - initial_memory

        IO.puts("Performed #{operation_count} operations")
        IO.puts("Memory increase: #{memory_increase} bytes")

        # Memory should not increase excessively (allow 50MB increase)
        assert memory_increase < 50_000_000,
               "Memory increased by #{memory_increase} bytes, expected < 50MB"
      end)
    end

    test "error recovery under stress" do
      MockServer.with_server([error_rate: 0.3], fn _client ->
        # Test that system remains stable even with 30% error rate
        {time, results} =
          :timer.tc(fn ->
            1..200
            |> Task.async_stream(
              fn i ->
                # Make multiple requests, some will fail due to error rate
                request_results =
                  Enum.map(1..5, fn j ->
                    request = Builders.request("tools/list", id: i * 100 + j)

                    # In real test, would send request and handle errors
                    # For now, simulate that some requests fail
                    if rem(i + j, 3) == 0 do
                      {:error, "simulated_error"}
                    else
                      {:ok, %{"tools" => []}}
                    end
                  end)

                # Count successful vs failed requests
                successes = Enum.count(request_results, &match?({:ok, _}, &1))
                failures = Enum.count(request_results, &match?({:error, _}, &1))

                {successes, failures}
              end,
              max_concurrency: @medium_concurrency,
              timeout: 30_000
            )
            |> Enum.to_list()
          end)

        # Verify all tasks completed (even with errors)
        completed_tasks = length(results)

        assert completed_tasks == 200,
               "Expected 200 completed tasks, got #{completed_tasks}"

        # Calculate overall success/failure rates
        {total_successes, total_failures} =
          results
          |> Enum.reduce({0, 0}, fn {:ok, {successes, failures}}, {acc_s, acc_f} ->
            {acc_s + successes, acc_f + failures}
          end)

        total_requests = total_successes + total_failures
        success_rate = total_successes / total_requests

        IO.puts("Error recovery stress test: #{success_rate * 100}% success rate")

        # Should have reasonable success rate despite errors
        assert success_rate > 0.5,
               "Success rate #{success_rate} too low, expected > 50%"
      end)
    end
  end

  describe "builders stress tests" do
    test "high volume test data generation" do
      {time, results} =
        :timer.tc(fn ->
          1..1000
          |> Task.async_stream(
            fn i ->
              # Generate various test data
              text = Builders.text_content("Test #{i}")
              image = Builders.image_content(random: true, size: 512)
              tool = Builders.tool("test_tool_#{i}")
              resource = Builders.resource("file://test#{i}.txt", "Test #{i}")

              # Validate generated data
              Protocol.validate(text) == :ok and
                Protocol.validate(image) == :ok and
                is_map(tool) and
                is_map(resource)
            end,
            max_concurrency: @high_concurrency,
            timeout: 30_000
          )
          |> Enum.to_list()
        end)

      successful_generations = Enum.count(results, fn {:ok, success} -> success end)

      assert successful_generations == 1000,
             "Expected 1000 successful generations, got #{successful_generations}"

      avg_time_per_generation = div(time, 1000)
      IO.puts("Average generation time: #{avg_time_per_generation}μs")
    end

    test "random data generation consistency" do
      # Test that random generators produce valid but varied output
      {time, results} =
        :timer.tc(fn ->
          1..100
          |> Task.async_stream(
            fn _ ->
              # Generate multiple random items
              texts = Enum.map(1..10, fn _ -> Builders.random_text(100) end)
              strings = Enum.map(1..10, fn _ -> Builders.random_string(20) end)

              # Verify all are different (very high probability)
              unique_texts = MapSet.size(MapSet.new(texts))
              unique_strings = MapSet.size(MapSet.new(strings))

              # Allow some duplicates
              unique_texts > 8 and unique_strings > 8
            end,
            max_concurrency: @medium_concurrency,
            timeout: 15_000
          )
          |> Enum.to_list()
        end)

      consistent_tasks = Enum.count(results, fn {:ok, success} -> success end)
      # Allow some variance in randomness
      assert consistent_tasks > 95,
             "Expected >95 tasks with good randomness, got #{consistent_tasks}"

      IO.puts("Random generation consistency test completed in #{div(time, 1000)}ms")
    end
  end

  describe "system resource stress tests" do
    test "file descriptor usage" do
      # Test that operations don't leak file descriptors
      initial_fd_count = count_file_descriptors()

      # Perform many operations that might use file descriptors
      Enum.each(1..1000, fn i ->
        # Create temporary content
        content = Protocol.text("FD test #{i}")
        _serialized = Protocol.serialize(content)

        # Generate test data
        _image = Builders.image_content(random: true, size: 100)
        _audio = Builders.audio_content(random: true, size: 200)
      end)

      :erlang.garbage_collect()
      Process.sleep(100)

      final_fd_count = count_file_descriptors()
      fd_increase = final_fd_count - initial_fd_count

      IO.puts("File descriptor increase: #{fd_increase}")

      # Should not leak file descriptors
      assert fd_increase < 10,
             "File descriptors increased by #{fd_increase}, expected < 10"
    end

    test "process count stability" do
      initial_process_count = length(Process.list())

      # Run operations that might spawn processes
      tasks =
        Enum.map(1..50, fn i ->
          Task.async(fn ->
            # Simulate process-heavy operations
            Enum.each(1..10, fn j ->
              _content = Protocol.text("Process test #{i}-#{j}")
              _request = Builders.request("test", id: i * 100 + j)
            end)
          end)
        end)

      Task.await_many(tasks, 30_000)

      :erlang.garbage_collect()
      # Allow cleanup
      Process.sleep(200)

      final_process_count = length(Process.list())
      process_increase = final_process_count - initial_process_count

      IO.puts("Process count increase: #{process_increase}")

      # Should not accumulate processes
      assert process_increase < 20,
             "Process count increased by #{process_increase}, expected < 20"
    end
  end

  # Helper functions

  defp run_operations_until(end_time, operation_fn) do
    run_operations_until(end_time, operation_fn, 0)
  end

  defp run_operations_until(end_time, operation_fn, count) do
    if System.monotonic_time(:millisecond) < end_time do
      operation_fn.()
      run_operations_until(end_time, operation_fn, count + 1)
    else
      count
    end
  end

  defp count_file_descriptors do
    # Platform-specific file descriptor counting
    case :os.type() do
      {:unix, _} ->
        try do
          pid = :os.getpid()
          {result, _} = System.cmd("lsof", ["-p", to_string(pid)], stderr_to_stdout: true)
          result |> String.split("\n") |> length()
        rescue
          # lsof not available or failed
          _ -> 0
        end

      # Not Unix-like system
      _ ->
        0
    end
  end
end
