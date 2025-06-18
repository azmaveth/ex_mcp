defmodule ExMCP.Testing.CaseTest do
  use ExUnit.Case, async: true

  alias ExMCP.TestCase

  describe "with_temp_file macro" do
    test "creates temporary file with content" do
      import ExMCP.TestCase

      result =
        with_temp_file("test content", ".txt") do
          assert File.exists?(file_path)
          assert File.read!(file_path) == "test content"
          assert String.ends_with?(file_path, ".txt")
          :test_result
        end

      assert result == :test_result
    end

    test "cleans up temporary file after block" do
      import ExMCP.TestCase

      captured_path =
        with_temp_file("content") do
          file_path
        end

      refute File.exists?(captured_path)
    end
  end

  describe "measure_time macro" do
    test "measures execution time" do
      {result, time_ms} =
        TestCase.measure_time do
          Process.sleep(10)
          :test_result
        end

      assert result == :test_result
      assert time_ms >= 10
      # Should be much less than 100ms
      assert time_ms < 100
    end
  end

  describe "with_timeout macro" do
    test "returns result when operation completes in time" do
      result =
        TestCase.with_timeout 1000 do
          Process.sleep(10)
          :completed
        end

      assert result == :completed
    end

    test "fails when operation times out" do
      assert_raise ExUnit.AssertionError, ~r/Operation timed out/, fn ->
        TestCase.with_timeout 50 do
          Process.sleep(100)
          :should_not_reach
        end
      end
    end
  end

  describe "repeat_test macro" do
    test "runs test multiple times" do
      counter = Agent.start_link(fn -> 0 end)
      {:ok, agent} = counter

      TestCase.repeat_test 5 do
        Agent.update(agent, &(&1 + 1))
      end

      assert Agent.get(agent, & &1) == 5
      Agent.stop(agent)
    end

    test "fails with context on iteration failure" do
      assert_raise ExUnit.AssertionError, ~r/iteration/, fn ->
        counter = Agent.start_link(fn -> 0 end)
        {:ok, agent} = counter

        TestCase.repeat_test 3 do
          count = Agent.get_and_update(agent, fn c -> {c + 1, c + 1} end)
          if count == 2, do: raise("test failure")
        end
      end
    end
  end

  describe "concurrent_test macro" do
    test "runs operations concurrently" do
      start_time = System.monotonic_time(:millisecond)

      # This would be defined in a real test module
      # concurrent_test "parallel sleep", count: 3 do |index|
      #   Process.sleep(50)
      #   index
      # end

      # Instead we'll test the underlying mechanism
      tasks =
        1..3
        |> Enum.map(fn index ->
          Task.async(fn ->
            Process.sleep(50)
            index
          end)
        end)

      results = Task.await_many(tasks, 1000)
      end_time = System.monotonic_time(:millisecond)

      assert results == [1, 2, 3]
      # Should take ~50ms, not ~150ms (sequential)
      assert end_time - start_time < 100
    end
  end

  describe "wait_until function" do
    test "returns :ok when condition becomes true" do
      agent = Agent.start_link(fn -> false end)
      {:ok, pid} = agent

      # Set condition to true after 50ms
      Task.start(fn ->
        Process.sleep(50)
        Agent.update(pid, fn _ -> true end)
      end)

      result =
        TestCase.wait_until(
          fn ->
            Agent.get(pid, & &1)
          end,
          timeout: 200,
          interval: 10
        )

      assert result == :ok
      Agent.stop(pid)
    end

    test "returns :timeout when condition never becomes true" do
      result = TestCase.wait_until(fn -> false end, timeout: 50, interval: 10)
      assert result == :timeout
    end
  end

  describe "run_parallel function" do
    test "executes functions in parallel and returns results" do
      functions = [
        fn ->
          Process.sleep(20)
          1
        end,
        fn ->
          Process.sleep(20)
          2
        end,
        fn ->
          Process.sleep(20)
          3
        end
      ]

      start_time = System.monotonic_time(:millisecond)
      results = TestCase.run_parallel(functions)
      end_time = System.monotonic_time(:millisecond)

      assert results == [1, 2, 3]
      # Should take ~20ms, not ~60ms (sequential)
      assert end_time - start_time < 50
    end

    test "respects timeout option" do
      functions = [
        fn ->
          Process.sleep(100)
          :result
        end
      ]

      assert_raise RuntimeError, fn ->
        TestCase.run_parallel(functions, timeout: 50)
      end
    end
  end

  describe "start_test_supervisor function" do
    test "starts supervisor with child specs" do
      child_specs = [
        {Agent, fn -> :initial_state end}
      ]

      {:ok, supervisor} = TestCase.start_test_supervisor(child_specs)

      # Verify supervisor is running
      assert Process.alive?(supervisor)

      # Verify child processes are started
      children = Supervisor.which_children(supervisor)
      assert length(children) == 1

      Supervisor.stop(supervisor)
    end
  end

  describe "flush_messages function" do
    test "returns all messages from mailbox" do
      send(self(), :message1)
      send(self(), :message2)
      send(self(), :message3)

      messages = TestCase.flush_messages()

      assert messages == [:message1, :message2, :message3]

      # Mailbox should be empty now
      assert TestCase.flush_messages() == []
    end

    test "returns empty list when no messages" do
      messages = TestCase.flush_messages()
      assert messages == []
    end
  end

  describe "unique_id function" do
    test "generates unique identifiers with prefix" do
      id1 = TestCase.unique_id("test")
      id2 = TestCase.unique_id("test")

      assert String.starts_with?(id1, "test_")
      assert String.starts_with?(id2, "test_")
      assert id1 != id2
    end

    test "respects length parameter" do
      id = TestCase.unique_id("test", 16)

      # Format: "test_" + 8 hex characters (16/2)
      # "test_" + 8 chars
      assert String.length(id) == 5 + 8
    end
  end

  describe "create_test_context function" do
    test "creates context with unique id" do
      context = TestCase.create_test_context()

      assert Map.has_key?(context, :test_id)
      assert Map.has_key?(context, :created_at)
      assert Map.has_key?(context, :cleanup_functions)
      assert is_binary(context.test_id)
      assert String.starts_with?(context.test_id, "ctx_")
    end

    test "merges initial context" do
      initial = %{custom_data: "test"}
      context = TestCase.create_test_context(initial)

      assert context.custom_data == "test"
      assert Map.has_key?(context, :test_id)
    end
  end

  describe "add_cleanup function" do
    test "adds cleanup function to context" do
      context = TestCase.create_test_context()

      cleanup_fn = fn -> :cleanup_executed end
      updated_context = TestCase.add_cleanup(context, cleanup_fn)

      assert length(updated_context.cleanup_functions) == 1
      assert hd(updated_context.cleanup_functions) == cleanup_fn
    end

    test "accumulates multiple cleanup functions" do
      context = TestCase.create_test_context()

      fn1 = fn -> :cleanup1 end
      fn2 = fn -> :cleanup2 end

      context = TestCase.add_cleanup(context, fn1)
      context = TestCase.add_cleanup(context, fn2)

      assert length(context.cleanup_functions) == 2
      assert fn2 in context.cleanup_functions
      assert fn1 in context.cleanup_functions
    end
  end
end
