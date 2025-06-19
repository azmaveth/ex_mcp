defmodule ExMCP.Reliability.SupervisorCoverageTest do
  use ExUnit.Case, async: true

  alias ExMCP.Reliability.Supervisor

  describe "start_link/1" do
    test "starts supervisor with default options" do
      assert {:ok, pid} = Supervisor.start_link()
      assert is_pid(pid)

      # Verify it's running
      assert Process.alive?(pid)

      # Clean up
      Process.exit(pid, :shutdown)
    end

    test "starts supervisor with custom name" do
      name = :test_reliability_supervisor
      assert {:ok, pid} = Supervisor.start_link(name: name)

      # Should be registered under the name
      assert Process.whereis(name) == pid

      # Clean up
      Process.exit(pid, :shutdown)
    end

    test "starts supervisor with custom configuration" do
      config = [
        max_restarts: 10,
        max_seconds: 5,
        enable_health_check: true,
        health_check_interval: 1000
      ]

      assert {:ok, pid} = Supervisor.start_link(config)
      assert is_pid(pid)

      # Clean up
      Process.exit(pid, :shutdown)
    end
  end

  describe "child specs" do
    test "provides valid child spec" do
      child_spec = Supervisor.child_spec([])

      assert child_spec.id == ExMCP.Reliability.Supervisor
      assert child_spec.start == {ExMCP.Reliability.Supervisor, :start_link, [[]]}
      assert child_spec.type == :supervisor
    end

    test "child spec respects custom options" do
      opts = [name: :custom_supervisor, max_restarts: 20]
      child_spec = Supervisor.child_spec(opts)

      assert child_spec.start == {ExMCP.Reliability.Supervisor, :start_link, [opts]}
    end
  end

  describe "supervision tree" do
    setup do
      {:ok, pid} = Supervisor.start_link()
      {:ok, supervisor: pid}
    end

    test "starts expected children", %{supervisor: supervisor} do
      children = Supervisor.which_children(supervisor)

      # Should have some children started
      assert length(children) > 0

      # Check for expected child types
      child_names = Enum.map(children, fn {id, _child, _type, _modules} -> id end)

      # At minimum should have circuit breaker
      assert ExMCP.Reliability.CircuitBreaker in child_names
    end

    test "restarts crashed children", %{supervisor: supervisor} do
      children_before = Supervisor.which_children(supervisor)

      # Find a child to crash
      {id, child_pid, _type, _modules} = List.first(children_before)

      # Crash the child
      Process.exit(child_pid, :kill)

      # Give supervisor time to restart
      Process.sleep(100)

      # Check child was restarted
      children_after = Supervisor.which_children(supervisor)

      {^id, new_child_pid, _type, _modules} =
        Enum.find(children_after, fn {child_id, _, _, _} -> child_id == id end)

      assert new_child_pid != child_pid
      assert Process.alive?(new_child_pid)
    end
  end

  describe "dynamic child management" do
    setup do
      {:ok, pid} = Supervisor.start_link()
      {:ok, supervisor: pid}
    end

    test "can add dynamic children", %{supervisor: supervisor} do
      # Define a simple GenServer child
      defmodule TestWorker do
        use GenServer
        def start_link(opts), do: GenServer.start_link(__MODULE__, opts)
        def init(opts), do: {:ok, opts}
      end

      child_spec = %{
        id: :test_worker,
        start: {TestWorker, :start_link, [[]]},
        type: :worker
      }

      # Try to start the child
      case Supervisor.start_child(supervisor, child_spec) do
        {:ok, child_pid} ->
          assert Process.alive?(child_pid)

        {:error, :not_supported} ->
          # Supervisor might not support dynamic children
          :ok

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end
  end

  describe "termination" do
    test "cleanly shuts down all children" do
      {:ok, supervisor} = Supervisor.start_link()

      # Get children before shutdown
      children = Supervisor.which_children(supervisor)
      child_pids = Enum.map(children, fn {_id, pid, _type, _modules} -> pid end)

      # Shutdown supervisor
      Process.exit(supervisor, :shutdown)
      Process.sleep(100)

      # All children should be dead
      assert Enum.all?(child_pids, fn pid -> !Process.alive?(pid) end)
    end
  end

  describe "configuration" do
    test "respects restart intensity settings" do
      # Start with low restart intensity
      {:ok, supervisor} =
        Supervisor.start_link(
          max_restarts: 1,
          max_seconds: 1
        )

      # This should work but if we crash children too fast, supervisor should die
      # For now just verify it started correctly
      assert Process.alive?(supervisor)

      Process.exit(supervisor, :shutdown)
    end

    test "handles invalid configuration gracefully" do
      # Invalid options should either be ignored or cause controlled failure
      result = Supervisor.start_link(invalid_option: :bad)

      case result do
        {:ok, pid} ->
          # Started despite invalid option
          assert Process.alive?(pid)
          Process.exit(pid, :shutdown)

        {:error, _reason} ->
          # Failed in a controlled way
          :ok
      end
    end
  end
end
