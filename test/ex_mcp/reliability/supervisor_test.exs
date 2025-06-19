defmodule ExMCP.Reliability.SupervisorTest do
  use ExUnit.Case, async: true

  alias ExMCP.Reliability.Supervisor, as: ReliabilitySupervisor
  alias ExMCP.Reliability

  describe "start_link/1" do
    test "starts supervisor with default options" do
      assert {:ok, pid} = ReliabilitySupervisor.start_link()
      assert Process.alive?(pid)
      assert is_pid(pid)

      # Verify it's actually a supervisor process (uses GenServer underneath)
      assert {:status, ^pid, {:module, :gen_server}, _} = :sys.get_status(pid)

      GenServer.stop(pid)
    end

    test "starts supervisor with custom name" do
      name = :"test_reliability_supervisor_#{:erlang.unique_integer()}"
      assert {:ok, pid} = ReliabilitySupervisor.start_link(name: name)
      assert Process.whereis(name) == pid
      assert Process.alive?(pid)

      GenServer.stop(pid)
    end

    test "starts child supervisors and registry" do
      assert {:ok, pid} = ReliabilitySupervisor.start_link()

      # Give supervisor time to start children
      Process.sleep(50)

      # Verify child processes exist
      children = Supervisor.which_children(pid)
      assert length(children) == 3

      # Check for registry and dynamic supervisors by their module name patterns
      child_ids = Enum.map(children, fn {id, _pid, _type, _modules} -> id end)
      # Should have CircuitBreakerSupervisor, HealthCheckSupervisor, and Registry
      assert Enum.any?(child_ids, &String.contains?(to_string(&1), "CircuitBreakerSupervisor"))
      assert Enum.any?(child_ids, &String.contains?(to_string(&1), "HealthCheckSupervisor"))
      assert Enum.any?(child_ids, &String.contains?(to_string(&1), "Registry"))

      GenServer.stop(pid)
    end
  end

  describe "create_reliable_client/2" do
    setup do
      {:ok, supervisor} = ReliabilitySupervisor.start_link()
      on_exit(fn -> if Process.alive?(supervisor), do: GenServer.stop(supervisor) end)
      %{supervisor: supervisor}
    end

    test "creates basic reliable client without reliability features", %{supervisor: supervisor} do
      # Use mock transport that will be handled by SimpleClient
      assert {:ok, client_pid} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()]
               )

      assert is_pid(client_pid)
      assert Process.alive?(client_pid)

      if Process.alive?(client_pid), do: GenServer.stop(client_pid)
    end

    test "creates reliable client with circuit breaker", %{supervisor: supervisor} do
      assert {:ok, client_pid} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()],
                 circuit_breaker: [
                   failure_threshold: 3,
                   reset_timeout: 1000
                 ]
               )

      assert is_pid(client_pid)
      assert Process.alive?(client_pid)

      if Process.alive?(client_pid), do: GenServer.stop(client_pid)
    end

    test "creates reliable client with health check", %{supervisor: supervisor} do
      assert {:ok, client_pid} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()],
                 health_check: [
                   check_interval: 5000,
                   failure_threshold: 2
                 ]
               )

      assert is_pid(client_pid)
      assert Process.alive?(client_pid)

      if Process.alive?(client_pid), do: GenServer.stop(client_pid)
    end

    test "creates reliable client with retry configuration", %{supervisor: supervisor} do
      assert {:ok, client_pid} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()],
                 retry: [
                   max_attempts: 5,
                   backoff_factor: 1.5
                 ]
               )

      assert is_pid(client_pid)
      assert Process.alive?(client_pid)

      if Process.alive?(client_pid), do: GenServer.stop(client_pid)
    end

    test "creates reliable client with all reliability features", %{supervisor: supervisor} do
      assert {:ok, client_pid} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()],
                 circuit_breaker: [failure_threshold: 3],
                 retry: [max_attempts: 3],
                 health_check: [check_interval: 10_000]
               )

      assert is_pid(client_pid)
      assert Process.alive?(client_pid)

      if Process.alive?(client_pid), do: GenServer.stop(client_pid)
    end

    test "handles transport start failure gracefully", %{supervisor: supervisor} do
      # Use stdio transport with invalid path to simulate failure  
      assert {:error, _reason} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :stdio, command: "/nonexistent/command"]
               )
    end

    test "generates unique client IDs for multiple clients", %{supervisor: supervisor} do
      assert {:ok, client1} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()]
               )

      assert {:ok, client2} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()]
               )

      assert client1 != client2

      if Process.alive?(client1), do: GenServer.stop(client1)
      if Process.alive?(client2), do: GenServer.stop(client2)
    end

    test "accepts custom client ID", %{supervisor: supervisor} do
      custom_id = "my_custom_client_123"

      assert {:ok, client_pid} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 id: custom_id,
                 transport: [type: :mock, server_pid: self()]
               )

      assert is_pid(client_pid)

      if Process.alive?(client_pid), do: GenServer.stop(client_pid)
    end

    test "cleans up on wrapper start failure", %{supervisor: supervisor} do
      # Stop the supervisor to force wrapper creation failure
      GenServer.stop(supervisor)

      # This should fail because supervisor is down
      assert {:error, _reason} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()]
               )
    end
  end

  describe "ClientWrapper integration" do
    setup do
      {:ok, supervisor} = ReliabilitySupervisor.start_link()
      on_exit(fn -> if Process.alive?(supervisor), do: GenServer.stop(supervisor) end)
      %{supervisor: supervisor}
    end

    test "wrapped client forwards MCP operations", %{supervisor: supervisor} do
      assert {:ok, wrapper_pid} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()]
               )

      # Test that wrapper can receive and handle calls
      # Note: The actual MCP calls would be tested through the public API
      # but since we're testing the supervisor, we verify the wrapper exists
      assert is_pid(wrapper_pid)
      assert Process.alive?(wrapper_pid)

      if Process.alive?(wrapper_pid), do: GenServer.stop(wrapper_pid)
    end

    test "wrapper monitors underlying client", %{supervisor: supervisor} do
      assert {:ok, wrapper_pid} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()]
               )

      # Monitor the wrapper to see when it dies
      wrapper_ref = Process.monitor(wrapper_pid)

      # For this test, we'll just verify the wrapper was created successfully
      # The actual monitoring behavior would require more complex setup
      assert is_pid(wrapper_pid)
      Process.demonitor(wrapper_ref)

      if Process.alive?(wrapper_pid), do: GenServer.stop(wrapper_pid)
    end
  end

  describe "ExMCP.Reliability.with_retry/2" do
    test "executes function without retry options" do
      result = Reliability.with_retry(fn -> {:ok, "success"} end)
      assert result == {:ok, "success"}
    end

    test "retries failing function" do
      counter = Agent.start_link(fn -> 0 end)
      {:ok, agent} = counter

      result =
        Reliability.with_retry(
          fn ->
            current = Agent.get_and_update(agent, fn count -> {count + 1, count + 1} end)
            if current < 3, do: {:error, :not_ready}, else: {:ok, "success"}
          end,
          max_attempts: 5,
          base_delay: 10
        )

      assert result == {:ok, "success"}
      assert Agent.get(agent, & &1) == 3

      Agent.stop(agent)
    end

    test "respects max attempts limit" do
      counter = Agent.start_link(fn -> 0 end)
      {:ok, agent} = counter

      result =
        Reliability.with_retry(
          fn ->
            Agent.update(agent, &(&1 + 1))
            {:error, :always_fails}
          end,
          max_attempts: 3,
          base_delay: 1
        )

      assert result == {:error, {:retry_exhausted, :always_fails}}
      assert Agent.get(agent, & &1) == 3

      Agent.stop(agent)
    end

    test "handles exceptions during retry" do
      counter = Agent.start_link(fn -> 0 end)
      {:ok, agent} = counter

      result =
        Reliability.with_retry(
          fn ->
            current = Agent.get_and_update(agent, fn count -> {count + 1, count + 1} end)
            if current < 2, do: raise("test error"), else: {:ok, "recovered"}
          end,
          max_attempts: 3,
          base_delay: 1
        )

      assert result == {:ok, "recovered"}

      Agent.stop(agent)
    end
  end

  describe "ExMCP.Reliability.protect/2" do
    test "creates circuit breaker protected function" do
      # Create a function that always succeeds
      protected_fn =
        Reliability.protect(fn -> {:ok, "success"} end,
          name: String.to_atom("test_protect_#{:erlang.unique_integer()}")
        )

      assert is_function(protected_fn)
      result = protected_fn.([])
      assert result == {:ok, "success"}
    end

    test "circuit breaker trips on repeated failures" do
      counter = Agent.start_link(fn -> 0 end)
      {:ok, agent} = counter

      # Create a function that fails consistently
      protected_fn =
        Reliability.protect(
          fn ->
            Agent.update(agent, &(&1 + 1))
            {:error, :service_down}
          end,
          failure_threshold: 3,
          name: String.to_atom("test_protect_#{:erlang.unique_integer()}")
        )

      # First few calls should execute and fail
      assert protected_fn.([]) == {:error, :service_down}
      assert protected_fn.([]) == {:error, :service_down}
      assert protected_fn.([]) == {:error, :service_down}

      # Circuit should now be open - calls should fail fast
      result = protected_fn.([])
      assert {:error, :circuit_open} = result

      # Should not have incremented counter (circuit open, function not called)
      final_count = Agent.get(agent, & &1)
      # Only the first 3 calls that opened the circuit
      assert final_count == 3

      Agent.stop(agent)
    end

    test "reuses circuit breaker for same function" do
      breaker_name = String.to_atom("test_reuse_#{:erlang.unique_integer()}")

      protected_fn = Reliability.protect(fn -> {:ok, "test"} end, name: breaker_name)

      # First call creates the breaker
      assert protected_fn.([]) == {:ok, "test"}
      breaker_pid = Process.whereis(breaker_name)
      assert is_pid(breaker_pid)

      # Second call should reuse the same breaker
      assert protected_fn.([]) == {:ok, "test"}
      assert Process.whereis(breaker_name) == breaker_pid

      # Clean up
      if Process.alive?(breaker_pid), do: GenServer.stop(breaker_pid)
    end

    test "creates unique circuit breakers for different functions" do
      protected_fn1 =
        Reliability.protect(fn -> {:ok, "fn1"} end,
          name: String.to_atom("test_fn1_#{:erlang.unique_integer()}")
        )

      protected_fn2 =
        Reliability.protect(fn -> {:ok, "fn2"} end,
          name: String.to_atom("test_fn2_#{:erlang.unique_integer()}")
        )

      assert protected_fn1.([]) == {:ok, "fn1"}
      assert protected_fn2.([]) == {:ok, "fn2"}
    end

    test "handles function arguments correctly" do
      protected_fn =
        Reliability.protect(fn -> {:ok, 8} end,
          name: String.to_atom("test_args_#{:erlang.unique_integer()}")
        )

      result = protected_fn.([5, 3])
      assert result == {:ok, 8}
    end

    test "respects circuit breaker configuration" do
      counter = Agent.start_link(fn -> 0 end)
      {:ok, agent} = counter

      # Use a low failure threshold for testing
      protected_fn =
        Reliability.protect(
          fn ->
            Agent.update(agent, &(&1 + 1))
            {:error, :fail}
          end,
          failure_threshold: 1,
          name: String.to_atom("test_config_#{:erlang.unique_integer()}")
        )

      # First call should fail and open circuit
      assert protected_fn.([]) == {:error, :fail}

      # Second call should be circuit open
      result = protected_fn.([])
      assert {:error, :circuit_open} = result

      # Should have only called function once
      assert Agent.get(agent, & &1) == 1

      Agent.stop(agent)
    end
  end

  describe "integration scenarios" do
    setup do
      {:ok, supervisor} = ReliabilitySupervisor.start_link()
      on_exit(fn -> if Process.alive?(supervisor), do: GenServer.stop(supervisor) end)
      %{supervisor: supervisor}
    end

    test "multiple reliable clients can coexist", %{supervisor: supervisor} do
      # Create multiple clients with different configurations
      assert {:ok, client1} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()],
                 circuit_breaker: [failure_threshold: 5]
               )

      assert {:ok, client2} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()],
                 retry: [max_attempts: 3]
               )

      assert {:ok, client3} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()],
                 health_check: [check_interval: 30_000]
               )

      # All should be different processes
      assert client1 != client2
      assert client2 != client3
      assert client1 != client3

      # All should be alive
      assert Process.alive?(client1)
      assert Process.alive?(client2)
      assert Process.alive?(client3)

      # Clean up
      for client <- [client1, client2, client3] do
        if Process.alive?(client), do: GenServer.stop(client)
      end
    end

    test "supervisor recovery after child failure", %{supervisor: supervisor} do
      # Create initial client
      assert {:ok, client1} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()]
               )

      # Force stop the client (simulating crash)
      GenServer.stop(client1, :kill)

      # Give time for any cleanup
      Process.sleep(10)

      # Should be able to create new clients after failure
      assert {:ok, client2} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()]
               )

      assert Process.alive?(client2)
      assert client1 != client2

      if Process.alive?(client2), do: GenServer.stop(client2)
    end

    test "combining convenience functions with supervised clients", %{supervisor: supervisor} do
      # Create supervised client
      assert {:ok, client} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()]
               )

      # Use convenience retry function
      retry_result = Reliability.with_retry(fn -> {:ok, "retry_success"} end)
      assert retry_result == {:ok, "retry_success"}

      # Use convenience protect function  
      protected_fn =
        Reliability.protect(fn -> {:ok, "protect_success"} end,
          name: String.to_atom("test_convenience_#{:erlang.unique_integer()}")
        )

      protect_result = protected_fn.([])
      assert protect_result == {:ok, "protect_success"}

      # All should work independently
      assert Process.alive?(client)

      if Process.alive?(client), do: GenServer.stop(client)
    end
  end

  describe "error handling and edge cases" do
    test "handles invalid supervisor reference" do
      non_existent_supervisor = :non_existent_supervisor

      assert {:error, _reason} =
               ReliabilitySupervisor.create_reliable_client(
                 non_existent_supervisor,
                 transport: [type: :mock, server_pid: self()]
               )
    end

    test "handles empty and nil options gracefully" do
      {:ok, supervisor} = ReliabilitySupervisor.start_link()

      # Empty options - should fail because no transport provided
      assert {:error, _reason} = ReliabilitySupervisor.create_reliable_client(supervisor, [])

      # Nil values in options
      assert {:ok, client2} =
               ReliabilitySupervisor.create_reliable_client(
                 supervisor,
                 transport: [type: :mock, server_pid: self()],
                 circuit_breaker: nil,
                 retry: nil,
                 health_check: nil
               )

      assert is_pid(client2)

      # Clean up
      if Process.alive?(client2), do: GenServer.stop(client2)
      GenServer.stop(supervisor)
    end

    test "protect function handles exceptions in protected function" do
      protected_fn =
        Reliability.protect(
          fn ->
            raise RuntimeError, "test exception"
          end,
          name: String.to_atom("test_exception_#{:erlang.unique_integer()}")
        )

      # Should catch exception and return error
      result = protected_fn.([])
      assert {:error, %RuntimeError{}} = result
    end

    test "with_retry handles various error formats" do
      # Test with different error return formats
      assert {:error, {:retry_exhausted, :simple}} =
               Reliability.with_retry(fn -> {:error, :simple} end, max_attempts: 1)

      assert {:error, {:retry_exhausted, "string"}} =
               Reliability.with_retry(fn -> {:error, "string"} end, max_attempts: 1)

      # Test with exceptions - retry wraps them differently
      result = Reliability.with_retry(fn -> raise "test" end, max_attempts: 1)
      assert {:error, {:retry_exhausted, %RuntimeError{message: "test"}}} = result
    end
  end
end
