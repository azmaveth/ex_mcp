defmodule ExMCP.DebugServiceTest do
  use ExUnit.Case, async: false
  import ExMCP.TestHelpers

  @moduletag :requires_beam

  # This setup block creates an isolated test environment for each test.
  # It starts a unique Horde instance and an ExMCP.TestServer using the :native transport.
  # This ensures that tests do not interfere with each other.
  setup %{test: test} do
    import ExMCP.HordeTestHelpers

    # Create unique Horde process names for this test
    supervisor_name = unique_process_name(test, "supervisor")
    registry_name = unique_process_name(test, "registry")

    # Start Horde processes for the test
    {:ok, _sup} =
      Horde.DynamicSupervisor.start_link(strategy: :one_for_one, name: supervisor_name)

    {:ok, _reg} = Horde.Registry.start_link([], name: registry_name, keys: :unique)

    # Ensure processes are stopped after the test
    on_exit(fn ->
      if Process.alive?(supervisor_name), do: GenServer.stop(supervisor_name)
      if Process.alive?(registry_name), do: GenServer.stop(registry_name)
    end)

    # Configure the application to use the isolated Horde processes
    Application.put_env(:ex_mcp, :horde_registry, registry_name)
    Application.put_env(:ex_mcp, :horde_supervisor, supervisor_name)

    # Start the server with the correct transport
    case ExMCP.TestHelpers.__setup_server_by_transport__(:native, transport: :native) do
      {:ok, server_config} ->
        # Register on_exit for the server process
        pid = server_config.pid
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid, :shutdown, 500) end)

        # Combine configs and return for use in the test
        config =
          Map.merge(
            %{supervisor_name: supervisor_name, registry_name: registry_name},
            server_config
          )

        {:ok, config}

      error ->
        error
    end
  end

  @doc """
  Test 1: Basic Service Registration
  This test verifies that the ExMCP.TestServer successfully starts and registers
  itself as a service with the Horde registry within a reasonable time.
  It uses the `wait_for_service_registration` helper, which is the standard way
  to await a service. A failure here points to a fundamental issue in the
  service startup or registration logic.
  """
  test "service should register successfully on startup", %{config: _config} do
    service_name = ExMCP.TestServer
    # Wait for up to 5 seconds for the service to appear.
    assert {:ok, ^service_name} = wait_for_service_registration(service_name)

    IO.puts("SUCCESS: Service '#{inspect(service_name)}' registered successfully.")
  end

  @doc """
  Test 2: Service Availability Timing
  This test checks for the service's presence at different moments in time.
  It helps diagnose timing-related issues. For instance, if the service is
  available immediately but not later, it might indicate it's crashing and
  restarting, or being de-registered incorrectly.
  """
  test "service availability at different time intervals", %{config: _config} do
    service_name = ExMCP.TestServer

    # Check immediately after setup. It may or may not be ready yet.
    _available_immediately = ExMCP.Native.service_available?(service_name)
    # Debug info: Service available immediately after setup: #{_available_immediately}

    # Wait for a short period and check again. It should be available now.
    Process.sleep(100)

    assert ExMCP.Native.service_available?(service_name),
           "Service should be available after 100ms"

    IO.puts("SUCCESS: Service was available after 100ms.")

    # Wait longer and confirm it's still there.
    Process.sleep(500)

    assert ExMCP.Native.service_available?(service_name),
           "Service should still be available after another 500ms"

    IO.puts("SUCCESS: Service remained available after 600ms total.")
  end

  @doc """
  Test 3: wait_for_service_registration Helper Function
  This test specifically validates the behavior of the `wait_for_service_registration`
  helper. It checks both the success case (finding a registered service) and the
  failure case (timing out when a service does not exist). This helps rule out
  a faulty helper function as the source of the problem.
  """
  test "wait_for_service_registration helper function behavior", %{config: _config} do
    # Test for a service that should exist.
    assert {:ok, ExMCP.TestServer} =
             wait_for_service_registration(ExMCP.TestServer, 1000),
           "Helper should find the existing TestServer service"

    IO.puts("SUCCESS: Helper function correctly found an existing service.")

    # Test for a service that does not exist to confirm timeout logic.
    assert {:error, :timeout} =
             wait_for_service_registration(:non_existent_service, 100),
           "Helper should time out for a non-existent service"

    IO.puts("SUCCESS: Helper function correctly timed out for a non-existent service.")
  end

  @doc """
  Test 4: Registry State Inspection
  This test directly inspects the state of the Horde registry to see what
  services are actually listed. It bypasses any application-level abstractions
  to provide a ground-truth view of the registry. This can help identify issues
  like service name mismatches or unexpected empty registries.
  """
  test "list available services and inspect registry state", %{config: config} do
    service_name = ExMCP.TestServer
    # First, wait for the service to ensure we're inspecting the state *after*
    # it should have been registered.
    :ok = wait_for_service_registration(service_name)

    registry_name = config.registry_name
    # Debug info: Inspecting Horde Registry: #{registry_name}

    # Horde.Registry.members/1 returns all {key, value} pairs. For services,
    # this is typically {service_name, pid}.
    # Get all registered services by doing a match on all entries
    members =
      Horde.Registry.select(registry_name, [{{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2"}}]}])

    # Debug info: Current members in registry: #{inspect(members)}

    # Assert that our test server is one of the registered members.
    assert Enum.any?(members, fn {name, _pid} -> name == service_name end),
           "The service '#{inspect(service_name)}' was not found in the registry members list."

    IO.puts("SUCCESS: Service '#{inspect(service_name)}' was found in the registry member list.")
  end
end
