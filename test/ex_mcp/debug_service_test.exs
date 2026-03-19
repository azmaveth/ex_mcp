defmodule ExMCP.DebugServiceTest do
  use ExUnit.Case, async: false
  import ExMCP.TestHelpers

  @moduletag :requires_beam

  # This setup block starts an ExMCP.TestServer using the :native transport
  # and registers it with the service registry for discovery.
  setup do
    # Ensure the ExMCP application is started (provides ServiceRegistry)
    Application.ensure_all_started(:ex_mcp)

    # Start the server with the native transport
    case ExMCP.TestHelpers.__setup_server_by_transport__(:native, transport: :native) do
      {:ok, server_config} ->
        pid = server_config.pid
        service_name = ExMCP.TestServer

        # Register the server with the service registry so it's discoverable
        ExMCP.Native.register_service(service_name)

        on_exit(fn ->
          if Process.alive?(pid), do: GenServer.stop(pid, :shutdown, 500)
        end)

        {:ok, Map.put(server_config, :service_name, service_name)}

      error ->
        error
    end
  end

  @doc """
  Test 1: Basic Service Registration
  This test verifies that the ExMCP.TestServer successfully starts and registers
  itself as a service with the service registry within a reasonable time.
  """
  test "service should register successfully on startup", _context do
    service_name = ExMCP.TestServer
    # Wait for up to 5 seconds for the service to appear.
    assert {:ok, ^service_name} = wait_for_service_registration(service_name)

    IO.puts("SUCCESS: Service '#{inspect(service_name)}' registered successfully.")
  end

  @doc """
  Test 2: Service Availability Timing
  This test checks for the service's presence at different moments in time.
  """
  test "service availability at different time intervals", _context do
    service_name = ExMCP.TestServer

    # The service was registered in setup, should be available immediately.
    assert ExMCP.Native.service_available?(service_name),
           "Service should be available immediately after registration"

    IO.puts("SUCCESS: Service was available immediately.")

    # Wait and confirm it's still there.
    Process.sleep(500)

    assert ExMCP.Native.service_available?(service_name),
           "Service should still be available after 500ms"

    IO.puts("SUCCESS: Service remained available after 500ms.")
  end

  @doc """
  Test 3: wait_for_service_registration Helper Function
  This test specifically validates the behavior of the `wait_for_service_registration`
  helper.
  """
  test "wait_for_service_registration helper function behavior", _context do
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
  This test directly inspects the registry to see what services are listed.
  """
  test "list available services and inspect registry state", _context do
    service_name = ExMCP.TestServer
    # First, wait for the service to ensure we're inspecting the state *after*
    # it should have been registered.
    {:ok, ^service_name} = wait_for_service_registration(service_name)

    # List all registered services using the service registry
    services = ExMCP.Native.list_services()

    # Assert that our test server is one of the registered services.
    assert Enum.any?(services, fn {name, _pid, _metadata} -> name == service_name end),
           "The service '#{inspect(service_name)}' was not found in the registry."

    IO.puts("SUCCESS: Service '#{inspect(service_name)}' was found in the registry.")
  end
end
