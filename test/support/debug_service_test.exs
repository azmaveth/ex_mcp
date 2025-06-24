defmodule ExMCP.DebugServiceTest do
  use ExUnit.Case, async: false # Using async: false to be safe with global registry
  import ExMCP.TestHelpers

  setup_all do
    # The service registry is part of the :ex_mcp application.
    # It needs to be running for services to register.
    {:ok, _} = Application.ensure_all_started(:ex_mcp)

    # Clean up on exit
    on_exit(fn ->
      Application.stop(:ex_mcp)
    end)

    :ok
  end

  # A minimal service for testing registration.
  defmodule DebugService do
    use ExMCP.Service, name: :debug_service

    @impl true
    def init(_args) do
      # You can add IO.inspect here to see when the service process starts
      IO.puts("DebugService: init callback")
      {:ok, %{}}
    end

    # A minimal implementation to satisfy the behaviour.
    @impl true
    def handle_mcp_request("initialize", _params, state) do
      IO.puts("DebugService: handle_mcp_request 'initialize'")
      {:ok, %{"protocolVersion" => "2025-06-18", "capabilities" => %{}}, state}
    end

    def handle_mcp_request(method, _params, state) do
      IO.puts("DebugService: handle_mcp_request '#{method}' (unhandled)")
      {:error, "Method not implemented", state}
    end
  end

  test "debug service registration and wait helper" do
    # 1. Check services before registration
    IO.puts("\n--- Checking services before start ---")
    services_before = ExMCP.Native.list_services()
    IO.inspect(services_before, label: "Services available before start")
    refute :debug_service in services_before

    # 2. Start the service
    IO.puts("\n--- Starting DebugService ---")
    {:ok, service_pid} = DebugService.start_link(%{})
    IO.inspect(service_pid, label: "DebugService started with PID")
    assert is_pid(service_pid)

    # The service registration is asynchronous.
    # We might need a small sleep to see the change, or we can just rely on the wait helper.
    # Let's check immediately after start to see what happens.
    Process.sleep(50) # Give a moment for registration message to be sent/processed.

    IO.puts("\n--- Checking services immediately after start ---")
    services_after_start = ExMCP.Native.list_services()
    IO.inspect(services_after_start, label: "Services available after start")

    # 3. Use the wait helper to ensure registration is complete
    IO.puts("\n--- Waiting for service registration ---")
    wait_result = wait_for_service_registration(:debug_service, 5000)
    IO.inspect(wait_result, label: "Result of wait_for_service_registration")

    assert wait_result == {:ok, :debug_service},
           "Service :debug_service should be registered"

    # 4. Check services after successful registration
    IO.puts("\n--- Checking services after wait helper ---")
    services_after_wait = ExMCP.Native.list_services()
    IO.inspect(services_after_wait, label: "Services available after waiting")
    assert :debug_service in services_after_wait

    # 5. Cleanup
    IO.puts("\n--- Cleaning up ---")
    GenServer.stop(service_pid)
    :ok = ExMCP.Native.unregister_service(:debug_service)
    IO.puts("DebugService stopped and unregistered")

    services_after_cleanup = ExMCP.Native.list_services()
    IO.inspect(services_after_cleanup, label: "Services available after cleanup")
    refute :debug_service in services_after_cleanup
  end
end
