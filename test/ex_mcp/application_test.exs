defmodule ExMCP.ApplicationTest do
  use ExUnit.Case, async: false

  setup do
    # Stop the application if it's already running
    Application.stop(:ex_mcp)
    # Give time for cleanup
    Process.sleep(10)
    :ok
  end

  describe "start/2" do
    test "starts the application supervisor" do
      # Start the application
      assert {:ok, pid} = ExMCP.Application.start(:normal, [])
      assert is_pid(pid)

      # Verify the supervisor is running
      assert Process.alive?(pid)

      # Verify it has the correct name
      assert Process.whereis(ExMCP.Supervisor) == pid

      # Clean up
      Process.exit(pid, :normal)
    end

    test "starts the ExMCP.ServiceRegistry" do
      # Start the application
      {:ok, sup_pid} = ExMCP.Application.start(:normal, [])

      # Give it a moment to start children
      Process.sleep(50)

      # Verify the registry is running
      registry_pid = Process.whereis(ExMCP.ServiceRegistry)
      assert is_pid(registry_pid)
      assert Process.alive?(registry_pid)

      # Clean up
      Process.exit(sup_pid, :normal)
    end

    test "supervisor restarts children on failure" do
      # Start the application
      {:ok, sup_pid} = ExMCP.Application.start(:normal, [])

      # Give it a moment to start children
      Process.sleep(50)

      # Get the registry pid
      registry_pid = Process.whereis(ExMCP.ServiceRegistry)
      assert is_pid(registry_pid)

      # Kill the registry
      Process.exit(registry_pid, :kill)

      # Give supervisor time to restart it
      Process.sleep(100)

      # Verify a new registry is running
      new_registry_pid = Process.whereis(ExMCP.ServiceRegistry)
      assert is_pid(new_registry_pid)
      assert new_registry_pid != registry_pid
      assert Process.alive?(new_registry_pid)

      # Clean up
      Process.exit(sup_pid, :normal)
    end

    test "uses one_for_one strategy" do
      # Start the application
      {:ok, sup_pid} = ExMCP.Application.start(:normal, [])

      # Get supervisor info
      sup_info = Supervisor.which_children(sup_pid)

      # Verify we have children
      assert length(sup_info) > 0

      # Verify the registry is among the children
      assert Enum.any?(sup_info, fn {id, _child, _type, _modules} ->
               id == ExMCP.ServiceRegistry
             end)

      # Clean up
      Process.exit(sup_pid, :normal)
    end
  end

  describe "application behaviour" do
    test "module implements Application behaviour" do
      assert {:module, ExMCP.Application} = Code.ensure_compiled(ExMCP.Application)

      # Check that it exports the required functions
      functions = ExMCP.Application.__info__(:functions)
      assert {:start, 2} in functions
    end
  end
end
