defmodule ExMCP.HordeTestHelpers do
  @moduledoc """
  Test helpers for creating isolated Horde processes to prevent conflicts
  during parallel test execution.

  ## Critical: Process Isolation Requirements

  **ANY test using Horde processes MUST be marked with `async: false`**
  to prevent race conditions during process startup/cleanup:

      defmodule MyTest do
        use ExUnit.Case, async: false  # REQUIRED!
        import ExMCP.HordeTestHelpers

        setup %{test: test} do
          registry_name = unique_process_name(test, "registry")
          supervisor_name = unique_process_name(test, "supervisor")

          {:ok, registry} = Horde.Registry.start_link(
            name: registry_name,
            keys: :unique
          )

          on_exit(fn ->
            if Process.alive?(registry), do: GenServer.stop(registry)
          end)

          %{registry: registry_name}
        end
      end

  ## Why async: false is Required

  Even with unique names, Horde processes have startup/shutdown timing that
  can cause conflicts when tests run in parallel. The `async: false` ensures
  tests run sequentially, preventing these timing issues.
  """

  @doc """
  Generates a unique process name for a test, combining the test name
  with a prefix to ensure uniqueness across parallel test runs.

  ## Parameters

  - `test` - The test name atom (from setup %{test: test})
  - `prefix` - String prefix for the process type (e.g., "registry", "supervisor")

  ## Examples

      setup %{test: test} do
        registry_name = unique_process_name(test, "registry")
        # => ExMCP.Test.Registry.test_creates_a_new_user_123456
      end

  The generated name includes:
  - A base module namespace (ExMCP.Test)
  - The prefix (capitalized)
  - The test name (sanitized)
  - A unique identifier based on the current time
  """
  def unique_process_name(test, prefix) when is_atom(test) and is_binary(prefix) do
    # Get a unique identifier
    unique_id = :erlang.unique_integer([:positive])

    # Sanitize test name to be module-safe
    test_name =
      test
      |> Atom.to_string()
      |> String.replace(~r/[^a-zA-Z0-9_]/, "_")
      |> String.replace(~r/_+/, "_")
      |> String.trim("_")

    # Capitalize the prefix
    capitalized_prefix = String.capitalize(prefix)

    # Construct the unique module name
    "ExMCP.Test.#{capitalized_prefix}.#{test_name}_#{unique_id}"
    |> String.to_atom()
  end

  @doc """
  A setup block helper for tests that use Horde.

  This macro is intended to be used within a `setup` block in your tests.
  It simplifies the process of generating unique names for the Horde registry
  and supervisor, starting them, and ensuring they are cleaned up.

  It returns a map with the unique names, which can be passed to other
  setup functions or used directly in the test.

  **Important:** This macro must be called from a `setup` block that receives
  the test context, for example `setup context do ... end`.

  ## Options
    - `:supervisor_prefix`: The string prefix for the Horde supervisor name. Defaults to `"supervisor"`.
    - `:registry_prefix`: The string prefix for the Horde registry name. Defaults to `"registry"`.

  ## Usage
      setup context do
        {:ok, horde} = setup_test_horde()
        # Start your application/supervisor with the unique horde names
        # e.g., start_supervised({MyApplication, horde_supervisor: horde.supervisor_name})
        %{horde: horde}
      end
  """
  defmacro setup_test_horde(opts \\ []) do
    quote do
      # This macro must be called from a setup block that receives the test context.
      # The context map is assumed to be in a variable named `context`.
      # We access `context.test` to get the current test name.
      context = var!(context, __MODULE__)
      test = context.test

      supervisor_prefix = Keyword.get(unquote(opts), :supervisor_prefix, "supervisor")
      registry_prefix = Keyword.get(unquote(opts), :registry_prefix, "registry")

      supervisor_name = ExMCP.HordeTestHelpers.unique_process_name(test, supervisor_prefix)
      registry_name = ExMCP.HordeTestHelpers.unique_process_name(test, registry_prefix)

      # Start Horde processes for the test
      {:ok, _sup} = Horde.Supervisor.start_link([], name: supervisor_name)
      {:ok, _reg} = Horde.Registry.start_link([], name: registry_name, keys: :unique)

      # Ensure processes are stopped after the test.
      on_exit(fn ->
        ExMCP.HordeTestHelpers.cleanup_horde_processes([supervisor_name, registry_name])
      end)

      {:ok,
       %{
         supervisor_name: supervisor_name,
         registry_name: registry_name
       }}
    end
  end

  @doc """
  Stops a list of named processes.

  Useful for cleaning up Horde processes after a test run. This is often
  called from an `on_exit` callback within a setup block.
  """
  def cleanup_horde_processes(process_names) when is_list(process_names) do
    for name <- process_names do
      case Process.whereis(name) do
        nil ->
          :ok

        pid ->
          ref = Process.monitor(pid)
          Process.exit(pid, :shutdown)

          receive do
            {:DOWN, ^ref, _, _, _} -> :ok
          after
            # If it doesn't die gracefully, kill it.
            100 -> Process.exit(pid, :kill)
          end
      end
    end
  end
end
