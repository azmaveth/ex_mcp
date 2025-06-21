#!/usr/bin/env elixir

# Script to clean up stray processes from crashed tests
# Usage: elixir scripts/cleanup_test_processes.exs

defmodule TestProcessCleanup do
  @moduledoc """
  Cleans up stray processes that may be left over from crashed tests.
  This includes Cowboy listeners, registered processes, and ports.
  """

  @test_ports [8080, 8081, 8082, 8083, 8084, 8085, 9000, 9001, 9002]
  @registered_names [
    :test_server,
    :test_http_server,
    :test_sse_server,
    ExMCP.TestServer,
    ExMCP.TestHTTPServer,
    ExMCP.TestSSEServer,
    ExMCP.Supervisor,
    ExMCP.ServiceRegistry,
    ExMCP.Registry,
    ExMCP.Testing.MockServer
  ]
  @cowboy_listeners [
    :test_http_server,
    :test_sse_server,
    :test_http_listener,
    :test_sse_listener,
    ExMCP.TestHTTPServer.HTTP,
    ExMCP.TestSSEServer.HTTP
  ]

  def run do
    IO.puts("🧹 Cleaning up test processes...\n")

    # Stop Cowboy listeners
    stop_cowboy_listeners()

    # Kill processes by registered name
    kill_registered_processes()

    # Close ports that might be in use
    close_test_ports()

    # Kill any beam processes that might be test-related
    kill_stray_beam_processes()

    IO.puts("\n✅ Cleanup complete!")
  end

  defp stop_cowboy_listeners do
    IO.puts("📡 Stopping Cowboy listeners...")
    
    for listener <- @cowboy_listeners do
      case :cowboy.stop_listener(listener) do
        :ok -> 
          IO.puts("  ✓ Stopped listener: #{inspect(listener)}")
        {:error, :not_found} -> 
          :ok
        error -> 
          IO.puts("  ⚠️  Failed to stop #{inspect(listener)}: #{inspect(error)}")
      end
    end
  end

  defp kill_registered_processes do
    IO.puts("\n🔍 Killing registered processes...")
    
    for name <- @registered_names do
      case Process.whereis(name) do
        nil -> 
          :ok
        pid when is_pid(pid) ->
          Process.exit(pid, :kill)
          IO.puts("  ✓ Killed process: #{inspect(name)} (#{inspect(pid)})")
      end
    end

    # Also check for any process with "test" in its registered name
    Process.registered()
    |> Enum.filter(fn name -> 
      name_str = Atom.to_string(name)
      String.contains?(name_str, "test") or String.contains?(name_str, "Test")
    end)
    |> Enum.each(fn name ->
      if name not in @registered_names do
        case Process.whereis(name) do
          nil -> :ok
          pid ->
            Process.exit(pid, :kill)
            IO.puts("  ✓ Killed additional test process: #{inspect(name)}")
        end
      end
    end)
  end

  defp close_test_ports do
    IO.puts("\n🔌 Checking for processes using test ports...")
    
    for port <- @test_ports do
      case find_process_using_port(port) do
        nil -> 
          :ok
        pid ->
          kill_process(pid)
          IO.puts("  ✓ Killed process using port #{port}: PID #{pid}")
      end
    end
  end

  defp find_process_using_port(port) do
    # Use lsof to find process using the port
    case System.cmd("lsof", ["-ti", ":#{port}"]) do
      {"", 0} -> 
        nil
      {output, 0} ->
        output
        |> String.trim()
        |> String.split("\n")
        |> List.first()
      _ -> 
        nil
    end
  end

  defp kill_process(pid_str) do
    System.cmd("kill", ["-9", pid_str])
  end

  defp kill_stray_beam_processes do
    IO.puts("\n🔎 Looking for stray beam.smp processes...")
    
    # Find beam.smp processes that might be test-related
    case System.cmd("ps", ["aux"]) do
      {output, 0} ->
        output
        |> String.split("\n")
        |> Enum.filter(fn line ->
          String.contains?(line, "beam.smp") and
          (String.contains?(line, "MIX_ENV=test") or
           String.contains?(line, "ex_mcp/test") or
           String.contains?(line, "_build/test"))
        end)
        |> Enum.each(fn line ->
          case extract_pid(line) do
            nil -> :ok
            pid ->
              # Don't kill our own process!
              if pid != to_string(System.pid()) do
                kill_process(pid)
                IO.puts("  ✓ Killed stray beam.smp process: PID #{pid}")
              end
          end
        end)
      _ -> 
        :ok
    end
  end

  defp extract_pid(ps_line) do
    parts = String.split(ps_line)
    case parts do
      [_user, pid | _] -> pid
      _ -> nil
    end
  end
end

# Run the cleanup
TestProcessCleanup.run()