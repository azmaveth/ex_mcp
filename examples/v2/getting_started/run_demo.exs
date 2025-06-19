#!/usr/bin/env elixir

# Demo Orchestrator Script
# Starts all servers, runs the universal client, then cleans up

Mix.install([
  {:ex_mcp, path: Path.expand("../../..", __DIR__)}
])

defmodule DemoOrchestrator do
  @demo_cookie :hello_mcp_demo_cookie
  
  def run do
    IO.puts("""
    ==========================================
    ExMCP v2 Complete Transport Demo
    ==========================================
    
    This script will:
    1. Start all transport servers
    2. Run the universal client
    3. Stop all servers and clean up
    """)
    
    # Start this node with a name for distributed coordination
    start_coordinator_node()
    
    # Start all servers
    servers = start_all_servers()
    
    # Wait for servers to be ready
    IO.puts("\nWaiting for servers to initialize...")
    Process.sleep(2000)
    
    # Run the client demo
    run_client_demo()
    
    # Clean up
    cleanup_servers(servers)
    
    IO.puts("\n✅ Demo complete!")
  end
  
  defp start_coordinator_node do
    node_name = :"coordinator@127.0.0.1"
    case Node.start(node_name, :shortnames) do
      {:ok, _} -> 
        IO.puts("Started coordinator node: #{node_name}")
      {:error, {:already_started, _}} -> 
        IO.puts("Coordinator node already running: #{node_name}")
      error -> 
        IO.puts("Failed to start coordinator node: #{inspect(error)}")
        System.halt(1)
    end
    
    Node.set_cookie(@demo_cookie)
  end
  
  defp start_all_servers do
    IO.puts("\n🚀 Starting servers...")
    
    servers = %{
      native: start_native_server(),
      http: start_http_server(),
      sse: start_sse_server()
    }
    
    IO.puts("✓ All servers started")
    servers
  end
  
  defp start_native_server do
    IO.puts("  Starting Native server...")
    
    port = Port.open(
      {:spawn_executable, System.find_executable("elixir")},
      [
        :binary,
        :exit_status,
        args: [
          "--name", "server@127.0.0.1",
          "--cookie", to_string(@demo_cookie),
          "hello_server_native.exs",
          "--distributed"
        ],
        cd: __DIR__
      ]
    )
    
    # Wait for native server to be available
    wait_for_native_server()
    
    {:native, port}
  end
  
  defp wait_for_native_server do
    server_node = :"server@127.0.0.1"
    
    Enum.reduce_while(1..30, nil, fn attempt, _acc ->
      case Node.connect(server_node) do
        true ->
          case :rpc.call(server_node, Process, :whereis, [:hello_native_server]) do
            pid when is_pid(pid) ->
              IO.puts("    ✓ Native server ready")
              {:halt, :ok}
            _ ->
              if attempt == 30 do
                IO.puts("    ✗ Native server failed to start")
                {:halt, :error}
              else
                Process.sleep(200)
                {:cont, nil}
              end
          end
        false ->
          if attempt == 30 do
            IO.puts("    ✗ Could not connect to native server node")
            {:halt, :error}
          else
            Process.sleep(200)
            {:cont, nil}
          end
      end
    end)
  end
  
  defp start_http_server do
    IO.puts("  Starting HTTP server...")
    
    port = Port.open(
      {:spawn_executable, System.find_executable("elixir")},
      [
        :binary,
        :exit_status,
        args: ["hello_server_http.exs"],
        cd: __DIR__
      ]
    )
    
    # Wait for HTTP server to be available
    wait_for_http_server("http://localhost:3000", "HTTP")
    
    {:http, port}
  end
  
  defp start_sse_server do
    IO.puts("  Starting SSE server...")
    
    port = Port.open(
      {:spawn_executable, System.find_executable("elixir")},
      [
        :binary,
        :exit_status,
        args: ["hello_server_sse.exs"],
        cd: __DIR__
      ]
    )
    
    # Wait for SSE server to be available
    wait_for_http_server("http://localhost:3001", "SSE")
    
    {:sse, port}
  end
  
  defp wait_for_http_server(url, name) do
    Enum.reduce_while(1..30, nil, fn attempt, _acc ->
      try do
        case :httpc.request(:get, {String.to_charlist(url), []}, [], []) do
          {:ok, {{_, 200, _}, _, _}} ->
            IO.puts("    ✓ #{name} server ready")
            {:halt, :ok}
          _ ->
            if attempt == 30 do
              IO.puts("    ✗ #{name} server failed to start")
              {:halt, :error}
            else
              Process.sleep(200)
              {:cont, nil}
            end
        end
      rescue
        _ ->
          if attempt == 30 do
            IO.puts("    ✗ #{name} server failed to start")
            {:halt, :error}
          else
            Process.sleep(200)
            {:cont, nil}
          end
      end
    end)
  end
  
  defp run_client_demo do
    IO.puts("\n🔄 Running universal client demo...")
    
    # Create a custom client that doesn't exit the process
    port = Port.open(
      {:spawn_executable, System.find_executable("elixir")},
      [
        :binary,
        :exit_status,
        args: [
          "--name", "client@127.0.0.1",
          "--cookie", to_string(@demo_cookie),
          "hello_client_all.exs"
        ],
        cd: __DIR__
      ]
    )
    
    # Collect and display output
    collect_client_output(port)
  end
  
  defp collect_client_output(port) do
    receive do
      {^port, {:data, data}} ->
        IO.write(data)
        collect_client_output(port)
        
      {^port, {:exit_status, 0}} ->
        IO.puts("\n✓ Client demo completed successfully")
        
      {^port, {:exit_status, status}} ->
        IO.puts("\n✗ Client demo failed with status: #{status}")
        
    after
      30_000 ->
        IO.puts("\n⚠ Client demo timed out")
        Port.close(port)
    end
  end
  
  defp cleanup_servers(servers) do
    IO.puts("\n🧹 Cleaning up servers...")
    
    # Close all ports
    Enum.each(servers, fn {name, {_type, port}} ->
      IO.puts("  Stopping #{name} server...")
      Port.close(port)
    end)
    
    # Give processes time to clean up
    Process.sleep(1000)
    
    # Disconnect from distributed nodes
    Node.disconnect(:"server@127.0.0.1")
    Node.disconnect(:"client@127.0.0.1")
    
    IO.puts("✓ Cleanup complete")
  end
end

# Handle interrupts gracefully
Process.flag(:trap_exit, true)

# Run the demo
try do
  DemoOrchestrator.run()
catch
  :exit, reason ->
    IO.puts("\nDemo interrupted: #{inspect(reason)}")
    System.halt(1)
end