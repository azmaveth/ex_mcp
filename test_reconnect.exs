defmodule ReconnectTest do
  alias ExMCP.Transport.Beam.{Server, Client}
  require Logger
  
  def run do
    # Set log level to see debug messages
    Logger.configure(level: :debug)
    
    IO.puts "Testing reconnection..."
    
    # Start server
    {:ok, server_pid, port} = Server.start_test_server(port: 0)
    IO.puts "Server started on port #{port}"
    
    # Connect client with auto-reconnect
    {:ok, client} = Client.connect(
      host: "localhost", 
      port: port, 
      auto_reconnect: true
    )
    IO.puts "Client connected"
    
    # Wait for connection ready
    Process.sleep(500)
    
    # Test initial connection
    case Client.call(client, %{"method" => "ping"}) do
      {:ok, result} -> 
        IO.puts "✓ Initial connection works: #{inspect(result)}"
      error -> 
        IO.puts "✗ Initial connection failed: #{inspect(error)}"
    end
    
    # Simulate network interruption by killing the connection process with normal exit
    IO.puts "Simulating network interruption..."
    client_state = :sys.get_state(client)
    
    if client_state.connection_pid do
      IO.puts "Terminating connection process #{inspect(client_state.connection_pid)} normally"
      # Use normal exit to avoid killing the client
      GenServer.stop(client_state.connection_pid, :normal)
    end
    IO.puts "Network interruption simulated"
    
    # Wait for reconnection
    IO.puts "Waiting 6 seconds for reconnection..."
    Process.sleep(6000)
    
    # Check if reconnection was attempted
    client_state_before = :sys.get_state(client)
    IO.puts "Client state before test: connected=#{client_state_before.connected}, connection_pid=#{inspect(client_state_before.connection_pid)}, attempts=#{client_state_before.reconnect_attempts}"
    
    # Test connection after reconnection
    case Client.call(client, %{"method" => "ping"}) do
      {:ok, result} -> 
        IO.puts "✓ Reconnection works: #{inspect(result)}"
      error -> 
        IO.puts "✗ Reconnection failed: #{inspect(error)}"
        
        # Wait a bit more and try again
        IO.puts "Waiting another 5 seconds for reconnection..."
        Process.sleep(5000)
        
        client_state_after_more_wait = :sys.get_state(client)
        IO.puts "Client state after more wait: connected=#{client_state_after_more_wait.connected}, connection_pid=#{inspect(client_state_after_more_wait.connection_pid)}, attempts=#{client_state_after_more_wait.reconnect_attempts}"
        
        case Client.call(client, %{"method" => "ping"}) do
          {:ok, result} -> 
            IO.puts "✓ Reconnection works after longer wait: #{inspect(result)}"
          error2 -> 
            IO.puts "✗ Reconnection still failed: #{inspect(error2)}"
        end
    end
    
    # Check client status
    status = Client.status(client)
    IO.puts "Final client status: #{inspect(status)}"
    
    # Check client state
    final_state = :sys.get_state(client)
    IO.puts "Final client state: connected=#{final_state.connected}, connection_pid=#{inspect(final_state.connection_pid)}, attempts=#{final_state.reconnect_attempts}"
    
    # Cleanup
    IO.puts "Cleaning up..."
    Client.close(client)
    Server.stop(server_pid)
    
    IO.puts "Test complete!"
  end
end

ReconnectTest.run()