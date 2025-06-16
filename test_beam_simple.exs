defmodule SimpleBeamTest do
  alias ExMCP.Transport.Beam.{Server, Client}
  
  def run do
    IO.puts "Starting simple BEAM transport test..."
    
    # Start server
    {:ok, server_pid, port} = Server.start_test_server(port: 0)
    IO.puts "Server started on port #{port}"
    
    # Connect client
    {:ok, client} = Client.connect(host: "localhost", port: port)
    IO.puts "Client connected"
    
    # Wait for connection to be ready
    Process.sleep(500)
    
    # Test basic ping
    case Client.call(client, %{"method" => "ping"}) do
      {:ok, response} ->
        IO.puts "Ping response: #{inspect(response)}"
      {:error, reason} ->
        IO.puts "Ping failed: #{inspect(reason)}"
    end
    
    # Cleanup
    Client.close(client)
    Server.stop(server_pid)
    
    IO.puts "Test complete!"
  end
end

SimpleBeamTest.run()