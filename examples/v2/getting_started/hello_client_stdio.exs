#!/usr/bin/env elixir

# Hello World Client - STDIO Transport
# Simple client that connects to a stdio server

# Add lib to path instead of using Mix.install
Code.prepend_path("_build/dev/lib/ex_mcp/ebin")
Code.prepend_path("_build/dev/lib/jason/ebin")

# Configure client for stdio transport
config = ExMCP.ClientConfig.new()
         |> ExMCP.ClientConfig.put_transport(:stdio, 
             command: ["elixir", "hello_server_stdio.exs"]
           )

IO.puts("Connecting to Hello World server...")

case ExMCP.connect(config) do
  {:ok, client} ->
    IO.puts("✓ Connected!")
    
    # Call the hello tool
    IO.puts("\nCalling say_hello tool...")
    {:ok, response} = ExMCP.call_tool(client, "say_hello", %{"name" => "World"})
    IO.puts("Response: #{ExMCP.Response.text_content(response)}")
    
    # Read the info resource
    IO.puts("\nReading server info...")
    {:ok, response} = ExMCP.read_resource(client, "hello://info")
    IO.puts("Info:\n#{ExMCP.Response.text_content(response)}")
    
    # Disconnect
    ExMCP.disconnect(client)
    IO.puts("\n✓ Disconnected")
    
  {:error, reason} ->
    IO.puts("Failed to connect: #{inspect(reason)}")
end