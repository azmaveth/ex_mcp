#!/usr/bin/env elixir

# Hello World Client - HTTP Transport
# Simple HTTP client example

# Add lib to path instead of using Mix.install
Code.prepend_path("_build/dev/lib/ex_mcp/ebin")
Code.prepend_path("_build/dev/lib/jason/ebin")

# Configure for HTTP
config = ExMCP.ClientConfig.new()
         |> ExMCP.ClientConfig.put_transport(:http,
             url: "http://localhost:3000",
             path: "/mcp"
           )

IO.puts("Connecting to HTTP server at http://localhost:3000/mcp...")

case ExMCP.connect(config) do
  {:ok, client} ->
    IO.puts("✓ Connected!")
    
    # Call hello tool
    {:ok, resp} = ExMCP.call_tool(client, "hello", %{"name" => "HTTP Client"})
    IO.puts("\nHello response: #{ExMCP.Response.text_content(resp)}")
    
    # Call echo tool
    test_data = %{
      message: "Hello from client",
      timestamp: System.system_time(:second)
    }
    {:ok, resp} = ExMCP.call_tool(client, "echo", %{"data" => test_data})
    IO.puts("\nEcho response: #{inspect(ExMCP.Response.data_content(resp), pretty: true)}")
    
    # Read resource
    {:ok, resp} = ExMCP.read_resource(client, "http://server/info")
    IO.puts("\nServer info: #{inspect(ExMCP.Response.data_content(resp), pretty: true)}")
    
    ExMCP.disconnect(client)
    IO.puts("\n✓ Disconnected")
    
  {:error, reason} ->
    IO.puts("Failed to connect: #{inspect(reason)}")
    IO.puts("\nMake sure the server is running:")
    IO.puts("  elixir hello_server_http.exs")
end