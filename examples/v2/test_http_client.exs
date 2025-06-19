#!/usr/bin/env elixir

# Test HTTP Client for v2

# Add lib to path
Code.prepend_path("_build/dev/lib/ex_mcp/ebin")
Code.prepend_path("_build/dev/lib/jason/ebin")

alias ExMCP.ClientConfig

# Create HTTP client configuration
config = ClientConfig.new()
         |> ClientConfig.put_transport(:http, 
           url: "http://localhost:8080",
           path: "/mcp/v1"
         )

IO.puts("Connecting to HTTP server...")

case ExMCP.connect(config) do
  {:ok, client} ->
    IO.puts("✓ Connected!")
    
    # List tools
    IO.puts("\nListing tools...")
    case ExMCP.list_tools(client) do
      {:ok, response} ->
        tools = ExMCP.Response.data_content(response)["tools"] || []
        IO.puts("Found #{length(tools)} tools:")
        for tool <- tools do
          IO.puts("  - #{tool["name"]}: #{tool["description"]}")
        end
      error ->
        IO.puts("Error: #{inspect(error)}")
    end
    
    # Call echo tool
    IO.puts("\nCalling echo tool...")
    case ExMCP.call(client, "echo", %{message: "Hello from HTTP client!"}) do
      {:ok, result} ->
        IO.puts("Result: #{inspect(result)}")
      error ->
        IO.puts("Error: #{inspect(error)}")
    end
    
    # Disconnect
    ExMCP.disconnect(client)
    IO.puts("\nDisconnected!")
    
  {:error, reason} ->
    IO.puts("Failed to connect: #{inspect(reason)}")
    IO.puts("\nMake sure the HTTP server is running:")
    IO.puts("  elixir examples/v2/working_http_example.exs")
end