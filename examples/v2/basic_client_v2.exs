#!/usr/bin/env elixir

# Basic ExMCP v2 Client Example
# 
# This example demonstrates the simplified v2 client API with:
# - Configuration builder pattern
# - Structured responses
# - Convenience functions

Mix.install([
  {:ex_mcp, path: "../.."}
])

# Create client configuration using the builder pattern
config = ExMCP.client_config()
         |> ExMCP.put_transport(:stdio)
         |> ExMCP.put_command(["python", "-m", "mcp.server.example"])

IO.puts("Connecting to MCP server...")

# Connect using the convenience function
case ExMCP.connect(config) do
  {:ok, client} ->
    IO.puts("✓ Connected successfully!")
    
    # List available tools
    IO.puts("\nListing tools...")
    {:ok, tools_response} = ExMCP.list_tools(client)
    
    # Extract tools from structured response
    tools = ExMCP.Response.tools(tools_response)
    IO.puts("Found #{length(tools)} tools:")
    
    for tool <- tools do
      IO.puts("  - #{tool["name"]}: #{tool["description"]}")
    end
    
    # Call a tool if available
    if first_tool = List.first(tools) do
      IO.puts("\nCalling tool: #{first_tool["name"]}")
      
      # Example arguments (adjust based on your tool)
      args = %{"input" => "Hello from ExMCP v2!"}
      
      case ExMCP.call_tool(client, first_tool["name"], args) do
        {:ok, response} ->
          # Handle different response types
          cond do
            text = ExMCP.Response.text_content(response) ->
              IO.puts("Text response: #{text}")
              
            json = ExMCP.Response.json_content(response) ->
              IO.puts("JSON response: #{inspect(json)}")
              
            response.type == :error ->
              IO.puts("Error response: #{response.content.message}")
              
            true ->
              IO.puts("Other response type: #{response.type}")
          end
          
        {:error, error} ->
          IO.puts("Tool call failed: #{error.message}")
      end
    end
    
    # List resources
    IO.puts("\nListing resources...")
    {:ok, resources_response} = ExMCP.list_resources(client)
    resources = ExMCP.Response.resources(resources_response)
    
    IO.puts("Found #{length(resources)} resources:")
    for resource <- resources do
      IO.puts("  - #{resource["uri"]}: #{resource["name"]}")
    end
    
    # Disconnect
    IO.puts("\nDisconnecting...")
    ExMCP.disconnect(client)
    IO.puts("✓ Disconnected")
    
  {:error, reason} ->
    IO.puts("Failed to connect: #{inspect(reason)}")
end