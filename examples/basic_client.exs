#!/usr/bin/env elixir

# Basic ExMCP Client Example
#
# This example demonstrates the simplified client API with:
# - Configuration builder pattern
# - Structured responses
# - Convenience functions
#
# To test this client:
# 1. Run a DSL server in another terminal:
#    elixir getting_started/dsl_server.exs
# 2. Update the command below to connect to your server

Mix.install([
  {:ex_mcp, path: Path.expand("../..", __DIR__)}
])

# Create client configuration using the builder pattern
config = ExMCP.ClientConfig.new()
         |> ExMCP.ClientConfig.put_transport(:stdio, command: ["elixir", "getting_started/hello_server_stdio.exs"])

IO.puts("Connecting to MCP server...")

# Connect using the convenience function
case ExMCP.connect(config) do
  {:ok, client} ->
    IO.puts("✓ Connected successfully!")

    # List available tools
    IO.puts("\nListing tools...")
    {:ok, tools_response} = ExMCP.list_tools(client)

    # Extract tools from structured response
    tools = ExMCP.Response.data_content(tools_response)["tools"] || []
    IO.puts("Found #{length(tools)} tools:")

    for tool <- tools do
      IO.puts("  - #{tool["name"]}: #{tool["description"]}")
    end

    # Call a tool if available
    if first_tool = List.first(tools) do
      IO.puts("\nCalling tool: #{first_tool["name"]}")

      # Example arguments (adjust based on your tool)
      args = %{"input" => "Hello from ExMCP!"}

      case ExMCP.call_tool(client, first_tool["name"], args) do
        {:ok, response} ->
          # Handle different response types
          cond do
            text = ExMCP.Response.text_content(response) ->
              IO.puts("Text response: #{text}")

            json = ExMCP.Response.data_content(response) ->
              IO.puts("JSON response: #{inspect(json)}")

            response.is_error ->
              IO.puts("Error response: #{ExMCP.Response.text_content(response)}")

            true ->
              IO.puts("Other response type: #{inspect(response)}")
          end

        {:error, error} ->
          IO.puts("Tool call failed: #{error.message}")
      end
    end

    # List resources
    IO.puts("\nListing resources...")
    {:ok, resources_response} = ExMCP.list_resources(client)
    resources = ExMCP.Response.data_content(resources_response)["resources"] || []

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
