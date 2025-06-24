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

    # List available tools using the unified API
    IO.puts("\nListing tools...")
    case ExMCP.tools(client) do
      tools when is_list(tools) ->
        IO.puts("Found #{length(tools)} tools:")

        for tool <- tools do
          IO.puts("  - #{tool["name"]}: #{tool["description"]}")
        end

        # Call a tool if available
        if first_tool = List.first(tools) do
          IO.puts("\nCalling tool: #{first_tool["name"]}")

          # Example arguments (adjust based on your tool)
          args = %{"input" => "Hello from ExMCP!"}

          case ExMCP.call(client, first_tool["name"], args) do
            result when is_binary(result) ->
              IO.puts("Tool response: #{result}")

            {:error, error} ->
              IO.puts("Tool call failed: #{inspect(error)}")

            other ->
              IO.puts("Tool response: #{inspect(other)}")
          end
        end

      {:error, error} ->
        IO.puts("Failed to list tools: #{inspect(error)}")
    end

    # List resources using the unified API
    IO.puts("\nListing resources...")
    case ExMCP.resources(client) do
      resources when is_list(resources) ->
        IO.puts("Found #{length(resources)} resources:")
        for resource <- resources do
          IO.puts("  - #{resource["uri"]}: #{resource["name"]}")
        end

      {:error, error} ->
        IO.puts("Failed to list resources: #{inspect(error)}")
    end

    # Disconnect
    IO.puts("\nDisconnecting...")
    ExMCP.disconnect(client)
    IO.puts("✓ Disconnected")

  {:error, reason} ->
    IO.puts("Failed to connect: #{inspect(reason)}")
end
