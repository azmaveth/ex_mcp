#!/usr/bin/env elixir

# Basic ExMCP client example. It starts an in-process BEAM server so the
# example is self-contained.

Mix.install([
  {:ex_mcp, path: Path.expand("..", __DIR__)}
])

defmodule BasicClientServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "basic-client-server", version: "1.0.0"

  tool "echo", "Echoes the provided message" do
    title "Echo"
    param :message, :string, required: true

    run fn %{message: message}, state ->
      {:ok, "Echo: #{message}", state}
    end
  end

  resource "demo://readme", "Demo resource" do
    title "Demo Readme"
    mime_type "text/plain"

    read fn %{uri: uri}, state ->
      {:ok, %{uri: uri, text: "This resource came from a BEAM-local MCP server."}, state}
    end
  end
end

{:ok, server} = BasicClientServer.start_link(transport: :beam)
{:ok, client} = ExMCP.Client.start_link(transport: :beam, server: server)

field = fn map, key -> Map.get(map, Atom.to_string(key)) || Map.get(map, key) end

{:ok, %{"tools" => tools}} = ExMCP.Client.list_tools(client, format: :map)
IO.puts("Tools: #{Enum.map_join(tools, ", ", &field.(&1, :name))}")

{:ok, result} =
  ExMCP.Client.call_tool(client, "echo", %{"message" => "Hello from ExMCP."}, format: :map)

[%{"text" => tool_text} | _] = result["content"]
IO.puts("Tool result: #{tool_text}")

{:ok, %{"resources" => resources}} = ExMCP.Client.list_resources(client, format: :map)
IO.puts("Resources: #{Enum.map_join(resources, ", ", &field.(&1, :uri))}")

{:ok, %{"contents" => [resource]}} =
  ExMCP.Client.read_resource(client, "demo://readme", format: :map)

IO.puts("Resource text: #{field.(resource, :text)}")

ExMCP.Client.stop(client)
GenServer.stop(server)
