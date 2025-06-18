#!/usr/bin/env elixir

# Minimal stdio server that just echoes

Mix.install([
  {:ex_mcp, path: "../../../"}
])

defmodule MinimalStdioServer do
  use ExMCP.ServerV2
  
  deftool "echo" do
    tool_description "Simple echo"
    args do
      field :text, :string, required: true
    end
  end
  
  @impl true
  def handle_tool_call("echo", %{"text" => text}, state) do
    {:ok, %{content: [text("Echo: #{text}")]}, state}
  end
end

# Start directly without the runner module
{:ok, server} = MinimalStdioServer.start_link()
{:ok, _transport} = ExMCP.Transport.Stdio.start_link(
  server: server,
  input: :stdio,
  output: :stdio
)

# Keep alive
Process.sleep(:infinity)