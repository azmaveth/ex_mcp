defmodule ExMCP.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start any global services here
      # For example, a registry for MCP connections:
      {Registry, keys: :unique, name: ExMCP.Registry}
    ]

    opts = [strategy: :one_for_one, name: ExMCP.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
