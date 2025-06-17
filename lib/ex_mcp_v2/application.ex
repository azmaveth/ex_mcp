defmodule ExMCPV2.Application do
  @moduledoc """
  Application module for ExMCP v2.

  This application provides the new high-level, developer-friendly framework
  for building MCP servers and clients.
  """

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Registry for capabilities
      {ExMCP.Registry, [name: ExMCP.Registry]}
    ]

    opts = [strategy: :one_for_one, name: ExMCPV2.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
