defmodule ExMCP.ServiceRegistry.Horde do
  @moduledoc """
  Distributed service registry adapter using Horde.

  Enables service discovery across multiple BEAM nodes via
  Horde's CRDT-based gossip protocol.

  ## Setup

  Add Horde to your dependencies:

      # mix.exs
      defp deps do
        [
          {:horde, "~> 0.9"}
        ]
      end

  Then configure ExMCP to use this adapter:

      # config/config.exs
      config :ex_mcp, :service_registry, ExMCP.ServiceRegistry.Horde
  """

  @behaviour ExMCP.ServiceRegistry

  @registry_name __MODULE__.Registry

  @impl true
  def child_specs(_opts) do
    ensure_horde!()

    [
      {Horde.Registry, keys: :unique, name: @registry_name, members: :auto}
    ]
  end

  @impl true
  def register(name, metadata) when is_atom(name) do
    case Horde.Registry.register(@registry_name, name, metadata) do
      {:ok, _pid} ->
        :ok

      {:error, {:already_registered, _pid}} ->
        :ok
    end
  end

  @impl true
  def unregister(name) when is_atom(name) do
    Horde.Registry.unregister(@registry_name, name)
    :ok
  end

  @impl true
  def lookup(name) when is_atom(name) do
    case Horde.Registry.lookup(@registry_name, name) do
      [{pid, _metadata}] -> {:ok, pid}
      [] -> {:error, :not_found}
    end
  end

  @doc """
  Looks up a service and verifies it is running on the specified node.

  This is useful for cross-node service calls where you need to ensure the
  service is on a specific node in the cluster.
  """
  @spec lookup_on_node(atom(), node()) :: {:ok, pid()} | {:error, :not_found}
  def lookup_on_node(name, target_node) when is_atom(name) and is_atom(target_node) do
    case Horde.Registry.lookup(@registry_name, name) do
      [{pid, _metadata}] ->
        if node(pid) == target_node do
          {:ok, pid}
        else
          {:error, :not_found}
        end

      [] ->
        {:error, :not_found}
    end
  end

  @impl true
  def list do
    Horde.Registry.select(@registry_name, [
      {{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}
    ])
  end

  defp ensure_horde! do
    unless Code.ensure_loaded?(Horde.Registry) do
      raise """
      Horde is required for ExMCP.ServiceRegistry.Horde but is not installed.

      Add it to your dependencies in mix.exs:

          defp deps do
            [
              {:horde, "~> 0.9"}
            ]
          end

      Or switch to the local registry (no extra deps needed):

          config :ex_mcp, :service_registry, ExMCP.ServiceRegistry.Local
      """
    end
  end
end
