defmodule ExMCP.ServiceRegistry.Local do
  @moduledoc """
  Local service registry adapter using Elixir's built-in `Registry`.

  This is the default adapter — no external dependencies required.
  Suitable for single-node deployments where all MCP services run
  within the same BEAM instance.

  For distributed clusters spanning multiple nodes, use
  `ExMCP.ServiceRegistry.Horde` instead.
  """

  @behaviour ExMCP.ServiceRegistry

  @registry_name __MODULE__.Registry

  @impl true
  def child_specs(_opts) do
    [
      {Registry, keys: :unique, name: @registry_name}
    ]
  end

  @impl true
  def register(name, metadata) when is_atom(name) do
    case Registry.register(@registry_name, name, metadata) do
      {:ok, _pid} ->
        :ok

      {:error, {:already_registered, _pid}} ->
        :ok
    end
  end

  @impl true
  def unregister(name) when is_atom(name) do
    Registry.unregister(@registry_name, name)
    :ok
  end

  @impl true
  def lookup(name) when is_atom(name) do
    case Registry.lookup(@registry_name, name) do
      [{pid, _metadata}] -> {:ok, pid}
      [] -> {:error, :not_found}
    end
  end

  @impl true
  def list do
    Registry.select(@registry_name, [{{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}])
  end
end
