defmodule ExMCP.ServiceRegistry do
  @moduledoc """
  Pluggable service registry behaviour for ExMCP.Native service discovery.

  By default, uses `ExMCP.ServiceRegistry.Local` which wraps Elixir's built-in
  `Registry` module — zero external dependencies required.

  For distributed clusters, configure the Horde adapter:

      # config/config.exs
      config :ex_mcp, :service_registry, ExMCP.ServiceRegistry.Horde

  ## Implementing a Custom Adapter

  Implement all callbacks defined in this module:

      defmodule MyApp.CustomRegistry do
        @behaviour ExMCP.ServiceRegistry

        @impl true
        def child_specs(_opts), do: [...]

        @impl true
        def register(name, metadata), do: ...

        # ... etc
      end
  """

  @doc """
  Returns child specs for the registry processes to be started under the supervision tree.
  """
  @callback child_specs(opts :: keyword()) :: [Supervisor.child_spec() | {module(), term()}]

  @doc """
  Registers the calling process under `name` with the given metadata.
  """
  @callback register(name :: atom(), metadata :: map()) :: :ok | {:error, term()}

  @doc """
  Unregisters the calling process from `name`.
  """
  @callback unregister(name :: atom()) :: :ok

  @doc """
  Looks up the process registered under `name`.
  """
  @callback lookup(name :: atom()) :: {:ok, pid()} | {:error, :not_found}

  @doc """
  Lists all registered services as `{name, pid, metadata}` tuples.
  """
  @callback list() :: [{atom(), pid(), map()}]

  @doc """
  Returns the configured service registry adapter.

  Defaults to `ExMCP.ServiceRegistry.Local`.
  """
  @spec adapter() :: module()
  def adapter do
    Application.get_env(:ex_mcp, :service_registry, ExMCP.ServiceRegistry.Local)
  end
end
