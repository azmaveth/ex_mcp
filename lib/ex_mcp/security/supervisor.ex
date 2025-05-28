defmodule ExMCP.Security.Supervisor do
  @moduledoc """
  Supervisor for MCP security components.

  Manages the lifecycle of security services required for
  MCP security best practices compliance.
  """

  use Supervisor

  @doc """
  Starts the security supervisor.

  Options:
  - `:approval_handler` - Module implementing ExMCP.Approval behaviour
  - `:consent_manager` - Consent manager configuration
  - `:client_registry` - Client registry configuration
  """
  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    approval_handler = Keyword.get(opts, :approval_handler)

    children = [
      # Client registry for accountability
      {ExMCP.Security.ClientRegistry, Keyword.get(opts, :client_registry, [])},

      # Consent manager for dynamic client authorization
      {ExMCP.Security.ConsentManager,
       [approval_handler: approval_handler] ++
         Keyword.get(opts, :consent_manager, [])}
    ]

    Supervisor.init(children, strategy: :rest_for_one)
  end

  @doc """
  Ensures security supervisor is started.

  This is called by ExMCP.Application when security features are enabled.
  """
  def ensure_started(opts \\ []) do
    case Process.whereis(__MODULE__) do
      nil ->
        # Start under the main application supervisor
        DynamicSupervisor.start_child(
          ExMCP.DynamicSupervisor,
          {__MODULE__, opts}
        )

      pid ->
        {:ok, pid}
    end
  end
end
