defmodule ExMCP.Tasks.StoreCall do
  @moduledoc false

  # Store-invocation primitive shared by `ExMCP.Tasks` and
  # `ExMCP.Server.Subscriptions`. It sits below both so subscription
  # authorization can consult the configured task store without depending on
  # the `ExMCP.Tasks` facade, which itself publishes through Subscriptions.

  alias ExMCP.Tasks.Store

  @spec call(atom(), [term()], keyword()) :: term() | {:error, :task_store_unavailable}
  def call(function, args, opts) do
    store = Keyword.get(opts, :store, Application.get_env(:ex_mcp, :task_store, Store.ETS))

    store_opts =
      Keyword.drop(opts, [
        :store,
        :owner,
        :principal_id,
        :tenant_id,
        :audience,
        :subscription_registry,
        :notify,
        :transport_ref
      ])

    apply(store, function, args ++ [store_opts])
  rescue
    _error -> {:error, :task_store_unavailable}
  catch
    :exit, _reason -> {:error, :task_store_unavailable}
  end
end
