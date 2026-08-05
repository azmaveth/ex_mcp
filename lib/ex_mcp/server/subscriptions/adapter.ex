defmodule ExMCP.Server.Subscriptions.Adapter do
  @moduledoc """
  Storage contract for modern MCP subscription registrations.

  The bundled adapter is node-local. Clustered HTTP deployments can provide
  an adapter backed by a shared registry and PubSub implementation.
  """

  alias ExMCP.Server.Subscriptions.Entry

  @type adapter_state :: term()

  @callback init(keyword()) :: {:ok, adapter_state()} | {:error, term()}
  @callback put(Entry.t(), adapter_state()) ::
              {:ok, adapter_state()} | {:error, term(), adapter_state()}
  @callback delete(String.t(), adapter_state()) :: {:ok, adapter_state()}
  @callback all(adapter_state()) :: {[Entry.t()], adapter_state()}
end
