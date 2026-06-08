defmodule ExMCP.ACP.Envelope do
  @moduledoc """
  Pure JSON-RPC 2.0 envelope builders for ACP messages.

  Method-specific modules should own their payload shapes. This module owns
  only the repeated JSON-RPC frame so request builders can stay pipe-friendly:

      Envelope.request("session/new")
      |> Envelope.with_params(params)
      |> Envelope.with_id(id)
  """

  alias ExMCP.Internal.JSONRPC

  @doc "Builds a JSON-RPC request envelope without params."
  @spec request(String.t()) :: map()
  defdelegate request(method), to: JSONRPC

  @doc "Builds a JSON-RPC request envelope with params."
  @spec request(String.t(), map()) :: map()
  defdelegate request(method, params), to: JSONRPC

  @doc "Builds a JSON-RPC request envelope with params and id."
  @spec request(String.t(), map(), integer() | String.t()) :: map()
  defdelegate request(method, params, id), to: JSONRPC

  @doc "Builds a JSON-RPC notification envelope."
  @spec notification(String.t(), map()) :: map()
  defdelegate notification(method, params \\ %{}), to: JSONRPC

  @doc "Builds a JSON-RPC success response envelope."
  @spec response(integer() | String.t(), any()) :: map()
  defdelegate response(id, result), to: JSONRPC

  @doc "Builds a JSON-RPC error response from an error map."
  @spec error(integer() | String.t() | nil, map()) :: map()
  defdelegate error(id, error), to: JSONRPC

  @doc "Builds a JSON-RPC error response from code, message, and optional data."
  @spec error(integer() | String.t() | nil, integer(), String.t(), any()) :: map()
  defdelegate error(id, code, message, data \\ nil), to: JSONRPC

  @doc "Adds params to an existing JSON-RPC envelope."
  @spec with_params(map(), map()) :: map()
  defdelegate with_params(envelope, params), to: JSONRPC

  @doc "Adds an id to an existing JSON-RPC envelope."
  @spec with_id(map(), integer() | String.t() | nil) :: map()
  defdelegate with_id(envelope, id), to: JSONRPC
end
