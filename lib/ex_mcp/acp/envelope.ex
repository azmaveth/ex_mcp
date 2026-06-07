defmodule ExMCP.ACP.Envelope do
  @moduledoc """
  Pure JSON-RPC 2.0 envelope builders for ACP messages.

  Method-specific modules should own their payload shapes. This module owns
  only the repeated JSON-RPC frame so request builders can stay pipe-friendly:

      Envelope.request("session/new")
      |> Envelope.with_params(params)
      |> Envelope.with_id(id)
  """

  alias ExMCP.ACP.Maps

  @jsonrpc "2.0"

  @spec request(String.t()) :: map()
  def request(method) when is_binary(method) do
    %{"jsonrpc" => @jsonrpc, "method" => method}
  end

  @spec request(String.t(), map()) :: map()
  def request(method, params) when is_map(params) do
    method
    |> request()
    |> with_params(params)
  end

  @spec request(String.t(), map(), integer() | String.t()) :: map()
  def request(method, params, id) when is_map(params) do
    method
    |> request(params)
    |> with_id(id)
  end

  @spec notification(String.t(), map()) :: map()
  def notification(method, params \\ %{}) when is_binary(method) and is_map(params) do
    request(method, params)
  end

  @spec response(integer() | String.t(), any()) :: map()
  def response(id, result) do
    %{"jsonrpc" => @jsonrpc, "result" => result, "id" => id}
  end

  @spec error(integer() | String.t() | nil, map()) :: map()
  def error(id, %{} = error) do
    %{"jsonrpc" => @jsonrpc, "error" => error, "id" => id}
  end

  @spec error(integer() | String.t() | nil, integer(), String.t(), any()) :: map()
  def error(id, code, message, data \\ nil) when is_integer(code) and is_binary(message) do
    error =
      %{"code" => code, "message" => message}
      |> Maps.put_present("data", data)

    error(id, error)
  end

  @spec with_params(map(), map()) :: map()
  def with_params(envelope, params) when is_map(envelope) and is_map(params) do
    Map.put(envelope, "params", params)
  end

  @spec with_id(map(), integer() | String.t() | nil) :: map()
  def with_id(envelope, id) when is_map(envelope) do
    Map.put(envelope, "id", id)
  end
end
