defmodule ExMCP.HttpPlug.Core do
  @moduledoc """
  Pure request/response decisions for `ExMCP.HttpPlug`.

  The Plug module owns side effects such as reading request bodies, writing
  responses, ETS/session management, and SSE processes. This module keeps the
  reusable protocol and origin decisions as data transformations.
  """

  alias ExMCP.Protocol.ResponseBuilder

  @type origin_context :: %{
          optional(:origin) => String.t() | nil,
          optional(:scheme) => String.t(),
          optional(:host) => String.t(),
          optional(:port) => non_neg_integer() | nil
        }

  @spec parse_json(binary()) :: {:ok, map()} | {:error, :parse_error | :invalid_json_rpc_envelope}
  def parse_json(body) do
    case Jason.decode(body) do
      {:ok, json} when is_map(json) -> {:ok, json}
      {:ok, _json} -> {:error, :invalid_json_rpc_envelope}
      {:error, _} -> {:error, :parse_error}
    end
  end

  @spec origin_allowed?(origin_context(), map()) :: boolean()
  def origin_allowed?(context, opts) do
    case Map.get(context, :origin) do
      nil ->
        true

      "" ->
        true

      origin ->
        explicit_origin_allowed?(origin, Map.get(opts, :allowed_origins)) or
          same_origin?(context, origin)
    end
  end

  @spec cors_response_origin(origin_context(), map()) :: String.t() | nil
  def cors_response_origin(context, %{allowed_origins: :any}) do
    Map.get(context, :origin) || "*"
  end

  def cors_response_origin(context, opts) do
    origin = Map.get(context, :origin)

    cond do
      is_nil(origin) -> nil
      origin_allowed?(context, opts) -> origin
      true -> nil
    end
  end

  @spec json_rpc_error(integer(), String.t(), any(), map() | nil) :: map()
  def json_rpc_error(code, message, id \\ nil, data \\ nil) do
    ResponseBuilder.build_error_response(code, message, data, id)
  end

  @spec oauth_guard_disabled_error() :: map()
  def oauth_guard_disabled_error do
    %{
      error: "server_error",
      error_description: "OAuth is enabled for this plug, but OAuth authorization is disabled"
    }
  end

  defp explicit_origin_allowed?(_origin, :any), do: true
  defp explicit_origin_allowed?(origin, origins) when is_list(origins), do: origin in origins
  defp explicit_origin_allowed?(_origin, _origins), do: false

  defp same_origin?(context, origin) do
    case URI.parse(origin) do
      %URI{scheme: scheme, host: host, port: port} when is_binary(scheme) and is_binary(host) ->
        scheme == Map.get(context, :scheme) and host == Map.get(context, :host) and
          port == Map.get(context, :port)

      _ ->
        false
    end
  end
end
