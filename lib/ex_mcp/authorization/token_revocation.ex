defmodule ExMCP.Authorization.TokenRevocation do
  @moduledoc """
  Client-side OAuth 2.0 Token Revocation (RFC 7009).

  Provides functionality for clients to revoke access or refresh tokens
  at an authorization server's revocation endpoint.

  ## Usage

      # Revoke an access token
      {:ok, :revoked} = TokenRevocation.revoke(
        "my_access_token",
        "https://auth.example.com/revoke",
        token_type_hint: "access_token",
        client_id: "my_client",
        client_secret: "my_secret"
      )

      # Revoke a refresh token
      {:ok, :revoked} = TokenRevocation.revoke(
        "my_refresh_token",
        "https://auth.example.com/revoke",
        token_type_hint: "refresh_token"
      )
  """

  require Logger

  @type revocation_opts :: [
          token_type_hint: String.t(),
          client_id: String.t(),
          client_secret: String.t(),
          auth_method: :client_secret_post | :client_secret_basic
        ]

  @doc """
  Revokes a token at the given revocation endpoint.

  Per RFC 7009, the server responds with 200 OK regardless of whether the token
  was valid or already revoked. A non-200 response indicates an error.

  ## Options

  - `:token_type_hint` - Either `"access_token"` or `"refresh_token"`.
    Helps the server optimize its lookup.
  - `:client_id` - The client identifier for authentication.
  - `:client_secret` - The client secret for authentication.
  - `:auth_method` - Authentication method. Defaults to `:client_secret_post`.
    Can be `:client_secret_basic` for HTTP Basic auth.
  """
  @spec revoke(String.t(), String.t(), revocation_opts()) ::
          {:ok, :revoked} | {:error, term()}
  def revoke(token, revocation_endpoint, opts \\ []) do
    with :ok <- validate_endpoint(revocation_endpoint) do
      body = build_revocation_body(token, opts)
      headers = build_headers(opts)

      case make_revocation_request(revocation_endpoint, headers, body) do
        {:ok, {{_, status, _}, _headers, _body}} when status in [200, 204] ->
          {:ok, :revoked}

        {:ok, {{_, status, _}, _headers, resp_body}} ->
          case Jason.decode(resp_body) do
            {:ok, %{"error" => error} = error_data} ->
              {:error,
               {:revocation_error, status, error, Map.get(error_data, "error_description")}}

            _ ->
              {:error, {:http_error, status, resp_body}}
          end

        {:error, reason} ->
          {:error, {:request_failed, reason}}
      end
    end
  end

  defp validate_endpoint(url) do
    case URI.parse(url) do
      %URI{scheme: "https"} -> :ok
      %URI{scheme: "http", host: host} when host in ["localhost", "127.0.0.1"] -> :ok
      _ -> {:error, :https_required}
    end
  end

  defp build_revocation_body(token, opts) do
    body = [{"token", token}]

    body =
      case Keyword.get(opts, :token_type_hint) do
        nil -> body
        hint -> [{"token_type_hint", hint} | body]
      end

    auth_method = Keyword.get(opts, :auth_method, :client_secret_post)

    if auth_method == :client_secret_post do
      body
      |> maybe_add_param("client_id", Keyword.get(opts, :client_id))
      |> maybe_add_param("client_secret", Keyword.get(opts, :client_secret))
    else
      body
      |> maybe_add_param("client_id", Keyword.get(opts, :client_id))
    end
  end

  defp build_headers(opts) do
    base_headers = [
      {"content-type", "application/x-www-form-urlencoded"},
      {"accept", "application/json"}
    ]

    auth_method = Keyword.get(opts, :auth_method, :client_secret_post)

    if auth_method == :client_secret_basic do
      client_id = Keyword.get(opts, :client_id, "")
      client_secret = Keyword.get(opts, :client_secret, "")
      credentials = Base.encode64("#{client_id}:#{client_secret}")
      [{"authorization", "Basic #{credentials}"} | base_headers]
    else
      base_headers
    end
  end

  defp maybe_add_param(body, _key, nil), do: body
  defp maybe_add_param(body, key, value), do: [{key, value} | body]

  defp make_revocation_request(endpoint, headers, body) do
    httpc_headers =
      Enum.map(headers, fn {k, v} ->
        {String.to_charlist(k), String.to_charlist(v)}
      end)

    encoded_body = URI.encode_query(body)

    request =
      {String.to_charlist(endpoint), httpc_headers, ~c"application/x-www-form-urlencoded",
       String.to_charlist(encoded_body)}

    ssl_opts = [
      ssl: [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        versions: [:"tlsv1.2", :"tlsv1.3"]
      ]
    ]

    :httpc.request(:post, request, ssl_opts, [])
  end
end
