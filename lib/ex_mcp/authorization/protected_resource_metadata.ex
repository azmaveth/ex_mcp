defmodule ExMCP.Authorization.ProtectedResourceMetadata do
  @moduledoc """
  OAuth 2.0 Protected Resource Metadata Discovery (RFC 9728 - Draft).

  This module implements the discovery mechanism for protected resources to
  advertise their authorization server relationships. This allows MCP servers
  to indicate which authorization servers protect their resources.

  ## Example

      # Discover authorization servers for a protected resource
      {:ok, metadata} = ProtectedResourceMetadata.discover("https://api.example.com/mcp")

      # Use discovered authorization server
      [auth_server | _] = metadata.authorization_servers
      {:ok, auth_metadata} = Authorization.discover_server_metadata(auth_server.issuer)
  """

  @type authorization_server :: %{
          issuer: String.t(),
          metadata_endpoint: String.t() | nil,
          scopes_supported: [String.t()] | nil,
          audience: String.t() | [String.t()] | nil
        }

  @type metadata :: %{
          authorization_servers: [authorization_server()]
        }

  @type www_authenticate_info :: %{
          realm: String.t() | nil,
          as_uri: String.t() | nil,
          resource_uri: String.t() | nil,
          error: String.t() | nil,
          error_description: String.t() | nil
        }

  @doc """
  Discovers protected resource metadata from the resource URL.

  Makes a request to /.well-known/oauth-protected-resource to discover
  which authorization servers protect this resource.
  """
  @spec discover(String.t()) :: {:ok, metadata()} | {:error, term()}
  def discover(resource_url) do
    with :ok <- validate_https_endpoint(resource_url),
         metadata_url <- build_metadata_url(resource_url) do
      case make_http_request(:get, metadata_url, [], "") do
        {:ok, {{_, 200, _}, _headers, body}} ->
          parse_metadata_response(body)

        {:ok, {{_, 404, _}, _headers, _body}} ->
          {:error, :no_metadata}

        {:ok, {{_, 401, _}, headers, _body}} ->
          # Check for WWW-Authenticate header
          case find_www_authenticate_header(headers) do
            {:ok, _auth_info} ->
              # Could extract metadata URL from header
              {:error, :unauthorized}

            :error ->
              {:error, :unauthorized}
          end

        {:ok, {{_, status, _}, _headers, body}} ->
          {:error, {:http_error, status, body}}

        {:error, reason} ->
          {:error, {:request_failed, reason}}
      end
    end
  end

  @doc """
  Parses WWW-Authenticate header for authorization information.

  Extracts Bearer authentication parameters including realm, as_uri,
  resource_uri, and error information.
  """
  @spec parse_www_authenticate(String.t()) :: {:ok, www_authenticate_info()} | {:error, term()}
  def parse_www_authenticate(header) do
    cond do
      not is_binary(header) or header == "" ->
        {:error, :invalid_header}

      String.starts_with?(header, "Bearer ") ->
        case parse_bearer_params(header) do
          %{} = params -> {:ok, params}
          :error -> {:error, :invalid_bearer_params}
        end

      true ->
        {:error, :not_bearer}
    end
  end

  # Private functions

  defp validate_https_endpoint(url) do
    case URI.parse(url) do
      %URI{scheme: "https"} -> :ok
      %URI{scheme: "http", host: host} when host in ["localhost", "127.0.0.1"] -> :ok
      _ -> {:error, :https_required}
    end
  end

  defp build_metadata_url(resource_url) do
    uri = URI.parse(resource_url)

    # Build base URL (scheme + host + port) - construct manually to avoid URI type issues
    scheme = uri.scheme || "https"
    host = uri.host || "localhost"
    port = uri.port

    base_url =
      if port && port != URI.default_port(scheme) do
        "#{scheme}://#{host}:#{port}"
      else
        "#{scheme}://#{host}"
      end

    base_url <> "/.well-known/oauth-protected-resource"
  end

  defp make_http_request(:get, url, headers, _body) do
    # Convert headers to charlist format for httpc
    httpc_headers =
      Enum.map(headers, fn {k, v} ->
        {String.to_charlist(k), String.to_charlist(v)}
      end)

    request = {String.to_charlist(url), httpc_headers}

    # SSL options for HTTPS
    ssl_opts = [
      ssl: [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        versions: [:"tlsv1.2", :"tlsv1.3"]
      ]
    ]

    :httpc.request(:get, request, ssl_opts, [])
  end

  defp parse_metadata_response(body) do
    case Jason.decode(body) do
      {:ok, %{"authorization_servers" => servers}} when is_list(servers) ->
        parsed_servers = Enum.map(servers, &parse_authorization_server/1)
        {:ok, %{authorization_servers: parsed_servers}}

      {:ok, _} ->
        {:error, {:invalid_metadata, "Missing authorization_servers"}}

      {:error, reason} ->
        {:error, {:json_decode_error, reason}}
    end
  end

  defp parse_authorization_server(server) do
    %{
      issuer: Map.fetch!(server, "issuer"),
      metadata_endpoint: Map.get(server, "metadata_endpoint"),
      scopes_supported: Map.get(server, "scopes_supported"),
      audience: Map.get(server, "audience")
    }
  end

  defp find_www_authenticate_header(headers) do
    case List.keyfind(headers, ~c"www-authenticate", 0) do
      {_, value} ->
        {:ok, List.to_string(value)}

      nil ->
        :error
    end
  end

  defp parse_bearer_params(header) do
    # Remove "Bearer " prefix
    params_string = String.replace_prefix(header, "Bearer ", "")

    # Return error for empty or malformed
    if params_string == "" or params_string == "Bearer" do
      :error
    else
      # Parse comma-separated key=value pairs
      params =
        params_string
        |> String.split(",")
        |> Enum.map(&String.trim/1)
        |> Enum.reduce(%{}, fn param, acc ->
          case String.split(param, "=", parts: 2) do
            [key, value] when key != "" and value != "" ->
              # Check for properly paired quotes
              if String.starts_with?(value, "\"") and not String.ends_with?(value, "\"") do
                # Unclosed quote - invalid
                Map.put(acc, :_invalid, true)
              else
                # Remove quotes if present
                clean_value = String.trim(value, "\"")
                Map.put(acc, key, clean_value)
              end

            _ ->
              acc
          end
        end)

      # Return parsed params or error if no valid params found
      if map_size(params) == 0 or Map.has_key?(params, :_invalid) do
        :error
      else
        %{
          realm: Map.get(params, :realm),
          as_uri: Map.get(params, :as_uri),
          resource_uri: Map.get(params, :resource_uri),
          error: Map.get(params, :error),
          error_description: Map.get(params, :error_description)
        }
      end
    end
  end
end
