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

  alias ExMCP.Authorization.MetadataFetcher
  alias ExMCP.Internal.Headers

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
  which authorization servers protect this resource. The request uses the
  shared HTTPS-only, public-address, pinned metadata fetch boundary. Test and
  local-development callers may explicitly enable the loopback-only HTTP
  exception supported by `ExMCP.Authorization.MetadataFetcher`.
  """
  @spec discover(String.t(), keyword()) :: {:ok, metadata()} | {:error, term()}
  def discover(resource_url, opts \\ []) do
    with :ok <- validate_endpoint(resource_url, opts),
         metadata_urls <- build_metadata_urls(resource_url) do
      fetch_metadata(metadata_urls, opts)
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

  defp validate_endpoint(url, opts) do
    case MetadataFetcher.validate_url(url, opts) do
      :ok -> :ok
      {:error, {:metadata_fetch_error, :https_required}} -> {:error, :https_required}
      {:error, _reason} -> {:error, :invalid_resource_url}
    end
  end

  defp build_metadata_urls(resource_url) do
    uri = URI.parse(resource_url)
    resource_path = uri.path || ""

    root_url =
      %URI{uri | path: "/.well-known/oauth-protected-resource", query: nil, fragment: nil}
      |> URI.to_string()

    case String.trim_trailing(resource_path, "/") do
      "" ->
        [root_url]

      path ->
        path_url =
          %URI{
            uri
            | path: "/.well-known/oauth-protected-resource#{path}",
              query: nil,
              fragment: nil
          }
          |> URI.to_string()

        [path_url, root_url]
    end
  end

  defp fetch_metadata([metadata_url | fallback_urls], opts) do
    case MetadataFetcher.fetch(metadata_url, opts) do
      {:ok, %{status: 200, body: body}} ->
        parse_metadata_response(body)

      {:ok, %{status: 404}} when fallback_urls != [] ->
        fetch_metadata(fallback_urls, opts)

      {:ok, %{status: 404}} ->
        {:error, :no_metadata}

      {:ok, %{status: 401, headers: headers}} ->
        # Check for WWW-Authenticate header
        case find_www_authenticate_header(headers) do
          {:ok, _auth_info} ->
            # Could extract metadata URL from header
            {:error, :unauthorized}

          :error ->
            {:error, :unauthorized}
        end

      {:ok, %{status: status, body: body}} ->
        {:error, {:http_error, status, body}}

      {:error, _reason} = error ->
        error
    end
  end

  defp fetch_metadata([], _opts), do: {:error, :no_metadata}

  defp parse_metadata_response(body) do
    case Jason.decode(body) do
      {:ok, %{"authorization_servers" => servers}} when is_list(servers) ->
        parse_authorization_servers(servers)

      {:ok, _} ->
        {:error, {:invalid_metadata, "Missing authorization_servers"}}

      {:error, reason} ->
        {:error, {:json_decode_error, reason}}
    end
  end

  defp parse_authorization_servers(servers) do
    servers
    |> Enum.reduce_while({:ok, []}, fn server, {:ok, acc} ->
      case parse_authorization_server(server) do
        {:ok, parsed} -> {:cont, {:ok, [parsed | acc]}}
        {:error, _reason} = error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, parsed} -> {:ok, %{authorization_servers: Enum.reverse(parsed)}}
      {:error, _reason} = error -> error
    end
  end

  defp parse_authorization_server(issuer) when is_binary(issuer) and issuer != "" do
    {:ok, %{issuer: issuer, metadata_endpoint: nil, scopes_supported: nil, audience: nil}}
  end

  defp parse_authorization_server(%{"issuer" => issuer} = server)
       when is_binary(issuer) and issuer != "" do
    {:ok,
     %{
       issuer: issuer,
       metadata_endpoint: Map.get(server, "metadata_endpoint"),
       scopes_supported: Map.get(server, "scopes_supported"),
       audience: Map.get(server, "audience")
     }}
  end

  defp parse_authorization_server(_server),
    do: {:error, {:invalid_metadata, "Invalid authorization server"}}

  defp find_www_authenticate_header(headers) do
    case Headers.get(headers, "www-authenticate") do
      value when is_binary(value) ->
        {:ok, value}

      nil ->
        :error

      _ ->
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
