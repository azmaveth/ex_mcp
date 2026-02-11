defmodule ExMCP.Authorization.ClientIdMetadata do
  @moduledoc """
  OAuth Client ID Metadata Document support for MCP authorization.

  Implements the OAuth Client ID Metadata Document mechanism where the
  client's `client_id` is a URL that resolves to a JSON document
  containing the client's OAuth metadata.

  This enables dynamic client registration-like behavior without
  requiring a registration endpoint.

  Available in protocol version 2025-11-25.
  """

  @type client_metadata :: %{String.t() => term()}

  @doc """
  Fetches and parses client metadata from a client_id URL.

  ## Parameters
  - `client_id_url` - The client_id URL to fetch metadata from
  - `opts` - Options including `:http_client` for custom HTTP client

  ## Returns
  - `{:ok, metadata}` - Successfully fetched client metadata
  - `{:error, reason}` - Failed to fetch or parse metadata
  """
  @spec fetch(String.t(), keyword()) :: {:ok, client_metadata()} | {:error, term()}
  def fetch(client_id_url, opts \\ []) do
    http_client = Keyword.get(opts, :http_client)

    case do_fetch(client_id_url, http_client) do
      {:ok, metadata} ->
        case validate(metadata, client_id_url) do
          :ok -> {:ok, metadata}
          {:error, _} = error -> error
        end

      {:error, _} = error ->
        error
    end
  end

  @doc """
  Validates client metadata structure.

  ## Required Fields
  - `client_id` - Must match the URL it was fetched from
  - `client_name` - Human-readable name for the client
  - `redirect_uris` - List of allowed redirect URIs

  ## Optional Fields
  - `client_uri` - URL of the client's home page
  - `logo_uri` - URL of the client's logo
  - `scope` - Space-separated list of requested scopes
  - `contacts` - List of contact emails
  - `tos_uri` - Terms of service URL
  - `policy_uri` - Privacy policy URL
  """
  @spec validate(client_metadata(), String.t()) :: :ok | {:error, term()}
  def validate(metadata, expected_client_id) do
    with :ok <- validate_client_id(metadata, expected_client_id) do
      validate_required_fields(metadata)
    end
  end

  @doc """
  Builds a client metadata document for this application.

  Useful for MCP clients that want to publish their own metadata.
  """
  @spec build_metadata(keyword()) :: client_metadata()
  def build_metadata(opts \\ []) do
    %{
      "client_id" => Keyword.fetch!(opts, :client_id),
      "client_name" => Keyword.fetch!(opts, :client_name),
      "redirect_uris" => Keyword.fetch!(opts, :redirect_uris)
    }
    |> maybe_put("client_uri", Keyword.get(opts, :client_uri))
    |> maybe_put("logo_uri", Keyword.get(opts, :logo_uri))
    |> maybe_put("scope", Keyword.get(opts, :scope))
    |> maybe_put("contacts", Keyword.get(opts, :contacts))
    |> maybe_put("tos_uri", Keyword.get(opts, :tos_uri))
    |> maybe_put("policy_uri", Keyword.get(opts, :policy_uri))
  end

  # Private helpers

  defp do_fetch(_url, nil), do: {:error, :no_http_client}

  defp do_fetch(url, http_client) do
    case http_client.get(url, [{"accept", "application/json"}]) do
      {:ok, %{status: 200, body: body}} when is_binary(body) ->
        case Jason.decode(body) do
          {:ok, metadata} when is_map(metadata) -> {:ok, metadata}
          _ -> {:error, :invalid_json}
        end

      {:ok, %{status: status}} ->
        {:error, {:http_error, status}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp validate_client_id(metadata, expected_client_id) do
    case Map.get(metadata, "client_id") do
      ^expected_client_id -> :ok
      nil -> {:error, :missing_client_id}
      actual -> {:error, {:client_id_mismatch, expected: expected_client_id, actual: actual}}
    end
  end

  defp validate_required_fields(metadata) do
    required = ["client_id", "client_name", "redirect_uris"]
    missing = Enum.reject(required, &Map.has_key?(metadata, &1))

    case missing do
      [] -> :ok
      [field | _] -> {:error, {:missing_required_field, field}}
    end
  end

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)
end
