defmodule ExMCP.Security.TokenHandler do
  @moduledoc """
  Handles token processing and sanitization of requests.

  This module is responsible for tasks like checking for token passthrough,
  classifying URLs, and stripping sensitive headers from requests.
  """
  alias URI

  @sensitive_headers [
    "authorization",
    "cookie",
    "x-api-key",
    "x-auth-token",
    "x-csrf-token"
  ]

  @doc """
  Checks for and prevents token passthrough to external resources.

  It classifies the URL, and if it's external, it strips sensitive headers.
  This is a key part of preventing confused deputy attacks.
  """
  @spec check_token_passthrough(String.t(), list({String.t(), String.t()}), map()) ::
          {:ok, list({String.t(), String.t()})}
  def check_token_passthrough(url, headers, config) do
    trusted_origins = Map.get(config, :trusted_origins, [])
    classification = classify_url(url, trusted_origins)
    new_headers = strip_sensitive_headers(headers, classification)
    {:ok, new_headers}
  end

  @doc """
  Classifies a URL as `:internal` or `:external` based on trusted origins.

  Trusted origins are hosts that are considered part of the same security
  domain. Wildcard matching (`*.example.com`) is supported for subdomains.
  """
  @spec classify_url(String.t(), [String.t()]) :: :internal | :external
  def classify_url(url, trusted_origins) do
    with %URI{host: host} <- URI.parse(url),
         true <- not is_nil(host) do
      normalized_host = String.downcase(host)

      is_trusted =
        Enum.any?(trusted_origins, fn trusted ->
          # Handle full URLs or just hosts
          trusted_host =
            case URI.parse(trusted) do
              %URI{host: h} when not is_nil(h) -> String.downcase(h)
              _ -> String.downcase(trusted)
            end

          if String.starts_with?(trusted_host, "*.") do
            String.ends_with?(normalized_host, String.trim_leading(trusted_host, "*"))
          else
            normalized_host == trusted_host
          end
        end)

      if is_trusted, do: :internal, else: :external
    else
      _error ->
        # If URL can't be parsed, treat it as external for safety.
        :external
    end
  end

  @doc """
  Strips sensitive headers if the resource classification is `:external`.
  """
  @spec strip_sensitive_headers(list({String.t(), String.t()}), :internal | :external) ::
          list({String.t(), String.t()})
  def strip_sensitive_headers(headers, :internal) do
    headers
  end

  def strip_sensitive_headers(headers, :external) do
    Enum.reject(headers, fn {name, _value} ->
      String.downcase(name) in @sensitive_headers
    end)
  end

  @doc """
  Extracts the origin (scheme://host:port) from a URL string.
  """
  @spec extract_origin(String.t()) :: {:ok, String.t()} | {:error, :invalid_uri}
  def extract_origin(url) do
    case URI.parse(url) do
      %URI{scheme: nil} ->
        {:error, :invalid_uri}

      %URI{host: nil} ->
        {:error, :invalid_uri}

      %URI{scheme: scheme, host: host, port: port} = _uri ->
        default_port = URI.default_port(scheme)
        port_str = if port && port != default_port, do: ":#{port}", else: ""
        {:ok, "#{scheme}://#{host}#{port_str}"}
    end
  end
end
