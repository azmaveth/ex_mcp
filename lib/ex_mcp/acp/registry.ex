defmodule ExMCP.ACP.Registry do
  @moduledoc """
  Helpers for the public ACP agent registry.

  The registry is distributed as JSON and lists ACP-compatible agents plus
  their distribution metadata. These helpers keep ExMCP clients from having to
  hard-code the CDN URL or common lookup details.
  """

  @default_url "https://cdn.agentclientprotocol.com/registry/v1/latest/registry.json"

  @type agent :: map()
  @type registry :: map()

  @doc "Returns the public ACP registry URL."
  @spec default_url() :: String.t()
  def default_url, do: @default_url

  @doc """
  Fetches and decodes the ACP registry.

  Options:

  - `:url` - registry URL, defaults to the public latest registry.
  - `:timeout` - request timeout in milliseconds, defaults to 15 seconds.
  - `:headers` - additional HTTP request headers.
  - `:http_client` - test hook taking `(url, headers, timeout)` and returning
    `{:ok, body}` or `{:error, reason}`.
  """
  @spec fetch(keyword()) :: {:ok, registry()} | {:error, any()}
  def fetch(opts \\ []) do
    url = Keyword.get(opts, :url, @default_url)
    timeout = Keyword.get(opts, :timeout, 15_000)
    headers = Keyword.get(opts, :headers, [])
    http_client = Keyword.get(opts, :http_client, &http_get/3)

    with {:ok, body} <- http_client.(url, headers, timeout) do
      parse(body)
    end
  end

  @doc "Decodes registry JSON."
  @spec parse(iodata()) :: {:ok, registry()} | {:error, Jason.DecodeError.t()}
  def parse(json) do
    json
    |> IO.iodata_to_binary()
    |> Jason.decode()
  end

  @doc "Returns all agent entries in a decoded registry map."
  @spec agents(registry()) :: [agent()]
  def agents(%{"agents" => agents}) when is_list(agents), do: agents
  def agents(_registry), do: []

  @doc "Finds an agent by exact id or name, falling back to case-insensitive lookup."
  @spec get_agent(registry(), String.t()) :: agent() | nil
  def get_agent(registry, id_or_name) when is_binary(id_or_name) do
    agents = agents(registry)

    Enum.find(agents, &(&1["id"] == id_or_name or &1["name"] == id_or_name)) ||
      Enum.find(agents, fn agent ->
        String.downcase(agent["id"] || "") == String.downcase(id_or_name) or
          String.downcase(agent["name"] || "") == String.downcase(id_or_name)
      end)
  end

  @doc "Searches agent id, name, and description fields case-insensitively."
  @spec find_agents(registry(), String.t()) :: [agent()]
  def find_agents(registry, query) when is_binary(query) do
    query = String.downcase(query)

    Enum.filter(agents(registry), fn agent ->
      ["id", "name", "description"]
      |> Enum.map(&(agent[&1] || ""))
      |> Enum.any?(&String.contains?(String.downcase(&1), query))
    end)
  end

  @doc ~S(Returns a distribution entry such as `"npx"` or `"binary"`.)
  @spec distribution(agent(), String.t() | atom()) :: map() | nil
  def distribution(agent, kind) do
    get_in(agent, ["distribution", to_string(kind)])
  end

  @doc """
  Builds an `npx` command from an agent registry entry.

  Returns `{:error, :npx_distribution_not_found}` when the agent does not have
  an `npx` distribution.
  """
  @spec npx_command(agent()) :: {:ok, [String.t()]} | {:error, :npx_distribution_not_found}
  def npx_command(agent) do
    case distribution(agent, "npx") do
      %{"package" => package} = npx ->
        {:ok, ["npx", "-y", package | Map.get(npx, "args", [])]}

      _ ->
        {:error, :npx_distribution_not_found}
    end
  end

  defp http_get(url, headers, timeout) do
    ensure_http_started()

    request = {String.to_charlist(url), normalize_headers(headers)}
    http_options = [timeout: timeout, connect_timeout: timeout]
    options = [body_format: :binary]

    case :httpc.request(:get, request, http_options, options) do
      {:ok, {{_, status, _}, _headers, body}} when status in 200..299 ->
        {:ok, body}

      {:ok, {{_, status, reason}, _headers, body}} ->
        {:error, {:http_error, status, to_string(reason), body}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp ensure_http_started do
    _ = Application.ensure_all_started(:ssl)
    _ = Application.ensure_all_started(:inets)
    :ok
  end

  defp normalize_headers(headers) do
    Enum.map(headers, fn
      {name, value} -> {String.to_charlist(to_string(name)), String.to_charlist(to_string(value))}
      %{"name" => name, "value" => value} -> {String.to_charlist(name), String.to_charlist(value)}
      %{name: name, value: value} -> {String.to_charlist(name), String.to_charlist(value)}
    end)
  end
end
