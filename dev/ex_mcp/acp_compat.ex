defmodule ExMCP.ACPCompat do
  @moduledoc """
  Repository-only tooling for tracking ACP ecosystem compatibility.

  The checked-in manifest is the reviewed source of truth. Remote catalogs and
  repositories are treated as untrusted drift inputs: this module reports
  changes but never installs agents or rewrites the manifest.
  """

  @default_manifest Path.expand("../../test/interop/acp_compatibility.json", __DIR__)
  @github_api "https://api.github.com"
  @timeout 30_000
  @max_response_bytes 5_242_880

  @type drift :: map()
  @type report :: %{
          manifest: String.t(),
          checked_at: String.t(),
          drift: [drift()],
          errors: [map()]
        }

  @doc "Returns the repository's default ACP compatibility manifest path."
  @spec default_manifest() :: String.t()
  def default_manifest, do: @default_manifest

  @doc "Loads and validates an ACP compatibility manifest."
  @spec load_manifest(Path.t()) :: {:ok, map()} | {:error, term()}
  def load_manifest(path \\ @default_manifest) do
    with {:ok, body} <- File.read(path),
         {:ok, manifest} <- Jason.decode(body),
         :ok <- validate_manifest(manifest) do
      {:ok, manifest}
    end
  end

  @doc "Validates the stable fields used by the drift checker and smoke suite."
  @spec validate_manifest(map()) :: :ok | {:error, term()}
  def validate_manifest(manifest) when is_map(manifest) do
    with :ok <- require_value(manifest, "schemaVersion", 1),
         {:ok, catalog} <- require_map(manifest, "catalog"),
         :ok <- validate_source(catalog, "catalog"),
         :ok <- validate_unique_strings(catalog["agents"], "catalog.agents"),
         {:ok, registry} <- require_map(manifest, "registry"),
         :ok <- validate_source(registry, "registry"),
         :ok <- validate_registry_agents(registry["agents"]),
         :ok <- validate_upstreams(manifest["adapterUpstreams"]) do
      validate_interop_agents(manifest["interopAgents"], catalog["agents"])
    end
  end

  def validate_manifest(_manifest), do: {:error, :manifest_must_be_an_object}

  @doc "Extracts the primary agent name from each bullet on the ACP agents page."
  @spec parse_catalog(binary()) :: {:ok, [String.t()]} | {:error, term()}
  def parse_catalog(markdown) when is_binary(markdown) do
    agents =
      ~r/^\s*[-*]\s+\[([^\]]+)\]\(https?:\/\/[^)]+\)/m
      |> Regex.scan(markdown, capture: :all_but_first)
      |> Enum.map(fn [name] -> String.trim(name) end)
      |> Enum.uniq()
      |> Enum.sort()

    if agents == [], do: {:error, :no_catalog_agents_found}, else: {:ok, agents}
  end

  @doc "Builds the stable registry identity/version snapshot used by the manifest."
  @spec registry_snapshot(map()) :: {:ok, [map()]} | {:error, term()}
  def registry_snapshot(%{"agents" => agents}) when is_list(agents) do
    snapshot =
      agents
      |> Enum.map(fn agent ->
        %{"id" => agent["id"], "version" => agent["version"]}
      end)
      |> Enum.sort_by(& &1["id"])

    if Enum.all?(snapshot, &valid_registry_agent?/1) do
      {:ok, snapshot}
    else
      {:error, :invalid_registry_agent}
    end
  end

  def registry_snapshot(_registry), do: {:error, :invalid_registry}

  @doc "Compares a manifest with already-fetched remote snapshots."
  @spec compare(map(), map()) :: [drift()]
  def compare(manifest, snapshots) do
    catalog_drift =
      set_drift(
        "catalog",
        manifest["catalog"]["agents"],
        snapshots[:catalog] || snapshots["catalog"] || []
      )

    registry_drift =
      registry_drift(
        manifest["registry"]["agents"],
        snapshots[:registry] || snapshots["registry"] || []
      )

    upstream_drift =
      Enum.flat_map(manifest["adapterUpstreams"], fn upstream ->
        actual = get_in(snapshots, [:upstreams, upstream["id"]])

        if is_binary(actual) and actual != upstream["commit"] do
          [
            %{
              "kind" => "adapter_upstream",
              "id" => upstream["id"],
              "repository" => upstream["repository"],
              "expected" => upstream["commit"],
              "actual" => actual,
              "compareUrl" =>
                upstream["repository"] <>
                  "/compare/#{upstream["commit"]}...#{actual}"
            }
          ]
        else
          []
        end
      end)

    catalog_drift ++ registry_drift ++ upstream_drift
  end

  @doc "Fetches every configured source and returns a drift report."
  @spec check_remote(map(), keyword()) :: report()
  def check_remote(manifest, opts \\ []) do
    http_client = Keyword.get(opts, :http_client, &http_get/4)
    timeout = Keyword.get(opts, :timeout, @timeout)

    {catalog, catalog_errors} = fetch_catalog(manifest["catalog"], http_client, timeout)
    {registry, registry_errors} = fetch_registry(manifest["registry"], http_client, timeout)

    {upstreams, upstream_errors} =
      fetch_upstreams(manifest["adapterUpstreams"], http_client, timeout, opts)

    snapshots = %{
      catalog: catalog || manifest["catalog"]["agents"],
      registry: registry || manifest["registry"]["agents"],
      upstreams: upstreams
    }

    %{
      manifest: Keyword.get(opts, :manifest_path, @default_manifest),
      checked_at: DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601(),
      drift: compare(manifest, snapshots),
      errors: catalog_errors ++ registry_errors ++ upstream_errors
    }
  end

  @doc "Returns a reviewed ecosystem smoke-test entry by id."
  @spec interop_agent(map(), String.t()) :: {:ok, map()} | {:error, :unknown_agent}
  def interop_agent(manifest, id) when is_binary(id) do
    case Enum.find(manifest["interopAgents"], &(&1["id"] == id)) do
      nil -> {:error, :unknown_agent}
      agent -> {:ok, agent}
    end
  end

  @doc "Formats a human-readable report for CI logs."
  @spec format_report(report()) :: String.t()
  def format_report(report) do
    header =
      "ACP compatibility check: #{length(report.drift)} drift item(s), " <>
        "#{length(report.errors)} error(s)"

    drift = Enum.map(report.drift, &format_drift/1)
    errors = Enum.map(report.errors, &"ERROR #{&1["source"]}: #{&1["reason"]}")

    Enum.join([header | drift ++ errors], "\n")
  end

  defp fetch_catalog(catalog, http_client, timeout) do
    case http_client.(catalog["source"], default_headers(), timeout, []) do
      {:ok, body} ->
        case parse_catalog(body) do
          {:ok, agents} -> {agents, []}
          {:error, reason} -> {nil, [source_error("catalog", reason)]}
        end

      {:error, reason} ->
        {nil, [source_error("catalog", reason)]}
    end
  end

  defp fetch_registry(registry, http_client, timeout) do
    with {:ok, body} <- http_client.(registry["source"], default_headers(), timeout, []),
         {:ok, decoded} <- Jason.decode(body),
         {:ok, snapshot} <- registry_snapshot(decoded) do
      {snapshot, []}
    else
      {:error, reason} -> {nil, [source_error("registry", reason)]}
    end
  end

  defp fetch_upstreams(upstreams, http_client, timeout, opts) do
    token = Keyword.get(opts, :github_token) || System.get_env("GITHUB_TOKEN")

    upstreams
    |> Task.async_stream(
      fn upstream -> fetch_upstream(upstream, http_client, timeout, token) end,
      ordered: true,
      timeout: timeout + 5_000
    )
    |> Enum.reduce({%{}, []}, fn
      {:ok, {:ok, id, sha}}, {heads, errors} ->
        {Map.put(heads, id, sha), errors}

      {:ok, {:error, id, reason}}, {heads, errors} ->
        {heads, errors ++ [source_error("adapter_upstream:#{id}", reason)]}

      {:exit, reason}, {heads, errors} ->
        {heads, errors ++ [source_error("adapter_upstream", reason)]}
    end)
  end

  defp fetch_upstream(upstream, http_client, timeout, token) do
    with {:ok, {owner, repo}} <- github_repository(upstream["repository"]),
         url = "#{@github_api}/repos/#{owner}/#{repo}/commits/#{upstream["branch"]}",
         {:ok, body} <- http_client.(url, github_headers(token), timeout, autoredirect: true),
         {:ok, %{"sha" => sha}} when is_binary(sha) <- Jason.decode(body) do
      {:ok, upstream["id"], sha}
    else
      {:error, reason} -> {:error, upstream["id"], reason}
      _other -> {:error, upstream["id"], :invalid_github_response}
    end
  end

  defp github_repository(url) do
    case Regex.run(~r{^https://github\.com/([^/]+)/([^/#]+?)(?:\.git)?$}, url,
           capture: :all_but_first
         ) do
      [owner, repo] -> {:ok, {owner, repo}}
      _other -> {:error, :invalid_github_repository}
    end
  end

  defp set_drift(kind, expected, actual) do
    added = actual -- expected
    removed = expected -- actual

    if added == [] and removed == [] do
      []
    else
      [%{"kind" => kind, "added" => added, "removed" => removed}]
    end
  end

  defp registry_drift(expected, actual) do
    expected_by_id = Map.new(expected, &{&1["id"], &1["version"]})
    actual_by_id = Map.new(actual, &{&1["id"], &1["version"]})
    expected_ids = expected_by_id |> Map.keys() |> Enum.sort()
    actual_ids = actual_by_id |> Map.keys() |> Enum.sort()

    version_changes =
      expected_ids
      |> Enum.filter(&Map.has_key?(actual_by_id, &1))
      |> Enum.flat_map(fn id ->
        expected_version = expected_by_id[id]
        actual_version = actual_by_id[id]

        if expected_version == actual_version do
          []
        else
          [
            %{
              "id" => id,
              "expectedVersion" => expected_version,
              "actualVersion" => actual_version
            }
          ]
        end
      end)

    added = actual_ids -- expected_ids
    removed = expected_ids -- actual_ids

    if added == [] and removed == [] and version_changes == [] do
      []
    else
      [
        %{
          "kind" => "registry",
          "added" => added,
          "removed" => removed,
          "versionChanges" => version_changes
        }
      ]
    end
  end

  defp format_drift(%{"kind" => "adapter_upstream"} = drift) do
    "DRIFT #{drift["id"]}: #{drift["expected"]} -> #{drift["actual"]} " <>
      "(#{drift["compareUrl"]})"
  end

  defp format_drift(drift) do
    details =
      drift
      |> Map.drop(["kind"])
      |> Jason.encode!()

    "DRIFT #{drift["kind"]}: #{details}"
  end

  defp validate_source(section, name) do
    uri =
      case section["source"] do
        source when is_binary(source) -> URI.parse(source)
        _other -> %URI{}
      end

    if uri.scheme == "https" and is_binary(uri.host) and uri.host != "" do
      :ok
    else
      {:error, {:invalid_source, name}}
    end
  end

  defp validate_registry_agents(agents) when is_list(agents) do
    cond do
      not Enum.all?(agents, &valid_registry_agent?/1) ->
        {:error, :invalid_registry_agents}

      duplicate_values?(Enum.map(agents, & &1["id"])) ->
        {:error, :duplicate_registry_agents}

      true ->
        :ok
    end
  end

  defp validate_registry_agents(_agents), do: {:error, :invalid_registry_agents}

  defp valid_registry_agent?(%{"id" => id, "version" => version}),
    do: is_binary(id) and id != "" and is_binary(version) and version != ""

  defp valid_registry_agent?(_agent), do: false

  defp validate_upstreams(upstreams) when is_list(upstreams) and upstreams != [] do
    valid? = Enum.all?(upstreams, &valid_upstream?/1)

    cond do
      not valid? -> {:error, :invalid_adapter_upstreams}
      duplicate_values?(Enum.map(upstreams, & &1["id"])) -> {:error, :duplicate_upstreams}
      true -> :ok
    end
  end

  defp validate_upstreams(_upstreams), do: {:error, :invalid_adapter_upstreams}

  defp valid_upstream?(upstream) do
    string_fields? =
      Enum.all?(~w(id adapter repository branch), &non_empty_string?(upstream[&1]))

    string_fields? and
      non_empty_string?(upstream["commit"]) and
      Regex.match?(~r/^[0-9a-f]{40}$/, upstream["commit"])
  end

  defp validate_interop_agents(agents, catalog_agents) when is_list(agents) and agents != [] do
    valid? = Enum.all?(agents, &valid_interop_agent?(&1, catalog_agents))

    cond do
      not valid? -> {:error, :invalid_interop_agents}
      duplicate_values?(Enum.map(agents, & &1["id"])) -> {:error, :duplicate_interop_agents}
      true -> :ok
    end
  end

  defp validate_interop_agents(_agents, _catalog_agents), do: {:error, :invalid_interop_agents}

  defp valid_interop_agent?(agent, catalog_agents) do
    command = agent["command"]

    non_empty_string?(agent["id"]) and
      non_empty_string?(agent["catalogName"]) and
      agent["catalogName"] in catalog_agents and
      agent["tier"] in ["initialize", "session"] and
      is_list(command) and command != [] and
      Enum.all?(command, &is_binary/1) and
      pinned_command?(command)
  end

  defp pinned_command?(["npx", "-y", package | _args]) do
    package
    |> String.split("@", trim: true)
    |> length()
    |> Kernel.>(1)
  end

  defp pinned_command?(_command), do: true

  defp validate_unique_strings(values, field) when is_list(values) and values != [] do
    cond do
      not Enum.all?(values, &(is_binary(&1) and &1 != "")) -> {:error, {:invalid_values, field}}
      duplicate_values?(values) -> {:error, {:duplicate_values, field}}
      true -> :ok
    end
  end

  defp validate_unique_strings(_values, field), do: {:error, {:invalid_values, field}}

  defp duplicate_values?(values), do: length(Enum.uniq(values)) != length(values)

  defp non_empty_string?(value), do: is_binary(value) and value != ""

  defp require_value(map, key, value) do
    if map[key] == value, do: :ok, else: {:error, {:invalid_value, key}}
  end

  defp require_map(map, key) do
    case map[key] do
      value when is_map(value) -> {:ok, value}
      _other -> {:error, {:invalid_map, key}}
    end
  end

  defp source_error(source, reason),
    do: %{"source" => source, "reason" => inspect(reason)}

  defp default_headers,
    do: [{"accept", "application/json, text/markdown;q=0.9, text/plain;q=0.8"}]

  defp github_headers(nil),
    do: [{"accept", "application/vnd.github+json"}, {"x-github-api-version", "2022-11-28"}]

  defp github_headers(token) do
    [{"authorization", "Bearer #{token}"} | github_headers(nil)]
  end

  defp http_get(url, headers, timeout, options) do
    _ = Application.ensure_all_started(:ssl)
    _ = Application.ensure_all_started(:inets)

    headers = [{"user-agent", "ex_mcp-acp-compat"} | headers]
    request = {String.to_charlist(url), charlist_headers(headers)}

    ssl_options = [
      verify: :verify_peer,
      cacerts: :public_key.cacerts_get(),
      versions: [:"tlsv1.2", :"tlsv1.3"],
      customize_hostname_check: [
        match_fun: :public_key.pkix_verify_hostname_match_fun(:https)
      ]
    ]

    http_options =
      [timeout: timeout, connect_timeout: timeout, ssl: ssl_options] ++ options

    case :httpc.request(:get, request, http_options, body_format: :binary) do
      {:ok, {{_, status, _}, _response_headers, body}}
      when status in 200..299 and byte_size(body) <= @max_response_bytes ->
        {:ok, body}

      {:ok, {{_, status, _}, _response_headers, body}}
      when status in 200..299 ->
        {:error, {:response_too_large, byte_size(body), @max_response_bytes}}

      {:ok, {{_, status, reason}, _response_headers, _body}} ->
        {:error, {:http_error, status, to_string(reason)}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp charlist_headers(headers) do
    Enum.map(headers, fn {name, value} ->
      {String.to_charlist(name), String.to_charlist(value)}
    end)
  end
end
