defmodule ExMCP.ACP.Adapters.Codex.MCP do
  @moduledoc false
  # Pure MCP server normalization and native Codex configuration output for
  # the Codex ACP adapter. Session-provided MCP servers are validated and
  # transport-defaulted here, and the native `mcp_servers`, `model_providers`,
  # `projects`, and sandbox writable-root config is assembled here. Whether a
  # normalized server or workspace is authorized, and whether authorized
  # workspaces may be trusted, is decided by the root adapter and passed in
  # explicitly; this module never consults callbacks or adapter options.

  alias ExMCP.Internal.Maps

  @type server :: map()
  @type native_entry :: {String.t(), map()}
  @type context :: %{
          optional(:cwd) => String.t() | nil,
          optional(:additional_directories) => [String.t()],
          optional(:gateway_config) => map() | nil,
          optional(:trust_authorized_workspaces) => boolean()
        }

  # MCP server normalization

  @spec normalize_server(term()) :: {:ok, server()} | {:error, String.t()}
  def normalize_server(%{"type" => "http"} = server) do
    case validate_http_mcp_server(server) do
      :ok -> {:ok, server}
      {:error, _reason} = error -> error
    end
  end

  def normalize_server(%{"type" => "stdio"} = server) do
    case validate_stdio_mcp_server(server) do
      :ok -> {:ok, server}
      {:error, _reason} = error -> error
    end
  end

  def normalize_server(%{"type" => "sse"}),
    do: {:error, "Codex doesn't support MCP SSE transport protocol"}

  def normalize_server(%{"type" => "acp"}),
    do: {:error, "Codex doesn't support MCP ACP transport protocol"}

  def normalize_server(server) when is_map(server) do
    if Map.has_key?(server, "command") do
      normalize_server(Map.put(server, "type", "stdio"))
    else
      {:error, "Unsupported MCP server transport"}
    end
  end

  def normalize_server(_server), do: {:error, "Invalid MCP server"}

  defp validate_http_mcp_server(server) do
    uri = if is_binary(server["url"]), do: URI.parse(server["url"]), else: %URI{}

    if valid_mcp_name?(server["name"]) and uri.scheme in ["http", "https"] and
         is_binary(uri.host) and uri.host != "" and valid_name_value_list?(server["headers"]) do
      :ok
    else
      {:error, "Invalid HTTP MCP server configuration"}
    end
  end

  defp validate_stdio_mcp_server(server) do
    if valid_mcp_name?(server["name"]) and is_binary(server["command"]) and
         server["command"] != "" and Path.type(server["command"]) == :absolute and
         is_list(server["args"]) and Enum.all?(server["args"], &is_binary/1) and
         valid_name_value_list?(server["env"]) do
      :ok
    else
      {:error, "Invalid stdio MCP server configuration"}
    end
  end

  defp valid_mcp_name?(name), do: is_binary(name) and String.trim(name) != ""

  defp valid_name_value_list?(values) when is_list(values) do
    Enum.all?(values, fn
      %{"name" => name, "value" => value} -> is_binary(name) and is_binary(value)
      {name, value} -> is_binary(name) and is_binary(value)
      _other -> false
    end)
  end

  defp valid_name_value_list?(_values), do: false

  # Native MCP server output

  @spec server_config(server()) :: native_entry()
  def server_config(%{"type" => "http"} = server) do
    {sanitize_mcp_server_name(server["name"]),
     %{}
     |> Map.put("url", server["url"])
     |> Maps.put_non_empty("http_headers", headers_to_map(server["headers"]))}
  end

  def server_config(%{"type" => "stdio"} = server) do
    {sanitize_mcp_server_name(server["name"]),
     %{}
     |> Map.put("command", server["command"])
     |> Maps.put_non_empty("args", server["args"])
     |> Maps.put_non_empty("env", env_to_map(server["env"]))}
  end

  @spec servers_config(%{optional(String.t()) => map()}) :: map() | nil
  def servers_config(entries) when map_size(entries) == 0, do: nil
  def servers_config(entries), do: %{"mcp_servers" => entries}

  defp sanitize_mcp_server_name(nil), do: "mcp_server"

  defp sanitize_mcp_server_name(name) do
    name
    |> to_string()
    |> String.trim()
    |> String.replace(~r/\s+/, "_")
    |> case do
      "" -> "mcp_server"
      sanitized -> sanitized
    end
  end

  defp headers_to_map(headers), do: name_value_list_to_map(headers)
  defp env_to_map(env), do: name_value_list_to_map(env)

  defp name_value_list_to_map(values) when is_list(values) do
    Map.new(values, fn
      %{"name" => name, "value" => value} -> {name, value}
      {name, value} -> {to_string(name), to_string(value)}
    end)
  end

  defp name_value_list_to_map(_values), do: nil

  # Native session configuration output

  @spec native_config(map(), map() | nil, context()) :: map() | nil
  def native_config(base_config, mcp_config, context) do
    additional_directories = Map.get(context, :additional_directories, [])

    base_config
    |> merge_gateway_config(Map.get(context, :gateway_config))
    |> maybe_merge_trusted_projects(
      Map.get(context, :trust_authorized_workspaces, false),
      Map.get(context, :cwd),
      additional_directories
    )
    |> merge_sandbox_workspace_roots(additional_directories)
    |> merge_config(mcp_config)
    |> empty_to_nil()
  end

  defp merge_gateway_config(config, nil), do: config

  defp merge_gateway_config(config, %{
         model_provider: model_provider,
         provider_config: provider_config
       }) do
    providers =
      config
      |> Map.get("model_providers", %{})
      |> case do
        providers when is_map(providers) -> providers
        _ -> %{}
      end
      |> Map.put(model_provider, provider_config)

    Map.put(config, "model_providers", providers)
  end

  defp maybe_merge_trusted_projects(config, true, cwd, additional_directories),
    do: merge_trusted_projects(config, cwd, additional_directories)

  defp maybe_merge_trusted_projects(config, _trust?, _cwd, _additional_directories), do: config

  defp merge_trusted_projects(config, cwd, additional_directories) do
    roots =
      [cwd | additional_directories]
      |> Enum.filter(&(is_binary(&1) and &1 != ""))
      |> Enum.uniq()

    if roots == [] do
      config
    else
      projects =
        config
        |> Map.get("projects", %{})
        |> case do
          projects when is_map(projects) -> projects
          _ -> %{}
        end
        |> Map.merge(Map.new(roots, &{&1, %{"trust_level" => "trusted"}}))

      Map.put(config, "projects", projects)
    end
  end

  defp merge_sandbox_workspace_roots(config, []), do: config

  defp merge_sandbox_workspace_roots(config, additional_directories) do
    sandbox =
      config
      |> Map.get("sandbox_workspace_write", %{})
      |> case do
        sandbox when is_map(sandbox) -> sandbox
        _ -> %{}
      end

    roots =
      sandbox
      |> Map.get("writable_roots", [])
      |> List.wrap()
      |> Enum.filter(&is_binary/1)
      |> Enum.concat(additional_directories)
      |> Enum.uniq()

    Map.put(config, "sandbox_workspace_write", Map.put(sandbox, "writable_roots", roots))
  end

  defp merge_config(config, nil), do: config
  defp merge_config(config, mcp_config), do: Map.merge(config, mcp_config)

  defp empty_to_nil(config) when map_size(config) == 0, do: nil
  defp empty_to_nil(config), do: config
end
