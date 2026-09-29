defmodule ExMCP.ACP.Adapters.ClaudeSDK.MCPConfig do
  @moduledoc false

  # Launch-time MCP configuration for Claude Code.
  #
  # Claude Code takes MCP servers only when it starts, through `--mcp-config`,
  # which accepts JSON files or inline JSON strings. Server definitions often
  # carry credentials (an HTTP `Authorization` header, a stdio server's `env`),
  # and a process's argv is readable by other local users through `ps` and
  # `/proc/<pid>/cmdline`, so `:mcp_servers` is never inlined. It is written to
  # a file only the current user can read (0600, in a fresh 0700 directory) and
  # only the path is passed. The directory is removed when the process that
  # built the command exits; under `ExMCP.ACP.AdapterBridge` that is the bridge,
  # which lives exactly as long as the Claude process it launched.

  alias ExMCP.ACP.Maps

  @file_name "mcp-config.json"
  @dir_prefix "ex_mcp_claude_sdk_mcp_"

  @doc """
  Resolves the MCP options in `opts` to `:mcp_config_path`, a list of config
  file paths: the caller's own `:mcp_config_path` (expanded against `:cwd`)
  followed by a private file holding `:mcp_servers`, which is removed from the
  options. Everything is validated before anything is written.
  """
  @spec prepare(keyword()) :: {:ok, keyword()} | {:error, term()}
  def prepare(opts) do
    with {:ok, paths} <- config_paths(opts),
         {:ok, json} <- encode_servers(Keyword.get(opts, :mcp_servers)),
         {:ok, generated} <- write_servers(json) do
      {:ok,
       opts
       |> Keyword.delete(:mcp_servers)
       |> Keyword.put(:mcp_config_path, paths ++ generated)}
    end
  end

  @doc """
  Names of the servers in a session request's ACP `mcpServers` that the launch
  options do not configure, which Claude Code will therefore not have. A launch
  with `:mcp_config_path` is taken to configure them all: the adapter does not
  read those files.
  """
  @spec unattached_servers(term(), keyword()) :: [String.t()]
  def unattached_servers([_ | _] = servers, opts) do
    if Keyword.get(opts, :mcp_config_path) in [nil, []] do
      launched = launched_names(Keyword.get(opts, :mcp_servers))
      servers |> Enum.map(&server_name/1) |> Enum.reject(&(&1 in launched))
    else
      []
    end
  end

  def unattached_servers(_servers, _opts), do: []

  defp launched_names(servers) when is_map(servers),
    do: servers |> Maps.stringify_keys() |> Map.keys()

  defp launched_names(_servers), do: []

  defp server_name(%{"name" => name}) when is_binary(name), do: name
  defp server_name(_server), do: "(unnamed)"

  defp config_paths(opts) do
    base = Keyword.get(opts, :cwd) || File.cwd!()

    opts
    |> Keyword.get(:mcp_config_path)
    |> List.wrap()
    |> Enum.reduce_while({:ok, []}, fn path, {:ok, acc} ->
      case config_path(path, base) do
        {:ok, path} -> {:cont, {:ok, [path | acc]}}
        {:error, _reason} = error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, paths} -> {:ok, Enum.reverse(paths)}
      error -> error
    end
  end

  # Relative paths resolve against the directory Claude Code is started in.
  defp config_path(path, base) when is_binary(path) and path != "" do
    expanded = Path.expand(path, base)

    if File.regular?(expanded) do
      {:ok, expanded}
    else
      invalid(:mcp_config_path, "#{inspect(path)} is not an existing file")
    end
  end

  defp config_path(_path, _base),
    do: invalid(:mcp_config_path, "expected a file path or a list of file paths")

  # The value is never echoed into an error: it may hold credentials.
  defp encode_servers(nil), do: {:ok, nil}
  defp encode_servers(servers) when servers == %{}, do: {:ok, nil}

  defp encode_servers(servers) when is_map(servers) do
    stringified = Maps.stringify_keys(servers)

    cond do
      not Enum.all?(servers, &valid_entry?/1) ->
        invalid(:mcp_servers, "each entry must map a server name to a Claude server config map")

      map_size(stringified) != map_size(servers) ->
        invalid(:mcp_servers, "server names must be unique once converted to strings")

      true ->
        case Jason.encode(%{"mcpServers" => stringified}) do
          {:ok, json} -> {:ok, json}
          {:error, _reason} -> invalid(:mcp_servers, "server configs must be JSON-encodable")
        end
    end
  end

  defp encode_servers(servers) when is_list(servers) do
    invalid(
      :mcp_servers,
      "expected a map of server name to Claude server config, got a list. " <>
        "ACP-style mcpServers lists are not accepted; give each server as " <>
        ~s(`"name" => %{"type" => "http", "url" => ..., "headers" => %{...}}` or ) <>
        ~s(`"name" => %{"command" => ..., "args" => [...], "env" => %{...}}`)
    )
  end

  defp encode_servers(_servers),
    do: invalid(:mcp_servers, "expected a map of server name to Claude server config")

  defp valid_entry?({name, config}) when is_binary(name) and name != "", do: is_map(config)
  defp valid_entry?({name, config}) when is_atom(name), do: is_map(config)
  defp valid_entry?(_entry), do: false

  defp write_servers(nil), do: {:ok, []}

  defp write_servers(json) do
    case System.tmp_dir() do
      nil ->
        {:error, {:mcp_config_write_failed, :no_tmp_dir}}

      tmp ->
        suffix = 12 |> :crypto.strong_rand_bytes() |> Base.url_encode64(padding: false)
        write_private(Path.join(tmp, @dir_prefix <> suffix), json)
    end
  end

  # `File.mkdir/1` fails on an existing path (a symlink included), so the
  # directory is always one this call created. Nothing is written until it is
  # 0700, and the file is created exclusively and made 0600 before its
  # contents go in.
  defp write_private(dir, json) do
    path = Path.join(dir, @file_name)

    case File.mkdir(dir) do
      :ok ->
        with :ok <- File.chmod(dir, 0o700),
             :ok <- write_exclusive(path, json) do
          remove_when_down(self(), dir)
          {:ok, [path]}
        else
          {:error, reason} ->
            File.rm_rf(dir)
            {:error, {:mcp_config_write_failed, reason}}
        end

      {:error, reason} ->
        {:error, {:mcp_config_write_failed, reason}}
    end
  end

  defp write_exclusive(path, contents) do
    with {:ok, device} <- File.open(path, [:write, :exclusive]) do
      try do
        with :ok <- File.chmod(path, 0o600), do: IO.binwrite(device, contents)
      after
        File.close(device)
      end
    end
  end

  defp remove_when_down(owner, dir) do
    spawn(fn ->
      ref = Process.monitor(owner)

      receive do
        {:DOWN, ^ref, :process, _pid, _reason} -> File.rm_rf(dir)
      end
    end)

    :ok
  end

  defp invalid(key, message), do: {:error, {:invalid_option, key, message}}
end
