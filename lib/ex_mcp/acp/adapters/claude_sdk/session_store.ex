defmodule ExMCP.ACP.Adapters.ClaudeSDK.SessionStore do
  @moduledoc """
  Pure helpers for Claude Code's SDK session store.

  Claude Code persists conversations as JSONL files under
  `CLAUDE_CONFIG_DIR/projects/<project-key>`. The project key and metadata
  extraction in this module intentionally follow the official Claude Agent SDK
  helpers so the ACP adapter can expose `session/list` and `session/delete`
  without shelling out to Node.
  """

  import Bitwise

  alias ExMCP.ACP.Types

  @read_window 65_536
  @max_project_key 200
  @claude_config_env "CLAUDE_CONFIG_DIR"
  @claude_config_env_atom :CLAUDE_CONFIG_DIR
  @uuid_re ~r/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i

  @type opts :: keyword() | map()

  @doc """
  Returns SDK-shaped metadata for sessions visible from the given options.

  Supported options:

  - `:claude_config_dir` - explicit Claude config directory
  - `:env` - environment map or keyword list, checked for `CLAUDE_CONFIG_DIR`
  - `:cwd` / `:dir` - restrict sessions to the matching project directory
  - `:include_worktrees` - include Git worktrees for the requested cwd, default `true`
  - `:offset` / `:limit` - optional in-memory pagination
  """
  @spec list_sessions(opts()) :: {:ok, [map()]} | {:error, term()}
  def list_sessions(opts \\ []) do
    config_dir = config_dir(opts)
    cwd = option(opts, :cwd, "cwd") || option(opts, :dir, "dir")
    include_worktrees = option(opts, :include_worktrees, "includeWorktrees", true)

    sessions =
      config_dir
      |> project_dir_entries(cwd, include_worktrees)
      |> Enum.flat_map(&session_files/1)
      |> Enum.reduce([], fn file, acc ->
        case read_session(file) do
          {:ok, session} -> [session | acc]
          :skip -> acc
        end
      end)
      |> Enum.sort_by(& &1["lastModified"], :desc)
      |> paginate(opts)

    {:ok, sessions}
  rescue
    error ->
      {:error, Exception.message(error)}
  end

  @doc """
  Returns ACP `SessionInfo` maps for Claude Code sessions.
  """
  @spec list_acp_sessions(opts()) :: {:ok, [map()]} | {:error, term()}
  def list_acp_sessions(opts \\ []) do
    with {:ok, sessions} <- list_sessions(opts) do
      sessions =
        sessions
        |> Enum.map(&to_acp_session/1)
        |> Enum.reject(&is_nil/1)

      {:ok, sessions}
    end
  end

  @doc """
  Deletes a persisted Claude session by UUID.

  When `:cwd` or `:dir` is supplied, deletion is restricted to that project and
  its worktrees. Without a cwd, all project directories under the configured
  Claude store are searched.
  """
  @spec delete_session(String.t(), opts()) :: :ok | {:error, term()}
  def delete_session(session_id, opts \\ [])

  def delete_session(session_id, opts) when is_binary(session_id) do
    if valid_session_id?(session_id) do
      do_delete_session(session_id, opts)
    else
      {:error, "Invalid Claude sessionId: #{inspect(session_id)}"}
    end
  end

  def delete_session(session_id, _opts),
    do: {:error, "Invalid Claude sessionId: #{inspect(session_id)}"}

  @doc """
  Resolves the Claude config directory from adapter options, environment, or
  `~/.claude`.
  """
  @spec config_dir(opts()) :: String.t()
  def config_dir(opts \\ []) do
    opts
    |> option(:claude_config_dir, "claudeConfigDir")
    |> case do
      path when is_binary(path) and path != "" ->
        normalize_config_path(path)

      _ ->
        opts
        |> env_config_dir()
        |> case do
          path when is_binary(path) and path != "" ->
            normalize_config_path(path)

          _ ->
            System.user_home!()
            |> Path.join(".claude")
            |> normalize_config_path()
        end
    end
  end

  @doc """
  Returns the official SDK project key for a cwd.
  """
  @spec project_key(String.t()) :: String.t()
  def project_key(path) when is_binary(path) do
    path
    |> normalize_path()
    |> project_key_from_normalized()
  end

  @doc false
  @spec valid_session_id?(String.t()) :: boolean()
  def valid_session_id?(session_id), do: Regex.match?(@uuid_re, session_id)

  defp do_delete_session(session_id, opts) do
    config_dir = config_dir(opts)
    cwd = option(opts, :cwd, "cwd") || option(opts, :dir, "dir")
    include_worktrees = option(opts, :include_worktrees, "includeWorktrees", true)

    entries = project_dir_entries(config_dir, cwd, include_worktrees)
    projects_dir = projects_dir(config_dir)

    entries
    |> Enum.find_value(fn %{dir: project_dir} ->
      session_file = Path.join(project_dir, "#{session_id}.jsonl")
      session_dir = Path.join(project_dir, session_id)

      with true <- safe_child?(projects_dir, session_file),
           true <- safe_child?(projects_dir, session_dir),
           {:ok, %File.Stat{type: :regular, size: size}} when size > 0 <- File.lstat(session_file),
           :ok <- File.rm(session_file),
           {:ok, _removed} <- File.rm_rf(session_dir) do
        :ok
      else
        _ -> nil
      end
    end)
    |> case do
      :ok -> :ok
      nil -> {:error, "Claude session #{session_id} was not found"}
    end
  end

  defp project_dir_entries(config_dir, nil, _include_worktrees),
    do: all_project_dir_entries(config_dir)

  defp project_dir_entries(config_dir, "", _include_worktrees),
    do: all_project_dir_entries(config_dir)

  defp project_dir_entries(config_dir, cwd, include_worktrees) when is_binary(cwd) do
    cwd = normalize_path(cwd)

    workspaces =
      if include_worktrees do
        [cwd | git_worktrees(cwd)]
      else
        [cwd]
      end

    workspaces
    |> Enum.uniq()
    |> Enum.flat_map(fn workspace ->
      config_dir
      |> project_dirs_for_workspace(workspace)
      |> Enum.map(&%{dir: &1, fallback_cwd: workspace})
    end)
    |> uniq_by(& &1.dir)
  end

  defp all_project_dir_entries(config_dir) do
    config_dir
    |> projects_dir()
    |> list_regular_dirs()
    |> Enum.map(&%{dir: &1, fallback_cwd: nil})
  end

  defp project_dirs_for_workspace(config_dir, workspace) do
    projects_dir = projects_dir(config_dir)
    key = project_key_from_normalized(workspace)
    exact_dir = Path.join(projects_dir, key)

    dirs =
      if regular_directory?(exact_dir) do
        [exact_dir]
      else
        []
      end

    case long_project_prefix(workspace) do
      nil ->
        dirs

      prefix ->
        matching_dirs =
          projects_dir
          |> list_regular_dirs()
          |> Enum.filter(&(Path.basename(&1) |> String.starts_with?(prefix)))

        uniq_by(dirs ++ matching_dirs, & &1)
    end
  end

  defp projects_dir(config_dir), do: Path.join(config_dir, "projects")

  defp session_files(%{dir: project_dir, fallback_cwd: fallback_cwd}) do
    with true <- regular_directory?(project_dir),
         {:ok, names} <- File.ls(project_dir) do
      names
      |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
      |> Enum.flat_map(fn name ->
        session_id = Path.rootname(name, ".jsonl")
        path = Path.join(project_dir, name)

        with true <- valid_session_id?(session_id),
             {:ok, %File.Stat{type: :regular}} <- File.lstat(path),
             {:ok, %File.Stat{size: size, mtime: mtime}} when size > 0 <-
               File.stat(path, time: :posix) do
          [
            %{
              session_id: session_id,
              path: path,
              project_dir: project_dir,
              fallback_cwd: fallback_cwd,
              file_size: size,
              last_modified: mtime * 1000
            }
          ]
        else
          _ -> []
        end
      end)
    else
      _ -> []
    end
  end

  defp read_session(file) do
    with {:ok, {head, tail}} <- read_windows(file.path, file.file_size),
         false <- sidechain?(head),
         {:ok, metadata} <- extract_metadata(head, tail, file) do
      {:ok,
       %{
         "sessionId" => file.session_id,
         "cwd" => metadata.cwd,
         "summary" => metadata.summary,
         "customTitle" => metadata.custom_title,
         "firstPrompt" => metadata.first_prompt,
         "createdAt" => metadata.created_at,
         "gitBranch" => metadata.git_branch,
         "tag" => metadata.tag,
         "lastModified" => file.last_modified,
         "fileSize" => file.file_size,
         "path" => file.path
       }}
    else
      _ -> :skip
    end
  end

  defp read_windows(path, size) do
    count = min(size, @read_window)
    tail_offset = max(size - @read_window, 0)

    case :file.open(String.to_charlist(path), [:read, :binary]) do
      {:ok, fd} ->
        try do
          with {:ok, head} <- :file.pread(fd, 0, count),
               {:ok, tail} <- :file.pread(fd, tail_offset, count) do
            {:ok, {head, tail}}
          else
            error -> {:error, error}
          end
        after
          _ = :file.close(fd)
        end

      error ->
        {:error, error}
    end
  end

  defp extract_metadata(head, tail, file) do
    head_maps = json_line_maps(head)
    tail_maps = json_line_maps(tail)

    custom_title =
      last_string(tail_maps, "customTitle") ||
        last_string(tail_maps, "aiTitle") ||
        first_string(head_maps, "customTitle") ||
        first_string(head_maps, "aiTitle")

    first_prompt = first_prompt(head_maps)

    summary =
      custom_title ||
        last_string(tail_maps, "lastPrompt") ||
        last_string(tail_maps, "summary") ||
        first_prompt

    if blank?(summary) do
      :skip
    else
      {:ok,
       %{
         cwd: first_string(head_maps, "cwd") || file.fallback_cwd,
         summary: summary,
         custom_title: custom_title,
         first_prompt: first_prompt,
         created_at: first_string(head_maps, "timestamp"),
         git_branch: last_string(tail_maps, "gitBranch") || first_string(head_maps, "gitBranch"),
         tag: last_tag(tail_maps)
       }}
    end
  end

  defp to_acp_session(session) do
    if blank?(session["cwd"]) do
      nil
    else
      session["sessionId"]
      |> Types.session_info(session["cwd"],
        title: sanitize_title(session["summary"]),
        updatedAt: iso_from_ms(session["lastModified"])
      )
      |> maybe_put_meta(session_meta(session))
    end
  end

  defp session_meta(session) do
    %{
      "fileSize" => session["fileSize"],
      "customTitle" => session["customTitle"],
      "firstPrompt" => session["firstPrompt"],
      "createdAt" => session["createdAt"],
      "gitBranch" => session["gitBranch"],
      "tag" => session["tag"]
    }
    |> compact()
  end

  defp maybe_put_meta(session, meta) when map_size(meta) == 0, do: session

  defp maybe_put_meta(session, meta),
    do: Map.put(session, "_meta", %{"ex_mcp.claude_sdk" => meta})

  defp sidechain?(head) do
    head
    |> first_jsonl_line()
    |> then(fn line ->
      String.contains?(line, ~s("isSidechain":true)) ||
        String.contains?(line, ~s("isSidechain": true))
    end)
  end

  defp first_jsonl_line(blob) do
    blob
    |> String.split("\n", parts: 2)
    |> List.first()
    |> case do
      nil -> ""
      line -> line
    end
  end

  defp json_line_maps(blob) do
    blob
    |> String.split("\n", trim: true)
    |> Enum.flat_map(fn line ->
      case Jason.decode(String.trim(line)) do
        {:ok, map} when is_map(map) -> [map]
        _ -> []
      end
    end)
  end

  defp first_string(maps, key) do
    Enum.find_value(maps, fn map ->
      map
      |> strings_for_key(key)
      |> Enum.find(&(not blank?(&1)))
    end)
  end

  defp last_string(maps, key) do
    maps
    |> Enum.reverse()
    |> Enum.find_value(fn map ->
      map
      |> strings_for_key(key)
      |> Enum.reverse()
      |> Enum.find(&(not blank?(&1)))
    end)
  end

  defp last_tag(maps) do
    maps
    |> Enum.reverse()
    |> Enum.find_value(fn
      %{"type" => "tag"} = map ->
        map
        |> strings_for_key("tag")
        |> Enum.reverse()
        |> Enum.find(&(not blank?(&1)))

      _ ->
        nil
    end)
  end

  defp strings_for_key(%{} = map, key) do
    direct =
      map
      |> Enum.flat_map(fn
        {^key, value} when is_binary(value) -> [value]
        _ -> []
      end)

    nested =
      map
      |> Map.values()
      |> Enum.flat_map(&strings_for_key(&1, key))

    direct ++ nested
  end

  defp strings_for_key(list, key) when is_list(list),
    do: Enum.flat_map(list, &strings_for_key(&1, key))

  defp strings_for_key(_value, _key), do: []

  defp first_prompt(maps) do
    Enum.find_value(maps, &prompt_from_line/1)
  end

  defp prompt_from_line(%{} = map) do
    role = get_in(map, ["message", "role"]) || map["role"]
    type = map["type"]

    cond do
      map["isMeta"] == true -> nil
      map["isCompactSummary"] == true -> nil
      type not in ["user", nil] and role != "user" -> nil
      type == nil and role != "user" -> nil
      true -> map |> prompt_content() |> prompt_from_content()
    end
  end

  defp prompt_from_line(_map), do: nil

  defp prompt_content(map), do: get_in(map, ["message", "content"]) || map["content"]

  defp prompt_from_content(content) when is_binary(content) do
    prompt_from_texts([content])
  end

  defp prompt_from_content(content) when is_list(content) do
    if Enum.any?(content, &tool_result_block?/1) do
      nil
    else
      content
      |> Enum.flat_map(&content_texts/1)
      |> prompt_from_texts()
    end
  end

  defp prompt_from_content(_content), do: nil

  defp content_texts(%{"type" => "text", "text" => text}) when is_binary(text), do: [text]
  defp content_texts(%{"text" => text}) when is_binary(text), do: [text]
  defp content_texts(text) when is_binary(text), do: [text]
  defp content_texts(_content), do: []

  defp tool_result_block?(%{"type" => "tool_result"}), do: true
  defp tool_result_block?(_block), do: false

  defp prompt_from_texts(texts) do
    Enum.reduce_while(texts, nil, fn text, fallback ->
      text = normalize_prompt_text(text)

      cond do
        blank?(text) ->
          {:cont, fallback}

        match = Regex.run(~r/<bash-input>(.*?)<\/bash-input>/s, text) ->
          command = match |> List.last() |> normalize_prompt_text()
          {:halt, truncate_prompt("! #{command}")}

        Regex.match?(~r/^<command-name>.*<\/command-name>$/s, text) ->
          {:cont, fallback || truncate_prompt(text)}

        Regex.match?(~r/^(?:\s*<[a-z][\w-]*(?:\s|>)|\[Request interrupted by user[^\]]*\])/, text) ->
          {:cont, fallback}

        true ->
          {:halt, truncate_prompt(text)}
      end
    end)
  end

  defp normalize_prompt_text(text) do
    text
    |> String.replace(~r/\s+/, " ")
    |> String.trim()
  end

  defp truncate_prompt(text) do
    if String.length(text) > 200 do
      String.slice(text, 0, 200) <> "..."
    else
      text
    end
  end

  defp sanitize_title(nil), do: nil

  defp sanitize_title(title) when is_binary(title) do
    title =
      title
      |> String.replace(~r/[\r\n]+/, " ")
      |> String.replace(~r/\s+/, " ")
      |> String.trim()

    if String.length(title) > 256 do
      String.slice(title, 0, 253) <> "..."
    else
      title
    end
  end

  defp iso_from_ms(nil), do: nil

  defp iso_from_ms(ms) when is_integer(ms) do
    ms
    |> DateTime.from_unix!(:millisecond)
    |> DateTime.to_iso8601()
  end

  defp paginate(sessions, opts) do
    offset = pagination_offset(opts)

    limit =
      opts
      |> option(:limit, "limit")
      |> integer_or_nil()

    sessions
    |> Enum.drop(max(offset, 0))
    |> maybe_take(limit)
  end

  defp maybe_take(sessions, nil), do: sessions
  defp maybe_take(sessions, limit) when limit > 0, do: Enum.take(sessions, limit)
  defp maybe_take(_sessions, _limit), do: []

  defp integer_or_nil(value) when is_integer(value), do: value

  defp integer_or_nil(value) when is_binary(value) do
    case Integer.parse(value) do
      {integer, ""} -> integer
      _ -> nil
    end
  end

  defp integer_or_nil(_value), do: nil

  defp pagination_offset(opts) do
    opts
    |> option(:offset, "offset")
    |> integer_or_nil()
    |> case do
      nil ->
        opts
        |> option(:cursor, "cursor")
        |> integer_or_nil()
        |> case do
          nil -> 0
          cursor -> cursor
        end

      offset ->
        offset
    end
  end

  defp git_worktrees(cwd) do
    with true <- File.dir?(cwd),
         git when is_binary(git) <- System.find_executable("git"),
         {output, 0} <-
           System.cmd(git, ["worktree", "list", "--porcelain"], cd: cwd, stderr_to_stdout: true) do
      output
      |> String.split("\n", trim: true)
      |> Enum.flat_map(fn
        "worktree " <> path -> [normalize_path(path)]
        _ -> []
      end)
    else
      _ -> []
    end
  rescue
    _ -> []
  end

  defp normalize_path(path) do
    path
    |> Path.expand()
    |> realpath_or_self()
    |> maybe_normalize_nfc()
  end

  defp normalize_config_path(path) do
    path
    |> Path.expand()
    |> maybe_normalize_nfc()
  end

  defp realpath_or_self(path) do
    if File.exists?(path) do
      path
      |> Path.split()
      |> resolve_path_parts([])
    else
      path
    end
  rescue
    _ -> path
  end

  defp resolve_path_parts(["/" | parts], seen), do: resolve_path_parts("/", parts, seen)
  defp resolve_path_parts(parts, seen), do: resolve_path_parts(File.cwd!(), parts, seen)

  defp resolve_path_parts(base, [], _seen), do: base

  defp resolve_path_parts(base, [part | rest], seen) do
    path = Path.join(base, part)

    case File.lstat(path) do
      {:ok, %File.Stat{type: :symlink}} ->
        resolve_link(base, path, rest, seen)

      {:ok, _stat} ->
        resolve_path_parts(path, rest, seen)

      {:error, _reason} ->
        Path.join([path | rest])
    end
  end

  defp resolve_link(base, path, rest, seen) do
    if path in seen do
      Path.join([path | rest])
    else
      case File.read_link(path) do
        {:ok, target} ->
          target_parts =
            target
            |> target_path(base)
            |> Path.split()

          resolve_path_parts(target_parts ++ rest, [path | seen])

        {:error, _reason} ->
          Path.join([path | rest])
      end
    end
  end

  defp target_path(target, base) do
    if Path.type(target) == :absolute do
      Path.expand(target)
    else
      Path.expand(target, base)
    end
  end

  defp maybe_normalize_nfc(path) do
    if :os.type() == {:unix, :darwin} do
      String.normalize(path, :nfc)
    else
      path
    end
  end

  defp project_key_from_normalized(path) do
    sanitized = Regex.replace(~r/[^a-zA-Z0-9]/, path, "-")

    if String.length(sanitized) <= @max_project_key do
      sanitized
    else
      String.slice(sanitized, 0, @max_project_key) <> "-" <> project_hash(path)
    end
  end

  defp long_project_prefix(path) do
    sanitized = Regex.replace(~r/[^a-zA-Z0-9]/, path, "-")

    if String.length(sanitized) > @max_project_key do
      String.slice(sanitized, 0, @max_project_key) <> "-"
    end
  end

  defp project_hash(path) do
    path
    |> utf16_code_units()
    |> Enum.reduce(0, fn code_unit, acc ->
      acc
      |> Kernel.*(31)
      |> Kernel.+(code_unit)
      |> int32()
    end)
    |> abs()
    |> Integer.digits(36)
    |> Enum.map(&base36_digit/1)
    |> List.to_string()
  end

  defp utf16_code_units(path) do
    path
    |> :unicode.characters_to_binary(:utf8, {:utf16, :little})
    |> then(fn binary -> for <<unit::little-16 <- binary>>, do: unit end)
  end

  defp int32(value) do
    value = band(value, 0xFFFF_FFFF)

    if value >= 0x8000_0000 do
      value - 0x1_0000_0000
    else
      value
    end
  end

  defp base36_digit(digit) when digit < 10, do: ?0 + digit
  defp base36_digit(digit), do: ?a + digit - 10

  defp list_regular_dirs(parent) do
    with true <- regular_directory?(parent),
         {:ok, names} <- File.ls(parent) do
      names
      |> Enum.map(&Path.join(parent, &1))
      |> Enum.filter(&regular_directory?/1)
    else
      _ -> []
    end
  end

  defp regular_directory?(path) do
    case File.lstat(path) do
      {:ok, %File.Stat{type: :directory}} -> true
      _ -> false
    end
  end

  defp safe_child?(parent, child) do
    parent = parent |> Path.expand() |> Path.join("")
    child = Path.expand(child)
    String.starts_with?(child, parent)
  end

  defp env_config_dir(opts) do
    opts
    |> option(:env, "env", [])
    |> env_get()
    |> case do
      nil -> System.get_env(@claude_config_env)
      value -> value
    end
  end

  defp env_get(%{} = env),
    do: Map.get(env, @claude_config_env) || Map.get(env, @claude_config_env_atom)

  defp env_get(env) when is_list(env) do
    Keyword.get(env, @claude_config_env_atom) ||
      case List.keyfind(env, @claude_config_env, 0) do
        {_, value} -> value
        nil -> nil
      end
  end

  defp env_get(_env), do: nil

  defp option(opts, atom_key, string_key, default \\ nil)

  defp option(opts, atom_key, string_key, default) when is_map(opts) do
    Map.get(opts, atom_key) || Map.get(opts, string_key) || default
  end

  defp option(opts, atom_key, _string_key, default) when is_list(opts) do
    Keyword.get(opts, atom_key, default)
  end

  defp option(_opts, _atom_key, _string_key, default), do: default

  defp compact(map) do
    map
    |> Enum.reject(fn {_key, value} -> is_nil(value) or blank?(value) end)
    |> Map.new()
  end

  defp uniq_by(list, fun) do
    {_seen, values} =
      Enum.reduce(list, {MapSet.new(), []}, fn item, {seen, acc} ->
        key = fun.(item)

        if MapSet.member?(seen, key) do
          {seen, acc}
        else
          {MapSet.put(seen, key), [item | acc]}
        end
      end)

    Enum.reverse(values)
  end

  defp blank?(nil), do: true
  defp blank?(value) when is_binary(value), do: String.trim(value) == ""
  defp blank?(_value), do: false
end
