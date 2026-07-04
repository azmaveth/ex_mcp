defmodule ExMCP.ACP.Adapters.Pi.SessionStore do
  @moduledoc false

  @version 1

  @spec default_map_path() :: String.t()
  def default_map_path do
    Path.join([System.user_home!(), ".ex_mcp", "pi", "session-map.json"])
  end

  @spec get(String.t(), String.t()) :: map() | nil
  def get(path, session_id) do
    path
    |> load()
    |> get_in(["sessions", session_id])
  end

  @spec delete(String.t(), String.t()) :: map() | nil
  def delete(path, session_id) when is_binary(session_id) do
    db = load(path)
    {entry, db} = pop_in(db, ["sessions", session_id])
    save(path, db)
    entry
  end

  @spec upsert(String.t(), map()) :: :ok
  def upsert(path, %{"sessionId" => session_id, "cwd" => cwd, "sessionFile" => session_file})
      when is_binary(session_id) and is_binary(cwd) and is_binary(session_file) do
    db = load(path)

    entry = %{
      "sessionId" => session_id,
      "cwd" => cwd,
      "sessionFile" => session_file,
      "updatedAt" => DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601()
    }

    sessions = Map.put(db["sessions"] || %{}, session_id, entry)
    save(path, %{"version" => @version, "sessions" => sessions})
  end

  def upsert(_path, _entry), do: :ok

  @spec list_pi_sessions(keyword()) :: [map()]
  def list_pi_sessions(opts \\ []) do
    opts
    |> sessions_dir()
    |> jsonl_files()
    |> Enum.flat_map(&session_info/1)
    |> Enum.sort_by(&(&1["updatedAt"] || ""), :desc)
  end

  @spec find_pi_session_file(String.t(), keyword()) :: String.t() | nil
  def find_pi_session_file(session_id, opts \\ []) do
    opts
    |> list_pi_sessions()
    |> Enum.find(&(&1["sessionId"] == session_id))
    |> case do
      %{"sessionFile" => file} -> file
      _ -> nil
    end
  end

  defp load(path) do
    with {:ok, raw} <- File.read(path),
         {:ok, %{"version" => @version, "sessions" => sessions}} when is_map(sessions) <-
           Jason.decode(raw) do
      %{"version" => @version, "sessions" => sessions}
    else
      _ -> %{"version" => @version, "sessions" => %{}}
    end
  end

  defp save(path, data) do
    path |> Path.dirname() |> File.mkdir_p!()
    File.write!(path, Jason.encode_to_iodata!(data, pretty: true))
  end

  defp sessions_dir(opts) do
    cond do
      dir = Keyword.get(opts, :session_dir) ->
        dir

      dir = pi_agent_settings_session_dir(pi_agent_dir()) ->
        dir

      true ->
        Path.join(pi_agent_dir(), "sessions")
    end
  end

  defp pi_agent_dir do
    case System.get_env("PI_CODING_AGENT_DIR") do
      nil -> Path.join(System.user_home!(), ".pi/agent")
      "" -> Path.join(System.user_home!(), ".pi/agent")
      dir -> Path.expand(dir)
    end
  end

  defp pi_agent_settings_session_dir(agent_dir) do
    path = Path.join(agent_dir, "settings.json")

    with {:ok, raw} <- File.read(path),
         {:ok, settings} <- Jason.decode(raw),
         dir when is_binary(dir) and dir != "" <- settings["sessionDir"] do
      if Path.type(dir) == :absolute, do: dir, else: Path.expand(dir, agent_dir)
    else
      _ -> nil
    end
  end

  defp jsonl_files(dir) do
    if File.dir?(dir), do: Path.wildcard(Path.join([dir, "**", "*.jsonl"])), else: []
  end

  defp session_info(path) do
    with {:ok, raw} <- File.read(path),
         [first | _] = lines <- String.split(raw, ~r/\R/, trim: true),
         {:ok, %{"type" => "session", "id" => session_id, "cwd" => cwd}} <- Jason.decode(first) do
      title = title_from_lines(lines) || first_user_message(lines)
      updated_at = updated_at_from_lines(lines) || file_mtime(path)

      [
        %{
          "sessionId" => session_id,
          "cwd" => cwd,
          "title" => title,
          "name" => title || session_id,
          "updatedAt" => updated_at,
          "sessionFile" => path
        }
      ]
    else
      _ -> []
    end
  end

  defp title_from_lines(lines) do
    lines
    |> Enum.reverse()
    |> Enum.find_value(fn line ->
      with {:ok, %{"type" => "session_info", "name" => name}} when is_binary(name) <-
             Jason.decode(line),
           true <- String.trim(name) != "" do
        String.trim(name)
      else
        _ -> nil
      end
    end)
  end

  defp first_user_message(lines) do
    Enum.find_value(lines, fn line ->
      with {:ok, %{"type" => "message", "message" => %{"role" => "user", "content" => content}}} <-
             Jason.decode(line),
           text when is_binary(text) <- content_text(content) do
        String.slice(text, 0, 80)
      else
        _ -> nil
      end
    end)
  end

  defp updated_at_from_lines(lines) do
    updated_at_from_lines(lines, :message) || updated_at_from_lines(lines, :any)
  end

  defp updated_at_from_lines(lines, mode) do
    lines
    |> Enum.reverse()
    |> Enum.find_value(fn line ->
      with {:ok, entry} when is_map(entry) <- Jason.decode(line),
           true <- mode == :any or entry["type"] == "message",
           timestamp when is_binary(timestamp) <- entry["timestamp"],
           {:ok, dt, _offset} <- DateTime.from_iso8601(timestamp) do
        DateTime.to_iso8601(dt)
      else
        _ -> nil
      end
    end)
  end

  defp content_text(text) when is_binary(text), do: text

  defp content_text(content) when is_list(content) do
    content
    |> Enum.find_value(fn
      %{"type" => "text", "text" => text} when is_binary(text) -> text
      _ -> nil
    end)
  end

  defp content_text(_content), do: nil

  defp file_mtime(path) do
    case File.stat(path) do
      {:ok, %{mtime: mtime}} -> format_mtime(mtime)
      _ -> nil
    end
  end

  defp format_mtime({{year, month, day}, {hour, min, sec}}) do
    :io_lib.format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [
      year,
      month,
      day,
      hour,
      min,
      sec
    ])
    |> IO.iodata_to_binary()
  end

  defp format_mtime(_mtime), do: nil
end
