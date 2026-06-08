defmodule ExMCP.ACP.Adapters.Pi.Startup do
  @moduledoc false

  alias ExMCP.ACP.Adapters.Pi.Settings

  @package "@earendil-works/pi-coding-agent"
  @notice_timeout 800
  @max_changelog_chars 20_000

  @spec build(String.t(), map(), [String.t()]) :: String.t() | nil
  def build(cwd, settings, prelude_lines \\ []) do
    if Settings.quiet_startup?(settings) do
      update_notice(cwd, settings)
    else
      sections =
        [
          version_section(cwd, settings),
          update_notice(cwd, settings),
          prelude_section(prelude_lines),
          context_section(cwd),
          inventory_section("Skills", skill_dirs(cwd)),
          inventory_section("Prompts", prompt_dirs(cwd)),
          inventory_section("Extensions", extension_dirs(cwd))
        ]
        |> Enum.reject(&blank?/1)

      case sections do
        [] -> nil
        _ -> Enum.join(sections, "\n\n")
      end
    end
  end

  @spec changelog(keyword()) :: {:ok, String.t()} | {:error, String.t()}
  def changelog(opts) do
    with {:ok, path} <- changelog_path(opts),
         {:ok, raw} <- File.read(path) do
      {:ok, String.slice(raw, 0, @max_changelog_chars)}
    else
      {:error, reason} -> {:error, reason}
      _ -> {:error, "Unable to read Pi changelog"}
    end
  end

  defp version_section(cwd, settings) do
    case pi_version(cwd, settings) do
      nil -> nil
      version -> "Pi #{version}"
    end
  end

  defp update_notice(cwd, %{"_opts" => opts} = settings) do
    if Settings.update_notice?(opts) do
      with local when is_binary(local) <- pi_version(cwd, settings),
           latest when is_binary(latest) <- npm_latest_version() do
        if local != latest do
          "Update available: Pi #{local} -> #{latest}"
        end
      end
    end
  end

  defp update_notice(_cwd, _settings), do: nil

  defp pi_version(cwd, settings) do
    cmd = Settings.pi_command(settings["_opts"] || [], settings)

    case run_bounded(cmd, ["--version"], cwd, @notice_timeout) do
      {:ok, output} ->
        output
        |> String.trim()
        |> String.replace_prefix("pi ", "")
        |> blank_to_nil()

      _ ->
        nil
    end
  end

  defp npm_latest_version do
    case run_bounded("npm", ["view", @package, "version"], nil, @notice_timeout) do
      {:ok, output} -> output |> String.trim() |> blank_to_nil()
      _ -> nil
    end
  end

  defp prelude_section([]), do: nil

  defp prelude_section(lines) do
    text =
      lines
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&blank?/1)
      |> Enum.join("\n")

    if text == "", do: nil, else: "Startup output\n#{text}"
  end

  defp context_section(cwd) do
    path = Path.join(cwd, "AGENTS.md")

    if File.exists?(path) do
      "Context\nAGENTS.md"
    end
  end

  defp inventory_section(label, dirs) do
    count =
      dirs
      |> Enum.flat_map(&markdown_files/1)
      |> length()

    if count > 0, do: "#{label}\n#{count} available", else: nil
  end

  defp skill_dirs(cwd) do
    [
      Path.join([System.user_home!(), ".pi", "agent", "skills"]),
      Path.join([System.user_home!(), ".agents", "skills"]),
      Path.join([cwd, ".pi", "skills"])
    ]
  end

  defp prompt_dirs(cwd) do
    [
      Path.join([System.user_home!(), ".pi", "agent", "prompts"]),
      Path.join([cwd, ".pi", "prompts"])
    ]
  end

  defp extension_dirs(cwd) do
    [
      Path.join([System.user_home!(), ".pi", "agent", "extensions"]),
      Path.join([cwd, ".pi", "extensions"]),
      Path.join([cwd, ".pi", "packages"])
    ]
  end

  defp markdown_files(dir) do
    if File.dir?(dir), do: Path.wildcard(Path.join([dir, "**", "*.md"])), else: []
  end

  defp changelog_path(opts) do
    cmd = Keyword.get(opts, :cli_path) || Keyword.get(opts, :pi_command) || "pi"

    case System.find_executable(cmd) do
      executable when is_binary(executable) ->
        candidates = [
          Path.expand(
            Path.join([Path.dirname(executable), "..", "lib", "node_modules", @package])
          ),
          Path.expand(Path.join([Path.dirname(executable), "..", @package])),
          Path.expand(Path.join([Path.dirname(executable), ".."]))
        ]

        Enum.find_value(candidates, {:error, "Pi changelog not found"}, fn dir ->
          path = Path.join(dir, "CHANGELOG.md")
          if File.exists?(path), do: {:ok, path}
        end)

      _ ->
        {:error, "Pi executable not found"}
    end
  end

  defp run_bounded(cmd, args, cwd, timeout) do
    task =
      Task.async(fn ->
        try do
          opts = [stderr_to_stdout: true]
          opts = if is_binary(cwd), do: Keyword.put(opts, :cd, cwd), else: opts
          System.cmd(cmd, args, opts)
        rescue
          _ -> {"", 127}
        end
      end)

    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, {output, 0}} -> {:ok, output}
      _ -> :error
    end
  end

  defp blank?(nil), do: true
  defp blank?(text) when is_binary(text), do: String.trim(text) == ""
  defp blank?(_), do: false

  defp blank_to_nil(text) when is_binary(text) do
    if String.trim(text) == "", do: nil, else: text
  end
end
