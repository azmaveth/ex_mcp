defmodule ExMCP.ACP.Adapters.Pi.SlashCommands do
  @moduledoc false

  @builtin_commands [
    %{"name" => "compact", "description" => "Manually compact the session context"},
    %{"name" => "autocompact", "description" => "Toggle automatic context compaction"},
    %{"name" => "export", "description" => "Export session to HTML"},
    %{"name" => "session", "description" => "Show session stats"},
    %{"name" => "name", "description" => "Set session display name"},
    %{"name" => "steering", "description" => "Get or set Pi steering mode"},
    %{"name" => "follow-up", "description" => "Get or set Pi follow-up mode"}
  ]

  @spec builtin_commands() :: [map()]
  def builtin_commands, do: @builtin_commands

  @spec load(String.t() | nil) :: [map()]
  def load(cwd) do
    cwd = cwd || File.cwd!()

    [
      {Path.join([System.user_home!(), ".pi", "agent", "prompts"]), "user"},
      {Path.expand(Path.join([".pi", "prompts"]), cwd), "project"}
    ]
    |> Enum.flat_map(fn {dir, source} -> load_dir(dir, source) end)
  end

  @spec available_commands([map()]) :: [map()]
  def available_commands(file_commands) do
    (file_commands ++ @builtin_commands)
    |> Enum.reduce({MapSet.new(), []}, fn command, {seen, acc} ->
      name = command["name"]

      if is_binary(name) and not MapSet.member?(seen, name) do
        {MapSet.put(seen, name),
         [%{"name" => name, "description" => command["description"] || "(command)"} | acc]}
      else
        {seen, acc}
      end
    end)
    |> elem(1)
    |> Enum.reverse()
  end

  @spec parse(String.t()) :: {:ok, String.t(), [String.t()]} | :error
  def parse(text) when is_binary(text) do
    trimmed = String.trim(text)

    case Regex.run(~r/^\/([A-Za-z][A-Za-z0-9_:-]*)(?:\s+(.*))?$/s, trimmed) do
      [_, name, args] -> {:ok, name, parse_args(args)}
      [_, name] -> {:ok, name, []}
      _ -> :error
    end
  end

  def parse(_text), do: :error

  @spec expand_file_command(String.t(), [String.t()], [map()]) :: String.t() | nil
  def expand_file_command(name, args, file_commands) do
    case Enum.find(file_commands, &(&1["name"] == name)) do
      nil -> nil
      %{"content" => content} -> substitute_args(content, args)
    end
  end

  @spec parse_args(String.t()) :: [String.t()]
  def parse_args(args_string) do
    args_string
    |> String.graphemes()
    |> Enum.reduce({[], "", nil}, fn ch, {args, current, quote} ->
      cond do
        quote && ch == quote ->
          {args, current, nil}

        quote ->
          {args, current <> ch, quote}

        ch in ["\"", "'"] ->
          {args, current, ch}

        ch in [" ", "\t"] ->
          if current == "", do: {args, "", nil}, else: {[current | args], "", nil}

        true ->
          {args, current <> ch, nil}
      end
    end)
    |> then(fn {args, current, _quote} ->
      if current == "", do: args, else: [current | args]
    end)
    |> Enum.reverse()
  end

  defp load_dir(dir, source) do
    if File.dir?(dir) do
      dir
      |> Path.join("**/*.md")
      |> Path.wildcard()
      |> Enum.flat_map(&load_file(&1, dir, source))
    else
      []
    end
  end

  defp load_file(path, root, source) do
    case File.read(path) do
      {:ok, raw} ->
        {frontmatter, content} = split_frontmatter(raw)
        name = path |> Path.basename() |> Path.rootname()
        subdir = path |> Path.dirname() |> Path.relative_to(root)
        source_label = source_label(source, subdir)

        description =
          frontmatter["description"] || first_line_description(content) || source_label

        [
          %{
            "name" => name,
            "description" => "#{description} #{source_label}",
            "content" => content,
            "source" => source_label
          }
        ]

      _ ->
        []
    end
  end

  defp split_frontmatter("---\n" <> rest) do
    case String.split(rest, "\n---", parts: 2) do
      [frontmatter, content] -> {parse_frontmatter(frontmatter), String.trim(content)}
      _ -> {%{}, "---\n" <> rest}
    end
  end

  defp split_frontmatter(content), do: {%{}, content}

  defp parse_frontmatter(frontmatter) do
    frontmatter
    |> String.split("\n")
    |> Enum.reduce(%{}, fn line, acc ->
      case Regex.run(~r/^(\w+):\s*(.*)$/, line) do
        [_, key, value] -> Map.put(acc, key, String.trim(value))
        _ -> acc
      end
    end)
  end

  defp first_line_description(content) do
    content
    |> String.split("\n")
    |> Enum.find(&(String.trim(&1) != ""))
    |> case do
      nil -> nil
      line -> String.slice(line, 0, 60)
    end
  end

  defp source_label(source, "."), do: "(#{source})"
  defp source_label(source, ""), do: "(#{source})"
  defp source_label(source, subdir), do: "(#{source}:#{String.replace(subdir, "/", ":")})"

  defp substitute_args(content, args) do
    content
    |> String.replace("$@", Enum.join(args, " "))
    |> then(fn text ->
      Regex.replace(~r/\$(\d+)/, text, fn _, number ->
        index = String.to_integer(number) - 1
        Enum.at(args, index, "")
      end)
    end)
  end
end
