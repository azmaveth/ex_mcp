defmodule ExMCP.ACP.Adapters.Pi.Tools do
  @moduledoc false

  @spec kind(String.t()) :: String.t()
  def kind("read"), do: "read"
  def kind("write"), do: "edit"
  def kind("edit"), do: "edit"
  def kind(_tool_name), do: "other"

  @spec locations(any(), String.t() | nil, integer() | nil) :: [map()] | nil
  def locations(args, cwd, line \\ nil)

  def locations(%{"path" => path}, cwd, line) when is_binary(path) do
    resolved =
      if Path.type(path) == :absolute do
        path
      else
        Path.expand(path, cwd || File.cwd!())
      end

    location = %{"path" => resolved}
    location = if is_integer(line), do: Map.put(location, "line", line), else: location
    [location]
  end

  def locations(_args, _cwd, _line), do: nil

  @spec find_unique_line_number(String.t(), String.t()) :: integer() | nil
  def find_unique_line_number(_text, ""), do: nil

  def find_unique_line_number(text, needle) when is_binary(text) and is_binary(needle) do
    case :binary.match(text, needle) do
      :nomatch ->
        nil

      {index, length} ->
        after_first = binary_part(text, index + length, byte_size(text) - index - length)

        if :binary.match(after_first, needle) == :nomatch do
          text
          |> binary_part(0, index)
          |> String.graphemes()
          |> Enum.count(&(&1 == "\n"))
          |> Kernel.+(1)
        end
    end
  end

  def find_unique_line_number(_text, _needle), do: nil

  @spec result_text(any()) :: String.t()
  def result_text(nil), do: ""

  def result_text(%{"content" => content, "details" => details}) when is_list(content) do
    content_text(content)
    |> case do
      "" -> details_text(details)
      text -> text
    end
  end

  def result_text(%{"content" => content}) when is_list(content), do: content_text(content)
  def result_text(%{"details" => details}) when is_map(details), do: details_text(details)
  def result_text(text) when is_binary(text), do: text

  def result_text(other) do
    case Jason.encode(other) do
      {:ok, json} -> json
      _ -> inspect(other)
    end
  end

  @spec text_content(String.t() | nil) :: [map()] | nil
  def text_content(nil), do: nil
  def text_content(""), do: nil

  def text_content(text) when is_binary(text) do
    [%{"type" => "content", "content" => %{"type" => "text", "text" => text}}]
  end

  defp content_text(content) do
    content
    |> Enum.flat_map(fn
      %{"type" => "text", "text" => text} when is_binary(text) -> [text]
      _ -> []
    end)
    |> Enum.join("")
  end

  defp details_text(nil), do: ""

  defp details_text(details) when is_map(details) do
    diff = details["diff"]

    if is_binary(diff) and String.trim(diff) != "" do
      diff
    else
      bash_output(details)
    end
  end

  defp details_text(_details), do: ""

  defp bash_output(details) do
    stdout = details["stdout"] || details["output"] || ""
    stderr = details["stderr"] || ""
    exit_code = details["exitCode"] || details["code"]

    []
    |> maybe_prepend(stdout)
    |> maybe_prepend(if(has_content?(stderr), do: "stderr:\n#{stderr}", else: nil))
    |> maybe_prepend(if(is_integer(exit_code), do: "exit code: #{exit_code}", else: nil))
    |> Enum.reverse()
    |> Enum.join("\n\n")
    |> String.trim_trailing()
  end

  defp maybe_prepend(parts, value) when is_binary(value) do
    if has_content?(value), do: [value | parts], else: parts
  end

  defp maybe_prepend(parts, _value), do: parts

  defp has_content?(value), do: is_binary(value) and String.trim(value) != ""
end
