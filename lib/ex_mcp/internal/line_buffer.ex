defmodule ExMCP.Internal.LineBuffer do
  @moduledoc false

  @type invalid_line :: {:invalid_json, String.t()}

  @spec drain_json(String.t()) :: {[any()], [invalid_line()], String.t()}
  def drain_json(buffer) when is_binary(buffer) do
    drain_json(buffer, [], [])
  end

  defp drain_json(buffer, messages, invalid) do
    case String.split(buffer, "\n", parts: 2) do
      [line, rest] ->
        line
        |> String.trim()
        |> parse_line(messages, invalid)
        |> then(fn {messages, invalid} -> drain_json(rest, messages, invalid) end)

      [partial] ->
        {Enum.reverse(messages), Enum.reverse(invalid), partial}
    end
  end

  defp parse_line("", messages, invalid), do: {messages, invalid}

  defp parse_line(line, messages, invalid) do
    if json_like?(line) do
      case Jason.decode(line) do
        {:ok, message} -> {[message | messages], invalid}
        {:error, _reason} -> {messages, [{:invalid_json, line} | invalid]}
      end
    else
      {messages, invalid}
    end
  end

  defp json_like?(line) do
    String.starts_with?(line, "{") or String.starts_with?(line, "[")
  end
end
