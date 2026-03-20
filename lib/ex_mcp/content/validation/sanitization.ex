defmodule ExMCP.Content.Validation.Sanitization do
  @moduledoc false
  # Content sanitization implementations extracted from Content.Validation.

  def apply_operation(operation, content) do
    case operation do
      :html_escape -> sanitize_html(content)
      :strip_scripts -> strip_scripts(content)
      :normalize_unicode -> normalize_unicode(content)
      {:limit_size, max_size} -> limit_content_size(content, max_size)
      :remove_metadata -> remove_metadata(content)
      :compress_media -> compress_media(content)
      _ -> content
    end
  end

  def apply_text_operation(operation, text) do
    case operation do
      :html_escape ->
        if Code.ensure_loaded?(HtmlEntities) do
          # credo:disable-for-next-line Credo.Check.Refactor.Apply
          apply(HtmlEntities, :encode, [text])
        else
          text
          |> String.replace("&", "&amp;")
          |> String.replace("<", "&lt;")
          |> String.replace(">", "&gt;")
          |> String.replace("\"", "&quot;")
          |> String.replace("'", "&#39;")
        end

      :strip_scripts ->
        Regex.replace(~r/<script\b[^<]*(?:(?!<\/script>)<[^<]*)*<\/script>/mi, text, "")

      :normalize_whitespace ->
        String.trim(text) |> String.replace(~r/\s+/, " ")

      {:truncate, length} ->
        String.slice(text, 0, length)

      _ ->
        text
    end
  rescue
    UndefinedFunctionError -> text
  end

  defp sanitize_html(%{type: :text} = content) do
    safe_text = apply_text_operation(:html_escape, content.text)
    %{content | text: safe_text}
  end

  defp sanitize_html(content), do: content

  defp strip_scripts(%{type: :text} = content) do
    safe_text = apply_text_operation(:strip_scripts, content.text)
    %{content | text: safe_text}
  end

  defp strip_scripts(content), do: content

  defp normalize_unicode(%{type: :text} = content) do
    normalized_text = :unicode.characters_to_binary(content.text, :utf8, :utf8)
    %{content | text: normalized_text}
  end

  defp normalize_unicode(content), do: content

  defp limit_content_size(content, _max_size), do: content
  defp remove_metadata(content), do: %{content | metadata: %{}}
  defp compress_media(content), do: content
end
