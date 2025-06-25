defmodule ExMCP.Content.Sanitizer do
  @moduledoc """
  Content sanitization utilities for MCP content.

  This module handles all sanitization operations including HTML escaping,
  script removal, and security-related content cleaning. Extracted from 
  the original Content.Validation module.
  """

  alias ExMCP.Content.Protocol

  @typedoc "Sanitization operation"
  @type sanitization_op ::
          :html_escape
          | :strip_scripts
          | :normalize_unicode
          | :limit_size
          | :remove_metadata
          | :compress_media
          | {:custom, function()}
          | atom()

  @doc """
  Sanitizes content by applying a list of sanitization operations.

  ## Examples

      safe_content = Sanitizer.sanitize(content, [
        :html_escape,
        :strip_scripts,
        {:limit_size, 1_000_000}
      ])
  """
  @spec sanitize(Protocol.content(), [sanitization_op()]) :: Protocol.content()
  def sanitize(content, operations) when is_list(operations) do
    Enum.reduce(operations, content, &apply_sanitization/2)
  end

  @doc """
  Sanitizes text content specifically.
  """
  @spec sanitize_text(String.t(), [sanitization_op()]) :: String.t()
  def sanitize_text(text, operations) when is_binary(text) and is_list(operations) do
    Enum.reduce(operations, text, &apply_text_sanitization/2)
  end

  @doc """
  Escapes HTML entities in text content.
  """
  @spec html_escape(String.t()) :: String.t()
  def html_escape(text) when is_binary(text) do
    text
    |> String.replace("&", "&amp;")
    |> String.replace("<", "&lt;")
    |> String.replace(">", "&gt;")
    |> String.replace("\"", "&quot;")
    |> String.replace("'", "&#39;")
  end

  @doc """
  Removes script tags and JavaScript from HTML content.
  """
  @spec strip_scripts(String.t()) :: String.t()
  def strip_scripts(text) when is_binary(text) do
    text
    |> String.replace(~r/<script[^>]*>.*?<\/script>/is, "")
    |> String.replace(~r/on\w+\s*=\s*["'][^"']*["']/i, "")
    |> String.replace(~r/javascript:/i, "")
  end

  @doc """
  Normalizes Unicode characters to prevent homograph attacks.
  """
  @spec normalize_unicode(String.t()) :: String.t()
  def normalize_unicode(text) when is_binary(text) do
    # TODO: Implement proper Unicode normalization
    # This would use :unicode.characters_to_nfc_binary/1 or similar
    text
  end

  @doc """
  Removes potentially dangerous metadata from content.
  """
  @spec remove_metadata(Protocol.content()) :: Protocol.content()
  def remove_metadata(%{type: :image} = content) do
    # TODO: Implement EXIF and other metadata removal
    content
  end

  def remove_metadata(content), do: content

  @doc """
  Sanitizes file paths to prevent directory traversal.
  """
  @spec sanitize_path(String.t()) :: String.t()
  def sanitize_path(path) when is_binary(path) do
    path
    |> String.replace("..", "")
    |> String.replace("~", "")
    |> String.replace(~r/[^\w\-\.\/]/, "")
    |> String.trim("/")
  end

  @doc """
  Removes SQL injection attempts from text.
  """
  @spec strip_sql_injection(String.t()) :: String.t()
  def strip_sql_injection(text) when is_binary(text) do
    sql_keywords = ~w[
      SELECT INSERT UPDATE DELETE DROP CREATE ALTER EXEC EXECUTE
      UNION DECLARE CAST CONVERT SCRIPT JAVASCRIPT
    ]

    Enum.reduce(sql_keywords, text, fn keyword, acc ->
      String.replace(acc, ~r/\b#{keyword}\b/i, "")
    end)
  end

  # Private helper functions

  defp apply_sanitization(:html_escape, %{type: :text, text: text} = content) do
    %{content | text: html_escape(text)}
  end

  defp apply_sanitization(:strip_scripts, %{type: :text, text: text} = content) do
    %{content | text: strip_scripts(text)}
  end

  defp apply_sanitization(:normalize_unicode, %{type: :text, text: text} = content) do
    %{content | text: normalize_unicode(text)}
  end

  defp apply_sanitization({:limit_size, max_size}, content) do
    limit_content_size(content, max_size)
  end

  defp apply_sanitization(:remove_metadata, content) do
    remove_metadata(content)
  end

  defp apply_sanitization({:custom, fun}, content) when is_function(fun, 1) do
    fun.(content)
  end

  defp apply_sanitization(_, content), do: content

  defp apply_text_sanitization(:html_escape, text), do: html_escape(text)
  defp apply_text_sanitization(:strip_scripts, text), do: strip_scripts(text)
  defp apply_text_sanitization(:normalize_unicode, text), do: normalize_unicode(text)
  defp apply_text_sanitization(:strip_sql_injection, text), do: strip_sql_injection(text)
  defp apply_text_sanitization({:custom, fun}, text) when is_function(fun, 1), do: fun.(text)
  defp apply_text_sanitization(_, text), do: text

  defp limit_content_size(%{type: :text, text: text} = content, max_size) do
    if byte_size(text) > max_size do
      truncated = binary_part(text, 0, max_size)
      %{content | text: truncated}
    else
      content
    end
  end

  defp limit_content_size(%{type: type, data: data} = content, max_size)
       when type in [:image, :audio] do
    if byte_size(data) > max_size do
      # For binary data, we can't just truncate - mark for removal
      Map.put(content, :error, "Content exceeds size limit")
    else
      content
    end
  end

  defp limit_content_size(content, _), do: content
end
