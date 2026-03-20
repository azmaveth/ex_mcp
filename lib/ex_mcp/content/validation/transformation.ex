defmodule ExMCP.Content.Validation.Transformation do
  @moduledoc false
  # Content transformation implementations extracted from Content.Validation.

  alias ExMCP.Content.Protocol

  def apply_operation(operation, content) do
    case operation do
      {:resize, opts} -> resize_content(content, opts)
      {:compress, opts} -> compress_content(content, opts)
      :generate_thumbnail -> generate_thumbnail(content)
      :extract_text -> extract_text_content(content)
      :normalize_encoding -> normalize_content_encoding(content)
      _ -> content
    end
  end

  def apply_with_validation(content, operation) do
    case operation do
      op when is_atom(op) or is_tuple(op) ->
        case apply_operation(op, content) do
          new_content when is_map(new_content) ->
            case Protocol.validate(new_content) do
              :ok -> {:ok, new_content}
              {:error, reason} -> {:error, "Validation failed after #{inspect(op)}: #{reason}"}
            end

          result ->
            {:ok, result}
        end

      _ ->
        {:error, "Unknown operation: #{inspect(operation)}"}
    end
  end

  defp resize_content(%{type: :image} = content, opts) do
    width = Keyword.get(opts, :width)
    height = Keyword.get(opts, :height)
    if width && height, do: %{content | width: width, height: height}, else: content
  end

  defp resize_content(content, _opts), do: content

  defp compress_content(content, _opts), do: content

  defp generate_thumbnail(%{type: :image} = content), do: content
  defp generate_thumbnail(content), do: content

  defp extract_text_content(content) do
    case content.type do
      :text -> content.text
      :image -> content.alt_text || ""
      :audio -> content.transcript || ""
      :resource -> get_in(content, [:resource, :text]) || ""
      :annotation -> get_in(content, [:annotation, :text]) || ""
    end
  end

  defp normalize_content_encoding(%{type: :text} = content) do
    normalized = :unicode.characters_to_binary(content.text, :utf8, :utf8)
    %{content | text: normalized}
  end

  defp normalize_content_encoding(content), do: content
end
