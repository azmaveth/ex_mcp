defmodule ExMCP.Content.Transformer do
  @moduledoc """
  Content transformation utilities for MCP content.

  This module handles all content transformation operations including
  format conversion, normalization, and media processing. Extracted from
  the original Content.Validation module.
  """

  alias ExMCP.Content.Protocol

  @typedoc "Transformation operation"
  @type transformation_op ::
          :normalize_whitespace
          | :convert_encoding
          | :compress_images
          | :resize_images
          | :extract_text
          | :generate_thumbnails
          | {:custom, function()}
          | atom()

  @doc """
  Transforms content by applying a list of transformation operations.

  ## Examples

      {:ok, transformed} = Transformer.transform(content, [
        :normalize_whitespace,
        {:resize_images, max_width: 800},
        :compress_images
      ])
  """
  @spec transform(Protocol.content(), [transformation_op()]) ::
          {:ok, Protocol.content()} | {:error, String.t()}
  def transform(content, operations) when is_list(operations) do
    result = Enum.reduce(operations, content, &apply_transformation/2)
    {:ok, result}
  rescue
    e -> {:error, "Transformation failed: #{Exception.message(e)}"}
  end

  @doc """
  Transforms content with validation after each operation.
  """
  @spec transform_with_validation(Protocol.content(), [transformation_op()]) ::
          {:ok, Protocol.content()} | {:error, String.t()}
  def transform_with_validation(content, operations) when is_list(operations) do
    Enum.reduce_while(operations, {:ok, content}, fn operation, {:ok, current_content} ->
      case apply_operation_with_validation(current_content, operation) do
        {:ok, transformed} -> {:cont, {:ok, transformed}}
        {:error, _} = error -> {:halt, error}
      end
    end)
  end

  @doc """
  Normalizes whitespace in text content.
  """
  @spec normalize_whitespace(String.t()) :: String.t()
  def normalize_whitespace(text) when is_binary(text) do
    text
    # Normalize line endings
    |> String.replace(~r/\r\n|\r/, "\n")
    # Collapse multiple spaces/tabs
    |> String.replace(~r/[ \t]+/, " ")
    # Limit consecutive newlines
    |> String.replace(~r/\n{3,}/, "\n\n")
    |> String.trim()
  end

  @doc """
  Converts text encoding to UTF-8.
  """
  @spec convert_encoding(String.t(), String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def convert_encoding(text, _from_encoding \\ "auto") when is_binary(text) do
    # TODO: Implement actual encoding conversion using :iconv or similar library
    {:error, "Encoding conversion not implemented - requires external encoding library"}
  end

  @doc """
  Extracts plain text from various content types.
  """
  @spec extract_text(Protocol.content()) :: {:ok, String.t()} | {:error, String.t()}
  def extract_text(%{type: :text, text: text, format: :plain}), do: {:ok, text}

  def extract_text(%{type: :text, text: text}) when is_binary(text), do: {:ok, text}

  def extract_text(%{type: :text, text: html, format: :html}) do
    # Simple HTML tag removal - in production, use a proper HTML parser
    text = String.replace(html, ~r/<[^>]+>/, " ")
    {:ok, normalize_whitespace(text)}
  end

  def extract_text(%{type: type}) when type in [:image, :audio, :video] do
    {:error, "Cannot extract text from #{type} content"}
  end

  def extract_text(_), do: {:error, "Unknown content type"}

  @doc """
  Compresses image data.
  """
  @spec compress_image(binary(), String.t(), keyword()) ::
          {:ok, binary()} | {:error, String.t()}
  def compress_image(_image_data, _mime_type, _opts \\ []) do
    # TODO: Implement actual image compression using ImageMagick, Mogrify, or similar
    {:error, "Image compression not implemented - requires external image processing library"}
  end

  @doc """
  Resizes image to fit within specified dimensions.
  """
  @spec resize_image(binary(), String.t(), keyword()) ::
          {:ok, binary()} | {:error, String.t()}
  def resize_image(_image_data, _mime_type, _opts) do
    # TODO: Implement actual image resizing using ImageMagick, Mogrify, or similar
    {:error, "Image resizing not implemented - requires external image processing library"}
  end

  @doc """
  Generates a thumbnail from image content.
  """
  @spec generate_thumbnail(binary(), String.t(), keyword()) ::
          {:ok, binary()} | {:error, String.t()}
  def generate_thumbnail(_image_data, _mime_type, _opts \\ []) do
    # TODO: Implement actual thumbnail generation using ImageMagick, Mogrify, or similar
    {:error, "Thumbnail generation not implemented - requires external image processing library"}
  end

  @doc """
  Converts content from one format to another.
  """
  @spec convert_format(Protocol.content(), atom()) ::
          {:ok, Protocol.content()} | {:error, String.t()}
  def convert_format(%{type: :text, format: from_format} = content, to_format) do
    case {from_format, to_format} do
      {same, same} -> {:ok, content}
      {:plain, :html} -> convert_text_to_html(content)
      {:html, :plain} -> convert_html_to_text(content)
      {:markdown, :html} -> convert_markdown_to_html(content)
      _ -> {:error, "Unsupported conversion from #{from_format} to #{to_format}"}
    end
  end

  def convert_format(_content, _to_format) do
    {:error, "Content must be text type for format conversion"}
  end

  # Private helper functions

  defp apply_transformation(:normalize_whitespace, %{type: :text, text: text} = content) do
    %{content | text: normalize_whitespace(text)}
  end

  defp apply_transformation({:convert_encoding, _from}, %{type: :text} = content) do
    # Encoding conversion not implemented - return content unchanged
    content
  end

  defp apply_transformation(:compress_images, %{type: :image} = content) do
    # Image compression not implemented - return content unchanged
    content
  end

  defp apply_transformation({:resize_images, _opts}, %{type: :image} = content) do
    # Image resizing not implemented - return content unchanged
    content
  end

  defp apply_transformation({:custom, fun}, content) when is_function(fun, 1) do
    fun.(content)
  end

  defp apply_transformation(_, content), do: content

  defp apply_operation_with_validation(content, operation) do
    case apply_transformation(operation, content) do
      %{} = transformed -> {:ok, transformed}
      {:error, _} = error -> error
    end
  end

  defp convert_text_to_html(%{text: text} = content) do
    html =
      text
      |> html_escape()
      |> String.replace("\n", "<br>\n")

    {:ok, %{content | format: :html, text: html}}
  end

  defp convert_html_to_text(%{type: :text, format: :html} = content) do
    case extract_text(content) do
      {:ok, text} -> {:ok, %{content | format: :plain, text: text}}
      error -> error
    end
  end

  defp convert_markdown_to_html(%{text: _markdown} = content) do
    # TODO: Implement markdown to HTML conversion
    # This would use Earmark or similar library
    {:ok, %{content | type: :html}}
  end

  defp html_escape(text) do
    text
    |> String.replace("&", "&amp;")
    |> String.replace("<", "&lt;")
    |> String.replace(">", "&gt;")
  end
end
