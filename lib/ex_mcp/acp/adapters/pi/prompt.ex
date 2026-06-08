defmodule ExMCP.ACP.Adapters.Pi.Prompt do
  @moduledoc false

  @type image :: map()

  @spec to_pi_message(any()) :: {String.t(), [image()]}
  def to_pi_message(prompt) when is_binary(prompt), do: {prompt, []}

  def to_pi_message(%{"content" => content}), do: to_pi_message(content)

  def to_pi_message(blocks) when is_list(blocks) do
    blocks
    |> Enum.reduce({"", []}, fn block, {message, images} ->
      case block do
        %{"type" => "text", "text" => text} when is_binary(text) ->
          {message <> text, images}

        %{"type" => "image"} = image ->
          {message, images ++ [normalize_image(image)]}

        %{"type" => "resource_link", "uri" => uri} ->
          {message <> "\n[Context] #{uri}", images}

        %{"type" => "resource", "resource" => resource} ->
          {message <> embedded_context_text(resource), images}

        %{"type" => "audio", "mimeType" => mime_type, "data" => data} ->
          bytes = byte_size(data || "")
          {message <> "\n[Audio] (#{mime_type}, #{bytes} bytes) not supported by Pi", images}

        _ ->
          {message, images}
      end
    end)
  end

  def to_pi_message(_prompt), do: {"", []}

  @spec normalize_images(any()) :: [image()]
  def normalize_images(images) when is_list(images), do: Enum.map(images, &normalize_image/1)
  def normalize_images(_images), do: []

  @spec normalize_image(map()) :: image()
  def normalize_image(%{"data" => data, "mimeType" => mime_type}) do
    %{
      "type" => "image",
      "data" => strip_data_url(data),
      "mimeType" => mime_type
    }
  end

  def normalize_image(%{"data" => data} = image) do
    %{
      "type" => "image",
      "data" => strip_data_url(data),
      "mimeType" => image["mimeType"] || detect_mime_type(data)
    }
  end

  def normalize_image(image), do: image

  defp embedded_context_text(%{"uri" => uri, "text" => text} = resource) when is_binary(text) do
    mime = resource["mimeType"] || "text/plain"
    "\n[Embedded Context] #{uri} (#{mime})\n#{text}"
  end

  defp embedded_context_text(%{"uri" => uri, "blob" => blob} = resource) when is_binary(blob) do
    mime = resource["mimeType"] || "application/octet-stream"
    bytes = byte_size(blob)
    "\n[Embedded Context] #{uri} (#{mime}, #{bytes} bytes)"
  end

  defp embedded_context_text(%{"uri" => uri}), do: "\n[Embedded Context] #{uri}"
  defp embedded_context_text(_resource), do: "\n[Embedded Context] (unknown)"

  defp strip_data_url(data) when is_binary(data) do
    case Regex.run(~r/^data:[^;]+;base64,(.+)$/s, data) do
      [_, base64] -> base64
      _ -> data
    end
  end

  defp strip_data_url(data), do: data

  defp detect_mime_type(data) when is_binary(data) do
    case Regex.run(~r/^data:([^;]+);base64,/, data) do
      [_, mime] -> mime
      _ -> "image/png"
    end
  end

  defp detect_mime_type(_data), do: "image/png"
end
