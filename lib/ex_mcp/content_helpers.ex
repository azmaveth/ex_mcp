defmodule ExMCP.ContentHelpers do
  @moduledoc """
  Helper functions for creating content objects.

  This module provides backward compatibility functions for DSL-generated modules.
  """

  @doc """
  Creates a text content object.
  """
  def text(content, annotations \\ %{}) do
    base = %{"type" => "text", "text" => content}
    if annotations == %{}, do: base, else: Map.put(base, "annotations", annotations)
  end

  @doc """
  Creates a JSON content object.
  """
  def json(data, annotations \\ %{}) do
    base = %{"type" => "text", "text" => Jason.encode!(data)}
    if annotations == %{}, do: base, else: Map.put(base, "annotations", annotations)
  end

  @doc """
  Creates a user message content object.
  """
  def user(content) do
    %{"role" => "user", "content" => content}
  end

  @doc """
  Creates an assistant message content object.
  """
  def assistant(content) do
    %{"role" => "assistant", "content" => content}
  end

  @doc """
  Creates a system message content object.
  """
  def system(content) do
    %{"role" => "system", "content" => content}
  end

  @doc """
  Creates an image content object.
  """
  def image(base64_data, mime_type, annotations \\ %{}) do
    base = %{
      "type" => "image",
      "data" => base64_data,
      "mimeType" => mime_type
    }

    if annotations == %{}, do: base, else: Map.put(base, "annotations", annotations)
  end

  @doc """
  Creates an audio content object.
  """
  def audio(base64_data, mime_type, annotations \\ %{}) do
    base = %{
      "type" => "audio",
      "data" => base64_data,
      "mimeType" => mime_type
    }

    if annotations == %{}, do: base, else: Map.put(base, "annotations", annotations)
  end

  @doc """
  Creates a resource content object.
  """
  def resource(uri, annotations \\ %{}) do
    base = %{
      "type" => "resource",
      "resource" => %{
        "uri" => uri
      }
    }

    if annotations == %{}, do: base, else: Map.put(base, "annotations", annotations)
  end
end
