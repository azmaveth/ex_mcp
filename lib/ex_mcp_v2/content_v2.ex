defmodule ExMCP.ContentV2 do
  @moduledoc """
  Content helper functions for creating MCP content types.

  Provides smart constructors for text, image, audio, and resource content
  that follow the MCP specification format.
  """

  @doc """
  Creates text content.

  ## Examples

      text("Hello, world!")
      text("Important message", audience: ["user"], priority: 0.9)
  """
  def text(content, annotations \\ %{}) do
    base = %{
      "type" => "text",
      "text" => content
    }

    if map_size(annotations) > 0 do
      Map.put(base, "annotations", annotations)
    else
      base
    end
  end

  @doc """
  Creates image content from base64 data.

  ## Examples

      image(base64_data, "image/png")
      image(base64_data, "image/jpeg", audience: ["user"])
  """
  def image(base64_data, mime_type, annotations \\ %{}) do
    base = %{
      "type" => "image",
      "data" => base64_data,
      "mimeType" => mime_type
    }

    if map_size(annotations) > 0 do
      Map.put(base, "annotations", annotations)
    else
      base
    end
  end

  @doc """
  Creates audio content from base64 data.

  ## Examples

      audio(base64_data, "audio/mp3")
      audio(base64_data, "audio/wav", audience: ["assistant"])
  """
  def audio(base64_data, mime_type, annotations \\ %{}) do
    base = %{
      "type" => "audio",
      "data" => base64_data,
      "mimeType" => mime_type
    }

    if map_size(annotations) > 0 do
      Map.put(base, "annotations", annotations)
    else
      base
    end
  end

  @doc """
  Creates embedded resource content.

  ## Examples

      resource("config://app/settings")
      resource("file://docs/guide.md", audience: ["assistant"])
  """
  def resource(uri, annotations \\ %{}) do
    base = %{
      "type" => "resource",
      "resource" => %{
        "uri" => uri
      }
    }

    if map_size(annotations) > 0 do
      Map.put(base, "annotations", annotations)
    else
      base
    end
  end

  @doc """
  Creates a user message for prompts.

  ## Examples

      user("Please review this code")
  """
  def user(content) do
    %{
      "role" => "user",
      "content" => content
    }
  end

  @doc """
  Creates an assistant message for prompts.

  ## Examples

      assistant("I'll help you review that code")
  """
  def assistant(content) do
    %{
      "role" => "assistant",
      "content" => content
    }
  end

  @doc """
  Creates a system message for prompts.

  ## Examples

      system("You are a helpful code reviewer")
  """
  def system(content) do
    %{
      "role" => "system",
      "content" => content
    }
  end

  @doc """
  Creates JSON content (convenience function).

  ## Examples

      json(%{debug: true, port: 8080})
  """
  def json(data, annotations \\ %{}) do
    text(Jason.encode!(data), annotations)
  end
end
