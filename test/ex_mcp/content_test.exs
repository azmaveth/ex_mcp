defmodule ExMCP.ContentTest do
  use ExUnit.Case, async: true

  alias ExMCP.ContentHelpers, as: Content

  describe "text/1 and text/2" do
    test "creates text content without annotations" do
      result = Content.text("Hello, world!")

      assert result == %{
               "type" => "text",
               "text" => "Hello, world!"
             }
    end

    test "creates text content with annotations" do
      annotations = %{"audience" => ["user"], "priority" => 0.9}
      result = Content.text("Important message", annotations)

      assert result == %{
               "type" => "text",
               "text" => "Important message",
               "annotations" => annotations
             }
    end

    test "omits annotations when empty" do
      result = Content.text("Hello", %{})

      assert result == %{
               "type" => "text",
               "text" => "Hello"
             }
    end
  end

  describe "image/2 and image/3" do
    test "creates image content without annotations" do
      result = Content.image("base64data", "image/png")

      assert result == %{
               "type" => "image",
               "data" => "base64data",
               "mimeType" => "image/png"
             }
    end

    test "creates image content with annotations" do
      annotations = %{"audience" => ["user"]}
      result = Content.image("base64data", "image/jpeg", annotations)

      assert result == %{
               "type" => "image",
               "data" => "base64data",
               "mimeType" => "image/jpeg",
               "annotations" => annotations
             }
    end
  end

  describe "audio/2 and audio/3" do
    test "creates audio content without annotations" do
      result = Content.audio("base64data", "audio/mp3")

      assert result == %{
               "type" => "audio",
               "data" => "base64data",
               "mimeType" => "audio/mp3"
             }
    end

    test "creates audio content with annotations" do
      annotations = %{"audience" => ["assistant"]}
      result = Content.audio("base64data", "audio/wav", annotations)

      assert result == %{
               "type" => "audio",
               "data" => "base64data",
               "mimeType" => "audio/wav",
               "annotations" => annotations
             }
    end
  end

  describe "resource/1 and resource/2" do
    test "creates resource content without annotations" do
      result = Content.resource("config://app/settings")

      assert result == %{
               "type" => "resource",
               "resource" => %{
                 "uri" => "config://app/settings"
               }
             }
    end

    test "creates resource content with annotations" do
      annotations = %{"audience" => ["assistant"]}
      result = Content.resource("file://docs/guide.md", annotations)

      assert result == %{
               "type" => "resource",
               "resource" => %{
                 "uri" => "file://docs/guide.md"
               },
               "annotations" => annotations
             }
    end
  end

  describe "message helpers" do
    test "user/1 creates user message" do
      result = Content.user("Please review this code")

      assert result == %{
               "role" => "user",
               "content" => "Please review this code"
             }
    end

    test "assistant/1 creates assistant message" do
      result = Content.assistant("I'll help you review that code")

      assert result == %{
               "role" => "assistant",
               "content" => "I'll help you review that code"
             }
    end

    test "system/1 creates system message" do
      result = Content.system("You are a helpful assistant")

      assert result == %{
               "role" => "system",
               "content" => "You are a helpful assistant"
             }
    end
  end

  describe "json/1 and json/2" do
    test "creates JSON text content" do
      data = %{debug: true, port: 8080}
      result = Content.json(data)

      expected_json = Jason.encode!(data)

      assert result == %{
               "type" => "text",
               "text" => expected_json
             }
    end

    test "creates JSON text content with annotations" do
      data = %{status: "ok"}
      annotations = %{"priority" => 0.5}
      result = Content.json(data, annotations)

      expected_json = Jason.encode!(data)

      assert result == %{
               "type" => "text",
               "text" => expected_json,
               "annotations" => annotations
             }
    end
  end
end
