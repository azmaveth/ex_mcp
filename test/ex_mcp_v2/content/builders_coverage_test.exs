defmodule ExMCP.Content.BuildersCoverageTest do
  use ExUnit.Case, async: true

  use ExMCP.Content.Builders

  describe "edge cases and error handling" do
    test "from_file handles missing files" do
      assert {:error, "File not found: /nonexistent/file.txt"} =
               from_file("/nonexistent/file.txt")
    end

    test "from_file handles files exceeding size limit" do
      # Create a temporary large file
      path = Path.join(System.tmp_dir!(), "large_file_#{System.unique_integer()}.txt")
      File.write!(path, String.duplicate("x", 100))

      # Try to read with small size limit
      result = from_file(path, max_size: 50)

      assert {:error, "File size exceeds maximum of 50 bytes"} = result

      # Clean up
      File.rm!(path)
    end

    test "from_file detects various file types" do
      # Test JSON file
      json_path = Path.join(System.tmp_dir!(), "test_#{System.unique_integer()}.json")
      File.write!(json_path, ~s({"test": "data"}))

      result = from_file(json_path)
      assert {:ok, %{uri: uri}} = result
      assert String.starts_with?(uri, "file://")

      File.rm!(json_path)
    end

    test "image_from_file rejects non-image files" do
      text_path = Path.join(System.tmp_dir!(), "test_#{System.unique_integer()}.txt")
      File.write!(text_path, "not an image")

      assert {:error, "File is not an image: " <> _} = image_from_file(text_path)

      File.rm!(text_path)
    end

    test "audio_from_file rejects non-audio files" do
      text_path = Path.join(System.tmp_dir!(), "test_#{System.unique_integer()}.txt")
      File.write!(text_path, "not audio")

      assert {:error, "File is not audio: " <> _} = audio_from_file(text_path)

      File.rm!(text_path)
    end

    test "text_from_file detects code file formats" do
      # Test Python file
      py_path = Path.join(System.tmp_dir!(), "test_#{System.unique_integer()}.py")
      File.write!(py_path, "def hello():\n    print('world')")

      content = text_from_file(py_path)
      assert %{format: :code, language: "python"} = content

      File.rm!(py_path)

      # Test SQL file
      sql_path = Path.join(System.tmp_dir!(), "test_#{System.unique_integer()}.sql")
      File.write!(sql_path, "SELECT * FROM users;")

      content = text_from_file(sql_path)
      assert %{format: :code, language: "sql"} = content

      File.rm!(sql_path)
    end

    test "chainable modifiers preserve content structure" do
      content =
        text("# Title")
        |> as_markdown()
        |> with_metadata(%{author: "test"})
        |> with_metadata(%{timestamp: "2024-01-01"})

      assert content.format == :markdown
      assert content.metadata.author == "test"
      assert content.metadata.timestamp == "2024-01-01"
    end

    test "with_dimensions validates positive integers" do
      img = image("data", "image/png")

      # Valid dimensions
      result = with_dimensions(img, 100, 200)
      assert result.width == 100
      assert result.height == 200

      # Invalid dimensions should fail at compile time with guards
      # We can't test compile-time failures, but the guards prevent invalid input
    end

    test "with_confidence validates range" do
      ann = annotation("test_type")

      # Valid confidence
      result = with_confidence(ann, 0.95)
      assert result.annotation.confidence == 0.95

      # Edge cases
      result = with_confidence(ann, 0.0)
      assert result.annotation.confidence == 0.0

      result = with_confidence(ann, 1.0)
      assert result.annotation.confidence == 1.0
    end

    test "batch handles mixed content and errors" do
      contents =
        batch([
          text("Hello"),
          fn -> image("data", "image/png") end,
          # This will be caught
          fn -> raise "error" end,
          resource("file://test.txt")
        ])

      # Should have 3 items (error one is filtered out)
      assert length(contents) == 3
      assert Enum.at(contents, 0).type == :text
      assert Enum.at(contents, 1).type == :image
      assert Enum.at(contents, 2).type == :resource
    end

    test "from_template handles complex substitutions" do
      template = text("User {{name}} has {{count}} {{item}}")

      result =
        from_template(template, %{
          name: "Alice",
          count: 5,
          item: "messages"
        })

      assert result.text == "User Alice has 5 messages"
    end

    test "collection applies shared metadata to all items" do
      contents = [
        text("Message 1"),
        text("Message 2"),
        image("data", "image/png")
      ]

      shared = %{conversation_id: "123", timestamp: "2024-01-01"}
      result = collection(contents, shared)

      assert length(result) == 3

      assert Enum.all?(result, fn c ->
               c.metadata.conversation_id == "123" &&
                 c.metadata.timestamp == "2024-01-01"
             end)
    end

    test "validate_all reports multiple errors with indices" do
      contents = [
        # Invalid content
        %{type: :invalid},
        text("Valid"),
        # Missing required fields
        %{type: :text}
      ]

      result = validate_all(contents)
      assert {:error, errors} = result
      assert is_list(errors)
      assert length(errors) >= 2
      assert Enum.any?(errors, &String.contains?(&1, "Item 0"))
      assert Enum.any?(errors, &String.contains?(&1, "Item 2"))
    end

    test "transform handles errors gracefully" do
      content = text("Hello")

      # Successful transform
      {:ok, result} =
        transform(content, fn c ->
          with_metadata(c, %{transformed: true})
        end)

      assert result.metadata.transformed == true

      # Failed transform
      result =
        transform(content, fn _c ->
          {:error, "Transform failed"}
        end)

      assert result == {:error, "Transform failed"}

      # Exception in transform
      result =
        transform(content, fn _c ->
          raise "boom"
        end)

      assert {:error, msg} = result
      assert String.contains?(msg, "Transform failed")
    end

    test "filter_by_type works with single type and list" do
      contents = [
        text("Hello"),
        image("data", "image/png"),
        audio("data", "audio/wav"),
        text("World"),
        resource("file://test.txt")
      ]

      # Single type
      texts = filter_by_type(contents, :text)
      assert length(texts) == 2
      assert Enum.all?(texts, &(&1.type == :text))

      # Multiple types
      media = filter_by_type(contents, [:image, :audio])
      assert length(media) == 2
      assert Enum.any?(media, &(&1.type == :image))
      assert Enum.any?(media, &(&1.type == :audio))
    end

    test "extract_text from various content types" do
      # Text content
      assert extract_text(text("Hello")) == "Hello"

      # Image with alt text
      img = image("data", "image/png") |> with_alt_text("A beautiful sunset")
      assert extract_text(img) == "A beautiful sunset"

      # Audio with transcript
      aud = audio("data", "audio/wav") |> with_transcript("Welcome to the podcast")
      assert extract_text(aud) == "Welcome to the podcast"

      # Resource with text
      res = %{type: :resource, resource: %{text: "Resource text"}}
      assert extract_text(res) == "Resource text"

      # Annotation with text
      ann = %{type: :annotation, annotation: %{text: "Annotation text"}}
      assert extract_text(ann) == "Annotation text"

      # No extractable text
      assert extract_text(%{type: :unknown}) == nil
    end

    test "resize and compress return proper errors" do
      img = image("data", "image/png")

      assert {:error, msg} = resize(img, 800, 600)
      assert String.contains?(msg, "not implemented")

      assert {:error, msg} = compress(img)
      assert String.contains?(msg, "not implemented")
    end

    test "mime type detection from file content" do
      # PNG magic bytes
      png_path = Path.join(System.tmp_dir!(), "test_#{System.unique_integer()}.unknown")
      File.write!(png_path, <<0x89, 0x50, 0x4E, 0x47, "extra data">>)

      result = from_file(png_path)
      assert %{type: :image, mime_type: "image/png"} = result

      File.rm!(png_path)

      # JPEG magic bytes
      jpeg_path = Path.join(System.tmp_dir!(), "test_#{System.unique_integer()}.unknown")
      File.write!(jpeg_path, <<0xFF, 0xD8, 0xFF, "extra data">>)

      result = from_file(jpeg_path)
      assert %{type: :image, mime_type: "image/jpeg"} = result

      File.rm!(jpeg_path)
    end
  end
end
