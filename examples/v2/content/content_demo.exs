#!/usr/bin/env elixir

# Content System Demo for ExMCP v2
# 
# This demonstrates the comprehensive content system including:
# - Type-safe content creation
# - Smart builders and file integration
# - Validation and sanitization
# - Advanced DSL features

Mix.install([
  {:ex_mcp, path: "../../../"}
])

defmodule ContentDemo do
  use ExMCP.Content.Builders
  
  alias ExMCP.Content.{Protocol, Builders, Validation}
  
  def run do
    IO.puts("🎨 ExMCP v2 Content System Demo\n")
    
    demonstrate_basic_content()
    IO.puts("\n" <> String.duplicate("=", 50) <> "\n")
    
    demonstrate_builders()
    IO.puts("\n" <> String.duplicate("=", 50) <> "\n")
    
    demonstrate_validation()
    IO.puts("\n" <> String.duplicate("=", 50) <> "\n")
    
    demonstrate_advanced_features()
  end
  
  defp demonstrate_basic_content do
    IO.puts("📝 Basic Content Creation")
    
    # Text content with different formats
    plain_text = Protocol.text("Hello, world!")
    markdown_text = Protocol.text("# Hello Markdown", format: :markdown)
    code_text = Protocol.text("console.log('Hello');", format: :code, language: "javascript")
    
    IO.puts("✅ Plain text: #{plain_text.text}")
    IO.puts("✅ Markdown: #{markdown_text.text} (format: #{markdown_text.format})")
    IO.puts("✅ Code: #{code_text.text} (language: #{code_text.language})")
    
    # Image content
    sample_image_data = Base.encode64("fake image data for demo")
    image = Protocol.image(sample_image_data, "image/png", 
      width: 100, 
      height: 100, 
      alt_text: "Demo image"
    )
    
    IO.puts("✅ Image: #{image.mime_type} #{image.width}x#{image.height}")
    IO.puts("   Alt text: #{image.alt_text}")
    
    # Audio content
    sample_audio_data = Base.encode64("fake audio data for demo")
    audio = Protocol.audio(sample_audio_data, "audio/wav", 
      duration: 5.2, 
      transcript: "Hello from audio"
    )
    
    IO.puts("✅ Audio: #{audio.mime_type} (#{audio.duration}s)")
    IO.puts("   Transcript: #{audio.transcript}")
    
    # Resource reference
    resource = Protocol.resource("file://demo/data.txt", 
      text: "Demo data file", 
      mime_type: "text/plain"
    )
    
    IO.puts("✅ Resource: #{resource.resource.uri}")
    IO.puts("   Description: #{resource.resource.text}")
    
    # Annotation
    annotation = Protocol.annotation("sentiment", 
      confidence: 0.95, 
      text: "positive"
    )
    
    IO.puts("✅ Annotation: #{annotation.annotation.type}")
    IO.puts("   Confidence: #{annotation.annotation.confidence}")
    IO.puts("   Value: #{annotation.annotation.text}")
    
    # Test serialization roundtrip
    IO.puts("\n🔄 Testing serialization roundtrip:")
    serialized = Protocol.serialize(markdown_text)
    {:ok, deserialized} = Protocol.deserialize(serialized)
    
    IO.puts("   Original: #{markdown_text.text}")
    IO.puts("   Roundtrip: #{deserialized.text}")
    IO.puts("   Match: #{deserialized.text == markdown_text.text}")
  end
  
  defp demonstrate_builders do
    IO.puts("🔧 Smart Builders and Chainable API")
    
    # Basic builders
    simple_text = text("Hello from builder!")
    IO.puts("✅ Builder text: #{simple_text.text}")
    
    # Chainable modifiers
    enhanced_text = text("# Important Message")
    |> as_markdown()
    |> with_metadata(%{
      author: "demo",
      priority: "high",
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    })
    
    IO.puts("✅ Enhanced text: #{enhanced_text.text}")
    IO.puts("   Format: #{enhanced_text.format}")
    IO.puts("   Author: #{enhanced_text.metadata.author}")
    IO.puts("   Priority: #{enhanced_text.metadata.priority}")
    
    # Image with modifiers
    sample_data = Base.encode64("demo image data")
    enhanced_image = image(sample_data, "image/jpeg")
    |> with_dimensions(800, 600)
    |> with_alt_text("Product screenshot")
    |> with_metadata(%{category: "marketing", version: "v2"})
    
    IO.puts("✅ Enhanced image: #{enhanced_image.width}x#{enhanced_image.height}")
    IO.puts("   Alt text: #{enhanced_image.alt_text}")
    IO.puts("   Category: #{enhanced_image.metadata.category}")
    
    # Audio with modifiers  
    audio_data = Base.encode64("demo audio data")
    enhanced_audio = audio(audio_data, "audio/mp3")
    |> with_duration(30.5)
    |> with_transcript("Welcome to our demo podcast episode")
    |> with_metadata(%{episode: 1, series: "tech_talks"})
    
    IO.puts("✅ Enhanced audio: #{enhanced_audio.duration}s")
    IO.puts("   Transcript: #{enhanced_audio.transcript}")
    IO.puts("   Episode: #{enhanced_audio.metadata.episode}")
    
    # Template system
    IO.puts("\n📄 Template System:")
    
    template = text("Hello, {{name}}! Welcome to {{platform}}. Your level is {{level}}.")
    personalized = from_template(template, %{
      name: "Alice",
      platform: "ExMCP v2",
      level: 42
    })
    
    IO.puts("✅ Template: #{template.text}")
    IO.puts("✅ Personalized: #{personalized.text}")
    
    # Batch operations
    IO.puts("\n📦 Batch Operations:")
    
    batch_contents = batch([
      text("First message"),
      text("Second message") |> as_markdown(),
      fn -> text("Generated message: #{:rand.uniform(100)}") end,
      image(sample_data, "image/png") |> with_alt_text("Batch image")
    ])
    
    IO.puts("✅ Batch created #{length(batch_contents)} content items:")
    batch_contents
    |> Enum.with_index()
    |> Enum.each(fn {content, index} ->
      case content.type do
        :text -> IO.puts("   #{index + 1}. Text: #{content.text}")
        :image -> IO.puts("   #{index + 1}. Image: #{content.alt_text}")
        _ -> IO.puts("   #{index + 1}. #{content.type}")
      end
    end)
    
    # Collection with shared metadata
    IO.puts("\n👥 Collection with Shared Metadata:")
    
    conversation_contents = collection([
      text("How are you today?"),
      text("I'm doing great, thanks!"),
      text("That's wonderful to hear.")
    ], %{
      conversation_id: "demo_chat_001",
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      participants: ["user", "assistant"]
    })
    
    IO.puts("✅ Conversation with #{length(conversation_contents)} messages")
    IO.puts("   Conversation ID: #{Enum.at(conversation_contents, 0).metadata.conversation_id}")
    IO.puts("   Participants: #{inspect(Enum.at(conversation_contents, 0).metadata.participants)}")
  end
  
  defp demonstrate_validation do
    IO.puts("🔍 Content Validation and Security")
    
    # Basic validation
    valid_content = text("This is safe content")
    case Validation.validate(valid_content, [:required_fields, :protocol_compliance]) do
      :ok -> IO.puts("✅ Valid content passed validation")
      {:error, errors} -> IO.puts("❌ Validation failed: #{inspect(errors)}")
    end
    
    # Size validation
    large_content = text(String.duplicate("x", 1000))
    case Validation.validate(large_content, [{:content_length, 500}]) do
      :ok -> IO.puts("✅ Content within size limit")
      {:error, errors} -> 
        IO.puts("❌ Content too large:")
        Enum.each(errors, fn error -> IO.puts("   - #{error.message}") end)
    end
    
    # MIME type validation
    valid_image = image(Base.encode64("fake"), "image/png")
    invalid_image = image(Base.encode64("fake"), "text/plain")
    
    allowed_types = ["image/png", "image/jpeg", "image/gif"]
    
    case Validation.validate(valid_image, [{:mime_types, allowed_types}]) do
      :ok -> IO.puts("✅ Image MIME type is allowed")
      {:error, _} -> IO.puts("❌ Image MIME type rejected")
    end
    
    case Validation.validate(invalid_image, [{:mime_types, allowed_types}]) do
      :ok -> IO.puts("✅ Image MIME type is allowed")
      {:error, errors} -> 
        IO.puts("❌ Image MIME type rejected:")
        Enum.each(errors, fn error -> IO.puts("   - #{error.message}") end)
    end
    
    # Security scanning
    IO.puts("\n🔒 Security Scanning:")
    
    safe_content = text("This is perfectly safe content")
    case Validation.scan_security(safe_content, [:malware, :xss]) do
      :safe -> IO.puts("✅ Content passed security scan")
      {:threat, threats} -> IO.puts("⚠️ Security threats: #{inspect(threats)}")
    end
    
    suspicious_content = text("Click here: javascript:alert('xss')")
    case Validation.scan_security(suspicious_content, [:malware, :xss]) do
      :safe -> IO.puts("✅ Content passed security scan")
      {:threat, threats} -> 
        IO.puts("⚠️ Security threats detected:")
        Enum.each(threats, fn threat -> IO.puts("   - #{threat}") end)
    end
    
    # Sensitive data detection
    IO.puts("\n🔐 Sensitive Data Detection:")
    
    clean_content = text("This message contains no sensitive information")
    case Validation.detect_sensitive_data(clean_content) do
      :ok -> IO.puts("✅ No sensitive data detected")
      {:sensitive, types} -> IO.puts("⚠️ Sensitive data: #{inspect(types)}")
    end
    
    sensitive_content = text("My email is user@example.com and phone is 555-123-4567")
    case Validation.detect_sensitive_data(sensitive_content) do
      :ok -> IO.puts("✅ No sensitive data detected")
      {:sensitive, types} -> 
        IO.puts("⚠️ Sensitive data detected:")
        Enum.each(types, fn type -> IO.puts("   - #{type}") end)
    end
    
    # Content sanitization
    IO.puts("\n🧹 Content Sanitization:")
    
    unsafe_content = text("<script>alert('xss')</script>Hello <b>world</b>!")
    safe_content = Validation.sanitize(unsafe_content, [:html_escape, :strip_scripts])
    
    IO.puts("   Original: #{unsafe_content.text}")
    IO.puts("   Sanitized: #{safe_content.text}")
    
    # Batch validation
    IO.puts("\n📋 Batch Validation:")
    
    mixed_contents = [
      text("Good content"),
      text(String.duplicate("x", 2000)),  # Too long
      image(Base.encode64("fake"), "image/png"),  # Good
      %{type: :text, text: nil}  # Invalid
    ]
    
    case Validation.validate_batch(mixed_contents, [{:content_length, 1000}, :required_fields]) do
      :ok -> IO.puts("✅ All content passed validation")
      {:error, results} ->
        IO.puts("❌ Batch validation results:")
        results
        |> Enum.with_index()
        |> Enum.each(fn {result, index} ->
          case result do
            :ok -> IO.puts("   #{index + 1}. ✅ Valid")
            {:error, errors} -> 
              IO.puts("   #{index + 1}. ❌ #{length(errors)} error(s)")
              Enum.each(errors, fn error -> IO.puts("      - #{error.message}") end)
          end
        end)
    end
  end
  
  defp demonstrate_advanced_features do
    IO.puts("🚀 Advanced Features")
    
    # Content analysis
    IO.puts("🔬 Content Analysis:")
    
    rich_text = text("Hello world! This is a demo with emojis 🌍 and unicode 世界")
    analysis = Validation.analyze(rich_text, [:scan_text, :measure_complexity])
    
    IO.puts("✅ Text analysis:")
    if Map.has_key?(analysis, :scan_text) do
      scan_result = analysis.scan_text
      IO.puts("   Word count: #{scan_result.word_count}")
      IO.puts("   Extracted: #{String.slice(scan_result.extracted_text, 0, 50)}...")
    end
    
    if Map.has_key?(analysis, :measure_complexity) do
      complexity = analysis.measure_complexity
      IO.puts("   Complexity: #{Float.round(complexity.complexity_score, 2)}")
    end
    
    # Metadata extraction
    IO.puts("\n📊 Metadata Extraction:")
    
    demo_image = image(Base.encode64("fake image data"), "image/jpeg", width: 1920, height: 1080)
    metadata = Validation.extract_metadata(demo_image)
    
    IO.puts("✅ Image metadata:")
    IO.puts("   Format: #{metadata.format}")
    IO.puts("   Dimensions: #{elem(metadata.dimensions, 0)}x#{elem(metadata.dimensions, 1)}")
    IO.puts("   MIME type: #{metadata.mime_type}")
    IO.puts("   Size: #{metadata.size_bytes} bytes")
    
    # Content transformation
    IO.puts("\n🔄 Content Transformation:")
    
    original = text("  Hello   World  \n\n  Extra   Spaces  ")
    {:ok, normalized} = Validation.transform(original, [:normalize_whitespace])
    
    IO.puts("   Original: #{inspect(original.text)}")
    IO.puts("   Normalized: #{inspect(normalized)}")  # Note: transform returns extracted text
    
    # Content filtering
    IO.puts("\n🎯 Content Filtering:")
    
    mixed_content = [
      text("Text message 1"),
      image(Base.encode64("fake"), "image/png") |> with_alt_text("Image 1"),
      text("Text message 2"),
      audio(Base.encode64("fake"), "audio/wav") |> with_transcript("Audio 1"),
      annotation("test") |> with_confidence(0.8)
    ]
    
    text_only = filter_by_type(mixed_content, :text)
    media_only = filter_by_type(mixed_content, [:image, :audio])
    
    IO.puts("✅ Content filtering:")
    IO.puts("   Total items: #{length(mixed_content)}")
    IO.puts("   Text only: #{length(text_only)}")
    IO.puts("   Media only: #{length(media_only)}")
    
    # Text extraction
    IO.puts("\n📝 Text Extraction:")
    
    IO.puts("✅ Text extraction from various content types:")
    Enum.each(mixed_content, fn content ->
      extracted = extract_text(content)
      type_str = content.type |> Atom.to_string() |> String.capitalize()
      
      case extracted do
        nil -> IO.puts("   #{type_str}: (no text)")
        text when is_binary(text) -> IO.puts("   #{type_str}: #{text}")
        _ -> IO.puts("   #{type_str}: #{inspect(extracted)}")
      end
    end)
    
    IO.puts("\n🎉 Content system demo completed!")
    IO.puts("\nThe ExMCP v2 content system provides:")
    IO.puts("• Type-safe content creation and validation")
    IO.puts("• Chainable builders for intuitive API")
    IO.puts("• Comprehensive security and sanitization")
    IO.puts("• Rich metadata and analysis capabilities")
    IO.puts("• File integration with automatic type detection")
    IO.puts("• Template system for dynamic content")
    IO.puts("• Batch operations for efficiency")
  end
end

# Run the demo
ContentDemo.run()