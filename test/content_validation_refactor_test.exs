defmodule ExMCP.ContentValidationRefactorTest do
  use ExUnit.Case, async: true

  alias ExMCP.Content.{SchemaValidator, Sanitizer, Transformer, SecurityScanner}
  alias ExMCP.Content.ValidationRefactored, as: Validation

  describe "SchemaValidator" do
    test "validates required fields for text content" do
      assert :ok = SchemaValidator.validate_required_fields(%{type: :text, text: "hello"})

      assert {:error, [error]} = SchemaValidator.validate_required_fields(%{type: :text})
      assert error.rule == :required_fields
      assert error.message =~ "cannot be empty"
    end

    test "validates content size limits" do
      large_text = String.duplicate("a", 1001)
      content = %{type: :text, text: large_text}

      assert {:error, [error]} = SchemaValidator.validate_max_size(content, 1000)
      assert error.rule == :max_size
      assert error.value == 1001
    end

    test "validates MIME types" do
      content = %{type: :image, mime_type: "image/png"}

      assert :ok = SchemaValidator.validate_mime_types(content, ["image/png", "image/jpeg"])

      assert {:error, [error]} = SchemaValidator.validate_mime_types(content, ["image/jpeg"])
      assert error.rule == :mime_types
    end
  end

  describe "Sanitizer" do
    test "escapes HTML entities" do
      assert "&lt;script&gt;" = Sanitizer.html_escape("<script>")
      assert "&amp;amp;" = Sanitizer.html_escape("&amp;")
    end

    test "strips script tags" do
      html = ~s[<p>Hello <script>alert('xss')</script>world</p>]
      assert ~s[<p>Hello world</p>] = Sanitizer.strip_scripts(html)
    end

    test "sanitizes content with multiple operations" do
      content = %{type: :text, text: "<script>alert('xss')</script>"}

      result = Sanitizer.sanitize(content, [:strip_scripts, :html_escape])
      assert result.text == ""
    end

    test "sanitizes file paths" do
      assert "home/user/file.txt" = Sanitizer.sanitize_path("../../../home/user/file.txt")
      assert "etc/passwd" = Sanitizer.sanitize_path("/etc/passwd")
    end
  end

  describe "Transformer" do
    test "normalizes whitespace" do
      text = "  Hello   \n\n\n  World  \t\t"
      assert "Hello\n\nWorld" = Transformer.normalize_whitespace(text)
    end

    test "transforms content with operations" do
      content = %{type: :text, text: "  HELLO  "}

      {:ok, result} = Transformer.transform(content, [:normalize_whitespace])
      assert result.text == "HELLO"
    end

    test "extracts text from HTML" do
      html_content = %{type: :html, text: "<p>Hello <b>world</b></p>"}

      {:ok, text} = Transformer.extract_text(html_content)
      assert text == "Hello world"
    end
  end

  describe "SecurityScanner" do
    test "detects sensitive data patterns" do
      content = %{type: :text, text: "My SSN is 123-45-6789 and credit card 4111111111111111"}

      threats = SecurityScanner.detect_sensitive_data(content)

      assert Enum.any?(threats, &(&1.type == :sensitive_data_ssn))
      assert Enum.any?(threats, &(&1.type == :sensitive_data_credit_card))
    end

    test "scans for injection attacks" do
      content = %{type: :text, text: "'; DROP TABLE users; --"}

      threats = SecurityScanner.scan_injection_attacks(content)

      assert Enum.any?(threats, &(&1.type == :injection_attack_sql_injection))
    end

    test "calculates overall threat level" do
      safe_content = %{type: :text, text: "Hello world"}
      dangerous_content = %{type: :text, text: "My private key: -----BEGIN RSA PRIVATE KEY-----"}

      {:ok, %{threat_level: :safe}} =
        SecurityScanner.scan_security(safe_content, [:sensitive_data])

      {:ok, %{threat_level: :critical}} =
        SecurityScanner.scan_security(dangerous_content, [:sensitive_data])
    end
  end

  describe "ValidationRefactored backward compatibility" do
    test "validate function works with rules list" do
      content = %{type: :text, text: "Hello"}

      assert :ok = Validation.validate(content, [:required_fields, {:max_size, 1000}])

      assert {:error, errors} = Validation.validate(content, [{:max_size, 3}])
      assert length(errors) == 1
    end

    test "batch validation works" do
      contents = [
        %{type: :text, text: "Hello"},
        # Invalid
        %{type: :text, text: ""},
        %{type: :text, text: "World"}
      ]

      assert {:error, errors} = Validation.validate_batch(contents, [:required_fields])
      # Second item failed
      assert Map.has_key?(errors, 1)
    end

    test "delegated functions work correctly" do
      content = %{type: :text, text: "<script>"}

      # Test delegation to Sanitizer
      sanitized = Validation.sanitize(content, [:strip_scripts])
      assert sanitized.text == ""

      # Test delegation to Transformer  
      {:ok, transformed} = Validation.transform(content, [:normalize_whitespace])
      assert transformed.text == "<script>"

      # Test delegation to SecurityScanner
      {:ok, scan_result} = Validation.scan_security(content, [:injection_attacks])
      assert scan_result.threat_level in [:safe, :low, :medium, :high, :critical]
    end

    test "custom validators can be registered" do
      :ok = Validation.register_validator(:always_fail, fn _ -> {:error, "Always fails"} end)

      content = %{type: :text, text: "Hello"}
      assert {:error, [error]} = Validation.validate(content, [:always_fail])
      assert error.message == "Always fails"
    end
  end
end
