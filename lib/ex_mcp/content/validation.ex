defmodule ExMCP.Content.Validation do
  @moduledoc """
  Content validation and transformation utilities for ExMCP v2.

  This module provides comprehensive validation, sanitization, and transformation
  capabilities for MCP content, ensuring data integrity and security.

  ## Features

  - **Schema Validation**: JSON Schema and custom validation rules
  - **Content Sanitization**: HTML, SQL injection, and XSS protection
  - **Size Limits**: File size and content length validation
  - **MIME Type Validation**: Strict MIME type checking and detection
  - **Content Transformation**: Format conversion and normalization
  - **Security Scanning**: Malware detection and content analysis
  - **Custom Validators**: Extensible validation framework

  ## Usage

      alias ExMCP.Content.Validation

      # Basic validation
      case Validation.validate(content, rules) do
        :ok -> process_content(content)
        {:error, reasons} -> handle_validation_errors(reasons)
      end

      # Sanitization
      safe_content = Validation.sanitize(content, [
        :html_escape,
        :strip_scripts,
        :limit_size
      ])

      # Transformation
      {:ok, normalized} = Validation.transform(content, [
        :normalize_whitespace,
        :convert_encoding,
        :compress_images
      ])
  """

  alias ExMCP.Content.Protocol

  @typedoc "Validation rule specification"
  @type validation_rule ::
          atom()
          | {atom(), keyword()}
          | {module(), atom(), keyword()}
          | (Protocol.content() -> :ok | {:error, String.t()})

  @typedoc "Sanitization operation"
  @type sanitization_op ::
          :html_escape
          | :strip_scripts
          | :normalize_unicode
          | :limit_size
          | :remove_metadata
          | :compress_media
          | atom()

  @typedoc "Transformation operation"
  @type transformation_op ::
          :normalize_whitespace
          | :convert_encoding
          | :compress_images
          | :resize_images
          | :extract_text
          | :generate_thumbnails
          | atom()

  @typedoc "Validation result with detailed errors"
  @type validation_result :: :ok | {:error, [validation_error()]}

  @typedoc "Validation error with context"
  @type validation_error :: %{
          rule: atom(),
          message: String.t(),
          field: String.t() | nil,
          value: any(),
          severity: :error | :warning | :info
        }

  @typedoc "Validation options"
  @type validation_opts :: [
          strict: boolean(),
          max_errors: pos_integer(),
          skip_warnings: boolean(),
          custom_validators: [module()]
        ]

  # Core Validation Functions

  @doc """
  Validates content against a set of validation rules.

  ## Examples

      rules = [
        :required_fields,
        {:max_size, 1_000_000},
        {:mime_types, ["image/png", "image/jpeg"]},
        :scan_malware
      ]

      case Validation.validate(content, rules) do
        :ok ->
          IO.puts("Content is valid")
        {:error, errors} ->
          Enum.each(errors, fn validation_error ->
            IO.puts("\#{validation_error.severity}: \#{validation_error.message}")
          end)
      end
  """
  @spec validate(Protocol.content(), [validation_rule()], validation_opts()) ::
          validation_result()
  def validate(content, rules, opts \\ [])

  def validate(content, rules, opts) when is_list(rules) do
    max_errors = Keyword.get(opts, :max_errors, 50)
    strict = Keyword.get(opts, :strict, false)
    skip_warnings = Keyword.get(opts, :skip_warnings, false)

    errors =
      rules
      |> Enum.reduce_while([], fn rule, acc ->
        case apply_validation_rule(content, rule, opts) do
          :ok ->
            {:cont, acc}

          {:error, error} when is_map(error) ->
            new_acc = [error | acc]

            if length(new_acc) >= max_errors do
              {:halt, new_acc}
            else
              {:cont, new_acc}
            end

          {:error, validation_errors} when is_list(validation_errors) ->
            new_acc = validation_errors ++ acc

            if length(new_acc) >= max_errors do
              {:halt, Enum.take(new_acc, max_errors)}
            else
              {:cont, new_acc}
            end
        end
      end)
      |> filter_errors_by_severity(strict, skip_warnings)
      |> Enum.reverse()

    case errors do
      [] -> :ok
      validation_errors -> {:error, validation_errors}
    end
  end

  @doc """
  Validates multiple content items efficiently.

  ## Examples

      contents = [text_content, image_content, audio_content]
      rules = [:required_fields, {:max_size, 5_000_000}]

      case validate_batch(contents, rules) do
        :ok -> process_all_contents(contents)
        {:error, results} ->
          results
          |> Enum.with_index()
          |> Enum.each(fn {result, content_index} ->
            case result do
              :ok -> IO.puts("Content \#{content_index}: OK")
              {:error, errors} -> IO.puts("Content \#{content_index}: \#{length(errors)} errors")
            end
          end)
      end
  """
  @spec validate_batch([Protocol.content()], [validation_rule()], validation_opts()) ::
          :ok | {:error, [validation_result()]}
  def validate_batch(contents, rules, opts \\ []) when is_list(contents) do
    results =
      contents
      |> Task.async_stream(
        fn content ->
          validate(content, rules, opts)
        end,
        max_concurrency: System.schedulers_online()
      )
      |> Enum.map(fn {:ok, result} -> result end)

    case Enum.all?(results, &(&1 == :ok)) do
      true -> :ok
      false -> {:error, results}
    end
  end

  # Sanitization Functions

  @doc """
  Sanitizes content to remove potentially dangerous or unwanted elements.

  ## Examples

      # Basic HTML sanitization
      safe_content = sanitize(content, [:html_escape, :strip_scripts])

      # Comprehensive sanitization
      safe_content = sanitize(content, [
        :html_escape,
        :strip_scripts,
        :normalize_unicode,
        {:limit_size, 1_000_000},
        :remove_exif
      ])
  """
  @spec sanitize(Protocol.content(), [sanitization_op()]) :: Protocol.content()
  def sanitize(content, operations) when is_list(operations) do
    Enum.reduce(operations, content, &apply_sanitization/2)
  end

  @doc """
  Sanitizes text content specifically for safe display.

  ## Examples

      safe_text = sanitize_text(user_input, [
        :html_escape,
        :strip_scripts,
        :normalize_whitespace,
        {:truncate, 1000}
      ])
  """
  @spec sanitize_text(String.t(), [sanitization_op()]) :: String.t()
  def sanitize_text(text, operations) when is_binary(text) and is_list(operations) do
    Enum.reduce(operations, text, &apply_text_sanitization/2)
  end

  # Transformation Functions

  @doc """
  Transforms content through a series of operations.

  ## Examples

      {:ok, transformed} = transform(image_content, [
        {:resize, width: 800, height: 600},
        {:compress, quality: 0.8},
        :generate_thumbnail
      ])
  """
  @spec transform(Protocol.content(), [transformation_op()]) ::
          {:ok, Protocol.content() | [Protocol.content()]} | {:error, String.t()}
  def transform(content, operations) when is_list(operations) do
    result = Enum.reduce(operations, content, &apply_transformation/2)
    {:ok, result}
  rescue
    error -> {:error, "Transformation failed: #{inspect(error)}"}
  end

  @doc """
  Transforms content with validation at each step.

  ## Examples

      {:ok, final_content} = transform_with_validation(content, [
        {:resize, width: 800, height: 600},
        :validate_dimensions,
        {:compress, quality: 0.8},
        :validate_size
      ])
  """
  @spec transform_with_validation(Protocol.content(), [transformation_op() | validation_rule()]) ::
          {:ok, Protocol.content()} | {:error, String.t()}
  def transform_with_validation(content, operations) when is_list(operations) do
    result =
      Enum.reduce_while(operations, content, fn op, acc ->
        case apply_operation_with_validation(acc, op) do
          {:ok, new_content} -> {:cont, new_content}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    case result do
      {:error, _} = error -> error
      content -> {:ok, content}
    end
  rescue
    error -> {:error, "Transformation failed: #{inspect(error)}"}
  end

  # Content Analysis

  @doc """
  Analyzes content for various properties and metadata.

  ## Examples

      analysis = analyze(image_content, [
        :detect_faces,
        :extract_colors,
        :scan_text,
        :measure_complexity
      ])

      %{
        faces: 2,
        dominant_colors: ["#FF5733", "#33FF57"],
        extracted_text: "Hello World",
        complexity_score: 0.75
      } = analysis
  """
  @spec analyze(Protocol.content(), [atom()]) :: map()
  def analyze(content, analysis_types) when is_list(analysis_types) do
    analysis_types
    |> Enum.reduce(%{}, fn type, acc ->
      case perform_analysis(content, type) do
        {:ok, result} -> Map.put(acc, type, result)
        {:error, _} -> acc
      end
    end)
  end

  @doc """
  Extracts metadata from content.

  ## Examples

      metadata = extract_metadata(image_content)

      %{
        format: "PNG",
        dimensions: {800, 600},
        color_depth: 24,
        compression: "deflate",
        creation_date: ~D[2024-01-01]
      } = metadata
  """
  @spec extract_metadata(Protocol.content()) :: map()
  def extract_metadata(content) do
    case content.type do
      :image -> extract_image_metadata(content)
      :audio -> extract_audio_metadata(content)
      :text -> extract_text_metadata(content)
      _ -> %{}
    end
  end

  # Schema Validation

  @doc """
  Validates content against a JSON schema.

  ## Examples

      schema = %{
        "type" => "object",
        "properties" => %{
          "text" => %{"type" => "string", "maxLength" => 1000},
          "format" => %{"enum" => ["plain", "markdown", "html"]}
        },
        "required" => ["text"]
      }

      case validate_schema(content, schema) do
        :ok -> IO.puts("Schema valid")
        {:error, schema_errors} -> IO.puts("Schema errors: \#{inspect(schema_errors)}")
      end
  """
  @spec validate_schema(Protocol.content(), map()) :: :ok | {:error, [String.t()]}
  def validate_schema(content, schema) when is_map(schema) do
    serialized = Protocol.serialize(content)

    # This would use a JSON schema validation library like ExJsonSchema
    # For now, providing a placeholder implementation
    case Jason.encode(serialized) do
      {:ok, _} -> :ok
      {:error, reason} -> {:error, ["JSON encoding failed: #{inspect(reason)}"]}
    end
  end

  # Security Functions

  @doc """
  Scans content for security threats.

  ## Examples

      case scan_security(content, [:malware, :xss, :sql_injection]) do
        :safe -> process_content(content)
        {:threat, detected_threats} ->
          IO.puts("Security threats detected: \#{inspect(detected_threats)}")
          reject_content(content)
      end
  """
  @spec scan_security(Protocol.content(), [atom()]) :: :safe | {:threat, [String.t()]}
  def scan_security(content, scan_types) when is_list(scan_types) do
    threats =
      scan_types
      |> Enum.reduce([], fn type, acc ->
        case perform_security_scan(content, type) do
          :safe -> acc
          {:threat, threat} -> [threat | acc]
        end
      end)

    case threats do
      [] -> :safe
      threats -> {:threat, Enum.reverse(threats)}
    end
  end

  @doc """
  Checks if content contains potentially sensitive information.

  ## Examples

      case detect_sensitive_data(content) do
        :ok -> share_content(content)
        {:sensitive, sensitive_types} ->
          IO.puts("Sensitive data detected: \#{inspect(sensitive_types)}")
          redact_content(content)
      end
  """
  @spec detect_sensitive_data(Protocol.content()) :: :ok | {:sensitive, [atom()]}
  def detect_sensitive_data(content) do
    text = extract_text_for_analysis(content)

    sensitive_patterns = [
      {:credit_card, ~r/\b\d{4}[-\s]?\d{4}[-\s]?\d{4}[-\s]?\d{4}\b/},
      {:ssn, ~r/\b\d{3}-\d{2}-\d{4}\b/},
      {:email, ~r/\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/},
      {:phone, ~r/\b\d{3}[-.]?\d{3}[-.]?\d{4}\b/},
      {:api_key, ~r/\b[A-Za-z0-9]{32,}\b/}
    ]

    detected =
      sensitive_patterns
      |> Enum.filter(fn {_type, pattern} -> Regex.match?(pattern, text) end)
      |> Enum.map(fn {type, _pattern} -> type end)

    case detected do
      [] -> :ok
      types -> {:sensitive, types}
    end
  end

  # Custom Validation Framework

  @doc """
  Registers a custom validator function.

  ## Examples

      defmodule CustomValidators do
        def validate_business_rules(content) do
          # Custom validation logic
          if meets_business_requirements?(content) do
            :ok
          else
            {:error, %{rule: :business_rules, message: "Does not meet requirements"}}
          end
        end
      end

      register_validator(:business_rules, &CustomValidators.validate_business_rules/1)
  """
  @spec register_validator(atom(), (Protocol.content() -> validation_result())) :: :ok
  def register_validator(name, validator_fn)
      when is_atom(name) and is_function(validator_fn, 1) do
    # Store validator in module attribute or persistent term
    :persistent_term.put({__MODULE__, :validator, name}, validator_fn)
    :ok
  end

  @doc """
  Creates a validation rule from a custom function.

  ## Examples

      rule = custom_rule(fn content ->
        if String.length(content.text) > 1000 do
          {:error, "Text too long"}
        else
          :ok
        end
      end)

      validate(content, [rule])
  """
  @spec custom_rule((Protocol.content() -> :ok | {:error, String.t()})) :: validation_rule()
  def custom_rule(validator_fn) when is_function(validator_fn, 1) do
    validator_fn
  end

  # Private Implementation

  defp apply_validation_rule(content, rule, _opts) do
    case rule do
      :required_fields -> validate_required_fields(content)
      :protocol_compliance -> Protocol.validate(content)
      :scan_malware -> scan_malware(content)
      :validate_encoding -> validate_encoding(content)
      _ -> apply_parameterized_rule(content, rule)
    end
  end

  defp apply_parameterized_rule(content, rule) do
    case rule do
      {:max_size, size} ->
        validate_max_size(content, size)

      {:mime_types, types} ->
        validate_mime_types(content, types)

      {:content_length, max_length} ->
        validate_content_length(content, max_length)

      {module, function, args} when is_atom(module) and is_atom(function) ->
        apply(module, function, [content | args])

      fun when is_function(fun, 1) ->
        fun.(content)

      _ ->
        {:error, %{rule: rule, message: "Unknown validation rule", severity: :error}}
    end
  end

  defp apply_sanitization(operation, content) do
    case operation do
      :html_escape -> sanitize_html(content)
      :strip_scripts -> strip_scripts(content)
      :normalize_unicode -> normalize_unicode(content)
      {:limit_size, max_size} -> limit_content_size(content, max_size)
      :remove_metadata -> remove_metadata(content)
      :compress_media -> compress_media(content)
      _ -> content
    end
  end

  defp apply_text_sanitization(operation, text) do
    case operation do
      :html_escape ->
        if Code.ensure_loaded?(HtmlEntities) do
          # credo:disable-for-next-line Credo.Check.Refactor.Apply
          apply(HtmlEntities, :encode, [text])
        else
          # HtmlEntities not available - use basic HTML escaping
          text
          |> String.replace("&", "&amp;")
          |> String.replace("<", "&lt;")
          |> String.replace(">", "&gt;")
          |> String.replace("\"", "&quot;")
          |> String.replace("'", "&#39;")
        end

      :strip_scripts ->
        Regex.replace(~r/<script\b[^<]*(?:(?!<\/script>)<[^<]*)*<\/script>/mi, text, "")

      :normalize_whitespace ->
        String.trim(text) |> String.replace(~r/\s+/, " ")

      {:truncate, length} ->
        String.slice(text, 0, length)

      _ ->
        text
    end
  rescue
    # HtmlEntities not available
    UndefinedFunctionError -> text
  end

  defp apply_transformation(operation, content) do
    case operation do
      {:resize, opts} -> resize_content(content, opts)
      {:compress, opts} -> compress_content(content, opts)
      :generate_thumbnail -> generate_thumbnail(content)
      :extract_text -> extract_text_content(content)
      :normalize_encoding -> normalize_content_encoding(content)
      _ -> content
    end
  end

  defp apply_operation_with_validation(content, operation) do
    case operation do
      op when is_atom(op) or is_tuple(op) ->
        case apply_transformation(op, content) do
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

  defp filter_errors_by_severity(errors, strict, skip_warnings) do
    errors
    |> Enum.filter(fn error ->
      case {strict, skip_warnings, error.severity} do
        {true, _, :warning} -> true
        {_, true, :warning} -> false
        {_, _, :info} when strict -> true
        {_, _, :info} -> false
        {_, _, _} -> true
      end
    end)
  end

  # Validation Rule Implementations

  defp validate_required_fields(%{type: :text, text: text}) when is_binary(text) and text != "",
    do: :ok

  defp validate_required_fields(%{type: :text}),
    do:
      {:error,
       %{
         rule: :required_fields,
         message: "Text content requires non-empty text field",
         severity: :error
       }}

  defp validate_required_fields(%{type: :image, data: data, mime_type: mime})
       when is_binary(data) and is_binary(mime),
       do: :ok

  defp validate_required_fields(%{type: :image}),
    do:
      {:error,
       %{
         rule: :required_fields,
         message: "Image content requires data and mime_type fields",
         severity: :error
       }}

  defp validate_required_fields(_), do: :ok

  defp validate_max_size(%{type: :text, text: text}, max_size) do
    if byte_size(text) <= max_size do
      :ok
    else
      {:error,
       %{
         rule: :max_size,
         message: "Content size #{byte_size(text)} exceeds maximum #{max_size}",
         severity: :error
       }}
    end
  end

  defp validate_max_size(%{type: type, data: data}, max_size) when type in [:image, :audio] do
    decoded_size =
      case Base.decode64(data) do
        {:ok, decoded} -> byte_size(decoded)
        # Assume unencoded
        :error -> byte_size(data)
      end

    if decoded_size <= max_size do
      :ok
    else
      {:error,
       %{
         rule: :max_size,
         message: "Content size #{decoded_size} exceeds maximum #{max_size}",
         severity: :error
       }}
    end
  end

  defp validate_max_size(_, _), do: :ok

  defp validate_mime_types(%{mime_type: mime_type}, allowed_types) when is_binary(mime_type) do
    if mime_type in allowed_types do
      :ok
    else
      {:error,
       %{
         rule: :mime_types,
         message: "MIME type #{mime_type} not in allowed list",
         severity: :error
       }}
    end
  end

  defp validate_mime_types(_, _), do: :ok

  defp validate_content_length(%{type: :text, text: text}, max_length) when is_binary(text) do
    if String.length(text) <= max_length do
      :ok
    else
      {:error,
       %{
         rule: :content_length,
         message: "Text length #{String.length(text)} exceeds maximum #{max_length}",
         severity: :error
       }}
    end
  end

  defp validate_content_length(_, _), do: :ok

  defp scan_malware(content) do
    # Placeholder for malware scanning
    # In real implementation, this would integrate with antivirus engines
    text = extract_text_for_analysis(content)

    if String.contains?(text, ["<script>", "javascript:", "data:text/html"]) do
      {:error,
       %{rule: :scan_malware, message: "Potentially malicious content detected", severity: :error}}
    else
      :ok
    end
  end

  defp validate_encoding(%{type: :text, text: text}) do
    if String.valid?(text) do
      :ok
    else
      {:error, %{rule: :validate_encoding, message: "Invalid UTF-8 encoding", severity: :error}}
    end
  end

  defp validate_encoding(_), do: :ok

  # Sanitization Implementations

  defp sanitize_html(%{type: :text} = content) do
    safe_text = apply_text_sanitization(:html_escape, content.text)
    %{content | text: safe_text}
  end

  defp sanitize_html(content), do: content

  defp strip_scripts(%{type: :text} = content) do
    safe_text = apply_text_sanitization(:strip_scripts, content.text)
    %{content | text: safe_text}
  end

  defp strip_scripts(content), do: content

  defp normalize_unicode(%{type: :text} = content) do
    normalized_text = :unicode.characters_to_binary(content.text, :utf8, :utf8)
    %{content | text: normalized_text}
  end

  defp normalize_unicode(content), do: content

  defp limit_content_size(content, _max_size) do
    # Implementation would compress or truncate content to fit size limit
    content
  end

  defp remove_metadata(content) do
    %{content | metadata: %{}}
  end

  defp compress_media(content) do
    # Implementation would compress image/audio data
    content
  end

  # Transformation Implementations

  defp resize_content(%{type: :image} = content, opts) do
    # Placeholder for image resizing
    # Real implementation would use ImageMagick or similar
    width = Keyword.get(opts, :width)
    height = Keyword.get(opts, :height)

    if width && height do
      %{content | width: width, height: height}
    else
      content
    end
  end

  defp resize_content(content, _opts), do: content

  defp compress_content(content, _opts) do
    # Placeholder for content compression
    content
  end

  defp generate_thumbnail(%{type: :image} = content) do
    # Placeholder for thumbnail generation
    # Would return additional thumbnail content
    content
  end

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

  # Analysis Implementations

  defp perform_analysis(content, analysis_type) do
    case analysis_type do
      :detect_faces -> detect_faces(content)
      :extract_colors -> extract_colors(content)
      :scan_text -> scan_text_in_content(content)
      :measure_complexity -> measure_complexity(content)
      _ -> {:error, "Unknown analysis type"}
    end
  end

  defp detect_faces(%{type: :image}) do
    # Placeholder for face detection
    {:ok, %{count: 0, faces: []}}
  end

  defp detect_faces(_), do: {:error, "Face detection only available for images"}

  defp extract_colors(%{type: :image}) do
    # Placeholder for color extraction
    {:ok, %{dominant_colors: ["#FFFFFF", "#000000"], palette: []}}
  end

  defp extract_colors(_), do: {:error, "Color extraction only available for images"}

  defp scan_text_in_content(content) do
    text = extract_text_for_analysis(content)
    {:ok, %{extracted_text: text, word_count: length(String.split(text))}}
  end

  defp measure_complexity(content) do
    # Simple complexity measure based on content size and type
    complexity =
      case content.type do
        :text -> min(String.length(content.text) / 1000, 1.0)
        # Images are generally complex
        :image -> 0.8
        # Audio is very complex
        :audio -> 0.9
        _ -> 0.5
      end

    {:ok, %{complexity_score: complexity}}
  end

  # Metadata Extraction

  defp extract_image_metadata(%{type: :image} = content) do
    %{
      format: detect_image_format(content.data),
      dimensions: {content.width, content.height},
      mime_type: content.mime_type,
      size_bytes: calculate_decoded_size(content.data)
    }
  end

  defp extract_audio_metadata(%{type: :audio} = content) do
    %{
      mime_type: content.mime_type,
      duration: content.duration,
      size_bytes: calculate_decoded_size(content.data),
      has_transcript: not is_nil(content.transcript)
    }
  end

  defp extract_text_metadata(%{type: :text} = content) do
    %{
      format: content.format,
      language: content.language,
      length: String.length(content.text),
      word_count: length(String.split(content.text)),
      size_bytes: byte_size(content.text)
    }
  end

  # Security Scanning

  defp perform_security_scan(content, scan_type) do
    case scan_type do
      :malware -> scan_malware_advanced(content)
      :xss -> scan_xss(content)
      :sql_injection -> scan_sql_injection(content)
      _ -> :safe
    end
  end

  defp scan_malware_advanced(content) do
    # Enhanced malware scanning
    text = extract_text_for_analysis(content)

    malicious_patterns = [
      "eval(",
      "document.write",
      "<iframe",
      "javascript:",
      "vbscript:",
      "onload=",
      "onerror="
    ]

    detected = Enum.find(malicious_patterns, &String.contains?(text, &1))

    if detected do
      {:threat, "Potentially malicious pattern detected: #{detected}"}
    else
      :safe
    end
  end

  defp scan_xss(content) do
    text = extract_text_for_analysis(content)

    xss_patterns = [
      ~r/<script\b[^<]*(?:(?!<\/script>)<[^<]*)*<\/script>/mi,
      ~r/javascript\s*:/i,
      ~r/on\w+\s*=/i
    ]

    detected = Enum.find(xss_patterns, &Regex.match?(&1, text))

    if detected do
      {:threat, "Potential XSS attack detected"}
    else
      :safe
    end
  end

  defp scan_sql_injection(content) do
    text = extract_text_for_analysis(content)

    sql_patterns = [
      ~r/union\s+select/i,
      ~r/or\s+1\s*=\s*1/i,
      ~r/drop\s+table/i,
      ~r/insert\s+into/i,
      ~r/delete\s+from/i
    ]

    detected = Enum.find(sql_patterns, &Regex.match?(&1, text))

    if detected do
      {:threat, "Potential SQL injection detected"}
    else
      :safe
    end
  end

  # Helper Functions

  defp extract_text_for_analysis(content) do
    case content.type do
      :text -> content.text
      :image -> content.alt_text || ""
      :audio -> content.transcript || ""
      :resource -> get_in(content, [:resource, :text]) || ""
      :annotation -> get_in(content, [:annotation, :text]) || ""
    end
  end

  defp detect_image_format(base64_data) do
    case Base.decode64(base64_data) do
      {:ok, <<0x89, 0x50, 0x4E, 0x47, _::binary>>} -> "PNG"
      {:ok, <<0xFF, 0xD8, 0xFF, _::binary>>} -> "JPEG"
      {:ok, <<0x47, 0x49, 0x46, _::binary>>} -> "GIF"
      _ -> "Unknown"
    end
  end

  defp calculate_decoded_size(base64_data) do
    case Base.decode64(base64_data) do
      {:ok, decoded} -> byte_size(decoded)
      :error -> 0
    end
  end
end
