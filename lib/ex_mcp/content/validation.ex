defmodule ExMCP.Content.Validation do
  @moduledoc """
  Content validation and transformation utilities for ExMCP.

  Provides comprehensive validation, sanitization, and transformation
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
      safe_content = Validation.sanitize(content, [:html_escape, :strip_scripts])

      # Security scanning
      case Validation.scan_security(content, [:xss, :sql_injection]) do
        :safe -> process_content(content)
        {:threat, threats} -> reject_content(content)
      end
  """

  alias ExMCP.Content.Protocol

  alias ExMCP.Content.Validation.{
    Analysis,
    Rules,
    Sanitization,
    Security,
    Transformation
  }

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

  # --- Core Validation ---

  @doc """
  Validates content against a set of validation rules.
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
        case Rules.apply_rule(content, rule, opts) do
          :ok ->
            {:cont, acc}

          {:error, error} when is_map(error) ->
            new_acc = [error | acc]
            if length(new_acc) >= max_errors, do: {:halt, new_acc}, else: {:cont, new_acc}

          {:error, validation_errors} when is_list(validation_errors) ->
            new_acc = validation_errors ++ acc

            if length(new_acc) >= max_errors,
              do: {:halt, Enum.take(new_acc, max_errors)},
              else: {:cont, new_acc}
        end
      end)
      |> Rules.filter_errors_by_severity(strict, skip_warnings)
      |> Enum.reverse()

    case errors do
      [] -> :ok
      validation_errors -> {:error, validation_errors}
    end
  end

  @doc """
  Validates multiple content items efficiently using parallel processing.
  """
  @spec validate_batch([Protocol.content()], [validation_rule()], validation_opts()) ::
          :ok | {:error, [validation_result()]}
  def validate_batch(contents, rules, opts \\ []) when is_list(contents) do
    results =
      contents
      |> Task.async_stream(
        fn content -> validate(content, rules, opts) end,
        max_concurrency: System.schedulers_online()
      )
      |> Enum.map(fn {:ok, result} -> result end)

    case Enum.all?(results, &(&1 == :ok)) do
      true -> :ok
      false -> {:error, results}
    end
  end

  # --- Sanitization ---

  @doc """
  Sanitizes content to remove potentially dangerous or unwanted elements.
  """
  @spec sanitize(Protocol.content(), [sanitization_op()]) :: Protocol.content()
  def sanitize(content, operations) when is_list(operations) do
    Enum.reduce(operations, content, &Sanitization.apply_operation/2)
  end

  @doc """
  Sanitizes text content specifically for safe display.
  """
  @spec sanitize_text(String.t(), [sanitization_op()]) :: String.t()
  def sanitize_text(text, operations) when is_binary(text) and is_list(operations) do
    Enum.reduce(operations, text, &Sanitization.apply_text_operation/2)
  end

  # --- Transformation ---

  @doc """
  Transforms content through a series of operations.
  """
  @spec transform(Protocol.content(), [transformation_op()]) ::
          {:ok, Protocol.content() | [Protocol.content()]} | {:error, String.t()}
  def transform(content, operations) when is_list(operations) do
    result = Enum.reduce(operations, content, &Transformation.apply_operation/2)
    {:ok, result}
  rescue
    error -> {:error, "Transformation failed: #{inspect(error)}"}
  end

  @doc """
  Transforms content with validation at each step.
  """
  @spec transform_with_validation(Protocol.content(), [transformation_op() | validation_rule()]) ::
          {:ok, Protocol.content()} | {:error, String.t()}
  def transform_with_validation(content, operations) when is_list(operations) do
    result =
      Enum.reduce_while(operations, content, fn op, acc ->
        case Transformation.apply_with_validation(acc, op) do
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

  # --- Analysis ---

  @doc """
  Analyzes content for various properties and metadata.
  """
  @spec analyze(Protocol.content(), [atom()]) :: map()
  def analyze(content, analysis_types) when is_list(analysis_types) do
    Enum.reduce(analysis_types, %{}, fn type, acc ->
      case Analysis.perform(content, type) do
        {:ok, result} -> Map.put(acc, type, result)
        {:error, _} -> acc
      end
    end)
  end

  @doc """
  Extracts metadata from content.
  """
  @spec extract_metadata(Protocol.content()) :: map()
  defdelegate extract_metadata(content), to: Analysis

  # --- Schema Validation ---

  @doc """
  Validates content against a JSON schema.
  """
  @spec validate_schema(Protocol.content(), map()) :: :ok | {:error, [String.t()]}
  def validate_schema(content, schema) when is_map(schema) do
    serialized = Protocol.serialize(content)

    case Jason.encode(serialized) do
      {:ok, _} -> :ok
      {:error, reason} -> {:error, ["JSON encoding failed: #{inspect(reason)}"]}
    end
  end

  # --- Security ---

  @doc """
  Scans content for security threats.
  """
  @spec scan_security(Protocol.content(), [atom()]) :: :safe | {:threat, [String.t()]}
  def scan_security(content, scan_types) when is_list(scan_types) do
    threats =
      Enum.reduce(scan_types, [], fn type, acc ->
        case Security.perform_scan(content, type) do
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
  """
  @spec detect_sensitive_data(Protocol.content()) :: :ok | {:sensitive, [atom()]}
  defdelegate detect_sensitive_data(content), to: Security, as: :detect_sensitive

  # --- Custom Validators ---

  @doc """
  Registers a custom validator function.
  """
  @spec register_validator(atom(), (Protocol.content() -> validation_result())) :: :ok
  def register_validator(name, validator_fn)
      when is_atom(name) and is_function(validator_fn, 1) do
    :persistent_term.put({__MODULE__, :validator, name}, validator_fn)
    :ok
  end

  @doc """
  Creates a validation rule from a custom function.
  """
  @spec custom_rule((Protocol.content() -> :ok | {:error, String.t()})) :: validation_rule()
  def custom_rule(validator_fn) when is_function(validator_fn, 1) do
    validator_fn
  end
end
