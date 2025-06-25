defmodule ExMCP.Content.ValidationRefactored do
  @moduledoc """
  Unified content validation module that orchestrates the decomposed validation components.

  This module provides the same public API as the original Content.Validation module
  but delegates to specialized modules following Single Responsibility Principle:

  - `SchemaValidator` - Schema and structure validation
  - `Sanitizer` - Content sanitization and cleaning
  - `Transformer` - Content transformation and conversion
  - `SecurityScanner` - Security threat detection

  ## Migration from Content.Validation

  This module provides full backward compatibility. Simply replace:

      alias ExMCP.Content.Validation

  With:

      alias ExMCP.Content.ValidationRefactored, as: Validation
  """

  alias ExMCP.Content.{
    SchemaValidator,
    Sanitizer,
    Transformer,
    SecurityScanner,
    ValidatorRegistry
  }

  alias ExMCP.Content.Protocol

  @type validation_rule ::
          atom()
          | {atom(), keyword()}
          | {module(), atom(), keyword()}
          | (Protocol.content() -> :ok | {:error, String.t()})

  @type validation_opts :: [
          strict: boolean(),
          max_errors: pos_integer(),
          skip_warnings: boolean(),
          custom_validators: [module()]
        ]

  @doc """
  Validates content against a set of validation rules.

  ## Examples

      rules = [
        :required_fields,
        {:max_size, 1_000_000},
        {:mime_types, ["image/png", "image/jpeg"]}
      ]
      
      case Validation.validate(content, rules) do
        :ok -> process_content(content)
        {:error, errors} -> handle_errors(errors)
      end
  """
  @spec validate(Protocol.content(), [validation_rule()], validation_opts()) ::
          :ok | {:error, [SchemaValidator.validation_error()]}
  def validate(content, rules, opts \\ [])

  def validate(content, rules, opts) when is_list(rules) do
    errors =
      Enum.reduce(rules, [], fn rule, acc ->
        case apply_validation_rule(content, rule, opts) do
          :ok -> acc
          {:error, rule_errors} -> acc ++ rule_errors
        end
      end)

    case errors do
      [] -> :ok
      _ -> {:error, filter_errors_by_severity(errors, opts)}
    end
  end

  @doc """
  Validates a batch of contents against rules.
  """
  @spec validate_batch([Protocol.content()], [validation_rule()], validation_opts()) ::
          {:ok, [Protocol.content()]} | {:error, map()}
  def validate_batch(contents, rules, opts \\ []) when is_list(contents) do
    results = Enum.map(contents, &validate(&1, rules, opts))

    errors =
      Enum.with_index(results)
      |> Enum.filter(fn {result, _} -> match?({:error, _}, result) end)
      |> Enum.map(fn {{:error, errors}, index} -> {index, errors} end)
      |> Map.new()

    case map_size(errors) do
      0 -> {:ok, contents}
      _ -> {:error, errors}
    end
  end

  @doc """
  Sanitizes content by applying sanitization operations.

  Delegates to `ExMCP.Content.Sanitizer.sanitize/2`.
  """
  defdelegate sanitize(content, operations), to: Sanitizer

  @doc """
  Sanitizes text content specifically.

  Delegates to `ExMCP.Content.Sanitizer.sanitize_text/2`.
  """
  defdelegate sanitize_text(text, operations), to: Sanitizer

  @doc """
  Transforms content by applying transformation operations.

  Delegates to `ExMCP.Content.Transformer.transform/2`.
  """
  defdelegate transform(content, operations), to: Transformer

  @doc """
  Transforms content with validation after each operation.

  Delegates to `ExMCP.Content.Transformer.transform_with_validation/2`.
  """
  defdelegate transform_with_validation(content, operations), to: Transformer

  @doc """
  Scans content for security threats.

  Delegates to `ExMCP.Content.SecurityScanner.scan_security/2`.
  """
  defdelegate scan_security(content, scan_types), to: SecurityScanner

  @doc """
  Detects sensitive data in content.

  Delegates to `ExMCP.Content.SecurityScanner.detect_sensitive_data/1`.
  """
  defdelegate detect_sensitive_data(content), to: SecurityScanner

  @doc """
  Validates content against a JSON Schema.

  Delegates to `ExMCP.Content.SchemaValidator.validate_schema/2`.
  """
  defdelegate validate_schema(content, schema), to: SchemaValidator

  @doc """
  Analyzes content and returns metadata and insights.
  """
  @spec analyze(Protocol.content(), [atom()]) :: {:ok, map()} | {:error, String.t()}
  def analyze(content, analysis_types) when is_list(analysis_types) do
    results =
      Enum.reduce(analysis_types, %{}, fn type, acc ->
        case analyze_type(content, type) do
          {:ok, result} -> Map.put(acc, type, result)
          {:error, _} -> acc
        end
      end)

    {:ok, results}
  end

  @doc """
  Extracts metadata from content.
  """
  @spec extract_metadata(Protocol.content()) :: map()
  def extract_metadata(content) do
    %{
      type: Map.get(content, :type),
      size: calculate_size(content),
      mime_type: Map.get(content, :mime_type),
      encoding: detect_encoding(content),
      created_at: Map.get(content, :created_at),
      modified_at: Map.get(content, :modified_at)
    }
    |> Enum.filter(fn {_, v} -> v != nil end)
    |> Map.new()
  end

  @doc """
  Registers a custom validator function.
  """
  @spec register_validator(atom(), function()) :: :ok | {:error, term()}
  def register_validator(name, validator_fn)
      when is_atom(name) and is_function(validator_fn, 1) do
    # Use ValidatorRegistry GenServer for thread-safe validator storage
    ValidatorRegistry.register_validator(name, validator_fn)
  end

  @doc """
  Creates a custom validation rule from a function.
  """
  @spec custom_rule(function()) :: validation_rule()
  def custom_rule(validator_fn) when is_function(validator_fn, 1) do
    validator_fn
  end

  # Private helper functions

  defp apply_validation_rule(content, :required_fields, _opts) do
    SchemaValidator.validate_required_fields(content)
  end

  defp apply_validation_rule(content, {:max_size, max_size}, _opts) do
    SchemaValidator.validate_max_size(content, max_size)
  end

  defp apply_validation_rule(content, {:mime_types, allowed_types}, _opts) do
    SchemaValidator.validate_mime_types(content, allowed_types)
  end

  defp apply_validation_rule(content, :format, _opts) do
    SchemaValidator.validate_format(content)
  end

  defp apply_validation_rule(content, {:schema, schema}, _opts) do
    SchemaValidator.validate_schema(content, schema)
  end

  defp apply_validation_rule(content, fun, _opts) when is_function(fun, 1) do
    case fun.(content) do
      true ->
        :ok

      false ->
        {:error,
         [
           %{
             rule: :custom,
             message: "Custom validation failed",
             field: nil,
             value: nil,
             severity: :error
           }
         ]}

      :ok ->
        :ok

      {:error, msg} ->
        {:error, [%{rule: :custom, message: msg, field: nil, value: nil, severity: :error}]}
    end
  end

  defp apply_validation_rule(content, {module, function, args}, _opts) do
    apply(module, function, [content | args])
  end

  defp apply_validation_rule(content, rule_name, _opts) when is_atom(rule_name) do
    # Check for registered custom validator using ValidatorRegistry
    case ValidatorRegistry.get_validator(rule_name) do
      {:error, :not_found} ->
        {:error,
         [
           %{
             rule: rule_name,
             message: "Unknown validation rule: #{rule_name}",
             field: nil,
             value: nil,
             severity: :error
           }
         ]}

      {:ok, validator_fn} ->
        apply_validation_rule(content, validator_fn, [])
    end
  end

  defp filter_errors_by_severity(errors, opts) do
    strict = Keyword.get(opts, :strict, false)
    skip_warnings = Keyword.get(opts, :skip_warnings, false)

    errors
    |> Enum.filter(fn error ->
      case error.severity do
        :error -> true
        :warning -> not skip_warnings
        :info -> strict
      end
    end)
  end

  defp analyze_type(content, :word_count) do
    case content do
      %{type: :text, text: text} ->
        count = text |> String.split(~r/\s+/) |> Enum.count()
        {:ok, count}

      _ ->
        {:error, "Cannot count words in non-text content"}
    end
  end

  defp analyze_type(_content, :language) do
    {:error, "Language detection not implemented - requires external language detection library"}
  end

  defp analyze_type(_content, :sentiment) do
    {:error, "Sentiment analysis not implemented - requires external sentiment analysis library"}
  end

  defp analyze_type(_content, :readability) do
    {:error, "Readability scoring not implemented - requires text analysis algorithms"}
  end

  defp analyze_type(_, _), do: {:error, "Unknown analysis type"}

  defp calculate_size(%{type: :text, text: text}), do: byte_size(text)
  defp calculate_size(%{data: data}) when is_binary(data), do: byte_size(data)
  defp calculate_size(_), do: nil

  defp detect_encoding(%{type: :text, text: text}) when is_binary(text) do
    # Simple UTF-8 detection
    case :unicode.characters_to_binary(text) do
      ^text -> "UTF-8"
      _ -> "unknown"
    end
  end

  defp detect_encoding(_), do: nil
end
