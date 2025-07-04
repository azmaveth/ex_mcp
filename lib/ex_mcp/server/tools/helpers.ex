defmodule ExMCP.Server.Tools.Helpers do
  @moduledoc """
  Helper functions for building tool responses and working with schemas.

  This module provides utilities to simplify common patterns when implementing
  MCP tools, including response builders, schema validators, and type converters.
  """

  @doc """
  Creates a simple text response.

  ## Examples

      iex> text_response("Hello, World!")
      [%{type: "text", text: "Hello, World!"}]
  """
  def text_response(text) do
    [%{type: "text", text: text}]
  end

  @doc """
  Creates an error response with text content.

  ## Examples

      iex> error_response("Something went wrong")
      %{content: [%{type: "text", text: "Something went wrong"}], isError: true}
  """
  def error_response(message) do
    %{
      content: text_response(message),
      isError: true
    }
  end

  @doc """
  Creates a response with both text and structured content.

  ## Examples

      iex> structured_response("Operation completed", %{status: "success", count: 42})
      %{
        content: [%{type: "text", text: "Operation completed"}],
        structuredContent: %{status: "success", count: 42}
      }
  """
  def structured_response(text, data) do
    %{
      content: text_response(text),
      structuredContent: data
    }
  end

  @doc """
  Creates an image response.

  ## Examples

      iex> image_response("https://example.com/image.png", "An example image")
      [%{
        type: "image",
        data: "https://example.com/image.png",
        mimeType: "image/png",
        description: "An example image"
      }]
  """
  def image_response(data, description, mime_type \\ nil) do
    mime = mime_type || infer_mime_type(data)

    [
      %{
        type: "image",
        data: data,
        mimeType: mime,
        description: description
      }
    ]
  end

  @doc """
  Creates a resource response.

  ## Examples

      iex> resource_response("file:///path/to/file.txt", "text/plain")
      [%{
        type: "resource",
        uri: "file:///path/to/file.txt",
        mimeType: "text/plain"
      }]
  """
  def resource_response(uri, mime_type) do
    [
      %{
        type: "resource",
        uri: uri,
        mimeType: mime_type
      }
    ]
  end

  @doc """
  Creates a multi-content response with mixed content types.

  ## Examples

      iex> multi_content_response([
      ...>   {:text, "Here is some text"},
      ...>   {:image, "data:image/png;base64,abc123", "A diagram"},
      ...>   {:resource, "file:///doc.pdf", "application/pdf"}
      ...> ])
  """
  def multi_content_response(contents) do
    Enum.map(contents, fn
      {:text, text} ->
        %{type: "text", text: text}

      {:image, data, description} ->
        %{type: "image", data: data, mimeType: infer_mime_type(data), description: description}

      {:resource, uri, mime_type} ->
        %{type: "resource", uri: uri, mimeType: mime_type}
    end)
  end

  @doc """
  Validates arguments against a JSON schema.

  Returns {:ok, validated_args} with defaults applied, or {:error, reason}.

  ## Examples

      iex> schema = %{
      ...>   type: "object",
      ...>   properties: %{
      ...>     name: %{type: "string"},
      ...>     age: %{type: "integer"}
      ...>   },
      ...>   required: ["name"]
      ...> }
      iex> validate_arguments(%{name: "Alice", age: 30}, schema)
      {:ok, %{name: "Alice", age: 30}}
  """
  def validate_arguments(arguments, schema) do
    # Keep string keys to avoid atom exhaustion
    string_keyed_args = stringify_keys(arguments)
    do_validate(string_keyed_args, schema, "#")
  end

  defp do_validate(data, schema, path) do
    with :ok <- validate_type(data, schema, path),
         :ok <- validate_enum(data, schema, path),
         {:ok, validated_data} <- validate_by_type(data, schema, path) do
      {:ok, validated_data}
    else
      {:error, _} = error -> error
    end
  end

  defp validate_by_type(data, schema, path) do
    type = get_data_type(data)

    case type do
      "object" -> validate_object(data, schema, path)
      "array" -> validate_array(data, schema, path)
      "string" -> validate_string(data, schema, path)
      "number" -> validate_number(data, schema, path)
      "integer" -> validate_number(data, schema, path)
      _ -> {:ok, data}
    end
  end

  # Type validation
  defp validate_type(_data, %{} = schema, _path) when not is_map_key(schema, :type), do: :ok

  defp validate_type(data, %{type: type}, path) do
    types = List.wrap(type)
    data_type = get_data_type(data)

    is_valid =
      cond do
        data_type in types -> true
        data_type == "integer" and "number" in types -> true
        true -> false
      end

    if is_valid do
      :ok
    else
      {:error, "#{path}: invalid type (got #{data_type}, expected one of: #{inspect(types)})"}
    end
  end

  defp get_data_type(data) when is_integer(data), do: "integer"
  defp get_data_type(data) when is_float(data), do: "number"
  defp get_data_type(data) when is_number(data), do: "number"
  defp get_data_type(data) when is_binary(data), do: "string"
  defp get_data_type(data) when is_boolean(data), do: "boolean"
  defp get_data_type(data) when is_list(data), do: "array"
  defp get_data_type(data) when is_map(data), do: "object"
  defp get_data_type(data) when is_nil(data), do: "null"

  # Enum validation
  defp validate_enum(_data, %{} = schema, _path) when not is_map_key(schema, :enum), do: :ok

  defp validate_enum(data, %{enum: enum_values}, path) do
    if data in enum_values do
      :ok
    else
      {:error, "#{path}: value #{inspect(data)} is not in enum list"}
    end
  end

  # Object validation
  defp validate_object(data, schema, path) do
    data_with_defaults = apply_object_defaults(data, schema)

    with :ok <- validate_required(data_with_defaults, schema, path),
         {:ok, validated_data} <- validate_properties(data_with_defaults, schema, path),
         :ok <- validate_additional_properties(validated_data, schema, path) do
      {:ok, validated_data}
    else
      {:error, _} = error -> error
    end
  end

  defp apply_object_defaults(data, %{properties: properties}) when is_map(properties) do
    Enum.reduce(properties, data, fn {prop, prop_schema}, acc ->
      prop_key = to_string(prop)

      if !Map.has_key?(acc, prop_key) and Map.has_key?(prop_schema, :default) do
        Map.put(acc, prop_key, prop_schema.default)
      else
        acc
      end
    end)
  end

  defp apply_object_defaults(data, _), do: data

  defp validate_required(_data, %{} = schema, _path) when not is_map_key(schema, :required),
    do: :ok

  defp validate_required(data, %{required: required_props}, path) do
    missing =
      Enum.filter(required_props, fn prop ->
        !Map.has_key?(data, to_string(prop))
      end)

    if Enum.empty?(missing) do
      :ok
    else
      {:error, "#{path}: missing required properties: #{inspect(missing)}"}
    end
  end

  defp validate_properties(data, %{properties: properties}, path) when is_map(properties) do
    Enum.reduce_while(properties, {:ok, data}, fn {prop, prop_schema}, {:ok, current_data} ->
      prop_key = to_string(prop)

      if Map.has_key?(current_data, prop_key) do
        value = Map.get(current_data, prop_key)

        case do_validate(value, prop_schema, "#{path}/#{prop}") do
          {:ok, validated_value} ->
            {:cont, {:ok, Map.put(current_data, prop_key, validated_value)}}

          error ->
            {:halt, error}
        end
      else
        {:cont, {:ok, current_data}}
      end
    end)
  end

  defp validate_properties(data, _, _), do: {:ok, data}

  defp validate_additional_properties(
         data,
         %{additionalProperties: false, properties: props},
         path
       ) do
    allowed_keys = Map.keys(props) |> Enum.map(&to_string/1)
    extra_keys = Map.keys(data) |> Enum.map(&to_string/1) |> Kernel.--(allowed_keys)

    if Enum.empty?(extra_keys) do
      :ok
    else
      {:error, "#{path}: has additional properties: #{inspect(extra_keys)}"}
    end
  end

  defp validate_additional_properties(_, _, _), do: :ok

  # Array validation
  defp validate_array(data, schema, path) do
    with :ok <- validate_min_items(data, schema, path),
         :ok <- validate_max_items(data, schema, path),
         :ok <- validate_unique_items(data, schema, path),
         {:ok, validated_data} <- validate_items(data, schema, path) do
      {:ok, validated_data}
    else
      {:error, _} = error -> error
    end
  end

  defp validate_min_items(_data, %{} = s, _) when not is_map_key(s, :minItems), do: :ok

  defp validate_min_items(data, %{minItems: min}, path) do
    if length(data) >= min, do: :ok, else: {:error, "#{path}: failed minItems constraint"}
  end

  defp validate_max_items(_data, %{} = s, _) when not is_map_key(s, :maxItems), do: :ok

  defp validate_max_items(data, %{maxItems: max}, path) do
    if length(data) <= max, do: :ok, else: {:error, "#{path}: failed maxItems constraint"}
  end

  defp validate_unique_items(_data, %{} = schema, _path)
       when not is_map_key(schema, :uniqueItems),
       do: :ok

  defp validate_unique_items(data, %{uniqueItems: true}, path) do
    if length(Enum.uniq(data)) == length(data),
      do: :ok,
      else: {:error, "#{path}: failed uniqueItems constraint"}
  end

  defp validate_unique_items(_data, _schema, _path), do: :ok

  defp validate_items(data, %{items: item_schema}, path) do
    data
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {item, i}, {:ok, acc} ->
      case do_validate(item, item_schema, "#{path}/#{i}") do
        {:ok, validated_item} -> {:cont, {:ok, [validated_item | acc]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, validated_list} -> {:ok, Enum.reverse(validated_list)}
      error -> error
    end
  end

  defp validate_items(data, _, _), do: {:ok, data}

  # String validation
  defp validate_string(data, schema, path) do
    with :ok <- validate_min_length(data, schema, path),
         :ok <- validate_max_length(data, schema, path),
         :ok <- validate_pattern(data, schema, path),
         :ok <- validate_format(data, schema, path) do
      {:ok, data}
    else
      {:error, _} = error -> error
    end
  end

  defp validate_min_length(_d, %{} = s, _) when not is_map_key(s, :minLength), do: :ok

  defp validate_min_length(data, %{minLength: min}, path) do
    if String.length(data) >= min,
      do: :ok,
      else: {:error, "#{path}: failed minLength constraint"}
  end

  defp validate_max_length(_d, %{} = s, _) when not is_map_key(s, :maxLength), do: :ok

  defp validate_max_length(data, %{maxLength: max}, path) do
    if String.length(data) <= max,
      do: :ok,
      else: {:error, "#{path}: failed maxLength constraint"}
  end

  defp validate_pattern(_d, %{} = s, _) when not is_map_key(s, :pattern), do: :ok

  defp validate_pattern(data, %{pattern: pattern}, path) do
    if Regex.match?(~r/#{pattern}/, data),
      do: :ok,
      else: {:error, "#{path}: failed pattern constraint"}
  end

  defp validate_format(_d, %{} = s, _) when not is_map_key(s, :format), do: :ok

  defp validate_format(data, %{format: "email"}, path) do
    if Regex.match?(~r/^[^\s@]+@[^\s@]+\.[^\s@]+$/, data),
      do: :ok,
      else: {:error, "#{path}: failed format constraint"}
  end

  defp validate_format(data, %{format: _}, _path), do: {:ok, data}

  # Number validation
  defp validate_number(data, schema, path) do
    with :ok <- validate_minimum(data, schema, path),
         :ok <- validate_maximum(data, schema, path),
         :ok <- validate_multiple_of(data, schema, path) do
      {:ok, data}
    else
      {:error, _} = error -> error
    end
  end

  defp validate_minimum(_d, %{} = s, _) when not is_map_key(s, :minimum), do: :ok

  defp validate_minimum(data, %{minimum: min} = schema, path) do
    exclusive = schema[:exclusiveMinimum] == true

    cond do
      exclusive and data > min -> :ok
      !exclusive and data >= min -> :ok
      exclusive -> {:error, "#{path}: failed exclusiveMinimum constraint"}
      true -> {:error, "#{path}: failed minimum constraint"}
    end
  end

  defp validate_maximum(_d, %{} = s, _) when not is_map_key(s, :maximum), do: :ok

  defp validate_maximum(data, %{maximum: max} = schema, path) do
    exclusive = schema[:exclusiveMaximum] == true

    cond do
      exclusive and data < max -> :ok
      !exclusive and data <= max -> :ok
      exclusive -> {:error, "#{path}: failed exclusiveMaximum constraint"}
      true -> {:error, "#{path}: failed maximum constraint"}
    end
  end

  defp validate_multiple_of(_d, %{} = s, _) when not is_map_key(s, :multipleOf), do: :ok

  defp validate_multiple_of(data, %{multipleOf: factor}, path) do
    remainder = rem(data, factor)

    if remainder == 0 or abs(remainder) < 1.0e-9 or abs(factor - remainder) < 1.0e-9 do
      :ok
    else
      {:error, "#{path}: failed multipleOf constraint"}
    end
  end

  # Key conversion helpers
  defp stringify_keys(map) when is_map(map) do
    for {k, v} <- map, into: %{} do
      key = to_string(k)
      {key, stringify_keys(v)}
    end
  end

  defp stringify_keys(list) when is_list(list), do: Enum.map(list, &stringify_keys/1)
  defp stringify_keys(other), do: other

  @doc """
  Generates a string schema with constraints.
  """
  def string_schema(opts \\ []) do
    base = %{type: "string"}

    opts
    |> Enum.reduce(base, fn
      {:min_length, v}, acc -> Map.put(acc, :minLength, v)
      {:max_length, v}, acc -> Map.put(acc, :maxLength, v)
      {:pattern, v}, acc -> Map.put(acc, :pattern, v)
      {:format, v}, acc -> Map.put(acc, :format, v)
      {:enum, v}, acc -> Map.put(acc, :enum, v)
      _, acc -> acc
    end)
  end

  @doc """
  Generates a number schema with constraints.
  """
  def number_schema(opts \\ []) do
    base = %{type: "number"}

    opts
    |> Enum.reduce(base, fn
      {:minimum, v}, acc -> Map.put(acc, :minimum, v)
      {:maximum, v}, acc -> Map.put(acc, :maximum, v)
      {:exclusive_minimum, true}, acc -> Map.put(acc, :exclusiveMinimum, true)
      {:exclusive_maximum, true}, acc -> Map.put(acc, :exclusiveMaximum, true)
      {:multiple_of, v}, acc -> Map.put(acc, :multipleOf, v)
      _, acc -> acc
    end)
  end

  @doc """
  Generates an array schema.
  """
  def array_schema(item_type, opts \\ []) do
    base = %{
      type: "array",
      items: %{type: to_string(item_type)}
    }

    opts
    |> Enum.reduce(base, fn
      {:min_items, v}, acc -> Map.put(acc, :minItems, v)
      {:max_items, v}, acc -> Map.put(acc, :maxItems, v)
      {:unique_items, true}, acc -> Map.put(acc, :uniqueItems, true)
      _, acc -> acc
    end)
  end

  @doc """
  Generates an object schema.
  """
  def object_schema(properties, opts \\ []) do
    base = %{
      type: "object",
      properties: properties
    }

    base =
      if opts[:required] do
        Map.put(base, :required, Enum.map(opts[:required], &to_string/1))
      else
        base
      end

    if opts[:additional_properties] == false do
      Map.put(base, :additionalProperties, false)
    else
      base
    end
  end

  defp infer_mime_type(data) do
    cond do
      String.starts_with?(data, "data:") ->
        # Data URI
        case Regex.run(~r/^data:([^;]+)/, data) do
          [_, mime] -> mime
          _ -> "application/octet-stream"
        end

      String.ends_with?(data, ".png") ->
        "image/png"

      String.ends_with?(data, ".jpg") || String.ends_with?(data, ".jpeg") ->
        "image/jpeg"

      String.ends_with?(data, ".gif") ->
        "image/gif"

      String.ends_with?(data, ".svg") ->
        "image/svg+xml"

      true ->
        "application/octet-stream"
    end
  end
end
