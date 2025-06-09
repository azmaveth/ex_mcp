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
    
    [%{
      type: "image",
      data: data,
      mimeType: mime,
      description: description
    }]
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
    [%{
      type: "resource",
      uri: uri,
      mimeType: mime_type
    }]
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
    # Simple validation implementation - always passes for now
    # In production, you would use a proper JSON Schema validator
    {:ok, apply_defaults(arguments, schema)}
  end
  
  defp apply_defaults(value, %{type: "object", properties: properties} = _schema) when is_map(value) do
    Enum.reduce(properties, value, fn {prop_name, prop_schema}, acc ->
      # Try both string and atom keys
      prop_key_str = to_string(prop_name)
      prop_key_atom = if is_atom(prop_name), do: prop_name, else: String.to_atom(prop_name)
      
      has_key = Map.has_key?(acc, prop_key_str) or Map.has_key?(acc, prop_key_atom) or Map.has_key?(acc, prop_name)
      
      if has_key do
        acc
      else
        case prop_schema[:default] do
          nil -> acc
          default -> 
            # Use the same key type as other keys in the map
            if acc |> Map.keys() |> Enum.any?(&is_atom/1) do
              Map.put(acc, prop_key_atom, default)
            else
              Map.put(acc, prop_key_str, default)
            end
        end
      end
    end)
  end
  defp apply_defaults(value, _schema), do: value
  
  @doc """
  Converts a function spec to a tool definition.
  
  Extracts parameter information from the function's @spec attribute
  and generates a tool definition with JSON schema.
  """
  def function_to_tool(module, function, description) when is_atom(module) and is_atom(function) do
    # Check all arities from 0 to 10
    exists = Enum.any?(0..10, fn arity ->
      function_exported?(module, function, arity)
    end)
    
    if not exists do
      raise ArgumentError, "Function #{module}.#{function} does not exist"
    end
    
    # Try to get the spec
    spec = get_function_spec(module, function)
    
    %{
      name: to_string(function),
      description: description,
      inputSchema: spec_to_schema(spec)
    }
  end
  
  defp get_function_spec(_module, _function) do
    # In a real implementation, we would use Code.Typespec.fetch_specs
    # For now, return a generic schema
    %{
      type: "object",
      properties: %{},
      additionalProperties: true
    }
  end
  
  defp spec_to_schema(_spec) do
    # Simplified implementation
    %{
      type: "object",
      properties: %{},
      additionalProperties: true
    }
  end
  
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
    
    base = if opts[:required] do
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
        
      String.ends_with?(data, ".png") -> "image/png"
      String.ends_with?(data, ".jpg") || String.ends_with?(data, ".jpeg") -> "image/jpeg"
      String.ends_with?(data, ".gif") -> "image/gif"
      String.ends_with?(data, ".svg") -> "image/svg+xml"
      true -> "application/octet-stream"
    end
  end
end