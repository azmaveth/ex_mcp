defmodule ExMCP.Content.SchemaValidator do
  @moduledoc """
  Schema validation for MCP content.

  > #### Experimental {: .warning}
  >
  > Helper utilities outside the core Handler/DSL path. Prefer tool
  > `output_schema` on `ExMCP.Server.DSL` for structured tool results.

  Field/size/MIME helpers are implemented. `validate_schema/2` uses
  `ExJsonSchema` when available.
  """

  alias ExMCP.Content.Protocol

  @type validation_error :: %{
          rule: atom(),
          message: String.t(),
          field: String.t() | nil,
          value: any(),
          severity: :error | :warning | :info
        }

  @type validation_result :: :ok | {:error, [validation_error()]}

  @doc """
  Validates content against a JSON Schema using ExJsonSchema.

  Content maps are converted to JSON-compatible string-keyed maps (atoms
  become strings) before validation.
  """
  @spec validate_schema(Protocol.content() | map(), map()) :: validation_result()
  def validate_schema(content, schema) when is_map(content) and is_map(schema) do
    if Code.ensure_loaded?(ExJsonSchema) do
      data = json_compatible(content)

      try do
        resolved =
          schema
          |> json_compatible()
          |> ExJsonSchema.Schema.resolve()

        case ExJsonSchema.Validator.validate(resolved, data) do
          :ok ->
            :ok

          {:error, errors} ->
            {:error, Enum.map(List.wrap(errors), &schema_error/1)}
        end
      rescue
        e ->
          {:error,
           [
             %{
               rule: :json_schema,
               message: "Schema validation failed: #{Exception.message(e)}",
               field: nil,
               value: nil,
               severity: :error
             }
           ]}
      end
    else
      {:error,
       [
         %{
           rule: :json_schema,
           message: "ExJsonSchema is not available",
           field: nil,
           value: nil,
           severity: :error
         }
       ]}
    end
  end

  defp schema_error({path, message}) when is_binary(message) do
    %{
      rule: :json_schema,
      message: message,
      field: path_to_field(path),
      value: nil,
      severity: :error
    }
  end

  defp schema_error(other) do
    %{
      rule: :json_schema,
      message: inspect(other),
      field: nil,
      value: nil,
      severity: :error
    }
  end

  defp path_to_field(path) when is_list(path), do: Enum.map_join(path, ".", &to_string/1)
  defp path_to_field(path) when is_binary(path), do: path
  defp path_to_field(_), do: nil

  defp json_compatible(map) when is_map(map) do
    Map.new(map, fn {key, value} ->
      {json_key(key), json_compatible(value)}
    end)
  end

  defp json_compatible(list) when is_list(list), do: Enum.map(list, &json_compatible/1)
  defp json_compatible(value) when is_atom(value), do: Atom.to_string(value)
  defp json_compatible(value), do: value

  defp json_key(key) when is_atom(key), do: Atom.to_string(key)
  defp json_key(key) when is_binary(key), do: key
  defp json_key(key), do: to_string(key)

  @doc """
  Validates that required fields are present and non-empty.
  """
  @spec validate_required_fields(Protocol.content()) :: validation_result()
  def validate_required_fields(%{type: :text, text: text}) when is_binary(text) and text != "",
    do: :ok

  def validate_required_fields(%{type: :text}),
    do:
      {:error,
       [
         %{
           rule: :required_fields,
           message: "Text content cannot be empty",
           field: "text",
           value: nil,
           severity: :error
         }
       ]}

  def validate_required_fields(%{type: :image, data: data, mime_type: mime})
      when is_binary(data) and is_binary(mime) and data != "" and mime != "",
      do: :ok

  def validate_required_fields(%{type: :image}),
    do:
      {:error,
       [
         %{
           rule: :required_fields,
           message: "Image content requires data and mime_type",
           field: nil,
           value: nil,
           severity: :error
         }
       ]}

  def validate_required_fields(_), do: :ok

  @doc """
  Validates content size against maximum limits.
  """
  @spec validate_max_size(Protocol.content(), pos_integer()) :: validation_result()
  def validate_max_size(%{type: :text, text: text}, max_size) do
    if byte_size(text) <= max_size do
      :ok
    else
      {:error,
       [
         %{
           rule: :max_size,
           message: "Text exceeds maximum size of #{max_size} bytes",
           field: "text",
           value: byte_size(text),
           severity: :error
         }
       ]}
    end
  end

  def validate_max_size(%{type: type, data: data}, max_size) when type in [:image, :audio] do
    if byte_size(data) <= max_size do
      :ok
    else
      {:error,
       [
         %{
           rule: :max_size,
           message: "#{type} data exceeds maximum size of #{max_size} bytes",
           field: "data",
           value: byte_size(data),
           severity: :error
         }
       ]}
    end
  end

  def validate_max_size(_, _), do: :ok

  @doc """
  Validates MIME types against allowed types.
  """
  @spec validate_mime_types(Protocol.content(), [String.t()]) :: validation_result()
  def validate_mime_types(%{mime_type: mime_type}, allowed_types) when is_binary(mime_type) do
    if mime_type in allowed_types do
      :ok
    else
      {:error,
       [
         %{
           rule: :mime_types,
           message:
             "MIME type #{mime_type} not allowed. Allowed types: #{inspect(allowed_types)}",
           field: "mime_type",
           value: mime_type,
           severity: :error
         }
       ]}
    end
  end

  def validate_mime_types(_, _), do: :ok

  @doc """
  Validates content format and structure.
  """
  @spec validate_format(Protocol.content()) :: validation_result()
  def validate_format(%{type: type} = content) do
    case type do
      :text ->
        validate_text_format(content)

      :image ->
        validate_image_format(content)

      :audio ->
        validate_audio_format(content)

      :video ->
        validate_video_format(content)

      _ ->
        {:error,
         [
           %{
             rule: :format,
             message: "Unknown content type: #{type}",
             field: "type",
             value: type,
             severity: :error
           }
         ]}
    end
  end

  defp validate_text_format(%{text: text}) when is_binary(text), do: :ok

  defp validate_text_format(_),
    do:
      {:error,
       [
         %{
           rule: :format,
           message: "Text content must have a string 'text' field",
           field: "text",
           value: nil,
           severity: :error
         }
       ]}

  defp validate_image_format(%{data: data, mime_type: mime})
       when is_binary(data) and is_binary(mime),
       do: :ok

  defp validate_image_format(_),
    do:
      {:error,
       [
         %{
           rule: :format,
           message: "Image content must have 'data' and 'mime_type' fields",
           field: nil,
           value: nil,
           severity: :error
         }
       ]}

  defp validate_audio_format(%{data: data, mime_type: mime})
       when is_binary(data) and is_binary(mime),
       do: :ok

  defp validate_audio_format(_),
    do:
      {:error,
       [
         %{
           rule: :format,
           message: "Audio content must have 'data' and 'mime_type' fields",
           field: nil,
           value: nil,
           severity: :error
         }
       ]}

  defp validate_video_format(%{url: url}) when is_binary(url), do: :ok

  defp validate_video_format(%{data: data, mime_type: mime})
       when is_binary(data) and is_binary(mime),
       do: :ok

  defp validate_video_format(_),
    do:
      {:error,
       [
         %{
           rule: :format,
           message: "Video content must have either 'url' or 'data' and 'mime_type'",
           field: nil,
           value: nil,
           severity: :error
         }
       ]}
end
