defmodule ExMCP.DSL.Tool do
  @moduledoc """
  DSL for defining MCP tools with an Elixir-native schema syntax.

  Provides the `deftool` macro that compiles to proper JSON Schema
  while offering a clean Elixir syntax for defining tool schemas.
  """

  @doc """
  Defines a tool with its schema and metadata.

  ## Examples

      # Design-compliant syntax (recommended)
      deftool "say_hello" do
        description "Says hello to a given name"
        input_schema %{
          type: "object",
          properties: %{name: %{type: "string"}},
          required: ["name"]
        }
      end

      # Alternative with Elixir-native schema (also compliant)
      deftool "calculate_sum" do
        description "Adds two numbers together"
        
        args do
          field :a, :number, required: true, description: "First number"
          field :b, :number, required: true, description: "Second number"
        end
      end
      
      # Legacy syntax (deprecated but supported)
      deftool "legacy_tool" do
        tool_description "Legacy description syntax"  # Deprecated - use description/1
        args do
          field :data, :string, required: true
        end
      end
  """
  defmacro deftool(name, do: body) do
    quote do
      @__tool_name__ unquote(name)
      @__tool_opts__ []

      unquote(body)

      # Register the tool in the module's metadata
      @__tools__ Map.put(
                   Module.get_attribute(__MODULE__, :__tools__) || %{},
                   unquote(name),
                   %{
                     name: unquote(name),
                     description: Module.get_attribute(__MODULE__, :__tool_description__),
                     input_schema:
                       Module.get_attribute(__MODULE__, :__tool_input_schema__) ||
                         ExMCP.DSL.Tool.__compile_schema__(
                           Module.get_attribute(__MODULE__, :__tool_fields__) || []
                         ),
                     annotations: Module.get_attribute(__MODULE__, :__tool_annotations__) || %{}
                   }
                 )

      # Clean up temporary attributes
      Module.delete_attribute(__MODULE__, :__tool_name__)
      Module.delete_attribute(__MODULE__, :__tool_description__)
      Module.delete_attribute(__MODULE__, :__tool_input_schema__)
      Module.delete_attribute(__MODULE__, :__tool_fields__)
      Module.delete_attribute(__MODULE__, :__tool_annotations__)
    end
  end

  @doc """
  Sets the description for the current tool (design-compliant syntax).
  """
  defmacro description(desc) do
    quote do
      @__tool_description__ unquote(desc)
    end
  end

  @doc """
  Sets the description for the current tool (deprecated syntax).
  """
  defmacro tool_description(desc) do
    quote do
      require Logger
      Logger.warning("tool_description/1 is deprecated. Use description/1 instead.")
      @__tool_description__ unquote(desc)
    end
  end

  @doc """
  Sets a raw JSON Schema (escape hatch for complex schemas).
  """
  defmacro input_schema(schema) do
    quote do
      @__tool_input_schema__ unquote(schema)
    end
  end

  @doc """
  Sets annotations for the current tool.
  """
  defmacro tool_annotations(annotations) do
    quote do
      @__tool_annotations__ unquote(annotations)
    end
  end

  @doc """
  Begins an args block for defining tool arguments.
  """
  defmacro args(do: body) do
    quote do
      @__tool_fields__ []
      unquote(body)
    end
  end

  @doc """
  Defines a field within an args block.

  ## Options

  - `:required` - Whether the field is required (default: false)
  - `:description` - Human-readable description
  - `:default` - Default value
  - `:min` - Minimum value (for numbers)
  - `:max` - Maximum value (for numbers)
  - `:pattern` - Regex pattern (for strings)

  ## Types

  - `:string` - String type
  - `:number` - Number type (integer or float)
  - `:integer` - Integer type
  - `:boolean` - Boolean type
  - `:object` - Nested object (can contain do block)
  - `{:array, type}` - Array of specified type
  """
  defmacro field(name, type, opts \\ [])

  # Handle the case where do block is passed in opts
  defmacro field(name, type, opts) when is_list(opts) do
    case Keyword.pop(opts, :do) do
      {nil, clean_opts} ->
        # No do block, regular field
        quote do
          field_def = %{
            name: unquote(name),
            type: unquote(type),
            opts: unquote(clean_opts)
          }

          @__tool_fields__ [field_def | Module.get_attribute(__MODULE__, :__tool_fields__) || []]
        end

      {body, clean_opts} ->
        # Has do block, handle as nested object
        quote do
          # Temporarily store parent fields
          parent_fields = Module.get_attribute(__MODULE__, :__tool_fields__) || []

          # Clear for nested collection
          Module.put_attribute(__MODULE__, :__tool_fields__, [])

          # Execute nested field definitions
          unquote(body)

          # Get nested fields
          nested_fields = Module.get_attribute(__MODULE__, :__tool_fields__) || []

          # Restore parent fields and add the object field with nested
          Module.put_attribute(__MODULE__, :__tool_fields__, parent_fields)

          field_def = %{
            name: unquote(name),
            type: unquote(type),
            opts: unquote(clean_opts),
            nested_fields: Enum.reverse(nested_fields)
          }

          Module.put_attribute(__MODULE__, :__tool_fields__, [
            field_def | Module.get_attribute(__MODULE__, :__tool_fields__) || []
          ])
        end
    end
  end

  @doc """
  Compiles Elixir field definitions to JSON Schema.

  This is a public function so it can be called at compile time
  and also tested independently.
  """
  def __compile_schema__(fields) when is_list(fields) do
    properties =
      fields
      |> Enum.reverse()
      |> Enum.map(&compile_field/1)
      |> Enum.into(%{})

    required_fields =
      fields
      |> Enum.reverse()
      |> Enum.filter(fn field -> Keyword.get(field.opts, :required, false) end)
      |> Enum.map(fn field -> to_string(field.name) end)

    schema = %{
      "type" => "object",
      "properties" => properties
    }

    if length(required_fields) > 0 do
      Map.put(schema, "required", required_fields)
    else
      schema
    end
  end

  # Compile a single field definition to JSON Schema
  defp compile_field(%{name: name, type: type, opts: opts} = field) do
    json_name = to_string(name)
    base_schema = type_to_schema(type)

    schema =
      base_schema
      |> maybe_add_description(opts)
      |> maybe_add_default(opts)
      |> maybe_add_constraints(type, opts)
      |> maybe_add_nested_properties(field)

    {json_name, schema}
  end

  # Convert Elixir types to JSON Schema types
  defp type_to_schema(:string), do: %{"type" => "string"}
  defp type_to_schema(:number), do: %{"type" => "number"}
  defp type_to_schema(:integer), do: %{"type" => "integer"}
  defp type_to_schema(:boolean), do: %{"type" => "boolean"}
  defp type_to_schema(:object), do: %{"type" => "object"}

  defp type_to_schema({:array, item_type}) do
    %{
      "type" => "array",
      "items" => type_to_schema(item_type)
    }
  end

  # Add optional description
  defp maybe_add_description(schema, opts) do
    case Keyword.get(opts, :description) do
      nil -> schema
      desc -> Map.put(schema, "description", desc)
    end
  end

  # Add optional default value
  defp maybe_add_default(schema, opts) do
    case Keyword.get(opts, :default) do
      nil -> schema
      default -> Map.put(schema, "default", default)
    end
  end

  # Add type-specific constraints
  defp maybe_add_constraints(schema, :string, opts) do
    schema
    |> maybe_add_pattern(opts)
  end

  defp maybe_add_constraints(schema, type, opts) when type in [:number, :integer] do
    schema
    |> maybe_add_minimum(opts)
    |> maybe_add_maximum(opts)
  end

  defp maybe_add_constraints(schema, _type, _opts), do: schema

  defp maybe_add_pattern(schema, opts) do
    case Keyword.get(opts, :pattern) do
      nil -> schema
      pattern -> Map.put(schema, "pattern", pattern)
    end
  end

  defp maybe_add_minimum(schema, opts) do
    case Keyword.get(opts, :min) do
      nil -> schema
      min -> Map.put(schema, "minimum", min)
    end
  end

  defp maybe_add_maximum(schema, opts) do
    case Keyword.get(opts, :max) do
      nil -> schema
      max -> Map.put(schema, "maximum", max)
    end
  end

  # Add nested object properties
  defp maybe_add_nested_properties(schema, %{nested_fields: nested_fields})
       when is_list(nested_fields) do
    nested_schema = __compile_schema__(nested_fields)

    Map.put(schema, "properties", nested_schema["properties"])
    |> maybe_add_required_from_nested(nested_schema)
  end

  defp maybe_add_nested_properties(schema, _field), do: schema

  # Add required fields from nested schema if any
  defp maybe_add_required_from_nested(schema, nested_schema) do
    case nested_schema["required"] do
      nil -> schema
      [] -> schema
      required_fields -> Map.put(schema, "required", required_fields)
    end
  end
end
