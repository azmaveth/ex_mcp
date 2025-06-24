defmodule ExMCP.DSL.Tool do
  @moduledoc """
  Simplified DSL for defining MCP tools.

  Provides the `deftool` macro for defining tools with metadata and JSON Schema.
  """

  require Logger
  alias ExMCP.DSL.Meta

  @doc """
  Defines a tool with its schema and metadata.

  ## Examples

      deftool "say_hello" do
        meta do
          name "Hello Tool"
          description "Says hello to a given name"
          version "1.0.0"
        end

        input_schema %{
          type: "object",
          properties: %{name: %{type: "string"}},
          required: ["name"]
        }
      end
  """
  defmacro deftool(name, do: body) do
    quote do
      # Import meta DSL functions
      import Meta, only: [meta: 1]

      # Clear any previous meta attributes
      Meta.clear_meta(__MODULE__)

      @__tool_name__ unquote(name)

      unquote(body)

      # Get accumulated meta and validate
      tool_meta = Meta.get_meta(__MODULE__)

      # Validate the tool definition before registering
      # credo:disable-for-next-line Credo.Check.Design.AliasUsage
      ExMCP.DSL.Tool.__validate_tool_definition__(
        unquote(name),
        tool_meta,
        Module.get_attribute(__MODULE__, :__tool_input_schema__)
      )

      # Register the tool in the module's metadata
      @__tools__ Map.put(
                   Module.get_attribute(__MODULE__, :__tools__) || %{},
                   unquote(name),
                   %{
                     name: unquote(name),
                     display_name: tool_meta[:name],
                     description: tool_meta[:description],
                     input_schema: Module.get_attribute(__MODULE__, :__tool_input_schema__),
                     annotations: Module.get_attribute(__MODULE__, :__tool_annotations__) || %{},
                     meta: tool_meta
                   }
                 )

      # Clean up temporary attributes
      Module.delete_attribute(__MODULE__, :__tool_name__)
      Module.delete_attribute(__MODULE__, :__tool_input_schema__)
      Module.delete_attribute(__MODULE__, :__tool_annotations__)
    end
  end

  @doc """
  Sets a raw JSON Schema for the tool input.
  """
  defmacro input_schema(schema) do
    quote do
      # Check for duplicate input_schema
      if Module.get_attribute(__MODULE__, :__tool_input_schema__) do
        raise CompileError,
          file: __ENV__.file,
          line: __ENV__.line,
          description: "input_schema/1 may only be defined once per tool"
      end

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
  Validates a tool definition at compile time.

  This function is called during the deftool macro expansion to ensure
  the tool definition is complete and valid.
  """
  def __validate_tool_definition__(name, meta, input_schema) do
    # Check for description in meta block
    unless meta[:description] do
      raise CompileError,
        description:
          "Tool #{inspect(name)} is missing a description. Use meta do description \"...\" end to provide one."
    end

    # Must have input_schema
    unless input_schema do
      raise CompileError,
        description: "Tool #{inspect(name)} must define input_schema/1."
    end

    :ok
  end
end
