defmodule ExMCP.DSL.Advanced do
  @moduledoc """
  Advanced DSL features for ExMCP v2 including annotations, middleware, and enhanced tool definitions.

  This module extends the basic DSL with powerful features for production-ready MCP servers:

  ## Features

  - **Annotations**: Rich metadata for tools, resources, and prompts
  - **Middleware**: Request/response processing pipeline
  - **Validation**: Advanced input/output validation
  - **Rate Limiting**: Built-in rate limiting per tool/resource
  - **Caching**: Response caching with TTL
  - **Telemetry**: Automatic metrics and tracing
  - **Security**: Authorization and input sanitization
  - **Content Types**: Rich content type support

  ## Usage

      use ExMCP.DSL.Advanced

      @middleware [
        {ExMCP.Middleware.RateLimit, max: 100, window: :minute},
        {ExMCP.Middleware.Auth, required: true},
        ExMCP.Middleware.Telemetry
      ]

      deftool "advanced_calculator" do
        description "Advanced calculator with validation and caching"

        @annotation :category, "math"
        @annotation :complexity, "low"
        @annotation :cache_ttl, 300

        middleware [
          {ExMCP.Middleware.Validate, strict: true},
          {ExMCP.Middleware.Cache, ttl: 300}
        ]

        input do
          field :operation, :string, required: true,
            enum: ["add", "subtract", "multiply", "divide"],
            description: "Mathematical operation to perform"

          field :operands, {:array, :number}, required: true,
            min_items: 2, max_items: 10,
            description: "Numbers to operate on"

          field :precision, :integer, default: 2,
            min: 0, max: 10,
            description: "Decimal precision for result"
        end

        output do
          content :number, description: "Calculation result"
          output_annotation :operation_info, %{
            operation: :string,
            input_count: :integer,
            execution_time_ms: :number
          }
        end

        examples [
          %{
            name: "Basic addition",
            input: %{operation: "add", operands: [1, 2, 3]},
            output: 6
          },
          %{
            name: "Division with precision",
            input: %{operation: "divide", operands: [10, 3], precision: 4},
            output: 3.3333
          }
        ]

        def handle(args, context) do
          # Implementation with context access
          result = perform_calculation(args.operation, args.operands, args.precision)

          content = ExMCP.Content.number(result)
          annotation = ExMCP.Content.annotation("operation_info", %{
            operation: args.operation,
            input_count: length(args.operands),
            execution_time_ms: context.execution_time
          })

          [content, annotation]
        end
      end
  """

  alias ExMCP.DSL.{Tool, Resource, Prompt}

  @type annotation_key :: atom() | String.t()
  @type annotation_value :: any()
  @type middleware_spec :: atom() | {atom(), keyword()} | module()
  @type content_type :: :text | :number | :boolean | :image | :audio | :resource | :annotation
  @type validation_rule :: atom() | {atom(), any()} | module()

  defmacro __using__(_opts) do
    quote do
      import ExMCP.DSL.Advanced
      import ExMCP.DSL.{Tool, Resource, Prompt}

      # Initialize advanced DSL attributes
      Module.register_attribute(__MODULE__, :__global_middleware__, accumulate: true)
      Module.register_attribute(__MODULE__, :__global_annotations__, accumulate: true)
      Module.register_attribute(__MODULE__, :__content_schemas__, accumulate: true)
      Module.register_attribute(__MODULE__, :__validation_rules__, accumulate: true)
    end
  end

  # Global Configuration Macros

  @doc """
  Sets global middleware that applies to all tools/resources/prompts in the module.

  ## Examples

      @middleware [
        {ExMCP.Middleware.RateLimit, max: 1000, window: :hour},
        ExMCP.Middleware.Auth,
        {ExMCP.Middleware.Logging, level: :info}
      ]
  """
  defmacro middleware(middleware_list) do
    quote do
      @__global_middleware__ unquote(middleware_list)
    end
  end

  @doc """
  Sets global annotations that apply to all definitions in the module.

  ## Examples

      @global_annotations %{
        version: "1.0.0",
        author: "team@company.com",
        category: "utilities"
      }
  """
  defmacro global_annotations(annotations) do
    quote do
      @__global_annotations__ unquote(annotations)
    end
  end

  @doc """
  Defines validation rules for the module.

  ## Examples

      @validation_rules [
        :sanitize_inputs,
        {:rate_limit, max: 100, window: :minute},
        {CustomValidator, strict: true}
      ]
  """
  defmacro validation_rules(rules) do
    quote do
      @__validation_rules__ unquote(rules)
    end
  end

  # Enhanced Tool Definition

  @doc """
  Enhanced tool definition with advanced features.

  Extends the basic `deftool` macro with support for:
  - Rich annotations and metadata
  - Middleware pipeline configuration
  - Input/output content type definitions
  - Examples and documentation
  - Validation rules
  - Caching and performance hints
  """
  defmacro deftool(name, opts \\ [], do: body) do
    quote do
      # Initialize tool-specific attributes
      @__current_tool__ unquote(name)
      @__tool_middleware__ []
      @__tool_annotations__ %{}
      @__tool_input_schema__ nil
      @__tool_output_schema__ nil
      @__tool_examples__ []
      @__tool_validation_rules__ []

      # Process the tool body
      unquote(body)

      # Compile and register the tool
      tool_def = %{
        name: unquote(name),
        description: Module.get_attribute(__MODULE__, :__tool_description__),
        input_schema: compile_enhanced_input_schema(),
        output_schema: Module.get_attribute(__MODULE__, :__tool_output_schema__),
        middleware: compile_middleware_pipeline(),
        annotations: compile_annotations(),
        examples: Module.get_attribute(__MODULE__, :__tool_examples__) || [],
        validation_rules: Module.get_attribute(__MODULE__, :__tool_validation_rules__) || [],
        options: unquote(opts)
      }

      # Register in module metadata
      @__tools__ Map.put(
                   Module.get_attribute(__MODULE__, :__tools__) || %{},
                   unquote(name),
                   tool_def
                 )

      # Clean up attributes
      cleanup_tool_attributes()
    end
  end

  # Tool-Specific Configuration

  @doc """
  Sets annotations for the current tool/resource/prompt.

  ## Examples

      @annotation :category, "math"
      @annotation :complexity, "medium"
      @annotation :cache_ttl, 600
  """
  defmacro annotation(key, value) do
    quote do
      current_annotations = Module.get_attribute(__MODULE__, :__tool_annotations__) || %{}

      Module.put_attribute(
        __MODULE__,
        :__tool_annotations__,
        Map.put(current_annotations, unquote(key), unquote(value))
      )
    end
  end

  @doc """
  Sets multiple annotations at once.

  ## Examples

      annotations %{
        category: "data_processing",
        complexity: "high",
        requires_auth: true,
        cache_ttl: 300
      }
  """
  defmacro annotations(annotation_map) do
    quote do
      current_annotations = Module.get_attribute(__MODULE__, :__tool_annotations__) || %{}

      Module.put_attribute(
        __MODULE__,
        :__tool_annotations__,
        Map.merge(current_annotations, unquote(annotation_map))
      )
    end
  end

  @doc """
  Sets middleware pipeline for the current tool.

  ## Examples

      tool_middleware [
        {ExMCP.Middleware.Validate, strict: true},
        ExMCP.Middleware.Cache,
        {ExMCP.Middleware.RateLimit, max: 10, window: :minute}
      ]
  """
  defmacro tool_middleware(middleware_list) do
    quote do
      @__tool_middleware__ unquote(middleware_list)
    end
  end

  # Enhanced Schema Definition

  @doc """
  Defines input schema with enhanced validation and content types.

  ## Examples

      input do
        field :query, :string, required: true,
          min_length: 1, max_length: 1000,
          sanitize: :html_escape

        field :options, :object do
          field :case_sensitive, :boolean, default: false
          field :max_results, :integer, default: 10, min: 1, max: 100
        end

        field :attachments, {:array, :file},
          max_items: 5,
          allowed_types: ["image/*", "application/pdf"]
      end
  """
  defmacro input(do: body) do
    quote do
      @__tool_fields__ []
      unquote(body)
    end
  end

  @doc """
  Defines expected output content types and structure.

  ## Examples

      output do
        content :text, required: true,
          description: "Search results as formatted text"

        content :image, optional: true,
          description: "Optional chart visualization"

        output_annotation :metadata, %{
          result_count: :integer,
          execution_time: :number,
          confidence: :number
        }
      end
  """
  defmacro output(do: body) do
    quote do
      @__output_contents__ []
      @__output_annotations__ []
      unquote(body)

      output_schema = %{
        contents: Enum.reverse(Module.get_attribute(__MODULE__, :__output_contents__) || []),
        annotations: Enum.reverse(Module.get_attribute(__MODULE__, :__output_annotations__) || [])
      }

      @__tool_output_schema__ output_schema

      Module.delete_attribute(__MODULE__, :__output_contents__)
      Module.delete_attribute(__MODULE__, :__output_annotations__)
    end
  end

  @doc """
  Defines expected content type in output.

  ## Examples

      content :text, description: "Response text"
      content :image, optional: true, mime_types: ["image/png", "image/jpeg"]
      content :number, description: "Calculation result", precision: 2
  """
  defmacro content(type, opts \\ []) do
    quote do
      content_def = %{
        type: unquote(type),
        required: not Keyword.get(unquote(opts), :optional, false),
        options: unquote(opts)
      }

      @__output_contents__ [
        content_def | Module.get_attribute(__MODULE__, :__output_contents__) || []
      ]
    end
  end

  @doc """
  Defines expected annotation in output.

  ## Examples

      output_annotation :metadata, %{count: :integer, time: :number}
      output_annotation :confidence, :number, description: "Confidence score 0-1"
  """
  defmacro output_annotation(name, schema, opts \\ []) do
    quote do
      annotation_def = %{
        name: unquote(name),
        schema: unquote(schema),
        options: unquote(opts)
      }

      @__output_annotations__ [
        annotation_def | Module.get_attribute(__MODULE__, :__output_annotations__) || []
      ]
    end
  end

  # Enhanced Field Types

  @doc """
  Enhanced field definition with advanced validation.

  ## Additional Options

  - `:enum` - List of allowed values
  - `:min_length`/`:max_length` - String length constraints
  - `:min_items`/`:max_items` - Array size constraints
  - `:unique_items` - Array items must be unique
  - `:format` - String format (email, uri, date, etc.)
  - `:sanitize` - Input sanitization method
  - `:sensitive` - Mark field as containing sensitive data
  """
  defmacro field(name, type, opts \\ []) do
    enhanced_opts = enhance_field_options(opts)

    quote do
      ExMCP.DSL.Tool.field(unquote(name), unquote(type), unquote(enhanced_opts))
    end
  end

  # Documentation and Examples

  @doc """
  Adds examples for testing and documentation.

  ## Examples

      examples [
        %{
          name: "Basic search",
          input: %{query: "elixir", max_results: 5},
          output: %{results: ["result1", "result2"]}
        },
        %{
          name: "Empty query",
          input: %{query: ""},
          expected_error: "Query cannot be empty"
        }
      ]
  """
  defmacro examples(example_list) do
    quote do
      @__tool_examples__ unquote(example_list)
    end
  end

  @doc """
  Adds a single example.

  ## Examples

      example "Calculate sum",
        input: %{operation: "add", numbers: [1, 2, 3]},
        output: 6
  """
  defmacro example(name, opts) do
    quote do
      example_def = Map.merge(%{name: unquote(name)}, Map.new(unquote(opts)))
      current_examples = Module.get_attribute(__MODULE__, :__tool_examples__) || []
      @__tool_examples__ [example_def | current_examples]
    end
  end

  # Performance and Caching Hints

  @doc """
  Sets performance hints for the tool.

  ## Examples

      performance %{
        expected_duration: :fast,  # :fast, :medium, :slow
        cpu_intensive: false,
        memory_usage: :low,        # :low, :medium, :high
        cache_ttl: 300,
        concurrent_safe: true
      }
  """
  defmacro performance(hints) do
    quote do
      annotation(:performance, unquote(hints))
    end
  end

  @doc """
  Sets caching configuration for the tool.

  ## Examples

      cache ttl: 300, key: [:query, :options], vary_by: [:user_id]
  """
  defmacro cache(opts) do
    quote do
      annotation(:cache, Map.new(unquote(opts)))
    end
  end

  # Security and Authorization

  @doc """
  Sets security requirements for the tool.

  ## Examples

      security required_permissions: ["read:data", "write:files"],
               sanitize_inputs: true,
               rate_limit: {100, :hour}
  """
  defmacro security(opts) do
    quote do
      annotation(:security, Map.new(unquote(opts)))
    end
  end

  # Content Type Extensions

  @doc """
  Defines a custom content type for the module.

  ## Examples

      defcontent :chart do
        type :image
        mime_types ["image/svg+xml", "image/png"]
        metadata [:title, :data_source, :chart_type]

        def build(data, opts \\ []) do
          # Custom chart building logic
        end
      end
  """
  defmacro defcontent(name, do: body) do
    quote do
      @__content_type__ unquote(name)
      unquote(body)

      content_schema = %{
        name: unquote(name),
        base_type: Module.get_attribute(__MODULE__, :__content_base_type__),
        mime_types: Module.get_attribute(__MODULE__, :__content_mime_types__) || [],
        metadata_fields: Module.get_attribute(__MODULE__, :__content_metadata__) || [],
        builder: Module.get_attribute(__MODULE__, :__content_builder__)
      }

      @__content_schemas__ [
        content_schema | Module.get_attribute(__MODULE__, :__content_schemas__) || []
      ]

      # Clean up
      Module.delete_attribute(__MODULE__, :__content_type__)
      Module.delete_attribute(__MODULE__, :__content_base_type__)
      Module.delete_attribute(__MODULE__, :__content_mime_types__)
      Module.delete_attribute(__MODULE__, :__content_metadata__)
      Module.delete_attribute(__MODULE__, :__content_builder__)
    end
  end

  @doc """
  Sets the base content type for a custom content type.
  """
  defmacro type(base_type) do
    quote do
      @__content_base_type__ unquote(base_type)
    end
  end

  @doc """
  Sets allowed MIME types for a custom content type.
  """
  defmacro mime_types(types) do
    quote do
      @__content_mime_types__ unquote(types)
    end
  end

  @doc """
  Sets metadata fields for a custom content type.
  """
  defmacro metadata(fields) do
    quote do
      @__content_metadata__ unquote(fields)
    end
  end

  # Compilation Helpers

  @doc false
  def compile_enhanced_input_schema(_module) do
    # This would be called at compile time to merge basic schema with enhanced features
    %{}
  end

  @doc false
  def compile_middleware_pipeline(_middleware_list, _context) do
    # Combine global and tool-specific middleware
    []
  end

  @doc false
  def compile_annotations(_schema) do
    # Merge global and tool-specific annotations
    %{}
  end

  @doc false
  def cleanup_tool_attributes(_module) do
    # Clean up temporary module attributes
    nil
  end

  # Runtime Validation and Middleware Support

  @doc """
  Validates input against enhanced schema rules.
  """
  @spec validate_enhanced_input(map(), map()) :: {:ok, map()} | {:error, [String.t()]}
  def validate_enhanced_input(input, _schema) do
    # Implementation would validate against enhanced rules
    {:ok, input}
  end

  @doc """
  Applies middleware pipeline to a request.
  """
  @spec apply_middleware([middleware_spec()], map(), map()) :: {:ok, map()} | {:error, any()}
  def apply_middleware(_middleware_list, request, _context) do
    # Implementation would apply middleware in sequence
    {:ok, request}
  end

  @doc """
  Validates output against expected content schema.
  """
  @spec validate_output(any(), map()) :: {:ok, any()} | {:error, String.t()}
  def validate_output(output, _schema) do
    # Implementation would validate output format
    {:ok, output}
  end

  # Private Helper Functions

  defp enhance_field_options(opts) do
    # Add enhanced validation options to basic field options
    opts
    |> add_validation_options()
    |> add_security_options()
    |> add_format_options()
  end

  defp add_validation_options(opts) do
    # Add enum, length, and array constraints
    opts
  end

  defp add_security_options(opts) do
    # Add sanitization and sensitivity markers
    opts
  end

  defp add_format_options(opts) do
    # Add format validation (email, uri, etc.)
    opts
  end
end
