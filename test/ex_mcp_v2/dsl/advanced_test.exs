defmodule ExMCP.DSL.AdvancedTest do
  use ExUnit.Case, async: true
  @moduletag :skip

  alias ExMCP.DSL.Advanced

  describe "module usage" do
    test "using the module imports necessary functions" do
      defmodule TestModule do
        use ExMCP.DSL.Advanced

        def test_imports do
          # These should be available after use
          {:module, _} = Code.ensure_compiled(ExMCP.DSL.Tool)
          {:module, _} = Code.ensure_compiled(ExMCP.DSL.Resource)
          {:module, _} = Code.ensure_compiled(ExMCP.DSL.Prompt)
          :ok
        end
      end

      assert TestModule.test_imports() == :ok
    end
  end

  describe "validate_enhanced_input/2" do
    test "validates input successfully" do
      input = %{"name" => "test", "value" => 42}
      schema = %{}

      assert {:ok, ^input} = Advanced.validate_enhanced_input(input, schema)
    end

    test "returns input unchanged for now" do
      # Current implementation just passes through
      input = %{"any" => "data", "nested" => %{"value" => true}}
      assert {:ok, ^input} = Advanced.validate_enhanced_input(input, %{})
    end
  end

  describe "apply_middleware/3" do
    test "applies middleware to request" do
      middleware = []
      request = %{"method" => "test", "params" => %{}}
      context = %{}

      assert {:ok, ^request} = Advanced.apply_middleware(middleware, request, context)
    end

    test "returns request unchanged for now" do
      # Current implementation just passes through
      middleware = [{SomeMiddleware, opt: true}]
      request = %{"data" => "test"}
      context = %{"user" => "alice"}

      assert {:ok, ^request} = Advanced.apply_middleware(middleware, request, context)
    end
  end

  describe "validate_output/2" do
    test "validates output successfully" do
      output = %{"result" => "success"}
      schema = %{}

      assert {:ok, ^output} = Advanced.validate_output(output, schema)
    end

    test "returns output unchanged for now" do
      # Current implementation just passes through
      output = [%{"type" => "text", "text" => "Hello"}]
      schema = %{"contents" => [:text]}

      assert {:ok, ^output} = Advanced.validate_output(output, schema)
    end
  end

  describe "type definitions" do
    test "annotation types are properly defined" do
      # These are type specs, so we just verify the module compiles
      assert {:module, ExMCP.DSL.Advanced} = Code.ensure_compiled(ExMCP.DSL.Advanced)
    end
  end

  describe "DSL macro integration" do
    test "deftool macro can be used with advanced features" do
      defmodule AdvancedToolExample do
        use ExMCP.DSL.Advanced
        import ExMCP.DSL.Tool, only: []
        import ExMCP.DSL.Advanced, only: [deftool: 2]

        @middleware [
          {TestMiddleware, max: 100}
        ]

        deftool "advanced_example" do
          description("An example with advanced features")

          @annotation(:category, "test")
          @annotation(:complexity, "low")

          input do
            field(:name, :string, required: true)
            field(:count, :integer, default: 1)
          end

          def handle(args, _context) do
            "Hello #{args.name}, count: #{args.count}"
          end
        end
      end

      # Verify the module compiles
      assert {:module, AdvancedToolExample} = Code.ensure_compiled(AdvancedToolExample)
    end

    test "annotations macro works" do
      defmodule AnnotationExample do
        use ExMCP.DSL.Advanced
        import ExMCP.DSL.Tool, only: []
        import ExMCP.DSL.Advanced, only: [deftool: 2]

        deftool "annotated_tool" do
          description("Tool with annotations")

          annotations(%{
            version: "1.0.0",
            author: "test@example.com",
            tags: ["utility", "simple"]
          })

          def handle(_args, _context) do
            "Result"
          end
        end
      end

      assert {:module, AnnotationExample} = Code.ensure_compiled(AnnotationExample)
    end

    test "performance hints can be set" do
      defmodule PerformanceExample do
        use ExMCP.DSL.Advanced
        import ExMCP.DSL.Tool, only: []
        import ExMCP.DSL.Advanced, only: [deftool: 2]

        deftool "fast_tool" do
          description("A fast tool")

          performance(%{
            expected_duration: :fast,
            cpu_intensive: false,
            memory_usage: :low,
            cache_ttl: 300
          })

          def handle(_args, _context) do
            "Fast result"
          end
        end
      end

      assert {:module, PerformanceExample} = Code.ensure_compiled(PerformanceExample)
    end

    test "security requirements can be configured" do
      defmodule SecurityExample do
        use ExMCP.DSL.Advanced
        import ExMCP.DSL.Tool, only: []
        import ExMCP.DSL.Advanced, only: [deftool: 2]

        deftool "secure_tool" do
          description("A secure tool")

          security(
            required_permissions: ["read:data"],
            sanitize_inputs: true,
            rate_limit: {10, :minute}
          )

          def handle(_args, _context) do
            "Secure result"
          end
        end
      end

      assert {:module, SecurityExample} = Code.ensure_compiled(SecurityExample)
    end

    test "examples can be defined" do
      defmodule ExampleTool do
        use ExMCP.DSL.Advanced
        import ExMCP.DSL.Tool, only: []
        import ExMCP.DSL.Advanced, only: [deftool: 2]

        deftool "calculator" do
          description("Calculator with examples")

          examples([
            %{
              name: "Addition",
              input: %{op: "add", a: 1, b: 2},
              output: 3
            },
            %{
              name: "Division by zero",
              input: %{op: "divide", a: 10, b: 0},
              expected_error: "Division by zero"
            }
          ])

          example("Multiplication",
            input: %{op: "multiply", a: 4, b: 5},
            output: 20
          )

          def handle(%{op: op, a: a, b: b}, _context) do
            case op do
              "add" -> a + b
              "multiply" -> a * b
              "divide" when b == 0 -> {:error, "Division by zero"}
              "divide" -> a / b
            end
          end
        end
      end

      assert {:module, ExampleTool} = Code.ensure_compiled(ExampleTool)
    end

    test "output schema can be defined" do
      defmodule OutputSchemaExample do
        use ExMCP.DSL.Advanced
        import ExMCP.DSL.Tool, only: []
        import ExMCP.DSL.Advanced, only: [deftool: 2]

        deftool "data_processor" do
          description("Processes data with structured output")

          output do
            content(:text, description: "Main result")
            content(:number, optional: true, description: "Score")

            output_annotation(:metadata, %{
              processing_time: :number,
              record_count: :integer
            })
          end

          def handle(_args, _context) do
            [
              %{type: "text", text: "Processed"},
              %{type: "number", value: 95.5},
              %{
                type: "annotation",
                name: "metadata",
                data: %{
                  processing_time: 1.23,
                  record_count: 42
                }
              }
            ]
          end
        end
      end

      assert {:module, OutputSchemaExample} = Code.ensure_compiled(OutputSchemaExample)
    end

    test "cache configuration works" do
      defmodule CacheExample do
        use ExMCP.DSL.Advanced
        import ExMCP.DSL.Tool, only: []
        import ExMCP.DSL.Advanced, only: [deftool: 2]

        deftool "cached_search" do
          description("Search with caching")

          cache(ttl: 600, key: [:query], vary_by: [:user_id])

          def handle(%{query: query}, _context) do
            "Results for: #{query}"
          end
        end
      end

      assert {:module, CacheExample} = Code.ensure_compiled(CacheExample)
    end

    test "tool middleware can be configured" do
      defmodule MiddlewareExample do
        use ExMCP.DSL.Advanced
        import ExMCP.DSL.Tool, only: []
        import ExMCP.DSL.Advanced, only: [deftool: 2]

        deftool "middleware_tool" do
          description("Tool with middleware")

          tool_middleware([
            {ValidateMiddleware, strict: true},
            CacheMiddleware,
            {RateLimitMiddleware, max: 50}
          ])

          def handle(_args, _context) do
            "Result"
          end
        end
      end

      assert {:module, MiddlewareExample} = Code.ensure_compiled(MiddlewareExample)
    end
  end

  describe "global configuration" do
    test "global middleware can be set" do
      defmodule GlobalMiddlewareExample do
        use ExMCP.DSL.Advanced

        @middleware [
          {AuthMiddleware, required: true},
          LoggingMiddleware
        ]

        deftool "tool1" do
          description("First tool")
          def handle(_, _), do: "Result 1"
        end

        deftool "tool2" do
          description("Second tool")
          def handle(_, _), do: "Result 2"
        end
      end

      assert {:module, GlobalMiddlewareExample} = Code.ensure_compiled(GlobalMiddlewareExample)
    end

    test "global annotations can be set" do
      defmodule GlobalAnnotationsExample do
        use ExMCP.DSL.Advanced

        @global_annotations %{
          api_version: "2.0",
          namespace: "com.example"
        }

        deftool "namespaced_tool" do
          description("Tool with global annotations")
          def handle(_, _), do: "Result"
        end
      end

      assert {:module, GlobalAnnotationsExample} = Code.ensure_compiled(GlobalAnnotationsExample)
    end

    test "validation rules can be configured" do
      defmodule ValidationRulesExample do
        use ExMCP.DSL.Advanced

        @validation_rules [
          :sanitize_html,
          {:max_input_size, 1024},
          {CustomValidator, level: :strict}
        ]

        deftool "validated_tool" do
          description("Tool with validation rules")
          def handle(_, _), do: "Result"
        end
      end

      assert {:module, ValidationRulesExample} = Code.ensure_compiled(ValidationRulesExample)
    end
  end

  describe "enhanced input schema" do
    test "input fields support advanced validation" do
      defmodule EnhancedInputExample do
        use ExMCP.DSL.Advanced
        import ExMCP.DSL.Tool, only: []
        import ExMCP.DSL.Advanced, only: [deftool: 2]

        deftool "form_processor" do
          description("Processes form data")

          input do
            field(:email, :string,
              required: true,
              format: :email,
              sensitive: true
            )

            field(:age, :integer, min: 18, max: 120)

            field(:tags, {:array, :string}, min_items: 1, max_items: 10, unique_items: true)

            field(:status, :string, enum: ["active", "pending", "inactive"])

            field(:bio, :string, min_length: 10, max_length: 500, sanitize: :markdown)
          end

          def handle(args, _context) do
            "Processed: #{args.email}"
          end
        end
      end

      assert {:module, EnhancedInputExample} = Code.ensure_compiled(EnhancedInputExample)
    end

    test "nested object fields work" do
      defmodule NestedInputExample do
        use ExMCP.DSL.Advanced
        import ExMCP.DSL.Tool, only: []
        import ExMCP.DSL.Advanced, only: [deftool: 2]

        deftool "user_manager" do
          description("Manages user data")

          input do
            field :user, :object do
              field(:name, :string, required: true)
              field(:email, :string, format: :email)

              field :preferences, :object do
                field(:theme, :string, enum: ["light", "dark"])
                field(:notifications, :boolean, default: true)
              end
            end
          end

          def handle(%{user: user}, _context) do
            "User: #{user.name}"
          end
        end
      end

      assert {:module, NestedInputExample} = Code.ensure_compiled(NestedInputExample)
    end
  end

  describe "custom content types" do
    test "custom content types can be defined" do
      defmodule CustomContentExample do
        use ExMCP.DSL.Advanced

        defcontent :chart do
          type(:image)
          mime_types(["image/svg+xml", "image/png"])
          metadata([:title, :x_axis, :y_axis])

          def build(data, opts \\ []) do
            %{
              type: "image",
              mime_type: "image/svg+xml",
              data: "<svg>#{data}</svg>",
              metadata: opts
            }
          end
        end

        deftool "chart_generator" do
          description("Generates charts")

          output do
            content(:chart, description: "Generated chart")
          end

          def handle(_args, _context) do
            "Chart data"
          end
        end
      end

      assert {:module, CustomContentExample} = Code.ensure_compiled(CustomContentExample)
    end
  end

  describe "compilation helpers" do
    test "compile_enhanced_input_schema returns empty map" do
      assert Advanced.compile_enhanced_input_schema(TestModule) == %{}
    end

    test "compile_middleware_pipeline returns empty list" do
      assert Advanced.compile_middleware_pipeline([], %{}) == []
    end

    test "compile_annotations returns empty map" do
      assert Advanced.compile_annotations(%{}) == %{}
    end

    test "cleanup_tool_attributes returns nil" do
      assert Advanced.cleanup_tool_attributes(TestModule) == nil
    end
  end
end
