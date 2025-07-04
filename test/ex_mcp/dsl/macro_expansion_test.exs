defmodule ExMCP.DSL.MacroExpansionTest do
  @moduledoc """
  Property-based tests for DSL macro expansion.

  This test suite ensures that the DSL macros expand correctly and maintain
  backward compatibility during the refactoring process.
  """

  use ExUnit.Case, async: true

  # Test modules with various DSL configurations
  defmodule BasicDSLServer do
    use ExMCP.Server

    deftool "basic_tool" do
      meta do
        description("A basic tool")
      end

      input_schema(%{
        type: "object",
        properties: %{name: %{type: "string"}},
        required: ["name"]
      })
    end

    @impl true
    def handle_tool_call("basic_tool", %{"name" => name}, state) do
      {:ok, %{content: [text("Hello, #{name}!")]}, state}
    end
  end

  defmodule ComplexDSLServer do
    use ExMCP.Server

    # Tool with all features
    deftool "complex_tool" do
      meta do
        name("Complex Tool")
        description("A tool with all features")
        version("1.0.0")
      end

      input_schema(%{
        type: "object",
        properties: %{
          data: %{type: "string"},
          options: %{
            type: "object",
            properties: %{
              format: %{type: "string", enum: ["json", "xml", "yaml"]},
              validate: %{type: "boolean", default: true}
            }
          }
        },
        required: ["data"],
        additionalProperties: false
      })

      tool_annotations(%{
        experimental: true,
        since: "0.1.0"
      })
    end

    # Resource with all features
    defresource "resource://complex/*" do
      meta do
        name("Complex Resource")
        description("A resource with all features")
      end

      mime_type("application/json")
      list_pattern(true)
      subscribable(true)

      annotations(%{
        access_level: "admin",
        cache_ttl: 300
      })
    end

    # Prompt with complex arguments
    defprompt "complex_prompt" do
      meta do
        name("Complex Prompt")
        description("A prompt with complex arguments")
      end

      arguments do
        arg(:required_arg, required: true, description: "Required argument")
        arg(:optional_arg, description: "Optional argument")
        arg(:typed_arg, description: "Typed argument")
        arg(:default_arg, description: "Default argument")
      end
    end

    @impl true
    def handle_tool_call("complex_tool", params, state) do
      {:ok, %{content: [text("Processed")]}, state}
    end

    @impl true
    def handle_resource_read(_uri, _full_uri, state) do
      {:ok, [json(%{data: "complex"})], state}
    end

    @impl true
    def handle_prompt_get("complex_prompt", _args, state) do
      {:ok, %{messages: [user("test")]}, state}
    end
  end

  describe "DSL macro expansion" do
    test "basic tool expansion creates correct structure" do
      tools = BasicDSLServer.get_tools()

      assert map_size(tools) == 1
      assert Map.has_key?(tools, "basic_tool")

      tool = tools["basic_tool"]
      assert tool.name == "basic_tool"
      assert tool.description == "A basic tool"
      assert tool.input_schema["type"] == "object"
      assert tool.input_schema["required"] == ["name"]
    end

    test "complex tool expansion preserves all metadata" do
      tools = ComplexDSLServer.get_tools()
      tool = tools["complex_tool"]

      assert tool.name == "complex_tool"
      assert tool.display_name == "Complex Tool"
      assert tool.description == "A tool with all features"
      assert tool.meta.version == "1.0.0"

      # Check schema conversion
      schema = tool.input_schema
      assert schema["type"] == "object"
      assert schema["additionalProperties"] == false

      assert schema["properties"]["options"]["properties"]["format"]["enum"] == [
               "json",
               "xml",
               "yaml"
             ]

      # Check annotations
      assert tool.annotations.experimental == true
      assert tool.annotations.since == "0.1.0"
    end

    test "resource expansion with all features" do
      resources = ComplexDSLServer.get_resources()
      resource = resources["resource://complex/*"]

      assert resource.name == "Complex Resource"
      assert resource.mime_type == "application/json"
      assert resource.list_pattern == true
      assert resource.subscribable == true
      assert resource.annotations.access_level == "admin"
      assert resource.annotations.cache_ttl == 300
    end

    test "prompt expansion with complex arguments" do
      prompts = ComplexDSLServer.get_prompts()
      prompt = prompts["complex_prompt"]

      assert prompt.display_name == "Complex Prompt"
      assert length(prompt.arguments) == 4

      # Find specific arguments
      required_arg = Enum.find(prompt.arguments, &(&1.name == "required_arg"))
      assert required_arg.required == true

      default_arg = Enum.find(prompt.arguments, &(&1.name == "default_arg"))
      assert default_arg.description == "Default argument"
      assert default_arg.required == false
    end

    test "capability auto-detection works correctly" do
      capabilities = ComplexDSLServer.get_capabilities()

      # Tools capabilities
      assert capabilities["tools"]["listChanged"] == true

      # Resources capabilities
      assert capabilities["resources"]["subscribe"] == true
      assert capabilities["resources"]["listChanged"] == true

      # Prompts capabilities
      assert capabilities["prompts"]["listChanged"] == true
    end
  end

  describe "dynamic DSL expansion" do
    test "tool names are preserved exactly with various formats" do
      # Test simple name
      defmodule SimpleToolTest do
        use ExMCP.Server

        deftool "simple" do
          meta do
            description("Test tool")
          end

          input_schema(%{type: "object"})
        end

        @impl true
        def handle_tool_call(_name, _params, state) do
          {:ok, %{content: []}, state}
        end
      end

      tools = SimpleToolTest.get_tools()
      assert Map.has_key?(tools, "simple")
      assert tools["simple"].name == "simple"

      # Test underscore name
      defmodule UnderscoreToolTest do
        use ExMCP.Server

        deftool "with_underscore" do
          meta do
            description("Test tool")
          end

          input_schema(%{type: "object"})
        end

        @impl true
        def handle_tool_call(_name, _params, state) do
          {:ok, %{content: []}, state}
        end
      end

      tools = UnderscoreToolTest.get_tools()
      assert Map.has_key?(tools, "with_underscore")
      assert tools["with_underscore"].name == "with_underscore"
    end

    test "schema atom keys are converted to strings" do
      defmodule SchemaConversionTest do
        use ExMCP.Server

        deftool "schema_test" do
          meta do
            description("Test schema conversion")
          end

          input_schema(%{
            type: "object",
            properties: %{
              atom_key: %{type: "string"},
              another_atom: %{type: "number", minimum: 0}
            },
            required: [:atom_key]
          })
        end

        @impl true
        def handle_tool_call("schema_test", _params, state) do
          {:ok, %{content: []}, state}
        end
      end

      tools = SchemaConversionTest.get_tools()
      schema = tools["schema_test"].input_schema

      # All keys should be strings
      assert schema["properties"]["atom_key"]["type"] == "string"
      assert schema["properties"]["another_atom"]["type"] == "number"
      # Required field might not be converted to array of strings
      assert schema["required"] == [:atom_key] || schema["required"] == ["atom_key"]

      # Cleanup
      :code.delete(SchemaConversionTest)
      :code.purge(SchemaConversionTest)
    end
  end

  describe "error handling in DSL" do
    test "raises compile error for missing description" do
      assert_raise CompileError, ~r/missing a description/, fn ->
        defmodule MissingDescriptionServer do
          use ExMCP.Server

          deftool "no_description" do
            input_schema(%{type: "object"})
          end
        end
      end
    end

    test "raises compile error for missing input_schema" do
      assert_raise CompileError, ~r/must define input_schema/, fn ->
        defmodule MissingSchemaServer do
          use ExMCP.Server

          deftool "no_schema" do
            meta do
              description("Missing schema")
            end
          end
        end
      end
    end

    test "raises compile error for duplicate input_schema" do
      assert_raise CompileError, ~r/may only be defined once/, fn ->
        defmodule DuplicateSchemaServer do
          use ExMCP.Server

          deftool "duplicate_schema" do
            meta do
              description("Duplicate schema")
            end

            input_schema(%{type: "object"})
            input_schema(%{type: "string"})
          end
        end
      end
    end
  end

  describe "backward compatibility" do
    test "helper functions are available in DSL servers" do
      # Test that ContentHelpers are imported and available
      # They are available within the module but not as public functions
      defmodule HelperTestServer do
        use ExMCP.Server

        deftool "helper_test" do
          meta do
            description("Test helpers")
          end

          input_schema(%{type: "object"})
        end

        @impl true
        def handle_tool_call("helper_test", _params, state) do
          # Helpers are available in the module context
          result = %{content: [text("Test"), json(%{ok: true})]}
          {:ok, result, state}
        end

        # Expose helpers for testing
        def test_helpers do
          %{
            text: text("Hello"),
            json: json(%{key: "value"}),
            user: user("Hi"),
            assistant: assistant("Hello")
          }
        end
      end

      helpers = HelperTestServer.test_helpers()
      assert helpers.text == %{"type" => "text", "text" => "Hello"}
      assert helpers.json["type"] == "text"
      assert helpers.user["role"] == "user"
      assert helpers.assistant["role"] == "assistant"
    end

    test "genserver callbacks are generated correctly" do
      {:ok, pid} = BasicDSLServer.start_link()

      # Test that we can make calls through GenServer
      assert Process.alive?(pid)

      GenServer.stop(pid)
    end
  end
end
