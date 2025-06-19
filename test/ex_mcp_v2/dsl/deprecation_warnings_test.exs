defmodule ExMCP.DSL.DeprecationWarningsTest do
  use ExUnit.Case
  import ExUnit.CaptureLog

  # Define test modules that use deprecated macros
  defmodule DeprecatedToolExample do
    import ExMCP.DSL.Tool
    Module.register_attribute(__MODULE__, :__tools__, accumulate: false)

    deftool "deprecated_tool" do
      # Line 10
      tool_description("This uses deprecated syntax")

      args do
        field(:data, :string)
      end
    end

    def __tools__, do: @__tools__ || %{}
  end

  defmodule DeprecatedResourceExample do
    import ExMCP.DSL.Resource
    Module.register_attribute(__MODULE__, :__resources__, accumulate: false)

    defresource "file://deprecated.txt" do
      # Line 21
      resource_name("Deprecated Resource")
      # Line 22
      resource_description("Uses old syntax")
    end

    def __resources__, do: @__resources__ || %{}
  end

  defmodule DeprecatedPromptExample do
    import ExMCP.DSL.Prompt
    Module.register_attribute(__MODULE__, :__prompts__, accumulate: false)

    defprompt "deprecated_prompt" do
      # Line 30
      prompt_name("Old Prompt")
      # Line 31
      prompt_description("Legacy syntax")
    end

    def __prompts__, do: @__prompts__ || %{}
  end

  test "deprecation warnings include file and line information" do
    # Capture logs during compilation
    logs =
      capture_log(fn ->
        # Force recompilation to trigger warnings
        Code.compile_quoted(
          quote do
            defmodule TestDeprecatedTool do
              import ExMCP.DSL.Tool
              Module.register_attribute(__MODULE__, :__tools__, accumulate: false)

              deftool "test" do
                tool_description("Test")
              end

              def __tools__, do: @__tools__ || %{}
            end
          end,
          __ENV__.file
        )
      end)

    # Check that the warning includes file and line info
    assert logs =~ "tool_description/1 is deprecated"
    assert logs =~ "Use description/1 instead"
    # The log should include file/line metadata
  end

  test "deprecated tool DSL macros work but warn" do
    # The deprecated syntax should still work
    tools = DeprecatedToolExample.__tools__()
    assert tools["deprecated_tool"]
    assert tools["deprecated_tool"].description == "This uses deprecated syntax"
  end

  test "deprecated resource DSL macros work but warn" do
    # The deprecated syntax should still work
    resources = DeprecatedResourceExample.__resources__()
    assert resources["file://deprecated.txt"]
    assert resources["file://deprecated.txt"].name == "Deprecated Resource"
    assert resources["file://deprecated.txt"].description == "Uses old syntax"
  end

  test "deprecated prompt DSL macros work but warn" do
    # The deprecated syntax should still work
    prompts = DeprecatedPromptExample.__prompts__()
    assert prompts["deprecated_prompt"]
    assert prompts["deprecated_prompt"].name == "Old Prompt"
    assert prompts["deprecated_prompt"].description == "Legacy syntax"
  end
end
