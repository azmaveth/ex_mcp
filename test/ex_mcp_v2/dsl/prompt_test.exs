defmodule ExMCP.DSL.PromptTest do
  use ExUnit.Case, async: true

  alias ExMCP.DSL.Prompt

  describe "validate_arguments/2" do
    setup do
      arguments = [
        %{name: "code", required: true, description: "Code to review"},
        %{name: "language", required: false, description: "Programming language"},
        %{name: "focus", required: false, description: "Review focus"}
      ]

      %{arguments: arguments}
    end

    test "validates arguments with all required fields", %{arguments: arguments} do
      args = %{"code" => "def hello, do: :world", "language" => "elixir"}

      assert Prompt.validate_arguments(args, arguments) == :ok
    end

    test "validates arguments with only required fields", %{arguments: arguments} do
      args = %{"code" => "def hello, do: :world"}

      assert Prompt.validate_arguments(args, arguments) == :ok
    end

    test "returns error for missing required arguments", %{arguments: arguments} do
      args = %{"language" => "elixir", "focus" => "performance"}

      assert Prompt.validate_arguments(args, arguments) ==
               {:error, "Missing required argument: code"}
    end

    test "validates empty arguments when no required fields" do
      arguments = [
        %{name: "optional", required: false, description: "Optional field"}
      ]

      assert Prompt.validate_arguments(%{}, arguments) == :ok
    end

    test "validates arguments when no arguments defined" do
      assert Prompt.validate_arguments(%{"anything" => "value"}, []) == :ok
    end
  end

  describe "arguments_to_mcp_format/1" do
    test "converts argument definitions to MCP format" do
      arguments = [
        %{name: "code", required: true, description: "Code to review"},
        %{name: "language", required: false, description: "Programming language"},
        %{name: "focus", required: false, description: nil}
      ]

      mcp_format = Prompt.arguments_to_mcp_format(arguments)

      assert mcp_format == [
               %{
                 "name" => "code",
                 "required" => true,
                 "description" => "Code to review"
               },
               %{
                 "name" => "language",
                 "required" => false,
                 "description" => "Programming language"
               },
               %{
                 "name" => "focus",
                 "required" => false
               }
             ]
    end

    test "handles empty argument list" do
      assert Prompt.arguments_to_mcp_format([]) == []
    end

    test "handles arguments without descriptions" do
      arguments = [
        %{name: "simple", required: true, description: nil}
      ]

      mcp_format = Prompt.arguments_to_mcp_format(arguments)

      assert mcp_format == [
               %{
                 "name" => "simple",
                 "required" => true
               }
             ]
    end
  end
end
