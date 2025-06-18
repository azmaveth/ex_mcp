defmodule ExMCP.DSL.ToolTest do
  use ExUnit.Case, async: true

  alias ExMCP.DSL.Tool

  describe "__compile_schema__/1" do
    test "compiles simple fields to JSON Schema" do
      fields = [
        %{name: :name, type: :string, opts: [required: true, description: "User name"]},
        %{name: :age, type: :integer, opts: [min: 0, max: 120]}
      ]

      schema = Tool.__compile_schema__(fields)

      assert schema == %{
               "type" => "object",
               "properties" => %{
                 "name" => %{
                   "type" => "string",
                   "description" => "User name"
                 },
                 "age" => %{
                   "type" => "integer",
                   "minimum" => 0,
                   "maximum" => 120
                 }
               },
               "required" => ["name"]
             }
    end

    test "compiles array fields" do
      fields = [
        %{name: :tags, type: {:array, :string}, opts: [description: "List of tags"]}
      ]

      schema = Tool.__compile_schema__(fields)

      assert schema["properties"]["tags"] == %{
               "type" => "array",
               "items" => %{"type" => "string"},
               "description" => "List of tags"
             }
    end

    test "compiles nested object fields" do
      fields = [
        %{
          name: :config,
          type: :object,
          opts: [],
          nested_fields: [
            %{name: :debug, type: :boolean, opts: [default: false]},
            %{name: :port, type: :integer, opts: [required: true]}
          ]
        }
      ]

      schema = Tool.__compile_schema__(fields)

      assert schema["properties"]["config"] == %{
               "type" => "object",
               "properties" => %{
                 "debug" => %{
                   "type" => "boolean",
                   "default" => false
                 },
                 "port" => %{
                   "type" => "integer"
                 }
               },
               "required" => ["port"]
             }
    end

    test "handles string patterns" do
      fields = [
        %{name: :email, type: :string, opts: [pattern: "^[^@]+@[^@]+$"]}
      ]

      schema = Tool.__compile_schema__(fields)

      assert schema["properties"]["email"]["pattern"] == "^[^@]+@[^@]+$"
    end

    test "handles default values" do
      fields = [
        %{name: :enabled, type: :boolean, opts: [default: true]}
      ]

      schema = Tool.__compile_schema__(fields)

      assert schema["properties"]["enabled"]["default"] == true
    end

    test "omits required array when no required fields" do
      fields = [
        %{name: :optional, type: :string, opts: []}
      ]

      schema = Tool.__compile_schema__(fields)

      refute Map.has_key?(schema, "required")
    end
  end
end
