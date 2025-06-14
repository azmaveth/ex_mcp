defmodule ExMCP.Server.Tools.HelpersTest do
  use ExUnit.Case, async: true

  alias ExMCP.Server.Tools.Helpers

  describe "text_response/1" do
    test "creates a simple text response" do
      response = Helpers.text_response("Hello, World!")
      assert response == [%{type: "text", text: "Hello, World!"}]
    end

    test "handles empty text" do
      response = Helpers.text_response("")
      assert response == [%{type: "text", text: ""}]
    end

    test "handles text with special characters" do
      response = Helpers.text_response("Hello\nWorld\t!")
      assert response == [%{type: "text", text: "Hello\nWorld\t!"}]
    end
  end

  describe "error_response/1" do
    test "creates an error response with text content" do
      response = Helpers.error_response("Something went wrong")
      assert response == %{
        content: [%{type: "text", text: "Something went wrong"}],
        isError: true
      }
    end

    test "handles detailed error messages" do
      error_msg = "Failed to process request: Invalid input format"
      response = Helpers.error_response(error_msg)
      assert response.content == [%{type: "text", text: error_msg}]
      assert response.isError == true
    end
  end

  describe "structured_response/2" do
    test "creates a response with both text and structured content" do
      response = Helpers.structured_response("Operation completed", %{status: "success", count: 42})
      assert response == %{
        content: [%{type: "text", text: "Operation completed"}],
        structuredContent: %{status: "success", count: 42}
      }
    end

    test "handles complex structured data" do
      data = %{
        users: [
          %{id: 1, name: "Alice"},
          %{id: 2, name: "Bob"}
        ],
        metadata: %{
          total: 2,
          timestamp: "2025-01-06T12:00:00Z"
        }
      }
      response = Helpers.structured_response("User data retrieved", data)
      assert response.structuredContent == data
    end

    test "handles nil structured content" do
      response = Helpers.structured_response("No additional data", nil)
      assert response == %{
        content: [%{type: "text", text: "No additional data"}],
        structuredContent: nil
      }
    end
  end

  describe "validate_arguments/2" do
    test "validates arguments against a simple schema" do
      schema = %{
        type: "object",
        properties: %{
          name: %{type: "string"},
          age: %{type: "integer"}
        },
        required: ["name"]
      }

      # Valid arguments
      assert {:ok, %{name: "Alice", age: 30}} = 
        Helpers.validate_arguments(%{name: "Alice", age: 30}, schema)

      # Missing required field
      assert {:error, reason} = Helpers.validate_arguments(%{age: 30}, schema)
      assert reason =~ "required"

      # Invalid type
      assert {:error, reason} = Helpers.validate_arguments(%{name: "Alice", age: "thirty"}, schema)
      assert reason =~ "type"
    end

    test "validates nested object schemas" do
      schema = %{
        type: "object",
        properties: %{
          user: %{
            type: "object",
            properties: %{
              name: %{type: "string"},
              email: %{type: "string", format: "email"}
            },
            required: ["name", "email"]
          }
        },
        required: ["user"]
      }

      # Valid nested object
      assert {:ok, _} = Helpers.validate_arguments(
        %{user: %{name: "Alice", email: "alice@example.com"}},
        schema
      )

      # Invalid email format
      assert {:error, reason} = Helpers.validate_arguments(
        %{user: %{name: "Alice", email: "not-an-email"}},
        schema
      )
      assert reason =~ "format"
    end

    test "validates array schemas" do
      schema = %{
        type: "object",
        properties: %{
          tags: %{
            type: "array",
            items: %{type: "string"},
            minItems: 1,
            maxItems: 5
          }
        }
      }

      # Valid array
      assert {:ok, _} = Helpers.validate_arguments(%{tags: ["tag1", "tag2"]}, schema)

      # Empty array when minItems is 1
      assert {:error, reason} = Helpers.validate_arguments(%{tags: []}, schema)
      assert reason =~ "minItems"

      # Too many items
      assert {:error, reason} = Helpers.validate_arguments(
        %{tags: ["tag1", "tag2", "tag3", "tag4", "tag5", "tag6"]},
        schema
      )
      assert reason =~ "maxItems"

      # Invalid item type
      assert {:error, reason} = Helpers.validate_arguments(%{tags: ["tag1", 123]}, schema)
      assert reason =~ "type"
    end

    test "validates enum values" do
      schema = %{
        type: "object",
        properties: %{
          status: %{type: "string", enum: ["active", "inactive", "pending"]}
        }
      }

      # Valid enum value
      assert {:ok, _} = Helpers.validate_arguments(%{status: "active"}, schema)

      # Invalid enum value
      assert {:error, reason} = Helpers.validate_arguments(%{status: "unknown"}, schema)
      assert reason =~ "enum"
    end

    test "validates pattern constraints" do
      schema = %{
        type: "object",
        properties: %{
          phone: %{type: "string", pattern: "^\\+?[1-9]\\d{1,14}$"}
        }
      }

      # Valid phone number
      assert {:ok, _} = Helpers.validate_arguments(%{phone: "+1234567890"}, schema)

      # Invalid phone number
      assert {:error, reason} = Helpers.validate_arguments(%{phone: "abc123"}, schema)
      assert reason =~ "pattern"
    end

    test "validates numeric constraints" do
      schema = %{
        type: "object",
        properties: %{
          age: %{type: "integer", minimum: 0, maximum: 150},
          price: %{type: "number", minimum: 0, exclusiveMinimum: true}
        }
      }

      # Valid values
      assert {:ok, _} = Helpers.validate_arguments(%{age: 25, price: 10.5}, schema)

      # Age out of range
      assert {:error, reason} = Helpers.validate_arguments(%{age: -5, price: 10}, schema)
      assert reason =~ "minimum"

      # Price at exclusive minimum
      assert {:error, reason} = Helpers.validate_arguments(%{age: 25, price: 0}, schema)
      assert reason =~ "exclusiveMinimum"
    end

    test "handles additional properties" do
      schema = %{
        type: "object",
        properties: %{
          name: %{type: "string"}
        },
        additionalProperties: false
      }

      # No additional properties
      assert {:ok, _} = Helpers.validate_arguments(%{name: "Alice"}, schema)

      # With additional properties
      assert {:error, reason} = Helpers.validate_arguments(
        %{name: "Alice", extra: "value"},
        schema
      )
      assert reason =~ "additional"
    end

    test "validates with default values" do
      schema = %{
        type: "object",
        properties: %{
          name: %{type: "string"},
          active: %{type: "boolean", default: true}
        }
      }

      # Without default field
      {:ok, validated} = Helpers.validate_arguments(%{name: "Alice"}, schema)
      assert validated == %{name: "Alice", active: true}

      # Overriding default
      {:ok, validated} = Helpers.validate_arguments(%{name: "Alice", active: false}, schema)
      assert validated == %{name: "Alice", active: false}
    end

    test "handles null values" do
      schema = %{
        type: "object",
        properties: %{
          name: %{type: ["string", "null"]},
          age: %{type: "integer"}
        }
      }

      # Null allowed
      assert {:ok, _} = Helpers.validate_arguments(%{name: nil, age: 25}, schema)

      # Null not allowed
      assert {:error, reason} = Helpers.validate_arguments(%{name: "Alice", age: nil}, schema)
      assert reason =~ "type"
    end
  end

  describe "function_to_tool/3" do
    defmodule TestModule do
      @spec echo(String.t()) :: {:ok, String.t()} | {:error, String.t()}
      def echo(message), do: {:ok, message}

      @spec add(integer(), integer()) :: integer()
      def add(a, b), do: a + b

      @spec greet(String.t(), keyword()) :: String.t()
      def greet(name, opts \\ []) do
        title = Keyword.get(opts, :title, "")
        if title != "", do: "Hello, #{title} #{name}!", else: "Hello, #{name}!"
      end
    end

    test "converts simple function with single parameter" do
      tool = Helpers.function_to_tool(TestModule, :echo, "Echo a message back")
      
      assert tool.name == "echo"
      assert tool.description == "Echo a message back"
      assert tool.inputSchema == %{
        type: "object",
        properties: %{
          message: %{type: "string"}
        },
        required: ["message"]
      }
    end

    test "converts function with multiple parameters" do
      tool = Helpers.function_to_tool(TestModule, :add, "Add two numbers")
      
      assert tool.name == "add"
      assert tool.description == "Add two numbers"
      assert tool.inputSchema == %{
        type: "object",
        properties: %{
          a: %{type: "integer"},
          b: %{type: "integer"}
        },
        required: ["a", "b"]
      }
    end

    test "handles functions with optional parameters" do
      tool = Helpers.function_to_tool(TestModule, :greet, "Greet a person")
      
      assert tool.name == "greet"
      assert tool.inputSchema.properties.name == %{type: "string"}
      assert tool.inputSchema.required == ["name"]
      # Optional parameters from keyword list are not included in required
    end

    test "raises error for non-existent function" do
      assert_raise ArgumentError, ~r/Function.*does not exist/, fn ->
        Helpers.function_to_tool(TestModule, :non_existent, "Does not exist")
      end
    end

    test "handles functions without specs" do
      defmodule NoSpecModule do
        def no_spec_function(arg), do: arg
      end

      # Should still create a basic tool, but with generic schema
      tool = Helpers.function_to_tool(NoSpecModule, :no_spec_function, "Function without spec")
      
      assert tool.name == "no_spec_function"
      assert tool.description == "Function without spec"
      assert tool.inputSchema == %{
        type: "object",
        properties: %{},
        additionalProperties: true
      }
    end
  end

  describe "response builder helpers" do
    test "builds image response" do
      response = Helpers.image_response("https://example.com/image.png", "An example image")
      assert response == [%{
        type: "image",
        data: "https://example.com/image.png",
        mimeType: "image/png",
        description: "An example image"
      }]
    end

    test "builds resource response" do
      response = Helpers.resource_response("file:///path/to/file.txt", "text/plain")
      assert response == [%{
        type: "resource",
        uri: "file:///path/to/file.txt",
        mimeType: "text/plain"
      }]
    end

    test "builds multi-content response" do
      response = Helpers.multi_content_response([
        {:text, "Here is some text"},
        {:image, "data:image/png;base64,abc123", "A diagram"},
        {:resource, "file:///doc.pdf", "application/pdf"}
      ])

      assert response == [
        %{type: "text", text: "Here is some text"},
        %{type: "image", data: "data:image/png;base64,abc123", mimeType: "image/png", description: "A diagram"},
        %{type: "resource", uri: "file:///doc.pdf", mimeType: "application/pdf"}
      ]
    end
  end

  describe "schema type helpers" do
    test "generates string schema with constraints" do
      schema = Helpers.string_schema(
        min_length: 3,
        max_length: 20,
        pattern: "^[a-zA-Z]+$",
        format: "email"
      )

      assert schema == %{
        type: "string",
        minLength: 3,
        maxLength: 20,
        pattern: "^[a-zA-Z]+$",
        format: "email"
      }
    end

    test "generates number schema with constraints" do
      schema = Helpers.number_schema(
        minimum: 0,
        maximum: 100,
        exclusive_minimum: true,
        multiple_of: 0.5
      )

      assert schema == %{
        type: "number",
        minimum: 0,
        maximum: 100,
        exclusiveMinimum: true,
        multipleOf: 0.5
      }
    end

    test "generates array schema" do
      schema = Helpers.array_schema(:string,
        min_items: 1,
        max_items: 10,
        unique_items: true
      )

      assert schema == %{
        type: "array",
        items: %{type: "string"},
        minItems: 1,
        maxItems: 10,
        uniqueItems: true
      }
    end

    test "generates object schema" do
      schema = Helpers.object_schema(%{
        name: %{type: "string"},
        age: %{type: "integer", minimum: 0}
      }, required: [:name])

      assert schema == %{
        type: "object",
        properties: %{
          name: %{type: "string"},
          age: %{type: "integer", minimum: 0}
        },
        required: ["name"]
      }
    end
  end
end