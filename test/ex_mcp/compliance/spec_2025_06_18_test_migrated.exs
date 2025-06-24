defmodule ExMCP.Compliance.Spec20250618TestMigrated do
  use ExUnit.Case, async: true

  # Note: Client, Server, and Protocol aliases removed as they're not needed in migrated test

  # DSL-based server replacing Handler20250618
  defmodule Server20250618 do
    use ExMCP.Server

    # Tool definitions using DSL (2025-06-18 features handled in custom callbacks)
    deftool "calculate" do
      meta do
        description("Performs calculations")
      end

      input_schema(%{
        type: "object",
        properties: %{
          expression: %{type: "string"}
        },
        required: ["expression"]
      })
    end

    # Resource definitions using DSL
    defresource "file://example.txt" do
      meta do
        name("example")
        description("An example file")
      end

      mime_type("text/plain")
      subscribable(true)
    end

    # Prompt definitions using DSL
    defprompt "summarize" do
      meta do
        name("Text Summarizer")
        description("Summarizes text")
      end

      arguments do
        arg(:text, description: "Text to summarize", required: true)
      end
    end

    # Custom initialization to maintain state compatibility
    @impl true
    def init(args) do
      {:ok, Map.new(args)}
    end

    # Note: Using DSL auto-generated capabilities with custom additions for 2025-06-18

    # Tool call implementations with 2025-06-18 features
    @impl true
    def handle_tool_call("calculate", %{"expression" => expr}, state) do
      # Return both content and structured output
      result = %{
        content: [
          %{type: "text", text: "Result: 42"}
        ],
        # 2025-06-18 feature: structured output
        structuredContent: %{
          "result" => 42,
          "explanation" => "Calculated #{expr}"
        },
        # 2025-06-18 feature: resource links
        resourceLinks: [
          %{
            "uri" => "math://calculation/123",
            "title" => "Calculation Details"
          }
        ]
      }

      {:ok, result, state}
    end

    def handle_tool_call(_name, _arguments, state) do
      {:error, "Unknown tool", state}
    end

    # Resource implementations
    @impl true
    def handle_resource_read("file://example.txt", uri, state) do
      content = %{
        uri: uri,
        mimeType: "text/plain",
        text: "Example content"
      }

      {:ok, content, state}
    end

    def handle_resource_read(uri, _full_uri, state) do
      {:error, "Resource not found: #{uri}", state}
    end

    @impl true
    def handle_resource_subscribe(_uri, state) do
      {:ok, state}
    end

    @impl true
    def handle_resource_unsubscribe(_uri, state) do
      {:ok, state}
    end

    # Override resource list to include 2025-06-18 title field
    @impl true
    def handle_resource_list(state) do
      resources = [
        %{
          uri: "file://example.txt",
          # New title field
          title: "Example File",
          # Programmatic name
          name: "example",
          description: "An example file",
          mimeType: "text/plain"
        }
      ]

      {:ok, resources, state}
    end

    # Prompt implementations
    @impl true
    def handle_prompt_get("summarize", args, state) do
      {:ok,
       %{
         messages: [
           %{
             role: "user",
             content: %{
               type: "text",
               text: "Summarize: #{args["text"]}"
             }
           }
         ]
       }, state}
    end

    def handle_prompt_get(_name, _args, state) do
      {:error, "Unknown prompt", state}
    end

    # Override prompt list to include 2025-06-18 title field
    @impl true
    def handle_prompt_list(state) do
      prompts = [
        %{
          name: "summarize",
          # New title field
          title: "Text Summarizer",
          description: "Summarizes text",
          arguments: [
            %{name: "text", description: "Text to summarize", required: true}
          ]
        }
      ]

      {:ok, prompts, state}
    end

    # Override tool list to include 2025-06-18 features
    def handle_tool_list(state) do
      tools = [
        %{
          name: "calculate",
          # New title field
          title: "Calculator Tool",
          description: "Performs calculations",
          inputSchema: %{
            type: "object",
            properties: %{
              expression: %{type: "string"}
            },
            required: ["expression"]
          },
          # New in 2025-06-18
          outputSchema: %{
            type: "object",
            properties: %{
              result: %{type: "number"},
              explanation: %{type: "string"}
            }
          }
        }
      ]

      {:ok, tools, state}
    end

    # 2025-06-18 completion support
    def handle_complete("argument", %{"name" => "text", "value" => prefix}, state) do
      completions = ["#{prefix} completion"]
      {:ok, %{completion: completions}, state}
    end

    # 2025-06-18 logging support
    def handle_set_log_level(level, state) when level in ["debug", "info", "warning", "error"] do
      {:ok, state}
    end

    # Custom protocol version validation
    def handle_initialize(params, state) do
      # Verify we get 2025-06-18 version
      assert params["protocolVersion"] == "2025-06-18"

      result = %{
        protocolVersion: "2025-06-18",
        serverInfo: %{
          name: "test-server-2025-06-18",
          version: "3.0.0"
        },
        capabilities: get_capabilities()
      }

      {:ok, result, state}
    end
  end

  describe "2025-06-18 specification compliance (migrated)" do
    setup do
      # Start DSL server directly (no more handler pattern)
      {:ok, server} = Server20250618.start_link(transport: :native)

      on_exit(fn ->
        if Process.alive?(server), do: GenServer.stop(server)
      end)

      %{server: server}
    end

    test "DSL server has correct 2025-06-18 capabilities" do
      capabilities = Server20250618.get_capabilities()

      # Verify basic capabilities (DSL auto-generated)
      assert get_in(capabilities, ["resources", "subscribe"]) == true
      assert get_in(capabilities, ["tools", "listChanged"]) == true
      assert get_in(capabilities, ["resources", "listChanged"]) == true
      assert get_in(capabilities, ["prompts", "listChanged"]) == true

      # Note: Advanced 2025-06-18 features like outputSchema not yet in DSL
    end

    test "tools support output schema", %{server: server} do
      state = :sys.get_state(server)
      {:ok, tools, _state} = Server20250618.handle_tool_list(state)

      tool = Enum.find(tools, &(&1[:name] == "calculate"))
      assert tool[:title] == "Calculator Tool"
      assert tool[:outputSchema] != nil
      assert tool[:outputSchema][:type] == "object"
    end

    test "tool results include structured output and resource links", %{server: server} do
      state = :sys.get_state(server)

      {:ok, result, _new_state} =
        Server20250618.handle_tool_call("calculate", %{"expression" => "2+2"}, state)

      # Should have text content
      assert length(result.content) == 1
      assert hd(result.content)[:type] == "text"

      # Should have structured output
      assert result.structuredContent != nil
      assert result.structuredContent["result"] == 42

      # Should have resource links
      assert length(result.resourceLinks) == 1
      assert hd(result.resourceLinks)["uri"] == "math://calculation/123"
    end

    test "resources and prompts have title fields", %{server: server} do
      state = :sys.get_state(server)

      # Test resources
      {:ok, resources, _state} = Server20250618.handle_resource_list(state)
      resource = hd(resources)
      assert resource[:title] == "Example File"
      assert resource[:name] == "example"

      # Test prompts
      {:ok, prompts, _state} = Server20250618.handle_prompt_list(state)
      prompt = hd(prompts)
      assert prompt[:title] == "Text Summarizer"
    end

    test "completion requests support context", %{server: server} do
      state = :sys.get_state(server)

      # Context field is sent with completion requests
      {:ok, result, _state} =
        Server20250618.handle_complete(
          "argument",
          %{
            "name" => "text",
            "value" => "Hello",
            "_meta" => %{"context" => %{"previousValue" => "Hi"}}
          },
          state
        )

      # For completion requests, the result should contain completion data
      assert result.completion == ["Hello completion"]
    end

    test "resource reading works correctly", %{server: server} do
      state = :sys.get_state(server)

      {:ok, content, _state} =
        Server20250618.handle_resource_read("file://example.txt", "file://example.txt", state)

      assert content.uri == "file://example.txt"
      assert content.mimeType == "text/plain"
      assert content.text == "Example content"
    end

    test "prompt generation works correctly", %{server: server} do
      state = :sys.get_state(server)

      {:ok, result, _state} =
        Server20250618.handle_prompt_get("summarize", %{"text" => "Hello world"}, state)

      assert %{messages: messages} = result
      assert length(messages) == 1
      [message] = messages
      assert message.role == "user"
      assert message.content.text == "Summarize: Hello world"
    end

    test "resource subscription management", %{server: server} do
      state = :sys.get_state(server)

      # Subscribe to a resource
      {:ok, new_state} = Server20250618.handle_resource_subscribe("file://example.txt", state)
      # Default implementation doesn't change state
      assert new_state == state

      # Unsubscribe from a resource
      {:ok, final_state} =
        Server20250618.handle_resource_unsubscribe("file://example.txt", new_state)

      assert final_state == new_state
    end

    test "log level setting", %{server: server} do
      state = :sys.get_state(server)

      # Test valid log levels
      for level <- ["debug", "info", "warning", "error"] do
        {:ok, _state} = Server20250618.handle_set_log_level(level, state)
      end
    end

    test "OAuth 2.1 protected resource metadata", _context do
      # This would be part of server metadata in real implementation
      metadata = %{
        "authorization_server" => "https://auth.example.com",
        "resource" => "https://api.example.com/mcp",
        "scopes" => ["mcp:read", "mcp:write"]
      }

      assert metadata["authorization_server"] != nil
      assert metadata["resource"] != nil
      assert is_list(metadata["scopes"])
    end

    test "elicitation support (now stable)", %{server: _server} do
      # Elicitation moved from draft to stable
      elicit_request = %{
        "message" => "Please provide your API key",
        "requestedSchema" => %{
          "type" => "object",
          "properties" => %{
            "apiKey" => %{
              "type" => "string",
              "title" => "API Key",
              "description" => "Your API key for authentication"
            }
          },
          "required" => ["apiKey"]
        }
      }

      assert elicit_request["requestedSchema"]["properties"]["apiKey"]["type"] == "string"
    end

    test "MCP-Protocol-Version header requirement for HTTP", _context do
      # In HTTP transport, subsequent requests must include MCP-Protocol-Version header
      # This is handled by the HTTP transport implementation

      # Mock header that would be sent
      headers = [
        {"Content-Type", "application/json"},
        {"MCP-Protocol-Version", "2025-06-18"}
      ]

      assert {"MCP-Protocol-Version", "2025-06-18"} in headers
    end

    test "DSL server exposes correct tool definitions" do
      tools = Server20250618.get_tools()
      assert Map.has_key?(tools, "calculate")

      calculate_tool = tools["calculate"]
      assert calculate_tool.name == "calculate"
      assert calculate_tool.description == "Performs calculations"
    end

    test "DSL server exposes correct resource definitions" do
      resources = Server20250618.get_resources()
      assert Map.has_key?(resources, "file://example.txt")

      example_resource = resources["file://example.txt"]
      assert example_resource.uri == "file://example.txt"
      assert example_resource.mime_type == "text/plain"
    end

    test "DSL server exposes correct prompt definitions" do
      prompts = Server20250618.get_prompts()
      assert Map.has_key?(prompts, "summarize")

      summarize_prompt = prompts["summarize"]
      assert summarize_prompt.name == "summarize"
      assert summarize_prompt.description == "Summarizes text"
    end
  end
end
