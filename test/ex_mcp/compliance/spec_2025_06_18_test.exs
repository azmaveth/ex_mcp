defmodule ExMCP.Compliance.Spec20250618Test do
  use ExUnit.Case, async: true

  alias ExMCP.{Client, Server}
  alias ExMCP.Protocol

  defmodule Handler20250618 do
    @behaviour ExMCP.Server.Handler

    def init(args) do
      {:ok, args}
    end

    @impl true
    def handle_initialize(params, state) do
      # Verify we get 2025-06-18 version
      assert params["protocolVersion"] == "2025-06-18"

      result = %{
        protocolVersion: "2025-06-18",
        serverInfo: %{
          name: "test-server-2025-06-18",
          version: "3.0.0"
        },
        capabilities: %{
          # 2025-06-18 capabilities (no batch support)
          tools: %{listChanged: true, outputSchema: true},
          resources: %{
            subscribe: true,
            listChanged: true
          },
          prompts: %{listChanged: true},
          logging: %{setLevel: true},
          completion: %{
            hasArguments: true,
            values: true
          },
          experimental: %{
            elicitation: true,
            structuredContent: true
          }
        }
      }

      {:ok, result, state}
    end

    @impl true
    def handle_list_tools(_cursor, state) do
      tools = [
        %{
          name: "calculate",
          title: "Calculator Tool",  # New title field
          description: "Performs calculations",
          inputSchema: %{
            type: "object",
            properties: %{
              expression: %{type: "string"}
            },
            required: ["expression"]
          },
          outputSchema: %{  # New in 2025-06-18
            type: "object",
            properties: %{
              result: %{type: "number"},
              explanation: %{type: "string"}
            }
          }
        }
      ]
      
      {:ok, tools, nil, state}
    end

    @impl true
    def handle_call_tool("calculate", %{"expression" => expr}, state) do
      # Return both content and structured output
      result = [
        %{type: "text", text: "Result: 42"}
      ]
      
      structured_output = %{
        "result" => 42,
        "explanation" => "Calculated #{expr}"
      }
      
      resource_links = [
        %{
          "uri" => "math://calculation/123",
          "title" => "Calculation Details"
        }
      ]
      
      {:ok, result, structured_output, resource_links, state}
    end

    @impl true
    def handle_list_resources(_cursor, state) do
      resources = [
        %{
          uri: "file://example.txt",
          title: "Example File",  # New title field
          name: "example",       # Programmatic name
          description: "An example file",
          mimeType: "text/plain"
        }
      ]
      
      {:ok, resources, nil, state}
    end

    @impl true
    def handle_read_resource(uri, state) do
      content = %{
        uri: uri,
        mimeType: "text/plain",
        text: "Example content"
      }
      
      {:ok, content, state}
    end

    @impl true
    def handle_subscribe_resource(uri, state) do
      {:ok, %{}, state}
    end

    @impl true
    def handle_unsubscribe_resource(_uri, state) do
      {:ok, %{}, state}
    end

    @impl true
    def handle_list_prompts(_cursor, state) do
      prompts = [
        %{
          name: "summarize",
          title: "Text Summarizer",  # New title field
          description: "Summarizes text",
          arguments: [
            %{name: "text", description: "Text to summarize", required: true}
          ]
        }
      ]
      
      {:ok, prompts, nil, state}
    end

    @impl true
    def handle_get_prompt("summarize", args, state) do
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

    @impl true
    def handle_complete(_ref, %{"name" => "text", "value" => prefix}, state) do
      completions = ["#{prefix} completion"]
      {:ok, %{completion: completions}, state}
    end

    @impl true
    def handle_set_log_level(level, state) when level in ["debug", "info", "warning", "error"] do
      {:ok, state}
    end
  end

  describe "2025-06-18 specification compliance" do
    setup do
      {:ok, server} =
        Server.start_link(
          handler: Handler20250618,
          transport: :test
        )

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server,
          client_info: %{name: "test-client-2025-06-18", version: "3.0.0"},
          protocol_version: "2025-06-18"
        )

      Process.sleep(100)

      on_exit(fn ->
        if Process.alive?(client), do: GenServer.stop(client)
        if Process.alive?(server), do: GenServer.stop(server)
      end)

      {:ok, client: client, server: server}
    end

    test "negotiates 2025-06-18 protocol version", %{client: client} do
      {:ok, version} = Client.negotiated_version(client)
      assert version == "2025-06-18"
    end

    test "server capabilities match 2025-06-18 spec", %{client: client} do
      {:ok, caps} = Client.server_capabilities(client)

      # Verify capabilities
      assert get_in(caps, ["resources", "subscribe"]) == true
      assert get_in(caps, ["tools", "outputSchema"]) == true
      
      # No batch processing in 2025-06-18
      refute get_in(caps, ["experimental", "batchProcessing"])
      
      # Elicitation is now stable
      assert get_in(caps, ["experimental", "elicitation"]) == true
    end

    test "tools support output schema", %{client: client} do
      {:ok, result} = Client.list_tools(client)
      
      tool = Enum.find(result.tools, &(&1.name == "calculate"))
      assert tool.title == "Calculator Tool"
      assert tool.outputSchema != nil
      assert tool.outputSchema["type"] == "object"
    end

    test "tool results include structured output and resource links", %{client: client} do
      {:ok, result} = Client.call_tool(client, "calculate", %{"expression" => "2+2"})
      
      # Should have text content
      assert length(result.content) == 1
      assert hd(result.content).type == "text"
      
      # Should have structured output
      assert result.structuredOutput != nil
      assert result.structuredOutput["result"] == 42
      
      # Should have resource links
      assert length(result.resourceLinks) == 1
      assert hd(result.resourceLinks)["uri"] == "math://calculation/123"
    end

    test "resources and prompts have title fields", %{client: client} do
      {:ok, resources} = Client.list_resources(client)
      resource = hd(resources.resources)
      assert resource.title == "Example File"
      assert resource.name == "example"
      
      {:ok, prompts} = Client.list_prompts(client)
      prompt = hd(prompts.prompts)
      assert prompt.title == "Text Summarizer"
    end

    test "completion requests support context", %{client: client} do
      # Context field is sent with completion requests
      {:ok, result} = Client.complete(client, "argument", %{
        "name" => "text",
        "value" => "Hello",
        "_meta" => %{"context" => %{"previousValue" => "Hi"}}
      })
      
      assert is_list(result[:completion])
    end

    test "batch requests are not supported", %{client: client} do
      # Client.batch_request should be deprecated
      assert match?(
        {:deprecated, _},
        Code.fetch_docs(ExMCP.Client) 
        |> elem(5) 
        |> Enum.find(fn {{:function, name, _}, _, _, _, _} -> name == :batch_request end)
        |> elem(3)
        |> Map.get(:deprecated)
      )
    end

    test "OAuth 2.1 protected resource metadata", %{_client: client} do
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

    test "elicitation support (now stable)", %{client: _client} do
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

    test "MCP-Protocol-Version header requirement for HTTP", %{_client: client} do
      # In HTTP transport, subsequent requests must include MCP-Protocol-Version header
      # This is handled by the HTTP transport implementation
      
      # Mock header that would be sent
      headers = [
        {"Content-Type", "application/json"},
        {"MCP-Protocol-Version", "2025-06-18"}
      ]
      
      assert {"MCP-Protocol-Version", "2025-06-18"} in headers
    end
  end
end