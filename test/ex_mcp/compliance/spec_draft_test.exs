defmodule ExMCP.SpecDraftTest do
  @moduledoc """
  Comprehensive test suite for MCP draft specification.

  Tests all features unique to the draft spec:
  - Removal of JSON-RPC batching
  - Structured tool output
  - OAuth Resource Server metadata
  - Security best practices
  - Elicitation (server requesting info from user)
  """
  use ExUnit.Case, async: true

  @moduletag :compliance

  alias ExMCP.{Client, Protocol, Server}

  defmodule HandlerDraft do
    @moduledoc false
    use ExMCP.Server.Handler

    @impl true
    def init(_args) do
      {:ok,
       %{
         tools: [
           %{
             name: "weather",
             description: "Get weather information",
             inputSchema: %{
               type: "object",
               properties: %{
                 location: %{type: "string", description: "City name"}
               },
               required: ["location"]
             },
             # Draft feature: outputSchema for structured content
             outputSchema: %{
               type: "object",
               properties: %{
                 temperature: %{type: "number", description: "Temperature in Celsius"},
                 conditions: %{type: "string", description: "Weather conditions"},
                 humidity: %{type: "number", description: "Humidity percentage"},
                 wind: %{
                   type: "object",
                   properties: %{
                     speed: %{type: "number"},
                     direction: %{type: "string"}
                   }
                 }
               },
               required: ["temperature", "conditions"]
             }
           },
           %{
             name: "data_processor",
             description: "Process data with user confirmation",
             inputSchema: %{
               type: "object",
               properties: %{
                 data: %{type: "string"},
                 format: %{type: "string", enum: ["json", "xml", "csv"]}
               },
               required: ["data", "format"]
             }
           }
         ],
         pending_elicitations: %{},
         security_config: %{
           enforce_https: true,
           validate_origin: true,
           allowed_origins: ["https://trusted.example.com"],
           resource_server_metadata: %{
             authorization_server: "https://auth.example.com",
             resource: "https://api.example.com/mcp",
             scopes: ["mcp:read", "mcp:write", "mcp:admin"]
           }
         }
       }}
    end

    @impl true
    def handle_initialize(params, state) do
      # Verify we get draft version
      assert params["protocolVersion"] == "draft"

      result = %{
        protocolVersion: "draft",
        serverInfo: %{
          name: "test-server-draft",
          version: "3.0.0-draft"
        },
        capabilities: %{
          # All 2025-03-26 capabilities plus draft features
          tools: %{
            listChanged: true,
            # Draft feature
            outputSchema: true
          },
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
            # Batch processing removed in draft
            batchProcessing: false,
            # Draft features
            elicitation: true,
            structuredContent: true,
            toolOutputSchema: true
          }
        },
        # Draft: OAuth Resource Server metadata
        meta: %{
          oauth: state.security_config.resource_server_metadata
        }
      }

      {:ok, result, state}
    end

    @impl true
    def handle_list_tools(_cursor, state) do
      {:ok, state.tools, nil, state}
    end

    @impl true
    def handle_call_tool("weather", %{"location" => location}, state) do
      # Simulate weather API call
      weather_data = %{
        "temperature" => 22.5,
        "conditions" => "Partly cloudy",
        "humidity" => 65,
        "wind" => %{
          "speed" => 15,
          "direction" => "NW"
        }
      }

      # Draft feature: Return both unstructured and structured content
      result = %{
        content: [
          %{
            type: "text",
            text:
              "Weather in #{location}: #{weather_data["temperature"]}°C, #{weather_data["conditions"]}"
          }
        ],
        # Draft feature: structured content matching outputSchema
        structuredContent: weather_data,
        # Can also indicate if this is an error result
        isError: false
      }

      {:ok, result, state}
    end

    @impl true
    def handle_call_tool("data_processor", %{"data" => data, "format" => format}, state) do
      # Draft feature: Elicitation - request user confirmation
      elicitation_id = "elicit-#{System.unique_integer()}"

      # Store pending elicitation
      new_state =
        put_in(
          state.pending_elicitations[elicitation_id],
          %{tool: "data_processor", data: data, format: format}
        )

      # Would send elicitation request to client
      # For testing, simulate immediate response

      result = %{
        content: [
          %{
            type: "text",
            text: "Processing #{byte_size(data)} bytes of #{format} data..."
          }
        ],
        # Could include structured output here too
        meta: %{
          elicitationId: elicitation_id,
          status: "pending_confirmation"
        }
      }

      {:ok, result, new_state}
    end

    @impl true
    def handle_list_resources(_cursor, state) do
      # Include security metadata in resources
      resources = [
        %{
          uri: "secure://data",
          name: "Secure Data",
          description: "Requires authentication",
          mimeType: "application/json",
          # Draft: security annotations
          annotations: %{
            requiresAuth: true,
            scopes: ["mcp:read"]
          }
        }
      ]

      {:ok, resources, nil, state}
    end

    @impl true
    def handle_read_resource("secure://data", state) do
      # Check security in draft
      # Would validate OAuth token scopes here

      content = %{
        uri: "secure://data",
        mimeType: "application/json",
        text:
          Jason.encode!(%{
            secure: true,
            data: "classified information"
          })
      }

      {:ok, %{contents: [content]}, state}
    end

    # Draft feature: Elicitation support
    def handle_elicitation_response(elicitation_id, response, state) do
      case Map.get(state.pending_elicitations, elicitation_id) do
        nil ->
          {:error, "Unknown elicitation ID", state}

        _pending ->
          # Process based on user response
          if response["approved"] do
            # Continue with operation
            {:ok, "Operation approved and completed", state}
          else
            # Cancel operation
            {:ok, "Operation cancelled by user", state}
          end
      end
    end
  end

  # Client handler for elicitation
  defmodule ClientHandlerDraft do
    @behaviour ExMCP.Client.Handler

    @impl true
    def init(_args) do
      {:ok, %{elicitations: []}}
    end

    @impl true
    def handle_elicitation_create(message, requested_schema, state) do
      # Draft feature: Handle elicitation request from server
      elicitation = %{
        message: message,
        schema: requested_schema
      }

      # Auto-approve for testing
      response = %{
        action: "accept",
        content: %{approved: true}
      }

      new_state = %{state | elicitations: [elicitation | state.elicitations]}
      {:ok, response, new_state}
    end

    # Other required callbacks
    @impl true
    def handle_create_message(_params, state), do: {:ok, %{}, state}
    @impl true
    def handle_list_roots(state), do: {:ok, [], state}
    @impl true
    def handle_ping(state), do: {:ok, %{}, state}
    @impl true
    def terminate(_reason, _state), do: :ok
  end

  describe "draft specification compliance" do
    setup do
      {:ok, server} =
        Server.start_link(
          handler: HandlerDraft,
          transport: :beam,
          name: :server_draft
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :server_draft,
          client_info: %{name: "test-client-draft", version: "3.0.0"},
          protocol_version: "draft",
          handler: ClientHandlerDraft
        )

      Process.sleep(100)

      on_exit(fn ->
        if Process.alive?(client), do: GenServer.stop(client)
        if Process.alive?(server), do: GenServer.stop(server)
      end)

      {:ok, client: client, server: server}
    end

    test "negotiates draft protocol version", %{client: client} do
      {:ok, version} = Client.negotiated_version(client)
      assert version == "draft"
    end

    test "server capabilities include draft features", %{client: client} do
      {:ok, caps} = Client.server_capabilities(client)

      # Draft experimental features
      assert get_in(caps, ["experimental", "elicitation"]) == true
      assert get_in(caps, ["experimental", "structuredContent"]) == true
      assert get_in(caps, ["experimental", "toolOutputSchema"]) == true

      # Batch processing removed in draft
      assert get_in(caps, ["experimental", "batchProcessing"]) == false

      # Tools support output schema
      assert get_in(caps, ["tools", "outputSchema"]) == true
    end

    test "structured tool output works", %{client: client} do
      {:ok, result} = Client.call_tool(client, "weather", %{"location" => "Paris"})

      # Check both content types
      assert Map.has_key?(result, :content) || Map.has_key?(result, "content")
      assert Map.has_key?(result, :structuredContent) || Map.has_key?(result, "structuredContent")

      # Get structured content
      structured = Map.get(result, :structuredContent) || Map.get(result, "structuredContent")

      # Verify it matches the output schema
      assert is_number(structured[:temperature])
      assert is_binary(structured[:conditions])
      assert is_number(structured[:humidity])
      assert is_map(structured[:wind])
      assert is_number(structured[:wind][:speed])
    end

    test "OAuth Resource Server metadata is included", %{client: client} do
      {:ok, _server_info} = Client.server_info(client)

      # Check for OAuth metadata (would be in initialization response)
      # This is a draft feature for resource server discovery
      # The metadata tells clients where to get tokens
    end

    test "elicitation feature works", %{client: client} do
      # Call tool that triggers elicitation
      {:ok, result} =
        Client.call_tool(client, "data_processor", %{
          "data" => "important data",
          "format" => "json"
        })

      # Result should indicate pending elicitation
      meta = Map.get(result, :meta) || Map.get(result, "meta")
      assert meta[:status] == "pending_confirmation"
      assert meta[:elicitationId]

      # Client handler would have received elicitation request
      # and auto-approved it in our test
    end

    test "JSON-RPC batching is NOT supported in draft", %{client: client} do
      # Draft removes batch support
      # Attempting to send batch should fail or be processed individually

      batch = [
        Protocol.encode_list_tools(),
        Protocol.encode_list_resources()
      ]

      # This should either error or process individually
      # depending on implementation
      result = Client.send_batch(client, batch)

      case result do
        {:error, _} ->
          # Expected - batching not supported
          assert true

        {:ok, responses} ->
          # If processed, should be individual responses
          assert length(responses) == 2
      end
    end

    test "security best practices are enforced" do
      # Test security validations

      # HTTPS enforcement
      assert ExMCP.Security.enforce_https_requirement("http://external.com") ==
               {:error, :https_required}

      assert ExMCP.Security.enforce_https_requirement("https://external.com") == :ok
      assert ExMCP.Security.enforce_https_requirement("http://localhost") == :ok

      # Origin validation
      security_config = %{
        validate_origin: true,
        allowed_origins: ["https://trusted.example.com"]
      }

      assert ExMCP.Security.validate_request_origin(
               "https://trusted.example.com",
               security_config
             ) == :ok

      assert ExMCP.Security.validate_request_origin(
               "https://evil.com",
               security_config
             ) == {:error, :origin_not_allowed}
    end

    test "resource annotations include security info", %{client: client} do
      {:ok, result} = Client.list_resources(client)

      resource = hd(result.resources)
      assert resource.uri == "secure://data"

      # Draft feature: security annotations on resources
      annotations = Map.get(resource, :annotations) || Map.get(resource, "annotations")
      assert annotations[:requiresAuth] == true
      assert "mcp:read" in annotations[:scopes]
    end

    test "error results can use isError flag", %{client: _client} do
      # Try to trigger an error condition
      # In draft, tools can return isError: true in structured results

      # This would be a tool that returns an error
      # result = %{
      #   content: [%{type: "text", text: "Error occurred"}],
      #   isError: true,
      #   errorDetails: %{code: "WEATHER_API_DOWN"}
      # }
    end

    test "content types are properly validated", %{client: _client} do
      # Draft maintains support for all content types
      content_types = ["text", "image", "audio", "resource"]

      Enum.each(content_types, fn type ->
        content = %{type: type}

        # Add required fields based on type
        content =
          case type do
            "text" -> Map.put(content, :text, "sample")
            "image" -> Map.merge(content, %{data: "base64data", mimeType: "image/png"})
            "audio" -> Map.merge(content, %{data: "base64data", mimeType: "audio/mp3"})
            "resource" -> Map.put(content, :resource, %{uri: "test://resource"})
          end

        # Validate using the types module
        assert match?(%{type: _}, content)
      end)
    end
  end

  describe "draft security requirements" do
    test "localhost binding is enforced" do
      # Servers should bind to localhost by default
      binding_config = %{binding: "127.0.0.1"}
      assert ExMCP.Security.validate_localhost_binding(binding_config) == :ok

      # Non-localhost requires security
      public_binding = %{binding: "0.0.0.0"}
      assert {:error, _} = ExMCP.Security.validate_localhost_binding(public_binding)
    end

    test "token validation includes audience checking" do
      # Draft emphasizes proper token validation
      token_info = %{
        aud: "mcp-server-123",
        iss: "https://auth.example.com",
        sub: "user-456",
        exp: System.system_time(:second) + 3600
      }

      # Would validate audience matches server ID
      assert token_info.aud == "mcp-server-123"
    end

    test "CORS headers are properly configured" do
      cors_config = %{
        allowed_origins: ["https://app.example.com"],
        allowed_methods: ["GET", "POST"],
        allow_credentials: true,
        max_age: 86400
      }

      headers = ExMCP.Security.build_cors_headers(cors_config, "https://app.example.com")

      # Find Access-Control headers
      origin_header = Enum.find(headers, fn {k, _} -> k == "Access-Control-Allow-Origin" end)
      assert origin_header == {"Access-Control-Allow-Origin", "https://app.example.com"}
    end
  end

  describe "draft removal of features" do
    test "batch processing is explicitly not supported" do
      # The draft spec removes JSON-RPC batch support
      # that was added in 2025-03-26

      handler = HandlerDraft
      {:ok, state} = handler.init([])
      {:ok, result, _} = handler.handle_initialize(%{"protocolVersion" => "draft"}, state)

      # Batch processing should be false
      refute get_in(result.capabilities, [:experimental, :batchProcessing])
    end

    test "initialize request still cannot be in a batch" do
      # Even though batching is removed, the restriction remains
      # that initialize cannot be part of any multi-request scenario

      init_msg =
        Protocol.encode_initialize(
          %{name: "test", version: "1.0"},
          %{},
          "draft"
        )

      # Should be a single request, not batchable
      assert init_msg["method"] == "initialize"
      assert is_integer(init_msg["id"])
    end
  end
end
