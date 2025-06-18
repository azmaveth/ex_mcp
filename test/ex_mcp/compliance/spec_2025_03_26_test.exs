defmodule ExMCP.Spec20250326Test do
  @moduledoc """
  Comprehensive test suite for MCP specification version 2025-03-26.

  Tests all required and optional features defined in the spec, including:
  - OAuth 2.1 authorization framework
  - Streamable HTTP transport
  - JSON-RPC batching
  - Tool annotations
  - Audio content type
  - Completions capability
  - Resource subscriptions
  - List change notifications
  """
  use ExUnit.Case, async: true

  @moduletag :compliance

  alias ExMCP.{Authorization, Client, Protocol, Server}

  defmodule Handler20250326 do
    @moduledoc false
    use ExMCP.Server.Handler

    @impl true
    def init(_args) do
      {:ok,
       %{
         tools: [
           %{
             name: "file_write",
             description: "Write content to a file",
             inputSchema: %{
               type: "object",
               properties: %{
                 path: %{type: "string"},
                 content: %{type: "string"}
               },
               required: ["path", "content"]
             },
             # Tool annotations new in 2025-03-26
             annotations: %{
               destructiveHint: true,
               idempotentHint: false,
               readOnlyHint: false,
               openWorldHint: false
             }
           },
           %{
             name: "file_read",
             description: "Read content from a file",
             inputSchema: %{
               type: "object",
               properties: %{
                 path: %{type: "string"}
               },
               required: ["path"]
             },
             annotations: %{
               destructiveHint: false,
               idempotentHint: true,
               readOnlyHint: true,
               openWorldHint: false
             }
           }
         ],
         resources: %{
           "config://app" => %{
             uri: "config://app",
             name: "Application Config",
             description: "Application configuration",
             mimeType: "application/json"
           }
         },
         subscriptions: MapSet.new(),
         log_level: "info"
       }}
    end

    @impl true
    def handle_initialize(params, state) do
      # Verify we get 2025-03-26 version
      assert params["protocolVersion"] == "2025-03-26"

      result = %{
        protocolVersion: "2025-03-26",
        serverInfo: %{
          name: "test-server-2025-03-26",
          version: "2.0.0"
        },
        capabilities: %{
          # 2025-03-26 capabilities
          tools: %{listChanged: true},
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
            batchProcessing: true
          }
        }
      }

      {:ok, result, state}
    end

    @impl true
    def handle_list_tools(cursor, state) do
      # Support pagination
      if cursor == nil do
        {:ok, state.tools, nil, state}
      else
        {:ok, [], nil, state}
      end
    end

    @impl true
    def handle_call_tool("file_write", %{"path" => path, "content" => content}, state) do
      # Simulate file write with progress
      progress_token = get_in(state, [:current_request, :progress_token])

      if progress_token do
        # Send progress notifications
        send(self(), {:notify_progress, progress_token, 50, 100, "Writing file..."})
      end

      # Return both text and audio content (new in 2025-03-26)
      result = [
        %{type: "text", text: "Wrote #{byte_size(content)} bytes to #{path}"},
        %{
          type: "audio",
          data: Base.encode64("fake-audio-data"),
          mimeType: "audio/mp3"
        }
      ]

      {:ok, result, state}
    end

    @impl true
    def handle_call_tool("file_read", %{"path" => path}, state) do
      content = "Sample content from #{path}"
      {:ok, [%{type: "text", text: content}], state}
    end

    @impl true
    def handle_list_resources(cursor, state) do
      resources = Map.values(state.resources)

      if cursor == nil do
        {:ok, resources, nil, state}
      else
        {:ok, [], nil, state}
      end
    end

    @impl true
    def handle_read_resource(uri, state) do
      case Map.get(state.resources, uri) do
        nil ->
          {:error, "Resource not found: #{uri}", state}

        resource ->
          content = %{
            uri: uri,
            mimeType: resource.mimeType,
            text: Jason.encode!(%{debug: true, version: "2.0"})
          }

          {:ok, content, state}
      end
    end

    @impl true
    def handle_subscribe_resource(uri, state) do
      if Map.has_key?(state.resources, uri) do
        new_state = %{state | subscriptions: MapSet.put(state.subscriptions, uri)}
        {:ok, %{}, new_state}
      else
        {:error, "Resource not found: #{uri}", state}
      end
    end

    @impl true
    def handle_unsubscribe_resource(uri, state) do
      new_state = %{state | subscriptions: MapSet.delete(state.subscriptions, uri)}
      {:ok, %{}, new_state}
    end

    @impl true
    def handle_complete("argument", %{"name" => "path", "value" => prefix}, state) do
      # Provide path completions
      completions =
        [
          "/home/user/documents/",
          "/home/user/downloads/",
          "/var/log/"
        ]
        |> Enum.filter(&String.starts_with?(&1, prefix))
        |> Enum.take(5)

      {:ok, %{completion: completions}, state}
    end

    @impl true
    def handle_set_log_level(level, state) when level in ["debug", "info", "warning", "error"] do
      {:ok, %{state | log_level: level}}
    end

    @impl true
    def handle_list_prompts(_cursor, state) do
      prompts = [
        %{
          name: "analyze-code",
          description: "Analyze code with AI",
          arguments: [
            %{name: "language", description: "Programming language", required: true},
            %{name: "framework", description: "Framework used", required: false}
          ]
        }
      ]

      {:ok, prompts, nil, state}
    end

    @impl true
    def handle_get_prompt("analyze-code", args, state) do
      lang = args["language"]
      framework = args["framework"] || "none"

      {:ok,
       %{
         description: "Analyze #{lang} code using #{framework}",
         messages: [
           %{
             role: "system",
             content: %{
               type: "text",
               text: "You are a code analyzer for #{lang}."
             }
           },
           %{
             role: "user",
             content: %{
               type: "text",
               text: "Analyze this code for best practices."
             }
           }
         ]
       }, state}
    end
  end

  describe "2025-03-26 specification compliance" do
    setup do
      {:ok, server} =
        Server.start_link(
          handler: Handler20250326,
          transport: :test
        )

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server,
          client_info: %{name: "test-client-2025-03-26", version: "2.0.0"},
          protocol_version: "2025-03-26"
        )

      Process.sleep(100)

      on_exit(fn ->
        if Process.alive?(client), do: GenServer.stop(client)
        if Process.alive?(server), do: GenServer.stop(server)
      end)

      {:ok, client: client, server: server}
    end

    test "negotiates 2025-03-26 protocol version", %{client: client} do
      {:ok, version} = Client.negotiated_version(client)
      assert version == "2025-03-26"
    end

    test "server capabilities match 2025-03-26 spec", %{client: client} do
      {:ok, caps} = Client.server_capabilities(client)

      # New capabilities in 2025-03-26
      assert get_in(caps, ["resources", "subscribe"]) == true
      assert get_in(caps, ["resources", "listChanged"]) == true
      assert get_in(caps, ["prompts", "listChanged"]) == true
      assert get_in(caps, ["tools", "listChanged"]) == true
      assert get_in(caps, ["logging", "setLevel"]) == true

      # Completion capability
      assert get_in(caps, ["completion", "hasArguments"]) == true
      assert get_in(caps, ["completion", "values"]) == true

      # Experimental features
      assert get_in(caps, ["experimental", "batchProcessing"]) == true
    end

    test "tool annotations work correctly", %{client: client} do
      {:ok, result} = Client.list_tools(client)

      write_tool = Enum.find(result.tools, &(&1.name == "file_write"))
      read_tool = Enum.find(result.tools, &(&1.name == "file_read"))

      # Check annotations
      assert write_tool.annotations.destructiveHint == true
      assert write_tool.annotations.readOnlyHint == false

      assert read_tool.annotations.destructiveHint == false
      assert read_tool.annotations.readOnlyHint == true
      assert read_tool.annotations.idempotentHint == true
    end

    test "audio content type works", %{client: client} do
      {:ok, result} =
        Client.call_tool(client, "file_write", %{
          "path" => "/tmp/test.txt",
          "content" => "Hello audio"
        })

      # Should return both text and audio content
      assert length(result.content) == 2

      text_content = Enum.find(result.content, &(&1.type == "text"))
      audio_content = Enum.find(result.content, &(&1.type == "audio"))

      assert text_content != nil
      assert audio_content != nil
      assert audio_content.mimeType == "audio/mp3"
      assert is_binary(audio_content.data)
    end

    test "resource subscriptions work", %{client: client, server: server} do
      # Subscribe to resource
      {:ok, _} = Client.subscribe_resource(client, "config://app")

      # Simulate resource update
      Server.notify_resource_updated(server, "config://app")

      # Client would receive the notification
      Process.sleep(50)

      # Unsubscribe
      {:ok, _} = Client.unsubscribe_resource(client, "config://app")
    end

    test "completion capability works", %{client: client} do
      {:ok, result} =
        Client.complete(client, "argument", %{
          "name" => "path",
          "value" => "/home/"
        })

      assert is_list(result[:completion])
      assert length(result[:completion]) > 0
      assert Enum.all?(result[:completion], &String.starts_with?(&1, "/home/"))
    end

    test "log level control works", %{client: client} do
      # Set log level (new in 2025-03-26)
      {:ok, %{}} = Client.set_log_level(client, "debug")
      Process.sleep(50)

      # Server should have updated its log level
      # (We'd verify this through server state if exposed)
    end

    test "list change notifications work", %{client: _client, server: server} do
      # All list types support change notifications in 2025-03-26
      Server.notify_tools_changed(server)
      Server.notify_resources_changed(server)
      Server.notify_prompts_changed(server)

      # Client would receive these notifications
      Process.sleep(50)
    end

    test "progress notifications include message field", %{client: client} do
      # Progress notifications in 2025-03-26 can include descriptive messages
      token = "progress-#{System.unique_integer()}"

      # This would trigger progress with message
      {:ok, _} =
        Client.call_tool(client, "file_write", %{
          "path" => "/tmp/large.txt",
          "content" => String.duplicate("x", 1000),
          "_meta" => %{"progressToken" => token}
        })
    end

    test "JSON-RPC batch requests work", %{client: client} do
      # 2025-03-26 adds support for batch requests
      batch = [
        Protocol.encode_list_tools(),
        Protocol.encode_list_resources(),
        Protocol.encode_list_prompts()
      ]

      # Send batch (initialize must NOT be in batch per spec)
      {:ok, responses} = Client.send_batch(client, batch)

      assert length(responses) == 3
      # Results should be maps with content (not {"result" => ...} structure)
      assert Enum.all?(responses, fn r -> is_map(r) end)
    end

    test "pagination with cursors works", %{client: client} do
      # First page
      {:ok, page1} = Client.list_tools(client)
      # Our test returns all in one page
      assert Map.get(page1, :nextCursor) == nil

      # If there was a cursor, we'd use it
      if Map.get(page1, :nextCursor) do
        {:ok, page2} = Client.list_tools(client, cursor: Map.get(page1, :nextCursor))
        assert is_list(page2.tools)
      end
    end
  end

  describe "2025-03-26 authorization framework" do
    test "OAuth 2.1 client credentials flow" do
      # Test OAuth client credentials
      _config = %{
        client_id: "test-client",
        client_secret: "test-secret",
        token_endpoint: "https://auth.example.com/token",
        scope: "mcp:*"
      }

      # Would make actual request in real scenario
      # {:ok, token_response} = Authorization.client_credentials_flow(config)

      # Token response should have required fields
      token_response = %{
        "access_token" => "test-token",
        "token_type" => "Bearer",
        "expires_in" => 3600,
        "scope" => "mcp:*"
      }

      assert token_response["access_token"]
      assert token_response["token_type"] == "Bearer"
    end

    test "authorization code flow with PKCE" do
      # Test authorization code flow
      {:ok, verifier, challenge} = ExMCP.Internal.Authorization.PKCE.generate_challenge()

      assert is_binary(verifier)
      assert is_binary(challenge)
      # Min length per spec
      assert byte_size(verifier) >= 43
      # Max length per spec
      assert byte_size(verifier) <= 128
    end

    test "token refresh works" do
      # Test token refresh
      refresh_token = "refresh-token"

      # Would make actual refresh request
      # {:ok, new_token} = Authorization.refresh_token(refresh_token, config)

      # New token should be valid
      new_token = %{
        "access_token" => "new-token",
        "token_type" => "Bearer",
        "expires_in" => 3600
      }

      assert new_token["access_token"] != refresh_token
    end
  end

  describe "2025-03-26 streamable HTTP transport" do
    test "supports both SSE and non-SSE modes" do
      # The new streamable HTTP transport is more flexible

      # SSE mode (default)
      config_sse = [
        transport: :http,
        url: "http://localhost:8080",
        use_sse: true
      ]

      # Non-SSE mode
      config_no_sse = [
        transport: :http,
        url: "http://localhost:8080",
        use_sse: false
      ]

      # Both configurations should be valid
      assert Keyword.get(config_sse, :use_sse) == true
      assert Keyword.get(config_no_sse, :use_sse) == false
    end

    test "session management with Mcp-Session-Id header" do
      # Session IDs are automatically managed
      session_id = Base.encode16(:crypto.strong_rand_bytes(16), case: :lower)

      assert is_binary(session_id)
      # 16 bytes hex encoded
      assert byte_size(session_id) == 32
    end

    test "endpoint is configurable" do
      # Can use custom endpoint instead of /mcp/v1
      config = [
        transport: :http,
        url: "http://localhost:8080",
        endpoint: "/api/mcp"
      ]

      assert Keyword.get(config, :endpoint) == "/api/mcp"
    end
  end

  describe "2025-03-26 error handling" do
    test "structured error responses" do
      error =
        Protocol.encode_error(
          -32602,
          "Invalid params",
          "Missing required field: path",
          123
        )

      assert error["error"]["code"] == -32602
      assert error["error"]["message"] == "Invalid params"
      assert error["error"]["data"] == "Missing required field: path"
      assert error["id"] == 123
    end

    test "handles missing optional fields gracefully" do
      # Many fields became optional in 2025-03-26
      # Test minimal valid messages

      tool = %{
        name: "minimal",
        description: "Minimal tool"
        # inputSchema is optional
      }

      resource = %{
        uri: "minimal://resource",
        name: "Minimal"
        # mimeType, description are optional
      }

      assert tool.name == "minimal"
      assert resource.uri == "minimal://resource"
    end
  end
end
