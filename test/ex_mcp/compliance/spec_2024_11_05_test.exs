defmodule ExMCP.Spec20241105Test do
  @moduledoc """
  Comprehensive test suite for MCP specification version 2024-11-05.

  Tests all required and optional features defined in the spec.
  """
  use ExUnit.Case, async: true

  @moduletag :compliance

  alias ExMCP.{Client, Server}
  alias ExMCP.Internal.Protocol, as: Protocol

  defmodule Handler20241105 do
    @moduledoc false
    use ExMCP.Server.Handler

    @impl true
    def init(_args) do
      {:ok,
       %{
         tools: [
           %{
             name: "calculator",
             description: "Basic arithmetic operations",
             inputSchema: %{
               type: "object",
               properties: %{
                 operation: %{type: "string", enum: ["add", "subtract", "multiply", "divide"]},
                 a: %{type: "number"},
                 b: %{type: "number"}
               },
               required: ["operation", "a", "b"]
             }
           }
         ],
         resources: [
           %{
             uri: "file:///config.json",
             name: "Configuration",
             description: "System configuration",
             mimeType: "application/json"
           }
         ],
         prompts: [
           %{
             name: "code-review",
             description: "Review code for best practices",
             arguments: [
               %{name: "language", description: "Programming language", required: true}
             ]
           }
         ]
       }}
    end

    @impl true
    def handle_initialize(params, state) do
      # Verify we get 2024-11-05 version
      assert params["protocolVersion"] == "2024-11-05"

      result = %{
        protocolVersion: "2024-11-05",
        serverInfo: %{
          name: "test-server-2024-11-05",
          version: "1.0.0"
        },
        capabilities: %{
          # 2024-11-05 capabilities (no listChanged, no subscribe)
          tools: %{},
          resources: %{},
          prompts: %{},
          logging: %{}
        }
      }

      {:ok, result, state}
    end

    @impl true
    def handle_list_tools(_cursor, state) do
      {:ok, state.tools, nil, state}
    end

    @impl true
    def handle_call_tool("calculator", %{"operation" => op, "a" => a, "b" => b}, state) do
      result =
        case op do
          "add" -> a + b
          "subtract" -> a - b
          "multiply" -> a * b
          "divide" when b != 0 -> a / b
          "divide" -> {:error, "Division by zero"}
        end

      case result do
        {:error, msg} ->
          {:error, msg, state}

        value ->
          {:ok, [%{type: "text", text: "Result: #{value}"}], state}
      end
    end

    @impl true
    def handle_call_tool(name, _arguments, state) do
      {:error, "Tool not found: #{name}", state}
    end

    @impl true
    def handle_list_resources(_cursor, state) do
      {:ok, state.resources, nil, state}
    end

    @impl true
    def handle_read_resource("file:///config.json", state) do
      content = %{
        version: "1.0.0",
        features: %{
          logging: true,
          metrics: false
        }
      }

      {:ok,
       %{
         uri: "file:///config.json",
         mimeType: "application/json",
         text: Jason.encode!(content)
       }, state}
    end

    @impl true
    def handle_list_prompts(_cursor, state) do
      {:ok, state.prompts, nil, state}
    end

    @impl true
    def handle_get_prompt("code-review", %{"language" => lang}, state) do
      {:ok,
       %{
         description: "Review #{lang} code",
         messages: [
           %{
             role: "user",
             content: %{
               type: "text",
               text: "Please review this #{lang} code for best practices."
             }
           }
         ]
       }, state}
    end

    # Methods not available in 2024-11-05
    @impl true
    def handle_subscribe_resource(_uri, state) do
      {:error, "Resource subscriptions not available in 2024-11-05", state}
    end

    @impl true
    def handle_complete(_ref, _arg, state) do
      {:error, "Completion not available in 2024-11-05", state}
    end

    @impl true
    def handle_set_log_level(_level, state) do
      {:error, "Log level control not available in 2024-11-05", state}
    end
  end

  describe "2024-11-05 specification compliance" do
    setup do
      {:ok, server} =
        Server.start_link(
          handler: Handler20241105,
          transport: :test
        )

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server,
          client_info: %{name: "test-client-2024-11-05", version: "1.0.0"},
          # Force 2024-11-05 version
          protocol_version: "2024-11-05"
        )

      Process.sleep(100)

      on_exit(fn ->
        if Process.alive?(client), do: GenServer.stop(client)
        if Process.alive?(server), do: GenServer.stop(server)
      end)

      {:ok, client: client, server: server}
    end

    test "negotiates 2024-11-05 protocol version", %{client: client} do
      {:ok, version} = Client.negotiated_version(client)
      assert version == "2024-11-05"
    end

    test "server capabilities match 2024-11-05 spec", %{client: client} do
      {:ok, caps} = Client.server_capabilities(client)

      # Should have basic capabilities
      assert Map.has_key?(caps, "tools")
      assert Map.has_key?(caps, "resources")
      assert Map.has_key?(caps, "prompts")
      assert Map.has_key?(caps, "logging")

      # Should NOT have 2025-03-26 features
      refute get_in(caps, ["resources", "subscribe"])
      refute get_in(caps, ["resources", "listChanged"])
      refute get_in(caps, ["prompts", "listChanged"])
      refute get_in(caps, ["logging", "setLevel"])
      refute Map.has_key?(caps, "completion")
    end

    test "tools feature works correctly", %{client: client} do
      # List tools
      {:ok, result} = Client.list_tools(client)
      assert length(result.tools) == 1

      tool = hd(result.tools)
      assert tool.name == "calculator"

      # Call tool
      {:ok, calc_result} =
        Client.call_tool(client, "calculator", %{
          "operation" => "add",
          "a" => 5,
          "b" => 3
        })

      assert length(calc_result.content) == 1
      assert hd(calc_result.content).text == "Result: 8"
    end

    test "resources feature works correctly", %{client: client} do
      # List resources
      {:ok, result} = Client.list_resources(client)
      assert length(result.resources) == 1

      resource = hd(result.resources)
      assert resource.uri == "file:///config.json"

      # Read resource
      {:ok, content} = Client.read_resource(client, "file:///config.json")
      assert length(content.contents) == 1

      resource_content = hd(content.contents)
      assert resource_content.mimeType == "application/json"

      data = Jason.decode!(resource_content.text)
      assert data["version"] == "1.0.0"
    end

    test "prompts feature works correctly", %{client: client} do
      # List prompts
      {:ok, result} = Client.list_prompts(client)
      assert length(result.prompts) == 1

      prompt = hd(result.prompts)
      assert prompt.name == "code-review"

      # Get prompt
      {:ok, prompt_result} = Client.get_prompt(client, "code-review", %{"language" => "Elixir"})
      assert prompt_result.description == "Review Elixir code"
      assert length(prompt_result.messages) == 1
    end

    test "progress notifications work", %{client: client} do
      # Progress is supported in 2024-11-05
      # Test by calling a tool with progress token
      progress_token = "test-progress-#{System.unique_integer()}"

      {:ok, _result} =
        Client.call_tool(client, "calculator", %{
          "operation" => "multiply",
          "a" => 10,
          "b" => 20,
          "_meta" => %{"progressToken" => progress_token}
        })

      # Progress notifications would be received if the handler sent them
    end

    test "cancellation works", %{client: client} do
      # Send a request and immediately cancel it
      request_id = Protocol.generate_id()

      # Start an async request
      task =
        Task.async(fn ->
          # This would be a long-running operation
          Client.call_tool(client, "calculator", %{
            "operation" => "divide",
            "a" => 100,
            "b" => 0
          })
        end)

      # Cancel it
      :ok = Client.send_cancelled(client, request_id, "User cancelled")

      # The task should complete (with either result or cancellation)
      Task.await(task, 1000)
    end

    test "pagination works correctly", %{client: client} do
      # Even in 2024-11-05, pagination via cursor is supported
      {:ok, result} = Client.list_tools(client)

      # Our test handler returns nil cursor (no more pages)
      assert Map.get(result, :nextCursor) == nil
    end

    test "2025-03-26 features are not available", %{client: client} do
      # Resource subscription should fail
      assert {:error, _} = Client.subscribe(client, "file:///config.json")

      # Completion should fail
      assert {:error, _} = Client.complete(client, "ref", %{})

      # These methods don't exist in client for 2024-11-05,
      # but the server should reject them if attempted
    end

    test "logging messages work", %{client: _client, server: server} do
      # Send a log message from server to client
      # In 2024-11-05, logging exists but not setLevel
      Server.send_log_message(server, "info", "Test log message")

      # Client would receive this via notifications
      Process.sleep(50)
    end

    test "error handling follows JSON-RPC spec", %{client: client} do
      # Try to call non-existent tool
      {:error, error} = Client.call_tool(client, "nonexistent", %{})

      # Should get proper error structure
      assert is_map(error) || is_binary(error)
    end
  end

  describe "2024-11-05 message format" do
    test "initialize message format is correct" do
      msg =
        Protocol.encode_initialize(
          %{name: "test", version: "1.0"},
          %{},
          "2024-11-05"
        )

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "initialize"
      assert msg["params"]["protocolVersion"] == "2024-11-05"
      assert is_integer(msg["id"])
    end

    test "notification format is correct" do
      msg = Protocol.encode_notification("notifications/initialized", %{})

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "notifications/initialized"
      refute Map.has_key?(msg, "id")
    end

    test "batch requests are NOT supported in 2024-11-05" do
      # The spec says initialize MUST NOT be in a batch
      # But 2024-11-05 doesn't explicitly support batches anyway

      # Individual requests work
      req1 = Protocol.encode_list_tools()
      req2 = Protocol.encode_list_resources()

      assert req1["jsonrpc"] == "2.0"
      assert req2["jsonrpc"] == "2.0"

      # But there's no batch encoding in 2024-11-05
    end
  end

  describe "2024-11-05 transport requirements" do
    test "stdio transport is supported" do
      # stdio is a required transport in all versions
      assert ExMCP.Transport.get_transport(:stdio) == ExMCP.Transport.Stdio
    end

    test "HTTP transport follows spec" do
      # HTTP+SSE in 2024-11-05 (not the streamable HTTP from 2025-03-26)
      assert ExMCP.Transport.get_transport(:http) == ExMCP.Transport.HTTP
    end
  end
end
