# ExMCP Conformance Test Server
#
# Implements the "everything server" pattern for MCP conformance testing.
# Matches the TypeScript SDK's everythingServer.ts fixtures.
#
# Start with: elixir test/conformance/server.exs [port]
# Then run:   npx @modelcontextprotocol/conformance server --url http://localhost:PORT/mcp

Mix.install([{:ex_mcp, path: "."}, {:plug_cowboy, "~> 2.7"}, {:jason, "~> 1.4"}])

defmodule ConformanceServer do
  use ExMCP.Server.Handler

  # Test data matching TypeScript SDK
  # 1x1 red PNG pixel
  @test_image_base64 "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8DwHwAFBQIAX8jx0gAAAABJRU5ErkJggg=="
  # Minimal WAV file
  @test_audio_base64 "UklGRiYAAABXQVZFZm10IBAAAAABAAEAQB8AAAB9AAACABAAZGF0YQIAAAA="

  @impl true
  def init(_args) do
    {:ok, %{subscriptions: MapSet.new()}}
  end

  @impl true
  def handle_initialize(_params, state) do
    {:ok,
     %{
       name: "mcp-conformance-test-server",
       version: "1.0.0",
       capabilities: %{
         tools: %{listChanged: true},
         resources: %{subscribe: true, listChanged: true},
         prompts: %{listChanged: true},
         logging: %{},
         completions: %{}
       }
     }, state}
  end

  # ── Tools ──────────────────────────────────────────────────────

  @impl true
  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        name: "test_simple_text",
        description: "Tests simple text content response",
        inputSchema: %{type: "object", properties: %{}}
      },
      %{
        name: "test_image_content",
        description: "Tests image content response",
        inputSchema: %{type: "object", properties: %{}}
      },
      %{
        name: "test_audio_content",
        description: "Tests audio content response",
        inputSchema: %{type: "object", properties: %{}}
      },
      %{
        name: "test_embedded_resource",
        description: "Tests embedded resource content response",
        inputSchema: %{type: "object", properties: %{}}
      },
      %{
        name: "test_multiple_content_types",
        description: "Tests response with multiple content types (text, image, resource)",
        inputSchema: %{type: "object", properties: %{}}
      },
      %{
        name: "test_tool_with_logging",
        description: "Tests tool that emits log messages during execution",
        inputSchema: %{type: "object", properties: %{}}
      },
      %{
        name: "test_error_handling",
        description: "Tests error response handling",
        inputSchema: %{type: "object", properties: %{}}
      },
      %{
        name: "test_tool_with_progress",
        description: "Tests tool that reports progress notifications",
        inputSchema: %{type: "object", properties: %{}}
      },
      %{
        name: "test_sampling",
        description: "Tests server-initiated sampling (LLM completion request)",
        inputSchema: %{
          type: "object",
          properties: %{prompt: %{type: "string", description: "The prompt to send to the LLM"}},
          required: ["prompt"]
        }
      },
      %{
        name: "test_elicitation",
        description: "Tests server-initiated elicitation (user input request)",
        inputSchema: %{
          type: "object",
          properties: %{message: %{type: "string", description: "The message to show the user"}},
          required: ["message"]
        }
      }
    ]

    {:ok, tools, nil, state}
  end

  @impl true
  def handle_call_tool("test_simple_text", _args, state) do
    {:ok, [%{type: "text", text: "This is a simple text response for testing."}], state}
  end

  def handle_call_tool("test_image_content", _args, state) do
    {:ok, [%{type: "image", data: @test_image_base64, mimeType: "image/png"}], state}
  end

  def handle_call_tool("test_audio_content", _args, state) do
    {:ok, [%{type: "audio", data: @test_audio_base64, mimeType: "audio/wav"}], state}
  end

  def handle_call_tool("test_embedded_resource", _args, state) do
    {:ok,
     [
       %{
         type: "resource",
         resource: %{
           uri: "test://embedded-resource",
           mimeType: "text/plain",
           text: "This is an embedded resource content."
         }
       }
     ], state}
  end

  def handle_call_tool("test_multiple_content_types", _args, state) do
    {:ok,
     [
       %{type: "text", text: "Multiple content types test:"},
       %{type: "image", data: @test_image_base64, mimeType: "image/png"},
       %{
         type: "resource",
         resource: %{
           uri: "test://mixed-content-resource",
           mimeType: "application/json",
           text: Jason.encode!(%{test: "data", value: 123})
         }
       }
     ], state}
  end

  def handle_call_tool("test_tool_with_logging", _args, state) do
    # Logging is handled by SSE interceptor in ConformanceRouter
    {:ok, [%{type: "text", text: "Tool with logging executed successfully"}], state}
  end

  def handle_call_tool("test_error_handling", _args, state) do
    {:error, "This tool intentionally returns an error for testing", state}
  end

  def handle_call_tool("test_tool_with_progress", _args, state) do
    # TODO: emit notifications/progress during execution
    {:ok, [%{type: "text", text: "0"}], state}
  end

  def handle_call_tool("test_sampling", %{"prompt" => prompt}, state) do
    # TODO: send sampling/createMessage to client
    {:ok, [%{type: "text", text: "Sampling not supported: #{prompt}"}], state}
  end

  def handle_call_tool("test_elicitation", %{"message" => message}, state) do
    # TODO: send elicitation/create to client
    {:ok, [%{type: "text", text: "Elicitation not supported: #{message}"}], state}
  end

  def handle_call_tool(name, _args, state) do
    {:error, "Unknown tool: #{name}", state}
  end

  # ── Resources ──────────────────────────────────────────────────

  @impl true
  def handle_list_resources(_cursor, state) do
    resources = [
      %{
        uri: "test://static-text",
        name: "Static Text Resource",
        description: "A static text resource for testing",
        mimeType: "text/plain"
      },
      %{
        uri: "test://static-binary",
        name: "Static Binary Resource",
        description: "A static binary resource (image) for testing",
        mimeType: "image/png"
      },
      %{
        uri: "test://watched-resource",
        name: "Watched Resource",
        description: "A resource that can be subscribed to",
        mimeType: "text/plain"
      }
    ]

    {:ok, resources, nil, state}
  end

  @impl true
  def handle_read_resource("test://static-text", state) do
    {:ok,
     [
       %{
         uri: "test://static-text",
         mimeType: "text/plain",
         text: "This is the content of the static text resource."
       }
     ], state}
  end

  def handle_read_resource("test://static-binary", state) do
    {:ok,
     [
       %{
         uri: "test://static-binary",
         mimeType: "image/png",
         blob: @test_image_base64
       }
     ], state}
  end

  def handle_read_resource("test://watched-resource", state) do
    {:ok,
     [
       %{
         uri: "test://watched-resource",
         mimeType: "text/plain",
         text: "Watched resource content"
       }
     ], state}
  end

  # Handle template URIs
  def handle_read_resource("test://template/" <> rest, state) do
    id = rest |> String.split("/") |> List.first()

    {:ok,
     [
       %{
         uri: "test://template/#{id}/data",
         mimeType: "application/json",
         text: Jason.encode!(%{id: id, templateTest: true, data: "Data for ID: #{id}"})
       }
     ], state}
  end

  def handle_read_resource(uri, state) do
    {:error, "Resource not found: #{uri}", state}
  end

  # ── Resource Templates ─────────────────────────────────────────

  @impl true
  def handle_list_resource_templates(_cursor, state) do
    templates = [
      %{
        uriTemplate: "test://template/{id}/data",
        name: "Resource Template",
        description: "A resource template with parameter substitution",
        mimeType: "application/json"
      }
    ]

    {:ok, templates, nil, state}
  end

  # ── Prompts ────────────────────────────────────────────────────

  @impl true
  def handle_list_prompts(_cursor, state) do
    prompts = [
      %{
        name: "test_simple_prompt",
        description: "A simple prompt without arguments"
      },
      %{
        name: "test_prompt_with_arguments",
        description: "A prompt with required arguments",
        arguments: [
          %{name: "arg1", description: "First test argument", required: true},
          %{name: "arg2", description: "Second test argument", required: true}
        ]
      },
      %{
        name: "test_prompt_with_embedded_resource",
        description: "A prompt that includes an embedded resource",
        arguments: [
          %{name: "resourceUri", description: "URI of the resource to embed", required: true}
        ]
      },
      %{
        name: "test_prompt_with_image",
        description: "A prompt that includes image content"
      }
    ]

    {:ok, prompts, nil, state}
  end

  @impl true
  def handle_get_prompt("test_simple_prompt", _args, state) do
    {:ok,
     %{
       messages: [
         %{role: "user", content: %{type: "text", text: "This is a simple prompt for testing."}}
       ]
     }, state}
  end

  def handle_get_prompt("test_prompt_with_arguments", args, state) do
    arg1 = Map.get(args, "arg1", "")
    arg2 = Map.get(args, "arg2", "")

    {:ok,
     %{
       messages: [
         %{
           role: "user",
           content: %{
             type: "text",
             text: "Prompt with arguments: arg1='#{arg1}', arg2='#{arg2}'"
           }
         }
       ]
     }, state}
  end

  def handle_get_prompt("test_prompt_with_embedded_resource", args, state) do
    resource_uri = Map.get(args, "resourceUri", "test://embedded-resource")

    {:ok,
     %{
       messages: [
         %{
           role: "user",
           content: %{
             type: "resource",
             resource: %{
               uri: resource_uri,
               mimeType: "text/plain",
               text: "Embedded resource content for testing."
             }
           }
         },
         %{
           role: "user",
           content: %{
             type: "text",
             text: "Please process the embedded resource above."
           }
         }
       ]
     }, state}
  end

  def handle_get_prompt("test_prompt_with_image", _args, state) do
    {:ok,
     %{
       messages: [
         %{
           role: "user",
           content: %{
             type: "image",
             data: @test_image_base64,
             mimeType: "image/png"
           }
         },
         %{
           role: "user",
           content: %{type: "text", text: "Please analyze the image above."}
         }
       ]
     }, state}
  end

  def handle_get_prompt(name, _args, state) do
    {:error, "Unknown prompt: #{name}", state}
  end

  # ── Subscribe/Unsubscribe ──────────────────────────────────────

  @impl true
  def handle_subscribe_resource(uri, state) do
    {:ok, %{state | subscriptions: MapSet.put(state.subscriptions, uri)}}
  end

  @impl true
  def handle_unsubscribe_resource(uri, state) do
    {:ok, %{state | subscriptions: MapSet.delete(state.subscriptions, uri)}}
  end
end

# Parse port from args
port = String.to_integer(List.first(System.argv(), "3001"))

IO.puts("Starting ExMCP conformance server on port #{port}...")

IO.puts(
  "Test with: npx @modelcontextprotocol/conformance server --url http://localhost:#{port}/mcp"
)

# DNS rebinding protection plug
defmodule DnsRebindingPlug do
  @behaviour Plug
  import Plug.Conn

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    host = get_req_header(conn, "host") |> List.first("")

    # Strip port from host header
    hostname = host |> String.split(":") |> List.first() |> String.downcase()

    if hostname in ["localhost", "127.0.0.1", "::1", "[::1]", "0.0.0.0"] do
      conn
    else
      conn
      |> put_resp_content_type("text/plain")
      |> send_resp(403, "Forbidden: Invalid Host header")
      |> halt()
    end
  end
end

# Compose plugs: DNS protection → custom POST handler → MCP handler
defmodule ConformanceRouter do
  @behaviour Plug
  import Plug.Conn
  require Logger

  # Tools that need SSE streaming
  # Tools that need SSE streaming
  @sse_tools ~w(test_tool_with_logging test_tool_with_progress test_sampling test_elicitation test_elicitation_sep1034_defaults test_elicitation_sep1330_enums)

  # Session state: maps session_id → %{sse_conn: conn, pending: queue}
  # Using ETS for cross-process state
  def init_sessions do
    if :ets.info(:conformance_sessions) == :undefined do
      :ets.new(:conformance_sessions, [:set, :public, :named_table])
    end
  end

  def store_sse_conn(session_id, pid) do
    :ets.insert(:conformance_sessions, {session_id, pid})
  end

  def get_sse_pid(session_id) do
    case :ets.lookup(:conformance_sessions, session_id) do
      [{_, pid}] -> pid
      _ -> nil
    end
  end

  # ETS for pending server→client requests
  def ensure_ets do
    if :ets.info(:conformance_pending) == :undefined do
      :ets.new(:conformance_pending, [:set, :public, :named_table])
    end
  end

  @mcp_plug_opts ExMCP.HttpPlug.init(
                   handler: ConformanceServer,
                   server_info: %{name: "mcp-conformance-test-server", version: "1.0.0"},
                   sse_enabled: true,
                   cors_enabled: true
                 )

  @impl true
  def init(_opts), do: []

  @impl true
  def call(conn, _opts) do
    ensure_ets()

    # DNS rebinding check first
    conn = DnsRebindingPlug.call(conn, [])

    if conn.halted do
      conn
    else
      case conn.method do
        "POST" -> handle_post(conn)
        "GET" -> handle_get(conn)
        _ -> ExMCP.HttpPlug.call(conn, @mcp_plug_opts)
      end
    end
  end

  # Handle GET requests — SSE stream for server→client messages
  defp handle_get(conn) do
    accepts_sse =
      conn
      |> get_req_header("accept")
      |> Enum.any?(&String.contains?(&1, "text/event-stream"))

    if accepts_sse do
      session_id = get_session_id(conn)
      handle_get_sse(conn, session_id)
    else
      ExMCP.HttpPlug.call(conn, @mcp_plug_opts)
    end
  end

  # Start GET SSE stream — register this process so tool handlers can send to it
  defp handle_get_sse(conn, session_id) do
    conn =
      conn
      |> put_resp_header("content-type", "text/event-stream")
      |> put_resp_header("cache-control", "no-cache")
      |> put_resp_header("connection", "keep-alive")
      |> put_resp_header("mcp-session-id", session_id)
      |> send_chunked(200)

    # Register this process as the SSE stream for this session
    :ets.insert(:conformance_pending, {"sse_pid:#{session_id}", self()})

    # Keep connection alive, forwarding messages as SSE events
    sse_loop(conn, session_id)
  end

  defp sse_loop(conn, session_id) do
    receive do
      {:sse_send, data} ->
        case chunk(conn, "event: message\ndata: #{Jason.encode!(data)}\n\n") do
          {:ok, conn} -> sse_loop(conn, session_id)
          {:error, _} -> conn
        end

      :sse_close ->
        conn
    after
      60_000 ->
        # Keep-alive timeout
        conn
    end
  end

  defp handle_post(conn) do
    {:ok, body, conn} = Plug.Conn.read_body(conn)

    case Jason.decode(body) do
      {:ok, %{"method" => "tools/call", "params" => %{"name" => name}, "id" => id} = request}
      when name in @sse_tools ->
        handle_sse_tool(conn, name, id, request)

      # Client response to a server-initiated request (has "result" or "error", no "method")
      {:ok, %{"id" => id, "result" => result}} when is_integer(id) or is_binary(id) ->
        handle_client_response(conn, id, {:ok, result})

      {:ok, %{"id" => id, "error" => error}} when is_integer(id) or is_binary(id) ->
        handle_client_response(conn, id, {:error, error})

      _ ->
        handle_normal_post(conn, body)
    end
  end

  # Route client response back to the waiting tool handler
  defp handle_client_response(conn, id, result) do
    session_id = get_session_id(conn)
    key = "pending:#{id}"

    case :ets.lookup(:conformance_pending, key) do
      [{_, pid}] ->
        :ets.delete(:conformance_pending, key)
        send(pid, {:client_response, id, result})

        conn
        |> put_resp_header("mcp-session-id", session_id)
        |> send_resp(202, "")

      _ ->
        # No pending request — might be a regular response, pass through
        handle_normal_post(conn, Jason.encode!(%{id: id, result: result}))
    end
  end

  defp handle_normal_post(conn, body) do
    case Jason.decode(body) do
      {:ok, request} ->
        session_id = get_session_id(conn)

        mcp_conn = ExMCP.MessageProcessor.new(request, transport: :http)

        processed =
          ExMCP.MessageProcessor.process(mcp_conn, %{
            handler: ConformanceServer,
            server_info: %{name: "mcp-conformance-test-server", version: "1.0.0"}
          })

        case processed.response do
          nil ->
            if Map.get(request, "id") == nil do
              conn
              |> put_resp_header("mcp-session-id", session_id)
              |> send_resp(202, "")
            else
              conn
              |> put_resp_content_type("application/json")
              |> put_resp_header("mcp-session-id", session_id)
              |> send_resp(
                500,
                Jason.encode!(%{
                  jsonrpc: "2.0",
                  error: %{code: -32603, message: "No response"},
                  id: request["id"]
                })
              )
            end

          response ->
            conn
            |> put_resp_content_type("application/json")
            |> put_resp_header("mcp-session-id", session_id)
            |> put_resp_header("mcp-protocol-version", "2025-11-25")
            |> send_resp(200, Jason.encode!(response))
        end

      {:error, _} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(
          400,
          Jason.encode!(%{jsonrpc: "2.0", error: %{code: -32700, message: "Parse error"}})
        )
    end
  end

  defp handle_sse_tool(conn, "test_tool_with_logging", id, _request) do
    session_id = get_session_id(conn)

    conn =
      conn
      |> put_resp_header("content-type", "text/event-stream")
      |> put_resp_header("cache-control", "no-cache")
      |> put_resp_header("connection", "keep-alive")
      |> put_resp_header("mcp-session-id", session_id)
      |> send_chunked(200)

    # Send log notifications
    for msg <- ["Tool execution started", "Tool processing data", "Tool execution completed"] do
      notification = %{
        jsonrpc: "2.0",
        method: "notifications/message",
        params: %{level: "info", logger: "conformance-test-server", data: msg}
      }

      {:ok, conn} = chunk(conn, "event: message\ndata: #{Jason.encode!(notification)}\n\n")
      Process.sleep(50)
    end

    # Send result
    result = %{
      jsonrpc: "2.0",
      id: id,
      result: %{content: [%{type: "text", text: "Tool with logging executed successfully"}]}
    }

    {:ok, conn} = chunk(conn, "event: message\ndata: #{Jason.encode!(result)}\n\n")
    conn
  end

  defp handle_sse_tool(conn, "test_tool_with_progress", id, request) do
    session_id = get_session_id(conn)
    progress_token = get_in(request, ["params", "_meta", "progressToken"]) || 0

    conn =
      conn
      |> put_resp_header("content-type", "text/event-stream")
      |> put_resp_header("cache-control", "no-cache")
      |> put_resp_header("connection", "keep-alive")
      |> put_resp_header("mcp-session-id", session_id)
      |> send_chunked(200)

    # Send progress notifications
    for {progress, total} <- [{0, 100}, {50, 100}, {100, 100}] do
      notification = %{
        jsonrpc: "2.0",
        method: "notifications/progress",
        params: %{
          progressToken: progress_token,
          progress: progress,
          total: total,
          message: "Completed step #{progress} of #{total}"
        }
      }

      {:ok, conn} = chunk(conn, "event: message\ndata: #{Jason.encode!(notification)}\n\n")
      Process.sleep(50)
    end

    # Send result
    result = %{
      jsonrpc: "2.0",
      id: id,
      result: %{content: [%{type: "text", text: "#{progress_token}"}]}
    }

    {:ok, conn} = chunk(conn, "event: message\ndata: #{Jason.encode!(result)}\n\n")
    conn
  end

  # Helper: send a request to the client via GET SSE stream and wait for response
  defp send_server_request(session_id, request_id, method, params) do
    sse_key = "sse_pid:#{session_id}"

    case :ets.lookup(:conformance_pending, sse_key) do
      [{_, sse_pid}] ->
        # Register this process to receive the response
        :ets.insert(:conformance_pending, {"pending:#{request_id}", self()})

        # Send request via SSE stream
        request = %{jsonrpc: "2.0", id: request_id, method: method, params: params}
        send(sse_pid, {:sse_send, request})

        # Wait for client response
        receive do
          {:client_response, ^request_id, result} -> result
        after
          10_000 -> {:error, "Timeout waiting for client response"}
        end

      _ ->
        {:error, "No SSE stream for session #{session_id}"}
    end
  end

  defp handle_sse_tool(conn, "test_sampling", id, request) do
    session_id = get_session_id(conn)
    prompt = get_in(request, ["params", "arguments", "prompt"]) || "Test prompt"

    conn =
      conn
      |> put_resp_header("content-type", "text/event-stream")
      |> put_resp_header("cache-control", "no-cache")
      |> put_resp_header("mcp-session-id", session_id)
      |> send_chunked(200)

    # Send sampling request to client via GET SSE
    req_id = System.unique_integer([:positive])

    case send_server_request(session_id, req_id, "sampling/createMessage", %{
           messages: [%{role: "user", content: %{type: "text", text: prompt}}],
           maxTokens: 100
         }) do
      {:ok, result} ->
        text =
          get_in(result, ["content", "text"]) || get_in(result, ["message", "content", "text"]) ||
            "No response"

        tool_result = %{
          jsonrpc: "2.0",
          id: id,
          result: %{content: [%{type: "text", text: "LLM response: #{text}"}]}
        }

        chunk(conn, "event: message\ndata: #{Jason.encode!(tool_result)}\n\n")

      {:error, reason} ->
        tool_result = %{
          jsonrpc: "2.0",
          id: id,
          result: %{content: [%{type: "text", text: "Sampling error: #{reason}"}]}
        }

        chunk(conn, "event: message\ndata: #{Jason.encode!(tool_result)}\n\n")
    end

    conn
  end

  defp handle_sse_tool(conn, "test_elicitation", id, request) do
    session_id = get_session_id(conn)
    message = get_in(request, ["params", "arguments", "message"]) || "Please provide info"

    conn =
      conn
      |> put_resp_header("content-type", "text/event-stream")
      |> put_resp_header("cache-control", "no-cache")
      |> put_resp_header("mcp-session-id", session_id)
      |> send_chunked(200)

    req_id = System.unique_integer([:positive])

    case send_server_request(session_id, req_id, "elicitation/create", %{
           message: message,
           requestedSchema: %{
             type: "object",
             properties: %{response: %{type: "string", description: "User's response"}},
             required: ["response"]
           }
         }) do
      {:ok, result} ->
        action = result["action"] || "unknown"
        content = Jason.encode!(result["content"] || %{})

        tool_result = %{
          jsonrpc: "2.0",
          id: id,
          result: %{
            content: [
              %{type: "text", text: "User response: action=#{action}, content=#{content}"}
            ]
          }
        }

        chunk(conn, "event: message\ndata: #{Jason.encode!(tool_result)}\n\n")

      {:error, reason} ->
        tool_result = %{
          jsonrpc: "2.0",
          id: id,
          result: %{content: [%{type: "text", text: "Elicitation error: #{reason}"}]}
        }

        chunk(conn, "event: message\ndata: #{Jason.encode!(tool_result)}\n\n")
    end

    conn
  end

  defp handle_sse_tool(conn, "test_elicitation_sep1034_defaults", id, _request) do
    session_id = get_session_id(conn)

    conn =
      conn
      |> put_resp_header("content-type", "text/event-stream")
      |> put_resp_header("cache-control", "no-cache")
      |> put_resp_header("mcp-session-id", session_id)
      |> send_chunked(200)

    req_id = System.unique_integer([:positive])

    case send_server_request(session_id, req_id, "elicitation/create", %{
           message: "Please review and update the form fields with defaults",
           requestedSchema: %{
             type: "object",
             properties: %{
               name: %{type: "string", description: "User name", default: "John Doe"},
               age: %{type: "integer", description: "User age", default: 30},
               score: %{type: "number", description: "User score", default: 95.5},
               status: %{
                 type: "string",
                 description: "User status",
                 enum: ["active", "inactive", "pending"],
                 default: "active"
               },
               verified: %{type: "boolean", description: "Verification status", default: true}
             },
             required: []
           }
         }) do
      {:ok, result} ->
        tool_result = %{
          jsonrpc: "2.0",
          id: id,
          result: %{
            content: [
              %{
                type: "text",
                text:
                  "Elicitation completed: action=#{result["action"]}, content=#{Jason.encode!(result["content"] || %{})}"
              }
            ]
          }
        }

        chunk(conn, "event: message\ndata: #{Jason.encode!(tool_result)}\n\n")

      {:error, reason} ->
        tool_result = %{
          jsonrpc: "2.0",
          id: id,
          result: %{content: [%{type: "text", text: "Error: #{reason}"}]}
        }

        chunk(conn, "event: message\ndata: #{Jason.encode!(tool_result)}\n\n")
    end

    conn
  end

  defp handle_sse_tool(conn, "test_elicitation_sep1330_enums", id, _request) do
    session_id = get_session_id(conn)

    conn =
      conn
      |> put_resp_header("content-type", "text/event-stream")
      |> put_resp_header("cache-control", "no-cache")
      |> put_resp_header("mcp-session-id", session_id)
      |> send_chunked(200)

    req_id = System.unique_integer([:positive])

    case send_server_request(session_id, req_id, "elicitation/create", %{
           message: "Please select options from the enum fields",
           requestedSchema: %{
             type: "object",
             properties: %{
               untitledSingle: %{
                 type: "string",
                 description: "Select one option",
                 enum: ["option1", "option2", "option3"]
               },
               titledSingle: %{
                 type: "string",
                 description: "Select one with titles",
                 oneOf: [
                   %{const: "value1", title: "First Option"},
                   %{const: "value2", title: "Second Option"},
                   %{const: "value3", title: "Third Option"}
                 ]
               },
               legacyEnum: %{
                 type: "string",
                 description: "Select one (legacy)",
                 enum: ["opt1", "opt2", "opt3"],
                 enumNames: ["Option One", "Option Two", "Option Three"]
               },
               untitledMulti: %{
                 type: "array",
                 description: "Select multiple",
                 minItems: 1,
                 maxItems: 3,
                 items: %{type: "string", enum: ["option1", "option2", "option3"]}
               },
               titledMulti: %{
                 type: "array",
                 description: "Select multiple with titles",
                 minItems: 1,
                 maxItems: 3,
                 items: %{
                   anyOf: [
                     %{const: "value1", title: "First Choice"},
                     %{const: "value2", title: "Second Choice"},
                     %{const: "value3", title: "Third Choice"}
                   ]
                 }
               }
             },
             required: []
           }
         }) do
      {:ok, result} ->
        tool_result = %{
          jsonrpc: "2.0",
          id: id,
          result: %{
            content: [
              %{
                type: "text",
                text:
                  "Elicitation completed: action=#{result["action"]}, content=#{Jason.encode!(result["content"] || %{})}"
              }
            ]
          }
        }

        chunk(conn, "event: message\ndata: #{Jason.encode!(tool_result)}\n\n")

      {:error, reason} ->
        tool_result = %{
          jsonrpc: "2.0",
          id: id,
          result: %{content: [%{type: "text", text: "Error: #{reason}"}]}
        }

        chunk(conn, "event: message\ndata: #{Jason.encode!(tool_result)}\n\n")
    end

    conn
  end

  defp get_session_id(conn) do
    case get_req_header(conn, "mcp-session-id") do
      [id | _] -> id
      _ -> "session-#{System.unique_integer([:positive])}"
    end
  end
end

# Start Cowboy
children = [
  {Plug.Cowboy, scheme: :http, plug: ConformanceRouter, options: [port: port]}
]

{:ok, _} = Supervisor.start_link(children, strategy: :one_for_one)

IO.puts("Server ready on http://localhost:#{port}/mcp")

# Keep the process alive
Process.sleep(:infinity)
