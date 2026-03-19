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
    # TODO: emit notifications/message during execution
    # For now, return the result directly
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
end

# Parse port from args
port = String.to_integer(List.first(System.argv(), "3001"))

IO.puts("Starting ExMCP conformance server on port #{port}...")

IO.puts(
  "Test with: npx @modelcontextprotocol/conformance server --url http://localhost:#{port}/mcp"
)

# Start Cowboy with our MCP handler
children = [
  {Plug.Cowboy,
   scheme: :http,
   plug:
     {ExMCP.HttpPlug,
      handler: ConformanceServer,
      server_info: %{name: "mcp-conformance-test-server", version: "1.0.0"},
      sse_enabled: true,
      cors_enabled: true},
   options: [port: port]}
]

{:ok, _} = Supervisor.start_link(children, strategy: :one_for_one)

IO.puts("Server ready on http://localhost:#{port}/mcp")

# Keep the process alive
Process.sleep(:infinity)
