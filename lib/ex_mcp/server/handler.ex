defmodule ExMCP.Server.Handler do
  @moduledoc """
  This module implements the standard MCP specification.

  Behaviour for implementing MCP server handlers.

  This behaviour defines callbacks for handling all MCP protocol operations including
  tools, resources, prompts, and the new sampling/LLM integration features.

  The handler behaviour pattern is an implementation detail but all callbacks
  correspond to official MCP protocol methods.

  ## Basic Example

      defmodule MyServer do
        use ExMCP.Server.Handler
        
        @impl true
        def handle_initialize(params, state) do
          # Check client's protocol version
          client_version = params["protocolVersion"]
          
          # Accept 2025-03-26 or propose 2024-11-05 as fallback
          negotiated_version = case client_version do
            "2025-03-26" -> "2025-03-26"
            "2024-11-05" -> "2024-11-05"
            _ -> "2025-03-26"  # Propose latest as default
          end
          
          {:ok, %{
            protocolVersion: negotiated_version,
            serverInfo: %{
              name: "my-server",
              version: "1.0.0"
            },
            capabilities: %{
              tools: %{},
              resources: %{},
              prompts: %{},
              sampling: %{}  # Enable LLM features
            }
          }, state}
        end
        
        @impl true
        def handle_list_tools(state) do
          tools = [
            %{
              name: "calculate",
              description: "Perform calculations",
              inputSchema: %{
                type: "object",
                properties: %{
                  expression: %{type: "string"}
                },
                required: ["expression"]
              }
            }
          ]
          {:ok, tools, state}
        end
        
        @impl true
        def handle_call_tool("calculate", params, state) do
          # Access progress token if provided
          progress_token = get_in(params, ["_meta", "progressToken"])
          
          # Your tool implementation
          case eval_expression(params["expression"]) do
            {:ok, result} ->
              # Send progress updates if token provided
              if progress_token do
                ExMCP.Server.notify_progress(self(), progress_token, 100, 100)
              end
              
              {:ok, [%{type: "text", text: "Result: \#{result}"}], state}
              
            {:error, reason} ->
              # Return tool execution error with isError flag
              error_result = %{
                content: [%{type: "text", text: "Calculation failed: \#{reason}"}],
                isError: true
              }
              {:ok, error_result, state}
          end
        end
      end

  ## Advanced Features

  ### Structured Tool Output (Draft Feature)

  > #### Draft Feature {: .info}
  > This implements a draft MCP specification feature that may change.

  Example implementation:

      defmodule WeatherServer do
        use ExMCP.Server.Handler
        
        @impl true
        def handle_list_tools(_cursor, state) do
          tools = [
            %{
              name: "get_weather",
              description: "Get current weather data",
              inputSchema: %{
                type: "object",
                properties: %{
                  location: %{type: "string", description: "City name"}
                },
                required: ["location"]
              },
              # Draft feature: declare expected output structure
              outputSchema: %{
                type: "object",
                properties: %{
                  temperature: %{type: "number", description: "Temperature in Celsius"},
                  conditions: %{type: "string", description: "Weather conditions"},
                  humidity: %{type: "number", description: "Humidity percentage"}
                },
                required: ["temperature", "conditions"]
              }
            }
          ]
          {:ok, tools, nil, state}
        end
        
        @impl true
        def handle_call_tool("get_weather", %{"location" => location}, state) do
          # Fetch weather data (example implementation)
          # In real code, this would call an actual weather API
          temp = 22.5
          conditions = "Partly cloudy"
          humidity = 65
          
          # Return both unstructured and structured content
          result = %{
            content: [%{
              type: "text", 
              text: "Current weather in \#{location}: \#{temp}°C, \#{conditions}"
            }],
            # Draft feature: structured content matching outputSchema
            structuredContent: %{
              "temperature" => temp,
              "conditions" => conditions,
              "humidity" => humidity
            }
          }
          
          {:ok, result, state}
        end
        
        # ... other callbacks ...
      end

  ### Sampling/LLM Integration

      @impl true
      def handle_create_message(params, state) do
        messages = params["messages"]
        model_prefs = params["modelPreferences"]
        
        # Integrate with your LLM provider
        response = call_llm_api(messages, model_prefs)
        
        result = %{
          content: %{type: "text", text: response.text},
          model: response.model,
          stopReason: "stop"
        }
        
        {:ok, result, state}
      end

  ### Progress Notifications

  For long-running operations, use progress tokens:

      @impl true
      def handle_call_tool("process_file", params, state) do
        progress_token = get_in(params, ["_meta", "progressToken"])
        file_path = params["path"]
        
        # Start async processing with progress updates
        Task.start(fn ->
          process_with_progress(file_path, progress_token, self())
        end)
        
        {:ok, [%{type: "text", text: "Processing started"}], state}
      end
      
      defp process_with_progress(path, token, server) when token != nil do
        # Send progress updates
        ExMCP.Server.notify_progress(server, token, 0, 100)
        # ... processing ...
        ExMCP.Server.notify_progress(server, token, 50, 100)
        # ... more processing ...
        ExMCP.Server.notify_progress(server, token, 100, 100)
      end

  ### Dynamic Content Notifications

  Notify clients when your server's content changes:

      def add_new_tool(server, tool_def) do
        # Add tool to your server state
        # Then notify clients
        ExMCP.Server.notify_tools_changed(server)
      end
      
      def update_resource(server, uri) do
        # Update the resource
        # Then notify clients
        ExMCP.Server.notify_resource_updated(server, uri)
      end

  ## Callback Reference

  The `use` macro provides default implementations for optional callbacks.
  You only need to implement the callbacks for features your server supports.
  """

  @type state :: any()
  @type initialize_result :: ExMCP.Types.initialize_result()
  @type tool :: ExMCP.Types.tool()
  @type resource :: ExMCP.Types.resource()
  @type prompt :: ExMCP.Types.prompt()

  @doc """
  Handles the initialize request from a client.

  The params map contains:
  - `"protocolVersion"` - The client's requested protocol version
  - `"capabilities"` - The client's declared capabilities
  - `"clientInfo"` - Information about the client implementation

  ## Version Negotiation

  The server should check the client's protocol version and either:
  1. Accept it by returning the same version
  2. Propose an alternative supported version
  3. Return an error if no compatible version exists

  ## Example

      def handle_initialize(params, state) do
        client_version = params["protocolVersion"]
        
        # Accept supported versions or propose latest
        negotiated_version = case client_version do
          "2025-03-26" -> "2025-03-26"
          "2024-11-05" -> "2024-11-05"
          _ -> "2025-03-26"  # Propose latest for unknown versions
        end
        
        # Use version-aware capabilities
        capabilities = ExMCP.Server.Capabilities.build_capabilities(__MODULE__, negotiated_version)
        
        {:ok, %{
          protocolVersion: negotiated_version,
          serverInfo: %{name: "my-server", version: "1.0.0"},
          capabilities: capabilities
        }, state}
      end
  """
  @callback handle_initialize(params :: map(), state()) ::
              {:ok, initialize_result(), state()} | {:error, any(), state()}

  @doc """
  Handles listing available tools.

  Supports pagination via optional cursor parameter.
  Should return tools and optional nextCursor for pagination.
  """
  @callback handle_list_tools(cursor :: String.t() | nil, state()) ::
              {:ok, tools :: [tool()], next_cursor :: String.t() | nil, state()}
              | {:error, any(), state()}

  @doc """
  Handles a tool call.

  The result can be returned in multiple formats:

  1. Simple format (array of content items):
      {:ok, [%{type: "text", text: "Success"}], state}
      
  2. Extended format (with isError flag):
      {:ok, %{content: [%{type: "text", text: "Error occurred"}], isError: true}, state}
      
  3. Structured output format (draft feature):
      {:ok, %{
        content: [%{type: "text", text: "Weather data"}],
        structuredContent: %{
          "temperature" => 22.5,
          "conditions" => "Partly cloudy",
          "humidity" => 65
        }
      }, state}

  > #### Draft Feature {: .info}
  > Structured tool output is a draft MCP specification feature that may change.

  Use the extended format with `isError: true` to indicate tool execution errors
  that should be reported to the client as part of the result (not protocol errors).

  When returning structured content, tools should provide both unstructured content
  (for backwards compatibility) and structured content that conforms to the tool's
  declared outputSchema.
  """
  @callback handle_call_tool(name :: String.t(), arguments :: map(), state()) ::
              {:ok, ExMCP.Types.tool_result() | list(map()), state()} | {:error, any(), state()}

  @doc """
  Handles listing available resources.

  Supports pagination via optional cursor parameter.
  Should return resources and optional nextCursor for pagination.
  """
  @callback handle_list_resources(cursor :: String.t() | nil, state()) ::
              {:ok, resources :: [resource()], next_cursor :: String.t() | nil, state()}
              | {:error, any(), state()}

  @doc """
  Handles reading a resource.
  """
  @callback handle_read_resource(uri :: String.t(), state()) ::
              {:ok, ExMCP.Types.resource_contents(), state()} | {:error, any(), state()}

  @doc """
  Handles listing available prompts.

  Supports pagination via optional cursor parameter.
  Should return prompts and optional nextCursor for pagination.
  """
  @callback handle_list_prompts(cursor :: String.t() | nil, state()) ::
              {:ok, prompts :: [prompt()], next_cursor :: String.t() | nil, state()}
              | {:error, any(), state()}

  @doc """
  Handles getting a prompt.
  """
  @callback handle_get_prompt(name :: String.t(), arguments :: map(), state()) ::
              {:ok, ExMCP.Types.prompt_message(), state()} | {:error, any(), state()}

  @doc """
  Handles a completion request.
  """
  @callback handle_complete(ref :: String.t(), params :: map(), state()) ::
              {:ok, result :: map(), state()} | {:error, any(), state()}

  @doc """
  Handles a sampling create message request.
  """
  @callback handle_create_message(params :: ExMCP.Types.create_message_params(), state()) ::
              {:ok, ExMCP.Types.create_message_result(), state()} | {:error, any(), state()}

  @doc """
  Handles listing available roots.
  """
  @callback handle_list_roots(state()) ::
              {:ok, [ExMCP.Types.root()], state()} | {:error, any(), state()}

  @doc """
  Handles resource subscription.
  """
  @callback handle_subscribe_resource(uri :: String.t(), state()) ::
              {:ok, map(), state()} | {:error, any(), state()}

  @doc """
  Handles resource unsubscription.
  """
  @callback handle_unsubscribe_resource(uri :: String.t(), state()) ::
              {:ok, map(), state()} | {:error, any(), state()}

  @doc """
  Handles listing resource templates.

  Supports pagination via optional cursor parameter.
  Should return resource templates and optional nextCursor for pagination.
  """
  @callback handle_list_resource_templates(cursor :: String.t() | nil, state()) ::
              {:ok, resource_templates :: [ExMCP.Types.resource_template()],
               next_cursor :: String.t() | nil, state()}
              | {:error, any(), state()}

  @doc """
  Called when the handler process is started.
  """
  @callback init(args :: any()) :: {:ok, state()} | {:error, any()}

  @doc """
  Called when the handler process is terminating.
  """
  @callback terminate(reason :: any(), state()) :: any()

  @doc """
  Handles setting the log level for the server.

  This callback is called when the client sends a logging/setLevel request.
  The level parameter will be one of: "debug", "info", "warning", "error".

  The implementation should adjust the server's logging verbosity accordingly.

  > #### Draft Feature {: .info}
  > This implements a draft MCP specification feature (`logging/setLevel`) that may change.

  @doc api: :draft
  """
  @callback handle_set_log_level(level :: String.t(), state()) ::
              {:ok, state()} | {:error, any(), state()}

  # Optional callbacks with defaults
  @optional_callbacks [
    handle_list_resources: 2,
    handle_read_resource: 2,
    handle_list_prompts: 2,
    handle_get_prompt: 3,
    handle_complete: 3,
    handle_create_message: 2,
    handle_list_roots: 1,
    handle_subscribe_resource: 2,
    handle_unsubscribe_resource: 2,
    handle_list_resource_templates: 2,
    handle_set_log_level: 2,
    terminate: 2
  ]

  defmacro __using__(_opts) do
    quote do
      @behaviour ExMCP.Server.Handler

      @impl true
      def init(_args), do: {:ok, %{}}

      @impl true
      def handle_list_resources(_cursor, state) do
        {:error, "Resources not implemented", state}
      end

      @impl true
      def handle_read_resource(_uri, state) do
        {:error, "Resource reading not implemented", state}
      end

      @impl true
      def handle_list_prompts(_cursor, state) do
        {:error, "Prompts not implemented", state}
      end

      @impl true
      def handle_get_prompt(_name, _arguments, state) do
        {:error, "Prompt retrieval not implemented", state}
      end

      @impl true
      def handle_complete(_ref, _params, state) do
        {:error, "Completion not implemented", state}
      end

      @impl true
      def handle_create_message(_params, state) do
        {:error, "Sampling not implemented", state}
      end

      @impl true
      def handle_list_roots(state) do
        {:error, "Roots not implemented", state}
      end

      @impl true
      def handle_subscribe_resource(_uri, state) do
        {:error, "Resource subscriptions not implemented", state}
      end

      @impl true
      def handle_unsubscribe_resource(_uri, state) do
        {:error, "Resource subscriptions not implemented", state}
      end

      @impl true
      def handle_list_resource_templates(_cursor, state) do
        {:error, "Resource templates not implemented", state}
      end

      @impl true
      def handle_set_log_level(_level, state) do
        # Default implementation just returns ok without changing anything
        {:ok, state}
      end

      @impl true
      def terminate(_reason, _state), do: :ok

      defoverridable init: 1,
                     handle_list_resources: 2,
                     handle_read_resource: 2,
                     handle_list_prompts: 2,
                     handle_get_prompt: 3,
                     handle_complete: 3,
                     handle_create_message: 2,
                     handle_list_roots: 1,
                     handle_subscribe_resource: 2,
                     handle_unsubscribe_resource: 2,
                     handle_list_resource_templates: 2,
                     handle_set_log_level: 2,
                     terminate: 2
    end
  end

  @doc """
  Builds server capabilities based on which callbacks are implemented.

  This is a convenience function that can be used in your handle_initialize/2
  callback to automatically generate capabilities based on your handler's
  implemented functions.

  ## Example

      def handle_initialize(params, state) do
        capabilities = ExMCP.Server.Handler.build_capabilities(__MODULE__)
        
        {:ok, %{
          protocolVersion: "2025-03-26",
          serverInfo: %{name: "my-server", version: "1.0.0"},
          capabilities: capabilities
        }, state}
      end
  """
  alias ExMCP.Server.Capabilities

  @spec build_capabilities(module()) :: map()
  def build_capabilities(handler_module) do
    Capabilities.build_capabilities(handler_module)
  end
end
