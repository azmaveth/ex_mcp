defmodule ExMCP.Server.Handler do
  @moduledoc """
  Behaviour for implementing MCP server handlers.

  This behaviour defines callbacks for handling all MCP protocol operations including
  tools, resources, prompts, and the new sampling/LLM integration features.

  ## Basic Example

      defmodule MyServer do
        use ExMCP.Server.Handler
        
        @impl true
        def handle_initialize(_params, state) do
          {:ok, %{
            name: "my-server",
            version: "1.0.0",
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
          progress_token = params["_progressToken"]
          
          # Your tool implementation
          result = eval_expression(params["expression"])
          
          # Send progress updates if token provided
          if progress_token do
            ExMCP.Server.notify_progress(self(), progress_token, 100, 100)
          end
          
          {:ok, [%{type: "text", text: "Result: \#{result}"}], state}
        end
      end

  ## Advanced Features

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
        progress_token = params["_progressToken"]
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

  Should return server information and capabilities.
  """
  @callback handle_initialize(params :: map(), state()) ::
              {:ok, initialize_result(), state()} | {:error, any(), state()}

  @doc """
  Handles listing available tools.
  """
  @callback handle_list_tools(state()) ::
              {:ok, [tool()], state()} | {:error, any(), state()}

  @doc """
  Handles a tool call.
  """
  @callback handle_call_tool(name :: String.t(), arguments :: map(), state()) ::
              {:ok, ExMCP.Types.tool_result(), state()} | {:error, any(), state()}

  @doc """
  Handles listing available resources.
  """
  @callback handle_list_resources(state()) ::
              {:ok, [resource()], state()} | {:error, any(), state()}

  @doc """
  Handles reading a resource.
  """
  @callback handle_read_resource(uri :: String.t(), state()) ::
              {:ok, ExMCP.Types.resource_content(), state()} | {:error, any(), state()}

  @doc """
  Handles listing available prompts.
  """
  @callback handle_list_prompts(state()) ::
              {:ok, [prompt()], state()} | {:error, any(), state()}

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
  """
  @callback handle_list_resource_templates(state()) ::
              {:ok, [ExMCP.Types.resource_template()], state()} | {:error, any(), state()}

  @doc """
  Called when the handler process is started.
  """
  @callback init(args :: any()) :: {:ok, state()} | {:error, any()}

  @doc """
  Called when the handler process is terminating.
  """
  @callback terminate(reason :: any(), state()) :: any()

  # Optional callbacks with defaults
  @optional_callbacks [
    handle_list_resources: 1,
    handle_read_resource: 2,
    handle_list_prompts: 1,
    handle_get_prompt: 3,
    handle_complete: 3,
    handle_create_message: 2,
    handle_list_roots: 1,
    handle_subscribe_resource: 2,
    handle_unsubscribe_resource: 2,
    terminate: 2
  ]

  defmacro __using__(_opts) do
    quote do
      @behaviour ExMCP.Server.Handler

      @impl true
      def init(_args), do: {:ok, %{}}

      @impl true
      def handle_list_resources(state) do
        {:error, "Resources not implemented", state}
      end

      @impl true
      def handle_read_resource(_uri, state) do
        {:error, "Resource reading not implemented", state}
      end

      @impl true
      def handle_list_prompts(state) do
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
      def handle_list_resource_templates(state) do
        {:error, "Resource templates not implemented", state}
      end

      @impl true
      def terminate(_reason, _state), do: :ok

      defoverridable init: 1,
                     handle_list_resources: 1,
                     handle_read_resource: 2,
                     handle_list_prompts: 1,
                     handle_get_prompt: 3,
                     handle_complete: 3,
                     handle_create_message: 2,
                     handle_list_roots: 1,
                     handle_subscribe_resource: 2,
                     handle_unsubscribe_resource: 2,
                     handle_list_resource_templates: 1,
                     terminate: 2
    end
  end
end
