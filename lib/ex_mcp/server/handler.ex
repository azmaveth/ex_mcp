defmodule ExMCP.Server.Handler do
  @moduledoc """
  Behaviour for implementing MCP server handlers.

  To create an MCP server, implement this behaviour in your module:

      defmodule MyServer do
        use ExMCP.Server.Handler
        
        @impl true
        def handle_initialize(_params, state) do
          {:ok, %{
            server_info: %{
              name: "my-server",
              version: "1.0.0"
            },
            capabilities: %{
              tools: %{},
              resources: %{list: true, read: true}
            }
          }, state}
        end
        
        @impl true
        def handle_list_tools(state) do
          tools = [
            %{
              name: "hello",
              description: "Says hello",
              input_schema: %{
                type: "object",
                properties: %{
                  "name" => %{type: "string"}
                },
                required: ["name"]
              }
            }
          ]
          {:ok, tools, state}
        end
        
        @impl true
        def handle_call_tool("hello", %{"name" => name}, state) do
          result = [%{type: "text", text: "Hello, \#{name}!"}]
          {:ok, result, state}
        end
        
        # ... implement other handlers ...
      end

  The `use` macro provides default implementations for all callbacks
  that return appropriate "not implemented" responses.
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
      def terminate(_reason, _state), do: :ok

      defoverridable init: 1,
                     handle_list_resources: 1,
                     handle_read_resource: 2,
                     handle_list_prompts: 1,
                     handle_get_prompt: 3,
                     handle_complete: 3,
                     handle_create_message: 2,
                     terminate: 2
    end
  end
end
