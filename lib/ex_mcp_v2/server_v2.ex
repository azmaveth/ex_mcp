defmodule ExMCP.ServerV2 do
  @moduledoc """
  High-level server implementation with DSL support for ExMCP v2.

  This module provides the `use ExMCP.ServerV2` macro that enables the
  developer-friendly DSL for defining tools, resources, and prompts.
  It automatically handles capability registration and provides
  sensible defaults for MCP server behavior.

  ## Usage

      defmodule MyServer do
        use ExMCP.ServerV2
        
        deftool "hello" do
          description "Says hello"
          
          args do
            field :name, :string, required: true, description: "Name to greet"
          end
        end
        
        defresource "config://app" do
          name "App Config"
          mime_type "application/json"
        end
        
        defprompt "greeting" do
          name "Greeting Template"
          
          arguments do
            arg :style, description: "Greeting style"
          end
        end
        
        # Handler callbacks
        @impl true
        def handle_tool_call("hello", %{"name" => name}, state) do
          {:ok, %{content: [text("Hello, \#{name}!")]}, state}
        end
        
        @impl true
        def handle_resource_read("config://app", _uri, state) do
          content = json(%{debug: true, port: 8080})
          {:ok, content, state}
        end
        
        @impl true
        def handle_prompt_get("greeting", args, state) do
          style = Map.get(args, "style", "friendly")
          messages = [
            user("Please greet me in a \#{style} way")
          ]
          {:ok, %{messages: messages}, state}
        end
      end
  """

  @type state :: term()
  @type content :: map()
  @type tool_result :: %{content: [content()], is_error?: boolean()}

  @doc """
  Callback for handling tool calls.

  Called when a client invokes a tool defined with `deftool`.
  """
  @callback handle_tool_call(tool_name :: String.t(), arguments :: map(), state) ::
              {:ok, tool_result(), state} | {:error, term(), state}

  @doc """
  Callback for handling resource reads.

  Called when a client reads a resource defined with `defresource`.
  """
  @callback handle_resource_read(uri :: String.t(), full_uri :: String.t(), state) ::
              {:ok, [content()], state} | {:error, term(), state}

  @doc """
  Callback for listing resources.

  Called when a client requests a list of available resources.
  """
  @callback handle_resource_list(state) ::
              {:ok, [map()], state} | {:error, term(), state}

  @doc """
  Callback for resource subscriptions.

  Called when a client subscribes to resource change notifications.
  """
  @callback handle_resource_subscribe(uri :: String.t(), state) ::
              {:ok, state} | {:error, term(), state}

  @doc """
  Callback for resource unsubscriptions.

  Called when a client unsubscribes from resource change notifications.
  """
  @callback handle_resource_unsubscribe(uri :: String.t(), state) ::
              {:ok, state} | {:error, term(), state}

  @doc """
  Callback for handling prompt requests.

  Called when a client requests a prompt defined with `defprompt`.
  """
  @callback handle_prompt_get(prompt_name :: String.t(), arguments :: map(), state) ::
              {:ok, %{messages: [map()]}, state} | {:error, term(), state}

  @doc """
  Callback for listing prompts.

  Called when a client requests a list of available prompts.
  """
  @callback handle_prompt_list(state) ::
              {:ok, [map()], state} | {:error, term(), state}

  @doc """
  Callback for custom request handling.

  Called for any requests not handled by the standard callbacks.
  Can be used for experimental features.
  """
  @callback handle_request(method :: String.t(), params :: map(), state) ::
              {:reply, map(), state} | {:error, term(), state} | {:noreply, state}

  @doc """
  Callback for server initialization.
  """
  @callback init(args :: term()) :: {:ok, state} | {:error, term()}

  # Make callbacks optional with default implementations
  @optional_callbacks [
    handle_resource_list: 1,
    handle_resource_subscribe: 2,
    handle_resource_unsubscribe: 2,
    handle_prompt_list: 1,
    handle_request: 3,
    init: 1
  ]

  defmacro __using__(opts \\ []) do
    quote do
      use GenServer

      # Import DSL macros - import all needed macros, conflicts resolved by import order
      import ExMCP.DSL.Tool
      import ExMCP.DSL.Resource
      import ExMCP.DSL.Prompt

      # Import content helpers
      import ExMCP.ContentV2,
        only: [
          text: 1,
          text: 2,
          image: 2,
          image: 3,
          audio: 2,
          audio: 3,
          resource: 1,
          resource: 2,
          user: 1,
          assistant: 1,
          system: 1,
          json: 1,
          json: 2
        ]

      @behaviour ExMCP.ServerV2

      # Initialize module attributes for collecting DSL definitions
      Module.register_attribute(__MODULE__, :__tools__, accumulate: false, persist: true)
      Module.register_attribute(__MODULE__, :__resources__, accumulate: false, persist: true)

      Module.register_attribute(__MODULE__, :__resource_templates__,
        accumulate: false,
        persist: true
      )

      Module.register_attribute(__MODULE__, :__prompts__, accumulate: false, persist: true)

      @__tools__ %{}
      @__resources__ %{}
      @__resource_templates__ %{}
      @__prompts__ %{}

      # Configuration options
      @server_opts unquote(opts)

      @doc """
      Starts the server.
      """
      def start_link(opts \\ []) do
        GenServer.start_link(__MODULE__, opts, name: __MODULE__)
      end

      @doc """
      Gets the server's capabilities based on defined tools, resources, and prompts.
      """
      def get_capabilities do
        capabilities = %{}

        # Add tools capability if any tools are defined
        capabilities =
          case get_tools() do
            tools when map_size(tools) > 0 ->
              Map.put(capabilities, "tools", %{"listChanged" => true})

            _ ->
              capabilities
          end

        # Add resources capability if any resources are defined
        capabilities =
          case get_resources() do
            resources when map_size(resources) > 0 ->
              subscribable = Enum.any?(Map.values(resources), & &1.subscribable)

              Map.put(capabilities, "resources", %{
                "subscribe" => subscribable,
                "listChanged" => true
              })

            _ ->
              capabilities
          end

        # Add prompts capability if any prompts are defined
        capabilities =
          case get_prompts() do
            prompts when map_size(prompts) > 0 ->
              Map.put(capabilities, "prompts", %{"listChanged" => true})

            _ ->
              capabilities
          end

        capabilities
      end

      @doc """
      Gets all defined tools.
      """
      def get_tools do
        case __MODULE__.__info__(:attributes)[:__tools__] do
          [map] when is_map(map) -> map
          map when is_map(map) -> map
          _ -> %{}
        end
      end

      @doc """
      Gets all defined resources.
      """
      def get_resources do
        case __MODULE__.__info__(:attributes)[:__resources__] do
          [map] when is_map(map) -> map
          map when is_map(map) -> map
          _ -> %{}
        end
      end

      @doc """
      Gets all defined resource templates.
      """
      def get_resource_templates do
        case __MODULE__.__info__(:attributes)[:__resource_templates__] do
          [map] when is_map(map) -> map
          map when is_map(map) -> map
          _ -> %{}
        end
      end

      @doc """
      Gets all defined prompts.
      """
      def get_prompts do
        case __MODULE__.__info__(:attributes)[:__prompts__] do
          [map] when is_map(map) -> map
          map when is_map(map) -> map
          _ -> %{}
        end
      end

      # Default implementations for optional callbacks
      def handle_resource_list(state), do: {:ok, [], state}
      def handle_resource_subscribe(_uri, state), do: {:ok, state}
      def handle_resource_unsubscribe(_uri, state), do: {:ok, state}
      def handle_prompt_list(state), do: {:ok, [], state}
      def handle_request(_method, _params, state), do: {:noreply, state}

      # Default GenServer init callback
      @impl GenServer
      def init(args) do
        # Register capabilities with the registry
        register_capabilities()

        # Default implementation returns empty map state
        {:ok, Map.new(args)}
      end

      # Register all capabilities with the ExMCP.Registry
      defp register_capabilities do
        # Register tools
        Enum.each(@__tools__, fn {name, tool} ->
          ExMCP.Registry.register(ExMCP.Registry, :tool, name, __MODULE__, tool)
        end)

        # Register resources
        Enum.each(@__resources__, fn {uri, resource} ->
          ExMCP.Registry.register(ExMCP.Registry, :resource, uri, __MODULE__, resource)
        end)

        # Register prompts
        Enum.each(@__prompts__, fn {name, prompt} ->
          ExMCP.Registry.register(ExMCP.Registry, :prompt, name, __MODULE__, prompt)
        end)
      end

      defoverridable init: 1,
                     handle_resource_list: 1,
                     handle_resource_subscribe: 2,
                     handle_resource_unsubscribe: 2,
                     handle_prompt_list: 1,
                     handle_request: 3
    end
  end

  @doc """
  Sends a resource update notification for subscribed clients.

  This function should be called by the server when a subscribed
  resource changes.
  """
  def notify_resource_update(server, uri) do
    GenServer.cast(server, {:notify_resource_update, uri})
  end
end
