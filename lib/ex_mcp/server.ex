defmodule ExMCP.Server do
  @moduledoc """
  High-level server implementation with DSL support.

  This module provides the `use ExMCP.Server` macro that enables the
  developer-friendly DSL for defining tools, resources, and prompts.
  It automatically handles capability registration and provides
  sensible defaults for MCP server behavior.

  ## Usage

      defmodule MyServer do
        use ExMCP.Server

        deftool "hello" do
          meta do
            name "Hello Tool"
            description "Says hello to someone"
          end

          input_schema %{
            type: "object",
            properties: %{name: %{type: "string"}},
            required: ["name"]
          }
        end

        defresource "config://app" do
          meta do
            name "App Config"
            description "Application configuration"
          end

          mime_type "application/json"
        end

        defprompt "greeting" do
          meta do
            name "Greeting Template"
            description "A greeting template"
          end

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

  alias ExMCP.Internal.StdioLoggerConfig
  alias ExMCP.Server.{Legacy, Transport}

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

      # Import all DSL macros - no more naming conflicts with meta block pattern!
      import ExMCP.DSL.Tool
      import ExMCP.DSL.Resource
      import ExMCP.DSL.Prompt
      import ExMCP.DSL.Handler

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

      @behaviour ExMCP.Server

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
      Starts the server with optional transport configuration.

      ## Options

      * `:transport` - Transport type (`:http`, `:stdio`, `:sse`, `:native`). Default: `:native`
      * `:port` - Port for HTTP/SSE transports. Default: 4000
      * `:host` - Host for HTTP transports. Default: "localhost"
      * Other options are passed to the underlying transport

      ## Examples

          # Start with HTTP transport
          MyServer.start_link(transport: :http, port: 8080)

          # Start with stdio transport
          MyServer.start_link(transport: :stdio)

          # Start with native transport (default)
          MyServer.start_link()
      """
      def start_link(opts \\ []) do
        transport = Keyword.get(opts, :transport, :native)

        case transport do
          :native ->
            # Start as a regular GenServer for native transport
            GenServer.start_link(__MODULE__, opts, name: __MODULE__)

          :stdio ->
            # Configure logging early for STDIO transport
            Application.put_env(:ex_mcp, :stdio_mode, true)
            configure_stdio_logging()
            # Use transport manager for STDIO transport
            server_info =
              case @server_opts do
                nil ->
                  %{name: to_string(__MODULE__), version: "1.0.0"}

                opts ->
                  Keyword.get(opts, :server_info, %{name: to_string(__MODULE__), version: "1.0.0"})
              end

            tools = get_tools() |> Map.values()
            Transport.start_server(__MODULE__, server_info, tools, opts)

          _ ->
            # Use transport manager for other transports
            server_info =
              case @server_opts do
                nil ->
                  %{name: to_string(__MODULE__), version: "1.0.0"}

                opts ->
                  Keyword.get(opts, :server_info, %{name: to_string(__MODULE__), version: "1.0.0"})
              end

            tools = get_tools() |> Map.values()

            Transport.start_server(__MODULE__, server_info, tools, opts)
        end
      end

      # Configure logging for STDIO transport to prevent stdout contamination
      defp configure_stdio_logging do
        StdioLoggerConfig.configure()
      end

      @doc """
      Gets the child specification for supervision trees.
      """
      def child_spec(opts) do
        %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [opts]},
          type: :worker,
          restart: :permanent,
          shutdown: 500
        }
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

      # Handle server info requests
      @impl GenServer
      def handle_call(:get_server_info, _from, state) do
        server_info =
          case @server_opts do
            nil ->
              %{name: to_string(__MODULE__), version: "1.0.0"}

            opts ->
              Keyword.get(opts, :server_info, %{name: to_string(__MODULE__), version: "1.0.0"})
          end

        {:reply, server_info, state}
      end

      # Default handle_call fallback
      def handle_call(request, _from, state) do
        {:reply, {:error, {:unknown_call, request}}, state}
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
  Starts an MCP server with the given options.

  This function provides compatibility with the legacy server API
  and delegates to the appropriate server implementation.

  ## Options

  * `:handler` - Handler module implementing ExMCP.Server.Handler
  * `:transport` - Transport type (:stdio, :http, :test, etc.)
  * Other options are passed to the underlying implementation
  """
  def start_link(opts) when is_list(opts) do
    case Keyword.get(opts, :handler) do
      nil ->
        # No handler specified, this might be a DSL server module calling start_link
        {:error, :no_handler_specified}

      _handler_module ->
        # Use the legacy server implementation for handler-based servers
        Legacy.start_link(opts)
    end
  end

  @doc """
  Sends a log message through the server.

  Compatibility function for the logging system.
  """
  def send_log_message(server, level, message, data) do
    GenServer.cast(server, {:send_log_message, level, message, data})
  end

  @doc """
  Sends a ping request to the client.

  The client must respond promptly or may be disconnected.
  """
  @spec ping(GenServer.server(), timeout()) :: {:ok, map()} | {:error, any()}
  def ping(server, timeout \\ 5000) do
    GenServer.call(server, :ping, timeout)
  end

  @doc """
  Sends a progress notification to the client.

  Used for long-running operations to report progress updates.
  """
  def notify_progress(server, progress_token, progress, total) do
    GenServer.cast(server, {:notify_progress, progress_token, progress, total})
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
