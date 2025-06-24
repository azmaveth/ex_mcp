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

  # Make callbacks optional with default implementations
  @optional_callbacks [
    handle_resource_read: 3,
    handle_resource_list: 1,
    handle_resource_subscribe: 2,
    handle_resource_unsubscribe: 2,
    handle_prompt_list: 1,
    handle_request: 3,
    handle_tool_call: 3,
    handle_prompt_get: 3
  ]

  defmacro __using__(opts \\ []) do
    imports = generate_imports()
    setup = generate_setup(opts)
    functions = generate_functions()

    quote do
      unquote(imports)
      unquote(setup)
      unquote(functions)
    end
  end

  defp generate_imports do
    quote do
      use GenServer
      import ExMCP.DSL.Tool
      import ExMCP.DSL.Resource
      import ExMCP.DSL.Prompt

      import ExMCP.ContentHelpers,
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
    end
  end

  defp generate_setup(opts) do
    quote do
      @behaviour ExMCP.Server
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
      @server_opts unquote(opts)
    end
  end

  defp generate_functions do
    quote do
      unquote(generate_start_link_function())
      unquote(generate_child_spec_function())
      unquote(generate_capabilities_function())
      unquote(generate_getter_functions())
      unquote(generate_default_callbacks())
      unquote(generate_genserver_callbacks())
      unquote(generate_helper_functions())
      unquote(generate_overridable_list())
    end
  end

  # This function generates comprehensive macro code for handling multiple transport types.
  # The complexity is justified by the need to handle native, stdio, test, HTTP, and SSE transports
  # with their respective configuration and startup sequences in a single generated function.
  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  defp generate_start_link_function do
    quote do
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
        do_start_link(transport, opts)
      end

      defp do_start_link(:native, opts) do
        # Extract name from opts if provided, otherwise use module name
        genserver_opts =
          if name = Keyword.get(opts, :name) do
            [name: name]
          else
            [name: __MODULE__]
          end

        GenServer.start_link(__MODULE__, opts, genserver_opts)
      end

      defp do_start_link(:stdio, opts) do
        Application.put_env(:ex_mcp, :stdio_mode, true)
        configure_stdio_logging()
        start_transport_server(opts)
      end

      defp do_start_link(:test, opts) do
        # For test transport, start as a GenServer directly to avoid recursion
        GenServer.start_link(__MODULE__, opts)
      end

      defp do_start_link(_transport, opts) do
        start_transport_server(opts)
      end

      defp start_transport_server(opts) do
        server_info = get_server_info_from_opts()
        tools = get_tools() |> Map.values()
        Transport.start_server(__MODULE__, server_info, tools, opts)
      end

      defp get_server_info_from_opts do
        case @server_opts do
          nil ->
            %{name: to_string(__MODULE__), version: "1.0.0"}

          opts ->
            Keyword.get(opts, :server_info, %{name: to_string(__MODULE__), version: "1.0.0"})
        end
      end

      defp configure_stdio_logging do
        StdioLoggerConfig.configure()
      end
    end
  end

  defp generate_child_spec_function do
    quote do
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
    end
  end

  defp generate_capabilities_function do
    quote do
      @doc """
      Gets the server's capabilities based on defined tools, resources, and prompts.
      """
      def get_capabilities do
        %{}
        |> maybe_add_tools_capability()
        |> maybe_add_resources_capability()
        |> maybe_add_prompts_capability()
      end

      defp maybe_add_tools_capability(capabilities) do
        case get_tools() do
          tools when map_size(tools) > 0 ->
            Map.put(capabilities, "tools", %{"listChanged" => true})

          _ ->
            capabilities
        end
      end

      defp maybe_add_resources_capability(capabilities) do
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
      end

      defp maybe_add_prompts_capability(capabilities) do
        case get_prompts() do
          prompts when map_size(prompts) > 0 ->
            Map.put(capabilities, "prompts", %{"listChanged" => true})

          _ ->
            capabilities
        end
      end
    end
  end

  defp generate_getter_functions do
    quote do
      @doc """
      Gets all defined tools.
      """
      def get_tools do
        get_attribute_map(:__tools__)
      end

      @doc """
      Gets all defined resources.
      """
      def get_resources do
        get_attribute_map(:__resources__)
      end

      @doc """
      Gets all defined resource templates.
      """
      def get_resource_templates do
        get_attribute_map(:__resource_templates__)
      end

      @doc """
      Gets all defined prompts.
      """
      def get_prompts do
        get_attribute_map(:__prompts__)
      end

      defp get_attribute_map(attribute) do
        case __MODULE__.__info__(:attributes)[attribute] do
          [map] when is_map(map) -> map
          map when is_map(map) -> map
          _ -> %{}
        end
      end
    end
  end

  defp generate_default_callbacks do
    quote do
      # Default implementations for optional callbacks
      def handle_resource_read(_uri, _full_uri, state), do: {:error, :resource_not_found, state}
      def handle_resource_list(state), do: {:ok, [], state}
      def handle_resource_subscribe(_uri, state), do: {:ok, state}
      def handle_resource_unsubscribe(_uri, state), do: {:ok, state}
      def handle_prompt_list(state), do: {:ok, [], state}
      def handle_request(_method, _params, state), do: {:noreply, state}

      def handle_tool_call(_tool_name, _arguments, state),
        do: {:error, :tool_not_implemented, state}

      def handle_prompt_get(_prompt_name, _arguments, state),
        do: {:error, :prompt_not_implemented, state}
    end
  end

  defp generate_genserver_callbacks do
    quote do
      unquote(generate_genserver_init())
      unquote(generate_genserver_handle_calls())
      unquote(generate_genserver_handle_casts())
      unquote(generate_genserver_handle_info())
    end
  end

  defp generate_genserver_init do
    quote do
      # Default GenServer init callback
      @impl GenServer
      def init(args) do
        register_capabilities()

        state =
          args
          |> Map.new()
          |> Map.put_new(:subscriptions, MapSet.new())

        {:ok, state}
      end
    end
  end

  defp generate_genserver_handle_calls do
    quote do
      unquote(generate_basic_handle_calls())
      unquote(generate_mcp_handle_calls())
      unquote(generate_fallback_handle_call())
    end
  end

  defp generate_basic_handle_calls do
    quote do
      # Handle server info requests
      @impl GenServer
      def handle_call(:get_server_info, _from, state) do
        server_info = get_server_info_from_opts()
        {:reply, server_info, state}
      end

      # Handle capabilities requests
      def handle_call(:get_capabilities, _from, state) do
        {:reply, get_capabilities(), state}
      end

      # Handle tools list requests
      def handle_call(:get_tools, _from, state) do
        {:reply, get_tools(), state}
      end

      # Handle resources list requests
      def handle_call(:get_resources, _from, state) do
        {:reply, get_resources(), state}
      end

      # Handle prompts list requests
      def handle_call(:get_prompts, _from, state) do
        {:reply, get_prompts(), state}
      end

      # Handle ping requests (server to client)
      def handle_call(:ping, _from, state) do
        {:reply, {:error, :ping_not_implemented}, state}
      end

      # Handle list roots requests (server to client)
      def handle_call(:list_roots, _from, state) do
        {:reply, {:error, :list_roots_not_implemented}, state}
      end
    end
  end

  defp generate_mcp_handle_calls do
    quote do
      # Handle tool call requests
      def handle_call({:handle_tool_call, tool_name, arguments}, _from, state) do
        result = handle_tool_call(tool_name, arguments, state)
        {:reply, result, state}
      end

      # Handle resource read requests
      def handle_call({:handle_resource_read, uri, full_uri}, _from, state) do
        case handle_resource_read(uri, full_uri, state) do
          {:ok, content, new_state} ->
            {:reply, {:ok, content, new_state}, new_state}

          {:error, reason, new_state} ->
            {:reply, {:error, reason, new_state}, new_state}
        end
      end

      # Handle prompt get requests
      def handle_call({:handle_prompt_get, prompt_name, arguments}, _from, state) do
        result = handle_prompt_get(prompt_name, arguments, state)
        {:reply, result, state}
      end

      # Handle custom requests
      def handle_call({:handle_request, method, params}, _from, state) do
        result = handle_request(method, params, state)
        {:reply, result, state}
      end
    end
  end

  defp generate_fallback_handle_call do
    quote do
      # Default handle_call fallback
      def handle_call(request, _from, state) do
        {:reply, {:error, {:unknown_call, request}}, state}
      end
    end
  end

  defp generate_genserver_handle_casts do
    quote do
      # Handle notify roots changed (server to client notification)
      @impl GenServer
      def handle_cast(:notify_roots_changed, state) do
        # This is a notification that should be sent to connected clients
        # For now, we just return :noreply since notification routing
        # is handled by the transport layer
        {:noreply, state}
      end

      # Default handle_cast fallback
      def handle_cast(_request, state) do
        {:noreply, state}
      end
    end
  end

  defp generate_genserver_handle_info do
    quote do
      # Handle test transport connection messages
      @impl GenServer
      def handle_info({:test_transport_connect, client_pid}, state) do
        # For test transport, update the transport state with the connected client
        # This mirrors the behavior from Legacy servers
        new_state = Map.put(state, :transport_client_pid, client_pid)
        {:noreply, new_state}
      end

      # Handle test transport messages
      def handle_info({:transport_message, message}, state) do
        # Forward transport messages for processing
        # This would typically be handled by a message processor
        {:noreply, state}
      end

      # Handle transport errors
      def handle_info({:transport_error, reason}, state) do
        require Logger
        Logger.error("Transport error in DSL server: #{inspect(reason)}")
        {:noreply, state}
      end

      # Handle transport closed
      def handle_info({:transport_closed}, state) do
        require Logger
        Logger.info("Transport closed in DSL server")
        {:stop, :normal, state}
      end

      # Default handle_info fallback
      def handle_info(_message, state) do
        {:noreply, state}
      end
    end
  end

  defp generate_helper_functions do
    quote do
      # Register all capabilities with the ExMCP.Registry
      defp register_capabilities do
        register_items(@__tools__, :tool)
        register_items(@__resources__, :resource)
        register_items(@__prompts__, :prompt)
      end

      defp register_items(items, type) do
        Enum.each(items, fn {key, value} ->
          ExMCP.Registry.register(ExMCP.Registry, type, key, __MODULE__, value)
        end)
      end
    end
  end

  defp generate_overridable_list do
    quote do
      defoverridable handle_resource_read: 3,
                     handle_resource_list: 1,
                     handle_resource_subscribe: 2,
                     handle_resource_unsubscribe: 2,
                     handle_prompt_list: 1,
                     handle_request: 3,
                     handle_tool_call: 3,
                     handle_prompt_get: 3
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
    case Keyword.has_key?(opts, :handler) do
      true ->
        # Use the legacy server implementation for handler-based servers
        Legacy.start_link(opts)

      false ->
        {:error, :no_handler_specified}
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
  Lists the roots available from the connected client.

  Sends a roots/list request to the client to discover what filesystem
  or conceptual roots the client has access to. This allows the server
  to understand what the client can provide access to.

  ## Parameters

  - `server` - Server process reference
  - `timeout` - Request timeout in milliseconds (default: 5000)

  ## Returns

  - `{:ok, %{roots: [root()]}}` - List of roots from client
  - `{:error, reason}` - Request failed

  ## Root Format

  Each root contains:
  - `uri` - URI identifying the root location (required)
  - `name` - Human-readable name for the root (optional)

  ## Examples

      {:ok, %{roots: roots}} = ExMCP.Server.list_roots(server)

      # Example roots format:
      [
        %{uri: "file:///home/user", name: "Home Directory"},
        %{uri: "file:///projects", name: "Projects"},
        %{uri: "config://app", name: "App Configuration"}
      ]
  """
  @spec list_roots(GenServer.server(), timeout()) :: {:ok, %{roots: [map()]}} | {:error, any()}
  def list_roots(server, timeout \\ 5000) do
    GenServer.call(server, :list_roots, timeout)
  end

  @doc """
  Notifies the client that the server's available roots have changed.

  Sends a notification to inform the client that the list of roots
  the server can access has been updated. This allows clients to
  refresh their understanding of what the server can provide.

  ## Parameters

  - `server` - Server process reference

  ## Returns

  - `:ok` - Notification sent successfully

  ## Example

      :ok = ExMCP.Server.notify_roots_changed(server)
  """
  @spec notify_roots_changed(GenServer.server()) :: :ok
  def notify_roots_changed(server) do
    GenServer.cast(server, :notify_roots_changed)
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
