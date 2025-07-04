defmodule ExMCP.DSL.CodeGenerator do
  @moduledoc """
  Code generation for the ExMCP.Server DSL.

  This module handles all the macro code generation for servers using the DSL,
  including:
  - Import statements and aliases
  - Module attribute setup
  - Capability detection functions
  - GenServer callbacks
  - Transport handling
  - Helper functions

  This module centralizes all code generation logic that was previously
  spread across the ExMCP.Server module.
  """

  alias ExMCP.Internal.StdioLoggerConfig
  alias ExMCP.Protocol.{RequestProcessor, RequestTracker}
  alias ExMCP.Server.Transport

  @doc """
  Generates all the code for a server using the DSL.

  ## Options

  - `:server_info` - Server information map with name and version
  """
  def generate(opts \\ []) do
    imports = generate_imports()
    setup = generate_setup(opts)
    functions = generate_functions()

    quote do
      unquote(imports)
      unquote(setup)
      unquote(functions)
    end
  end

  @doc """
  Generates import statements and aliases needed by DSL servers.
  """
  def generate_imports do
    quote do
      use GenServer
      import ExMCP.DSL.Tool
      import ExMCP.DSL.Resource
      import ExMCP.DSL.Prompt

      alias ExMCP.Internal.StdioLoggerConfig
      alias ExMCP.Protocol.{RequestProcessor, RequestTracker, ResponseBuilder}
      alias ExMCP.Server.Transport

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

  @doc """
  Generates module attribute setup for DSL servers.
  """
  def generate_setup(opts) do
    escaped_opts = Macro.escape(opts)

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
      @server_opts unquote(escaped_opts)
    end
  end

  @doc """
  Generates all function definitions for DSL servers.
  """
  def generate_functions do
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

  # Generate start_link function with transport support
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

      def get_server_info_from_opts do
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

  # Generate child_spec for supervision trees
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

  # Generate capability detection functions
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

  # Generate getter functions for DSL definitions
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

  # Generate default callback implementations
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

      def handle_initialize(_params, state),
        do: {:error, :initialize_not_implemented, state}
    end
  end

  # Generate GenServer callbacks
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
          |> Map.put(:__module__, __MODULE__)
          |> RequestTracker.init_state()

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

  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
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

      # Handle get pending requests
      def handle_call(:get_pending_requests, _from, state) do
        pending_ids = get_pending_request_ids(state)
        {:reply, pending_ids, state}
      end

      # Handle create message request - delegate to separate function (now async)
      def handle_call({:create_message, params}, from, state) do
        handle_create_message_request(params, from, state)
      end

      # Helper function for create message handling (now async)
      defp handle_create_message_request(params, from, state) do
        case Map.get(state, :transport_client_pid) do
          nil ->
            {:reply, {:error, :not_connected}, state}

          client_pid ->
            # Generate a unique request ID
            request_id = "msg_#{System.unique_integer([:positive])}"

            # Build the MCP request
            request = %{
              "jsonrpc" => "2.0",
              "id" => request_id,
              "method" => "sampling/createMessage",
              "params" => params
            }

            # Send the request via transport
            case Jason.encode(request) do
              {:ok, json} ->
                send(client_pid, {:transport_message, json})

                # Track the pending create_message request
                pending_create_messages = Map.get(state, :pending_create_messages, %{})

                updated_pending =
                  Map.put(pending_create_messages, request_id, {from, System.monotonic_time()})

                new_state = Map.put(state, :pending_create_messages, updated_pending)

                # Schedule a timeout for this request (30 seconds)
                Process.send_after(self(), {:create_message_timeout, request_id}, 30_000)

                {:noreply, new_state}

              {:error, error} ->
                {:reply, {:error, {:encode_failed, error}}, state}
            end
        end
      end
    end
  end

  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  defp generate_mcp_handle_calls do
    quote do
      # Handle MCP request processing
      def handle_call(
            {:process_request, %{"method" => method, "id" => id} = request},
            from,
            state
          ) do
        # Track the pending request
        new_state = track_pending_request(id, from, state)

        # Check if request was already cancelled
        if request_cancelled?(id, new_state) do
          # Request was cancelled before we could process it
          final_state = complete_pending_request(id, new_state)
          {:reply, {:error, :cancelled}, final_state}
        else
          # Process the request
          process_and_reply(request, from, new_state)
        end
      end

      # Handle non-request MCP messages
      def handle_call({:process_request, %{"method" => _method} = request}, _from, state) do
        # This is a notification (no id), process without tracking
        case process_request(request, state) do
          {:notification, new_state} ->
            {:reply, :ok, new_state}

          _ ->
            {:reply, :ok, state}
        end
      end

      # Handle cancellation requests
      def handle_call({:cancel_request, request_id}, _from, state) do
        case RequestTracker.handle_cancellation(request_id, state) do
          {:reply, from, new_state} ->
            # Request was still pending, reply with cancellation error
            GenServer.reply(from, {:error, :cancelled})
            {:reply, :ok, new_state}

          {:noreply, new_state} ->
            # Request not found or already completed
            {:reply, :ok, new_state}
        end
      end

      # Handle tool execution
      def handle_call({:execute_tool, tool_name, arguments}, _from, state) do
        case handle_tool_call(tool_name, arguments, state) do
          {:ok, result, new_state} ->
            {:reply, {:ok, result}, new_state}

          {:error, reason, new_state} ->
            {:reply, {:error, reason}, new_state}
        end
      end

      # Handle resource read
      def handle_call({:read_resource, uri}, _from, state) do
        case handle_resource_read(uri, uri, state) do
          {:ok, content, new_state} ->
            {:reply, {:ok, content}, new_state}

          {:error, reason, new_state} ->
            {:reply, {:error, reason}, new_state}
        end
      end

      # Handle prompt get
      def handle_call({:get_prompt, prompt_name, arguments}, _from, state) do
        case handle_prompt_get(prompt_name, arguments, state) do
          {:ok, prompt, new_state} ->
            {:reply, {:ok, prompt}, new_state}

          {:error, reason, new_state} ->
            {:reply, {:error, reason}, new_state}
        end
      end

      # Handle resource subscription
      def handle_call({:subscribe_resource, uri}, _from, state) do
        case handle_resource_subscribe(uri, state) do
          {:ok, new_state} ->
            {:reply, :ok, new_state}

          {:error, reason, new_state} ->
            {:reply, {:error, reason}, new_state}
        end
      end

      # Handle resource unsubscription
      def handle_call({:unsubscribe_resource, uri}, _from, state) do
        case handle_resource_unsubscribe(uri, state) do
          {:ok, new_state} ->
            {:reply, :ok, new_state}

          {:error, reason, new_state} ->
            {:reply, {:error, reason}, new_state}
        end
      end

      # Helper to process request and send reply
      defp process_and_reply(request, from, state) do
        case process_request(request, state) do
          {:response, response, new_state} ->
            # Mark request as complete
            request_id = Map.get(request, "id")
            final_state = complete_pending_request(request_id, new_state)
            {:reply, {:ok, response}, final_state}

          {:notification, new_state} ->
            {:reply, :ok, new_state}

          {:async, new_state} ->
            # Response will be sent later
            {:noreply, new_state}
        end
      end
    end
  end

  defp generate_fallback_handle_call do
    quote do
      # Fallback for unknown calls
      def handle_call(request, _from, state) do
        require Logger
        Logger.warning("Unhandled call: #{inspect(request)}")
        {:reply, {:error, :unknown_request}, state}
      end
    end
  end

  defp generate_genserver_handle_casts do
    quote do
      @impl GenServer
      def handle_cast({:notify_resource_update, uri}, state) do
        # This could be extended to notify connected clients
        # For now, just acknowledge the update
        require Logger
        Logger.debug("Resource updated: #{uri}")
        {:noreply, state}
      end

      # Fallback for unknown casts
      def handle_cast(msg, state) do
        require Logger
        Logger.warning("Unhandled cast: #{inspect(msg)}")
        {:noreply, state}
      end
    end
  end

  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
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
        # Process the message using similar logic to legacy server
        case Jason.decode(message) do
          {:ok, requests} when is_list(requests) ->
            # Check protocol version for batch support
            protocol_version = Map.get(state, :protocol_version, "2025-06-18")

            error_message =
              if protocol_version == "2025-06-18" do
                "Batch requests are not supported in protocol version 2025-06-18"
              else
                "Batch requests are not supported"
              end

            protocol_version = Map.get(state, :protocol_version, "2025-06-18")
            error_response = ResponseBuilder.build_batch_error(protocol_version)

            send_response(error_response, state)
            {:noreply, state}

          {:ok, request} when is_map(request) ->
            # Check if this is a create_message response first
            case create_message_response?(request, state) do
              {true, request_id} ->
                # Handle create_message response
                new_state = handle_create_message_response(message, request_id, state)
                {:noreply, new_state}

              false ->
                # Handle regular request
                case process_request(request, state) do
                  {:response, response, new_state} ->
                    send_response(response, new_state)
                    {:noreply, new_state}

                  {:notification, new_state} ->
                    {:noreply, new_state}
                end
            end

          {:error, error} ->
            require Logger
            Logger.error("Failed to decode message: #{inspect(error)}")
            {:noreply, state}
        end
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

      # Handle create_message timeout
      def handle_info({:create_message_timeout, request_id}, state) do
        pending_create_messages = Map.get(state, :pending_create_messages, %{})

        case Map.get(pending_create_messages, request_id) do
          nil ->
            # Request already completed or not found
            {:noreply, state}

          {from, _timestamp} ->
            # Reply with timeout error and remove from pending
            GenServer.reply(from, {:error, :timeout})
            updated_pending = Map.delete(pending_create_messages, request_id)
            new_state = Map.put(state, :pending_create_messages, updated_pending)
            {:noreply, new_state}
        end
      end

      # Default handle_info fallback
      def handle_info(_message, state) do
        {:noreply, state}
      end
    end
  end

  # credo:disable-for-lines:200 Credo.Check.Refactor.CyclomaticComplexity
  # credo:disable-for-lines:200 Credo.Check.Refactor.LongQuoteBlocks
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

      # Handle cancellation notifications from clients
      defp handle_cancellation_notification(%{"requestId" => request_id} = params, state) do
        require Logger
        reason = Map.get(params, "reason", "Request cancelled by client")

        Logger.debug("Received cancellation for request #{request_id}: #{reason}")

        # Use RequestTracker to handle cancellation
        case RequestTracker.handle_cancellation(request_id, state) do
          {:reply, from, new_state} ->
            # Request was still pending, reply with cancellation error
            GenServer.reply(from, {:error, :cancelled})
            {:noreply, new_state}

          {:noreply, new_state} ->
            # Request not found or already completed
            {:noreply, new_state}
        end
      end

      defp handle_cancellation_notification(params, state) do
        require Logger
        Logger.warning("Invalid cancellation notification: #{inspect(params)}")
        {:noreply, state}
      end

      # Check if a request has been cancelled
      defp request_cancelled?(request_id, state) do
        RequestTracker.cancelled?(request_id, state)
      end

      # Add a pending request to tracking
      defp track_pending_request(request_id, from, state) do
        RequestTracker.track_request(request_id, from, state)
      end

      # Remove a pending request from tracking (when completed)
      defp complete_pending_request(request_id, state) do
        RequestTracker.complete_request(request_id, state)
      end

      # Get list of pending request IDs
      defp get_pending_request_ids(state) do
        RequestTracker.get_pending_request_ids(state)
      end

      # Send response message via transport
      defp send_response(response, state) do
        case Map.get(state, :transport_client_pid) do
          nil ->
            require Logger
            Logger.warning("No transport client connected")

          client_pid when is_pid(client_pid) ->
            case Jason.encode(response) do
              {:ok, json} ->
                send(client_pid, {:transport_message, json})

              {:error, error} ->
                require Logger
                Logger.error("Failed to encode response: #{inspect(error)}")
            end
        end
      end

      # Process a single MCP request - delegate to RequestProcessor
      defp process_request(request, state) do
        RequestProcessor.process(request, state)
      end

      # Check if a message is a create_message response
      defp create_message_response?(request, state) do
        pending_create_messages = Map.get(state, :pending_create_messages, %{})

        case request do
          %{"id" => id, "result" => _} when is_binary(id) ->
            if Map.has_key?(pending_create_messages, id) do
              {true, id}
            else
              false
            end

          %{"id" => id, "error" => _} when is_binary(id) ->
            if Map.has_key?(pending_create_messages, id) do
              {true, id}
            else
              false
            end

          _ ->
            false
        end
      end

      # Handle create_message response
      defp handle_create_message_response(json_message, request_id, state) do
        pending_create_messages = Map.get(state, :pending_create_messages, %{})

        case Map.get(pending_create_messages, request_id) do
          nil ->
            # Request not found or already handled
            state

          {from, _timestamp} ->
            # Parse and reply with the response
            case Jason.decode(json_message) do
              {:ok, %{"result" => result}} ->
                GenServer.reply(from, {:ok, result})

              {:ok, %{"error" => error}} ->
                GenServer.reply(from, {:error, error})

              {:error, decode_error} ->
                GenServer.reply(from, {:error, {:decode_failed, decode_error}})
            end

            # Remove from pending
            updated_pending = Map.delete(pending_create_messages, request_id)
            Map.put(state, :pending_create_messages, updated_pending)
        end
      end
    end
  end

  defp generate_overridable_list do
    quote do
      # Mark callbacks as overridable
      defoverridable handle_tool_call: 3,
                     handle_resource_read: 3,
                     handle_resource_list: 1,
                     handle_resource_subscribe: 2,
                     handle_resource_unsubscribe: 2,
                     handle_prompt_get: 3,
                     handle_prompt_list: 1,
                     handle_request: 3,
                     handle_initialize: 2,
                     init: 1,
                     handle_call: 3,
                     handle_cast: 2,
                     handle_info: 2
    end
  end
end
