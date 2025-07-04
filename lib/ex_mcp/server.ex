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

  alias ExMCP.DSL.CodeGenerator
  alias ExMCP.Server.Legacy

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
  Callback for handling initialization requests.

  Called when a client sends an initialize request. Allows custom
  version negotiation and capability setup.
  """
  @callback handle_initialize(params :: map(), state) ::
              {:ok, map(), state} | {:error, term(), state}

  # Make callbacks optional with default implementations
  @optional_callbacks [
    handle_resource_read: 3,
    handle_resource_list: 1,
    handle_resource_subscribe: 2,
    handle_resource_unsubscribe: 2,
    handle_prompt_list: 1,
    handle_request: 3,
    handle_tool_call: 3,
    handle_prompt_get: 3,
    handle_initialize: 2
  ]

  defmacro __using__(opts \\ []) do
    CodeGenerator.generate(opts)
  end

  # All code generation functions have been moved to ExMCP.DSL.CodeGenerator
  # The following functions are kept commented for reference during the refactoring

  if false do
    defp generate_imports do
      quote do
        use GenServer
        import ExMCP.DSL.Tool
        import ExMCP.DSL.Resource
        import ExMCP.DSL.Prompt

        alias ExMCP.Protocol.ResponseBuilder

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

        def handle_initialize(_params, state),
          do: {:error, :initialize_not_implemented, state}
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

            client_pid when is_pid(client_pid) ->
              send_and_store_pending_request(params, from, client_pid, state)
          end
        end

        # Send message and store pending request (async)
        defp send_and_store_pending_request(params, from, client_pid, state) do
          message = ExMCP.Internal.Protocol.encode_create_message(params)
          request_id = generate_request_id()

          # Add request ID to the message for correlation
          message_with_id = Map.put(message, "id", request_id)

          case encode_and_send_message(message_with_id, client_pid) do
            :ok ->
              # Store the pending create_message request with a timeout
              pending_create_messages = Map.get(state, :pending_create_messages, %{})

              updated_pending =
                Map.put(
                  pending_create_messages,
                  request_id,
                  {from, System.monotonic_time(:millisecond)}
                )

              new_state = Map.put(state, :pending_create_messages, updated_pending)

              # Set up timeout for this request
              Process.send_after(self(), {:create_message_timeout, request_id}, 30_000)

              {:noreply, new_state}

            {:error, error} ->
              {:reply, {:error, error}, state}
          end
        end

        # Encode and send message to client
        defp encode_and_send_message(message, client_pid) do
          case Jason.encode(message) do
            {:ok, json} ->
              send(client_pid, {:transport_message, json})
              :ok

            {:error, error} ->
              {:error, error}
          end
        end

        # Generate unique request ID for create_message correlation
        defp generate_request_id do
          "create_msg_#{:erlang.unique_integer([:positive, :monotonic])}"
        end

        # Check if incoming message is a create_message response
        defp create_message_response?(request, state) do
          pending_create_messages = Map.get(state, :pending_create_messages, %{})
          request_id = Map.get(request, "id")

          if request_id && Map.has_key?(pending_create_messages, request_id) do
            {true, request_id}
          else
            false
          end
        end

        # Handle create_message response (called from handle_info)
        defp handle_create_message_response(response_data, request_id, state) do
          pending_create_messages = Map.get(state, :pending_create_messages, %{})

          case Map.get(pending_create_messages, request_id) do
            nil ->
              # Request not found (already timed out or completed)
              state

            {from, _timestamp} ->
              # Process response and reply to caller
              result =
                case Jason.decode(response_data) do
                  {:ok, %{"result" => result}} ->
                    {:ok, result}

                  {:ok, %{"error" => error}} ->
                    {:error, error}

                  _ ->
                    {:error, :invalid_response}
                end

              GenServer.reply(from, result)

              # Remove from pending requests
              updated_pending = Map.delete(pending_create_messages, request_id)
              Map.put(state, :pending_create_messages, updated_pending)
          end
        end
      end
    end

    # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
    defp generate_mcp_handle_calls do
      quote do
        # Handle tool call requests with cancellation support
        def handle_call({:handle_tool_call, tool_name, arguments, request_id}, from, state) do
          # Check if request was already cancelled
          if request_cancelled?(request_id, state) do
            {:reply, {:error, :cancelled}, state}
          else
            # Track the pending request
            new_state = track_pending_request(request_id, from, state)

            # Check if the module has custom handle_tool_call implementation
            if function_exported?(__MODULE__, :handle_tool_call, 3) do
              case handle_tool_call(tool_name, arguments, new_state) do
                {:ok, result, final_state} ->
                  completed_state = complete_pending_request(request_id, final_state)
                  {:reply, {:ok, result}, completed_state}

                {:error, reason, final_state} ->
                  completed_state = complete_pending_request(request_id, final_state)
                  {:reply, {:error, reason}, completed_state}
              end
            else
              # No custom implementation, return default error
              completed_state = complete_pending_request(request_id, new_state)
              {:reply, {:error, :tool_not_implemented}, completed_state}
            end
          end
        end

        # Fallback for tool calls without request_id (legacy support)
        def handle_call({:handle_tool_call, tool_name, arguments}, _from, state) do
          if function_exported?(__MODULE__, :handle_tool_call, 3) do
            result = handle_tool_call(tool_name, arguments, state)
            {:reply, result, state}
          else
            {:reply, {:error, :tool_not_implemented}, state}
          end
        end

        # Handle resource read requests with cancellation support
        def handle_call({:handle_resource_read, uri, full_uri, request_id}, from, state) do
          # Check if request was already cancelled
          if request_cancelled?(request_id, state) do
            {:reply, {:error, :cancelled}, state}
          else
            # Track the pending request
            new_state = track_pending_request(request_id, from, state)

            # Check if the module has custom handle_resource_read implementation
            if function_exported?(__MODULE__, :handle_resource_read, 3) do
              case handle_resource_read(uri, full_uri, new_state) do
                {:ok, content, final_state} ->
                  completed_state = complete_pending_request(request_id, final_state)
                  {:reply, {:ok, content}, completed_state}

                {:error, reason, final_state} ->
                  completed_state = complete_pending_request(request_id, final_state)
                  {:reply, {:error, reason}, completed_state}
              end
            else
              # No custom implementation, return default error
              completed_state = complete_pending_request(request_id, new_state)
              {:reply, {:error, :resource_not_found}, completed_state}
            end
          end
        end

        # Fallback for resource reads without request_id (legacy support)
        def handle_call({:handle_resource_read, uri, full_uri}, _from, state) do
          if function_exported?(__MODULE__, :handle_resource_read, 3) do
            case handle_resource_read(uri, full_uri, state) do
              {:ok, content, new_state} ->
                {:reply, {:ok, content}, new_state}

              {:error, reason, new_state} ->
                {:reply, {:error, reason}, new_state}
            end
          else
            {:reply, {:error, :resource_not_found}, state}
          end
        end

        # Handle prompt get requests with cancellation support
        def handle_call({:handle_prompt_get, prompt_name, arguments, request_id}, from, state) do
          # Check if request was already cancelled
          if request_cancelled?(request_id, state) do
            {:reply, {:error, :cancelled}, state}
          else
            # Check if custom implementation exists
            if function_exported?(__MODULE__, :handle_prompt_get, 3) do
              # Track the pending request
              new_state = track_pending_request(request_id, from, state)

              case handle_prompt_get(prompt_name, arguments, new_state) do
                {:ok, result, final_state} ->
                  completed_state = complete_pending_request(request_id, final_state)
                  {:reply, {:ok, result}, completed_state}

                {:error, reason, final_state} ->
                  completed_state = complete_pending_request(request_id, final_state)
                  {:reply, {:error, reason}, completed_state}
              end
            else
              {:reply, {:error, :prompt_not_implemented}, state}
            end
          end
        end

        # Fallback for prompt gets without request_id (legacy support)
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

        # Handle cancellation notifications from clients
        def handle_cast({:notification, "notifications/cancelled", params}, state) do
          handle_cancellation_notification(params, state)
        end

        # Handle generic notifications
        def handle_cast({:notification, method, params}, state) do
          # Forward to handle_request for custom notification handling
          # Default handle_request only returns {:noreply, state}
          case handle_request(method, params, state) do
            {:noreply, new_state} -> {:noreply, new_state}
          end
        end

        # Default handle_cast fallback
        def handle_cast(_request, state) do
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

        # Legacy process_request implementations kept for reference
        # These are now handled by RequestProcessor
        if false do
          defp process_request_old(%{"method" => "initialize"} = request, state) do
            id = Map.get(request, "id")
            params = Map.get(request, "params", %{})

            # Check if the module has custom handle_initialize implementation
            if function_exported?(__MODULE__, :handle_initialize, 2) do
              case handle_initialize(params, state) do
                {:ok, result, new_state} ->
                  response = ResponseBuilder.build_success_response(result, id)
                  {:response, response, new_state}

                {:error, reason, new_state} ->
                  error_response = ResponseBuilder.build_mcp_error(:server_error, id, reason)

                  {:response, error_response, new_state}
              end
            else
              # Default DSL server initialization with proper version validation
              client_version = Map.get(params, "protocolVersion", "2025-06-18")
              supported_versions = VersionRegistry.supported_versions()

              if client_version in supported_versions do
                result = %{
                  "protocolVersion" => client_version,
                  "serverInfo" => get_server_info_from_opts(),
                  "capabilities" => get_capabilities()
                }

                response = ResponseBuilder.build_success_response(result, id)
                new_state = Map.put(state, :protocol_version, client_version)
                {:response, response, new_state}
              else
                error_response =
                  ResponseBuilder.build_mcp_error(
                    :invalid_request,
                    id,
                    "Unsupported protocol version: #{client_version}",
                    %{"supported_versions" => supported_versions}
                  )

                {:response, error_response, state}
              end
            end
          end

          defp process_request(%{"method" => "tools/list"} = request, state) do
            id = Map.get(request, "id")
            tools = get_tools() |> Map.values()
            result = %{"tools" => tools}
            response = ResponseBuilder.build_success_response(result, id)
            {:response, response, state}
          end

          defp process_request(%{"method" => "tools/call"} = request, state) do
            id = Map.get(request, "id")
            params = Map.get(request, "params", %{})
            tool_name = Map.get(params, "name")
            arguments = Map.get(params, "arguments", %{})

            # Check if the module has custom handle_tool_call implementation
            if function_exported?(__MODULE__, :handle_tool_call, 3) do
              case handle_tool_call(tool_name, arguments, state) do
                {:ok, result, new_state} ->
                  response = ResponseBuilder.build_success_response(result, id)
                  {:response, response, new_state}

                {:error, reason, new_state} ->
                  error_response =
                    ResponseBuilder.build_mcp_error(:server_error, id, inspect(reason))

                  {:response, error_response, new_state}
              end
            else
              # No custom implementation, return default error
              error_response =
                ResponseBuilder.build_mcp_error(:server_error, id, "Tool not implemented")

              {:response, error_response, state}
            end
          end

          defp process_request(%{"method" => "resources/list"} = request, state) do
            id = Map.get(request, "id")
            resources = get_resources() |> Map.values()
            result = %{"resources" => resources}
            response = ResponseBuilder.build_success_response(result, id)
            {:response, response, state}
          end

          defp process_request(%{"method" => "resources/read"} = request, state) do
            id = Map.get(request, "id")
            params = Map.get(request, "params", %{})
            uri = Map.get(params, "uri")

            # Check if the module has custom handle_resource_read implementation
            if function_exported?(__MODULE__, :handle_resource_read, 3) do
              case handle_resource_read(uri, uri, state) do
                {:ok, content, new_state} ->
                  result = %{"contents" => [content]}
                  response = ResponseBuilder.build_success_response(result, id)
                  {:response, response, new_state}

                {:error, reason, new_state} ->
                  error_response =
                    ResponseBuilder.build_mcp_error(:server_error, id, inspect(reason))

                  {:response, error_response, new_state}
              end
            else
              # No custom implementation, return default error
              error_response =
                ResponseBuilder.build_mcp_error(
                  :server_error,
                  id,
                  "Resource reading not implemented"
                )

              {:response, error_response, state}
            end
          end

          defp process_request(%{"method" => "prompts/list"} = request, state) do
            id = Map.get(request, "id")
            prompts = get_prompts() |> Map.values()
            result = %{"prompts" => prompts}
            response = ResponseBuilder.build_success_response(result, id)
            {:response, response, state}
          end

          defp process_request(%{"method" => "prompts/get"} = request, state) do
            id = Map.get(request, "id")
            params = Map.get(request, "params", %{})
            prompt_name = Map.get(params, "name")
            arguments = Map.get(params, "arguments", %{})

            # Check if the module has custom handle_prompt_get implementation
            if function_exported?(__MODULE__, :handle_prompt_get, 3) do
              case handle_prompt_get(prompt_name, arguments, state) do
                {:ok, result, new_state} ->
                  response = ResponseBuilder.build_success_response(result, id)
                  {:response, response, new_state}

                {:error, reason, new_state} ->
                  error_response =
                    ResponseBuilder.build_mcp_error(:server_error, id, inspect(reason))

                  {:response, error_response, new_state}
              end
            else
              # No custom implementation, return default error
              error_response =
                ResponseBuilder.build_mcp_error(
                  :server_error,
                  id,
                  "Prompt retrieval not implemented"
                )

              {:response, error_response, state}
            end
          end

          defp process_request(%{"method" => "notifications/initialized"} = request, state) do
            # This is a notification from client, not a request - just acknowledge it
            {:notification, state}
          end

          defp process_request(%{"method" => method} = request, state) do
            # For other methods, return method not found
            id = Map.get(request, "id")

            error_response =
              ResponseBuilder.build_mcp_error(
                :method_not_found,
                id,
                "Method not found: #{method}"
              )

            {:response, error_response, state}
          end

          defp process_request(_request, state) do
            # Invalid request format
            error_response =
              ResponseBuilder.build_mcp_error(:invalid_request, nil, "Invalid Request")

            {:response, error_response, state}
          end
        end

        # end if false
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
                       handle_prompt_get: 3,
                       handle_initialize: 2
      end
    end
  end

  # if false

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
        # Use handler-based server implementation
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
    GenServer.call(server, {:list_roots, timeout}, timeout)
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
  def notify_progress(server, progress_token, progress) do
    GenServer.cast(server, {:notify_progress, progress_token, progress, nil})
  end

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

  @doc """
  Alias for notify_resource_update/2 for backward compatibility.
  """
  @spec notify_resource_updated(GenServer.server(), String.t()) :: :ok
  def notify_resource_updated(server, uri) do
    notify_resource_update(server, uri)
  end

  @doc """
  Notifies subscribed clients that the resource list has changed.
  """
  @spec notify_resources_changed(GenServer.server()) :: :ok
  def notify_resources_changed(server) do
    GenServer.cast(server, {:notify_resources_changed})
  end

  @doc """
  Notifies subscribed clients that the tools list has changed.
  """
  @spec notify_tools_changed(GenServer.server()) :: :ok
  def notify_tools_changed(server) do
    GenServer.cast(server, {:notify_tools_changed})
  end

  @doc """
  Notifies subscribed clients that the prompts list has changed.
  """
  @spec notify_prompts_changed(GenServer.server()) :: :ok
  def notify_prompts_changed(server) do
    GenServer.cast(server, {:notify_prompts_changed})
  end

  @doc """
  Gets the list of pending request IDs on the server.

  Returns a list of request IDs for requests that are currently being processed
  by the server. This can be used to monitor server load and track long-running
  operations.

  ## Examples

      {:ok, server} = MyServer.start_link()

      # Get pending requests
      pending = ExMCP.Server.get_pending_requests(server)
      # => ["req_123", "req_456"]
  """
  @spec get_pending_requests(GenServer.server()) :: [ExMCP.Types.request_id()]
  def get_pending_requests(server) do
    GenServer.call(server, :get_pending_requests)
  end

  @doc """
  Sends a cancellation notification to the server.

  This function allows external processes to notify the server that a request
  should be cancelled. The server will attempt to cancel the request if it's
  still pending.

  ## Parameters

  - `server` - Server process reference
  - `request_id` - The ID of the request to cancel
  - `reason` - Optional human-readable reason for cancellation

  ## Returns

  - `:ok` - Cancellation notification sent

  ## Examples

      :ok = ExMCP.Server.cancel_request(server, "req_123", "User cancelled")
      :ok = ExMCP.Server.cancel_request(server, 12345, nil)
  """
  @spec cancel_request(GenServer.server(), ExMCP.Types.request_id(), String.t() | nil) :: :ok
  def cancel_request(server, request_id, reason \\ nil) do
    params = %{"requestId" => request_id}
    params = if reason, do: Map.put(params, "reason", reason), else: params
    GenServer.cast(server, {:notification, "notifications/cancelled", params})
  end

  @doc """
  Sends a createMessage request to the connected client.

  This function allows the server to request message creation from the client
  using the sampling/createMessage method. This is part of the MCP sampling
  feature where servers can ask clients to generate LLM responses.

  ## Parameters

  - `server` - Server process reference
  - `params` - Parameters for message creation including messages, modelPreferences, etc.

  ## Returns

  - `{:ok, result}` - The created message response from the client
  - `{:error, reason}` - If the request fails

  ## Examples

      params = %{
        "messages" => [
          %{"role" => "user", "content" => %{"type" => "text", "text" => "Hello"}}
        ],
        "modelPreferences" => %{
          "hints" => [%{"name" => "gpt-4"}]
        }
      }

      {:ok, response} = ExMCP.Server.create_message(server, params)
  """
  @spec create_message(GenServer.server(), map()) :: {:ok, map()} | {:error, term()}
  def create_message(server, params) do
    # Try to call the server with create_message
    case GenServer.call(server, {:create_message, params}) do
      {:error, {:unknown_call, _}} ->
        # Fall back to direct protocol call for servers that don't implement this
        {:error, :not_implemented}

      result ->
        result
    end
  end
end
