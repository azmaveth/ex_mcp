defmodule ExMCP.Server.StdioServer do
  @moduledoc """
  STDIO transport server for MCP protocol.

  This server reads from stdin and writes to stdout, making it suitable
  for command-line tools and scripting environments.

  ## Important: STDIO Transport Requirements

  The MCP STDIO transport requires that ONLY JSON-RPC messages appear on stdout.
  All other output (logs, debug messages, etc.) MUST go to stderr to avoid
  contaminating the protocol stream.

  ## Handling Startup Output

  This module implements several strategies to handle startup output from Mix.install
  and other sources:

  1. **Automatic logging suppression** - Configures all loggers to emergency level
  2. **Startup delay** - Waits before reading stdin (configurable via `:stdio_startup_delay`)
  3. **Graceful non-JSON handling** - Ignores non-JSON lines instead of sending errors

  ## Configuration

  For scripts using Mix.install, add this before calling Mix.install:

      # Configure STDIO mode and startup delay
      Application.put_env(:ex_mcp, :stdio_mode, true)
      Application.put_env(:ex_mcp, :stdio_startup_delay, 500)  # ms

      # Suppress all logging for clean STDIO JSON-RPC
      System.put_env("ELIXIR_LOG_LEVEL", "emergency")

  ## Usage

      defmodule MyStdioServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.DSL, name: "my-stdio-server", version: "1.0.0"

        tool "hello", "Says hello" do
          param :name, :string, required: true
          run fn %{name: name}, state -> {:ok, "Hello, \#{name}!", state} end
        end
      end

      # Start with STDIO transport
      MyStdioServer.start_link(transport: :stdio)
  """

  use GenServer
  require Logger

  alias ExMCP.Error.ProtocolError
  alias ExMCP.Internal.{JSONRPC, StdioLoggerConfig, VersionRegistry}
  alias ExMCP.Protocol.ErrorCodes
  alias ExMCP.Server.{Dispatch, RequestContext, RequestState, ResultNormalizer, Subscriptions}

  @doc """
  Starts the STDIO server.

  ## Options

  * `:module` - The handler module implementing server callbacks
  * Other options are passed to GenServer.start_link
  """
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl GenServer
  def init(opts) do
    case validate_mrtr_configuration(opts) do
      :ok -> do_init(opts)
      {:error, reason} -> {:stop, {:mrtr_configuration_error, reason}}
    end
  end

  defp do_init(opts) do
    # CRITICAL: For STDIO transport, suppress ALL logging to avoid contaminating JSON stream
    # MCP STDIO protocol requires ONLY JSON-RPC messages on stdout
    configure_stdio_logging()

    module = Keyword.fetch!(opts, :module)
    {subscription_opts, owned_subscription_runtime} = ensure_subscription_runtime(opts)

    # Initialize the handler module state
    initial_state =
      case function_exported?(module, :init, 1) do
        true ->
          case module.init(opts) do
            {:ok, state} -> state
            _ -> %{}
          end

        false ->
          %{}
      end

    state = %{
      handler_module: module,
      handler_state: initial_state,
      request_id: 0,
      protocol_mode: Keyword.get(opts, :protocol_mode),
      connection_era: nil,
      instructions: Keyword.get(opts, :instructions),
      request_state: Keyword.get(opts, :request_state),
      endpoint: Keyword.get(opts, :endpoint, "stdio"),
      principal_id: Keyword.get(opts, :principal_id),
      tenant_id: Keyword.get(opts, :tenant_id),
      replay_cache: Keyword.get(opts, :replay_cache),
      require_replay_protection: Keyword.get(opts, :require_replay_protection, false),
      subscriptions: %{},
      subscription_options:
        subscription_opts
        |> Keyword.put_new(:endpoint, "stdio")
        |> Subscriptions.runtime_options(module),
      owned_subscription_runtime: owned_subscription_runtime
    }

    Logger.info("STDIO MCP server started with handler: #{module}")

    # Start reading from stdin in a separate process
    # Add a delay to allow Mix.install and other startup output to complete
    # This is especially important when Mix.install is used in the same process
    server = self()

    spawn_link(fn ->
      # Wait for any startup output to finish
      # 100ms is usually enough for small scripts, but Mix.install may need more
      startup_delay = Application.get_env(:ex_mcp, :stdio_startup_delay, 100)
      Process.sleep(startup_delay)
      read_stdin_loop(server)
    end)

    {:ok, state}
  end

  defp validate_mrtr_configuration(opts) do
    if Keyword.get(opts, :mrtr, false) do
      RequestState.validate_configuration(request_state: Keyword.get(opts, :request_state))
    else
      :ok
    end
  end

  @impl GenServer
  def handle_info({:stdin_line, line}, state) do
    case Jason.decode(line) do
      {:ok, request} ->
        handle_request(request, state)

      {:error, _error} ->
        # During startup, Mix.install and other tools may output non-JSON lines
        # We silently ignore these instead of sending error responses
        # Only log at debug level to avoid stderr contamination
        Logger.debug("Ignoring non-JSON line: #{inspect(line)}")
        {:noreply, state}
    end
  end

  def handle_info({:stdin_closed}, state) do
    Logger.info("STDIN closed, shutting down server")
    {:stop, :normal, state}
  end

  def handle_info(
        {:ex_mcp_subscription_message, listener, kind, message},
        state
      ) do
    send_response(message, state)
    Subscriptions.delivered(listener)

    state =
      if kind == :complete,
        do: remove_subscription_by_listener(state, listener),
        else: state

    {:noreply, state}
  end

  @impl GenServer
  def handle_call(:get_server_info, _from, state) do
    module = state.handler_module

    server_info =
      case function_exported?(module, :__server_info__, 0) do
        true -> module.__server_info__()
        false -> %{name: to_string(module), version: "1.0.0"}
      end

    {:reply, server_info, state}
  end

  def handle_call(request, from, state) do
    # Forward unknown calls to the handler module if it supports them
    module = state.handler_module

    case function_exported?(module, :handle_call, 3) do
      true ->
        case module.handle_call(request, from, state.handler_state) do
          {:reply, reply, new_handler_state} ->
            new_state = %{state | handler_state: new_handler_state}
            {:reply, reply, new_state}

          other ->
            other
        end

      false ->
        {:reply, {:error, {:unknown_call, request}}, state}
    end
  end

  @impl GenServer
  def handle_cast({:notify_resource_update, uri}, state) do
    publish_or_send("notifications/resources/updated", %{"uri" => uri}, state)
  end

  def handle_cast({:notify_resources_changed}, state) do
    publish_or_send("notifications/resources/list_changed", %{}, state)
  end

  def handle_cast({:notify_tools_changed}, state) do
    publish_or_send("notifications/tools/list_changed", %{}, state)
  end

  def handle_cast({:notify_prompts_changed}, state) do
    publish_or_send("notifications/prompts/list_changed", %{}, state)
  end

  def handle_cast(_request, state), do: {:noreply, state}

  # Handle incoming MCP requests.
  #
  # Method coverage and result shaping come from ExMCP.Server.Dispatch, the
  # same table the process-based transports use (audit M9), so stdio no longer
  # silently lacks completion/complete, resources/subscribe, roots/list,
  # logging/setLevel or the task methods. Only stdio-specific concerns —
  # protocol version negotiation and the custom `handle_request/3` escape
  # hatch — live here.
  defp handle_request(%{"method" => "initialize"} = request, state) do
    params = Map.get(request, "params", %{})
    negotiated_version = negotiate_version(params)

    request =
      Map.put(request, "params", Map.put(params, "protocolVersion", negotiated_version))

    state
    |> dispatch(request)
    |> put_protocol_version(negotiated_version)
  end

  defp handle_request(%{"method" => "subscriptions/listen"} = request, state) do
    open_subscription(request, state)
  end

  defp handle_request(%{"method" => "notifications/cancelled"} = request, state) do
    request_id = get_in(request, ["params", "requestId"])

    case Map.pop(state.subscriptions, request_id) do
      {nil, _subscriptions} ->
        dispatch(state, request)

      {_listener, subscriptions} ->
        :ok = Subscriptions.cancel(self(), request_id, state.subscription_options)
        {:noreply, %{state | subscriptions: subscriptions}}
    end
  end

  defp handle_request(%{"method" => method} = request, state) do
    cond do
      Dispatch.known_method?(method) ->
        dispatch(state, request)

      function_exported?(state.handler_module, :handle_request, 3) ->
        handle_custom_request(request, state)

      true ->
        # Unknown method (or notification): let the shared table answer.
        dispatch(state, request)
    end
  end

  defp handle_request(request, state) do
    dispatch(state, request)
  end

  defp dispatch(state, request) do
    state = maybe_pin_connection_era(request, state)

    dispatch_opts = [
      protocol_mode: effective_protocol_mode(state),
      instructions: state.instructions,
      request_state: state.request_state,
      endpoint: state.endpoint,
      principal_id: state.principal_id,
      tenant_id: state.tenant_id,
      replay_cache: state.replay_cache,
      require_replay_protection: state.require_replay_protection
    ]

    case Dispatch.dispatch(request, state.handler_module, state.handler_state, dispatch_opts) do
      {:response, response, handler_state} ->
        send_response(response, state)
        {:noreply, %{state | handler_state: handler_state}}

      {:notification, handler_state} ->
        {:noreply, %{state | handler_state: handler_state}}
    end
  end

  defp maybe_pin_connection_era(request, %{connection_era: nil} = state) do
    with {:ok, context} <- RequestContext.from_message(request),
         era when era in [:legacy, :modern] <- pin_candidate(context),
         true <- mode_allows_era?(state.protocol_mode, era) do
      %{state | connection_era: era}
    else
      _other -> state
    end
  end

  defp maybe_pin_connection_era(_request, state), do: state

  defp pin_candidate(%RequestContext{era: :modern}), do: :modern
  defp pin_candidate(%RequestContext{era: :legacy, method: "initialize"}), do: :legacy
  defp pin_candidate(_context), do: nil

  defp mode_allows_era?(:modern_only, :legacy), do: false
  defp mode_allows_era?(:legacy_only, :modern), do: false
  defp mode_allows_era?(_mode, _era), do: true

  defp effective_protocol_mode(%{protocol_mode: mode})
       when mode in [:legacy_only, :modern_only],
       do: mode

  defp effective_protocol_mode(%{connection_era: :legacy}), do: :legacy_only
  defp effective_protocol_mode(%{connection_era: :modern}), do: :modern_only
  defp effective_protocol_mode(state), do: state.protocol_mode

  # Servers may implement handle_request/3 to answer methods outside the MCP
  # method table (ExMCP extension).
  defp handle_custom_request(%{"method" => method} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})

    case state.handler_module.handle_request(method, params, state.handler_state) do
      {:reply, result, new_handler_state} ->
        send_response(JSONRPC.response(id, result), state)
        {:noreply, %{state | handler_state: new_handler_state}}

      {:error, error, new_handler_state} ->
        send_error_response(
          -32000,
          ResultNormalizer.error_message("Request error", error),
          id,
          state
        )

        {:noreply, %{state | handler_state: new_handler_state}}

      {:noreply, new_handler_state} ->
        {:noreply, %{state | handler_state: new_handler_state}}
    end
  end

  defp negotiate_version(params) do
    client_version = Map.get(params, "protocolVersion", VersionRegistry.latest_version())
    server_versions = VersionRegistry.supported_versions()

    case VersionRegistry.negotiate_version(client_version, server_versions) do
      {:ok, version} -> version
      {:error, :version_mismatch} -> VersionRegistry.latest_version()
    end
  end

  defp put_protocol_version({:noreply, state}, version) do
    {:noreply, Map.put(state, :protocol_version, version)}
  end

  # Send a successful response
  defp send_response(response, _state) do
    json = Jason.encode!(response)
    IO.puts(json)
  end

  # Send an error response
  defp send_error_response(code, message, id, _state) do
    response = JSONRPC.error(id, code, message)

    json = Jason.encode!(response)
    IO.puts(json)
  end

  # Configure logging for STDIO transport to prevent stdout contamination
  defp configure_stdio_logging do
    StdioLoggerConfig.configure()
  end

  # Read from stdin in a loop and send lines to the main process
  defp read_stdin_loop(server_pid) do
    case IO.read(:stdio, :line) do
      :eof ->
        send(server_pid, {:stdin_closed})

      {:error, reason} ->
        Logger.error("STDIN read error: #{inspect(reason)}")
        send(server_pid, {:stdin_closed})

      line when is_binary(line) ->
        line = String.trim(line)

        if line != "" do
          send(server_pid, {:stdin_line, line})
        end

        read_stdin_loop(server_pid)
    end
  end

  @impl GenServer
  def terminate(reason, state) do
    Subscriptions.remove_transport(self(), state.subscription_options)

    if function_exported?(state.handler_module, :terminate, 2) do
      state.handler_module.terminate(reason, state.handler_state)
    end

    stop_owned_subscription_runtime(state.owned_subscription_runtime)
  end

  defp open_subscription(request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params") || %{}

    with {:ok, context} <- RequestContext.from_message(request),
         :ok <- RequestContext.validate_protocol_mode(context, effective_protocol_mode(state)),
         :ok <- RequestContext.validate_method(context),
         :modern <- context.era,
         {:ok, entry} <-
           Subscriptions.listen(
             id,
             Map.get(params, "notifications"),
             self(),
             Keyword.put(
               state.subscription_options,
               :client_capabilities,
               context.client_capabilities
             )
           ) do
      subscriptions = Map.put(state.subscriptions, id, entry.listener_pid)
      {:noreply, %{state | subscriptions: subscriptions, connection_era: :modern}}
    else
      {:error, reason} ->
        send_response(subscription_error(id, reason), state)
        {:noreply, state}

      _other ->
        send_response(subscription_error(id, :modern_protocol_required), state)
        {:noreply, state}
    end
  end

  defp subscription_error(id, %ProtocolError{} = error) do
    JSONRPC.error(id, error.code, error.message, error.data)
  end

  defp subscription_error(id, reason) do
    JSONRPC.error(id, ErrorCodes.invalid_params(), "Subscription request rejected", %{
      "reason" => to_string_reason(reason)
    })
  end

  defp to_string_reason(reason) when is_atom(reason), do: Atom.to_string(reason)
  defp to_string_reason(reason), do: inspect(reason)

  defp publish_or_send(method, params, state) do
    if modern_connection?(state) do
      _counts =
        Subscriptions.publish(
          method,
          params,
          Keyword.put(state.subscription_options, :transport_ref, self())
        )

      {:noreply, state}
    else
      send_response(%{"jsonrpc" => "2.0", "method" => method, "params" => params}, state)
      {:noreply, state}
    end
  end

  defp modern_connection?(%{connection_era: :modern}), do: true
  defp modern_connection?(%{protocol_version: version}), do: VersionRegistry.modern?(version)

  defp remove_subscription_by_listener(state, listener) do
    subscriptions =
      Map.reject(state.subscriptions, fn {_id, listener_pid} -> listener_pid == listener end)

    %{state | subscriptions: subscriptions}
  end

  defp ensure_subscription_runtime(opts) do
    cond do
      Keyword.has_key?(opts, :subscription_registry) ->
        {opts, nil}

      Process.whereis(Subscriptions) ->
        {opts, nil}

      true ->
        {:ok, supervisor} = DynamicSupervisor.start_link(strategy: :one_for_one)

        {:ok, registry} =
          Subscriptions.start_link(
            name: nil,
            listener_supervisor: supervisor
          )

        {Keyword.put(opts, :subscription_registry, registry), {registry, supervisor}}
    end
  end

  defp stop_owned_subscription_runtime(nil), do: :ok

  defp stop_owned_subscription_runtime({registry, supervisor}) do
    if Process.alive?(registry), do: GenServer.stop(registry, :normal)
    if Process.alive?(supervisor), do: Supervisor.stop(supervisor, :normal)
    :ok
  end
end
