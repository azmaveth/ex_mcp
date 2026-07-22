defmodule ExMCP.Client do
  @moduledoc """
  Unified MCP client combining the best features of all implementations.

  This module provides a clean, consistent API for interacting with MCP servers
  while maintaining backward compatibility with existing code.

  ## Features

  - Simple connection with URL strings or transport specs
  - Automatic transport fallback via TransportManager
  - Automatic reconnection with exponential backoff after unexpected
    transport closure (see `start_link/1`)
  - Consistent return values with optional normalization
  - Convenience methods for common operations
  - Clean separation of concerns

  ## Examples

      # Connect with URL
      {:ok, client} = ExMCP.Client.connect("http://localhost:8080/mcp")

      # Connect with transport spec
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: "mcp-server"
      )

      # List and call tools
      {:ok, %{"tools" => tools}} = ExMCP.Client.list_tools(client)
      {:ok, result} = ExMCP.Client.call_tool(client, "weather", %{location: "NYC"})
  """

  alias ExMCP.Error
  alias ExMCP.Protocol.ErrorCodes

  use GenServer
  require Logger

  alias ExMCP.Client.{ConnectionManager, RequestHandler}
  alias ExMCP.Client.Operations.{Prompts, Resources, Tools}
  alias ExMCP.Internal.{Protocol, RequestParams, VersionInfo}
  alias ExMCP.Reliability.Retry
  alias ExMCP.Response

  # Reconnection defaults ported from the former state machine implementation:
  # exponential backoff starting at 1s, doubling per attempt, capped at 60s,
  # with up to 10 attempts before giving up.
  @default_max_reconnect_attempts 10
  @default_reconnect_backoff [initial: 1_000, max: 60_000, multiplier: 2]

  # Client state
  defstruct [
    :transport_mod,
    :transport_state,
    :server_info,
    :transport_opts,
    :pending_requests,
    :pending_batches,
    :cancelled_requests,
    :receiver_task,
    :health_check_ref,
    :health_check_interval,
    :connection_status,
    :last_activity,
    :reconnect_attempts,
    :reconnect_enabled,
    :max_reconnect_attempts,
    :reconnect_backoff,
    :reconnect_timer,
    :manual_disconnect,
    :client_info,
    :server_capabilities,
    :initialized,
    :default_retry_policy,
    :protocol_version,
    :default_timeout,
    # Monitor refs of in-flight async POST tasks (HTTP/SSE transport),
    # mapped to the request id each task serves.
    async_post_tasks: %{},
    # Memoized client handler: nil (not yet initialized), :none (no handler
    # configured) or {module, handler_state}. Initialized once; callback
    # returns update handler_state, so stateful client handlers work.
    client_handler: nil,
    # In-flight server-request handler tasks (sampling/elicitation/custom):
    # task pid => {monitor_ref, request_id, kind}. Handlers run off the
    # client loop so a slow sampling callback cannot block responses.
    server_request_tasks: %{}
  ]

  @type t :: GenServer.server()
  @type connection_spec :: String.t() | {atom(), keyword()} | [{atom(), keyword()}]

  # Public API

  @doc """
  Starts a client process with the given options.

  ## Options

  - `:transport` - Transport type (:stdio, :http, :beam, etc.)
  - `:transports` - List of transports for fallback
  - `:name` - Optional GenServer name
  - `:handshake_timeout` - Maximum time in milliseconds to wait for the
    server's `initialize` response during connection (default: 10_000).
    On expiry `start_link/1` fails with `{:error, :handshake_timeout}`.
  - `:health_check_interval` - Interval for health checks (default: 30_000)
  - `:reliability` - Reliability features configuration (optional)
  - `:retry_policy` - Default retry policy for all client operations (optional)
  - `:reconnect` - Automatically reconnect when the transport closes
    unexpectedly (default: `true`)
  - `:max_reconnect_attempts` - Consecutive failed reconnection attempts
    before giving up (default: 10)
  - `:reconnect_backoff` - Reconnection backoff policy (keyword list):
    - `:initial` - Delay before the first attempt in ms (default: 1000)
    - `:max` - Maximum delay between attempts in ms (default: 60_000)
    - `:multiplier` - Exponential backoff multiplier (default: 2)

  ## Automatic Reconnection

  When the transport closes unexpectedly while the client is connected, all
  pending requests fail with a connection error and the client transitions to
  `:reconnecting`. It then re-establishes the connection (including the MCP
  handshake) with exponential backoff and jitter. After
  `:max_reconnect_attempts` consecutive failures the client gives up and
  settles in `:disconnected`. Requests made while reconnecting return
  `{:error, :not_connected}`.

  Explicit `disconnect/1` or `stop/2` calls never trigger reconnection.
  Passing `reconnect: false` disables the behavior entirely.

  The reconnection lifecycle emits telemetry:

  - `[:ex_mcp, :client, :reconnect, :attempt]` - measurements
    `%{attempt: n, delay_ms: ms}`, emitted when an attempt is scheduled
  - `[:ex_mcp, :client, :reconnect, :success]` - reconnected and re-initialized
  - `[:ex_mcp, :client, :reconnect, :error]` - a single attempt failed
  - `[:ex_mcp, :client, :reconnect, :timeout]` - gave up after the final attempt

  ## Reliability Options

  The `:reliability` option accepts a keyword list with the following options:

  - `:circuit_breaker` - Circuit breaker configuration or `false` to disable
    - `:failure_threshold` - Number of failures before opening (default: 5)
    - `:success_threshold` - Number of successes to close half-open circuit (default: 3)  
    - `:reset_timeout` - Time before transitioning from open to half-open (default: 30_000)
    - `:timeout` - Operation timeout in milliseconds (default: 5_000)
  - `:health_check` - Health check configuration or `false` to disable
    - `:check_interval` - Interval between health checks (default: 60_000)
    - `:failure_threshold` - Health check failures before marking unhealthy (default: 3)
    - `:recovery_threshold` - Health check successes before marking healthy (default: 2)

  ## Reliability Examples

      # Client with circuit breaker protection
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: "my-server",
        reliability: [
          circuit_breaker: [
            failure_threshold: 3,
            reset_timeout: 10_000
          ]
        ]
      )

      # Client with both circuit breaker and health monitoring
      {:ok, client} = ExMCP.Client.start_link(
        transport: :http,
        url: "http://localhost:8080/mcp",
        reliability: [
          circuit_breaker: [failure_threshold: 5],
          health_check: [check_interval: 30_000]
        ]
      )

  ## Retry Policy Options

  The `:retry_policy` option accepts a keyword list with the following options:

  - `:max_attempts` - Maximum number of retry attempts (default: 3)
  - `:initial_delay` - Initial delay between retries in milliseconds (default: 200)
  - `:max_delay` - Maximum delay between retries in milliseconds (default: 5000)
  - `:backoff_factor` - Exponential backoff multiplier (default: 2)
  - `:jitter` - Add random jitter to prevent thundering herd (default: true)

  ## Retry Policy Examples

      # Client with default retry policy for all operations
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: "my-server",
        retry_policy: [
          max_attempts: 5,
          initial_delay: 200
        ]
      )

      # Individual operation with custom retry policy
      {:ok, tools} = ExMCP.Client.list_tools(client, 
        retry_policy: [max_attempts: 2, backoff_factor: 1.5])

      # Operation with no retries (override client default)
      {:ok, result} = ExMCP.Client.call_tool(client, "tool", %{}, 
        retry_policy: false)
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    {name_opts, start_opts} = Keyword.split(opts, [:name])

    case GenServer.start_link(__MODULE__, start_opts, name_opts) do
      {:ok, pid} ->
        {:ok, pid}

      {:error, reason} when is_map(reason) ->
        {:error, reason}

      {:error, {:shutdown, reason}} when is_map(reason) ->
        {:error, reason}

      {:error, {:shutdown, {:transport_connect_failed, details}}} ->
        {:error, {:connection_error, details}}

      {:error, {:shutdown, {:initialize_error, details}}} ->
        {:error, {:initialize_error, details}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Connects to an MCP server using a URL or connection spec.

  ## Examples

      # URL string
      {:ok, client} = ExMCP.Client.connect("http://localhost:8080/mcp")

      # Transport spec
      {:ok, client} = ExMCP.Client.connect({:stdio, command: "mcp-server"})

      # Multiple transports with fallback
      {:ok, client} = ExMCP.Client.connect([
        "http://localhost:8080/mcp",
        "stdio://mcp-server"
      ])

  Returns `{:error, {:invalid_transport_config, reason}}` when the connection
  spec cannot be normalized into a valid transport configuration.
  """
  @spec connect(connection_spec(), keyword()) :: {:ok, t()} | {:error, any()}
  def connect(connection_spec, opts \\ []) do
    transport_opts = do_parse_connection_spec(connection_spec)
    start_link(Keyword.merge(transport_opts, opts))
  catch
    :throw, {:transport_config_error, reason} ->
      {:error, {:invalid_transport_config, reason}}
  end

  @doc """
  Lists available tools from the server.

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Return format (:map or :struct, default: :struct)
  """
  @spec list_tools(t(), keyword() | timeout()) ::
          {:ok, %{String.t() => [map()]}} | {:error, any()}
  def list_tools(client, timeout_or_opts \\ [])

  def list_tools(client, timeout) when is_integer(timeout) do
    list_tools(client, timeout: timeout)
  end

  def list_tools(client, opts) when is_list(opts) do
    {params, opts} = RequestParams.take_cursor(opts)
    make_request(client, "tools/list", params, opts, 5_000)
  end

  @doc """
  Convenience alias for list_tools/2.
  """
  @spec tools(t(), keyword()) :: {:ok, %{String.t() => [map()]}} | {:error, any()}
  def tools(client, opts \\ []), do: Tools.tools(client, opts)

  @doc """
  Calls a tool with the given arguments.

  ## Options

  - `:timeout` - Request timeout (default: 30000)
  - `:format` - Return format (:map or :struct, default: :struct)
  """
  @spec call_tool(t(), String.t(), map(), keyword() | timeout()) ::
          {:ok, any()} | {:error, any()}
  def call_tool(client, tool_name, arguments, timeout_or_opts \\ 30_000)

  def call_tool(client, tool_name, arguments, timeout) when is_integer(timeout) do
    call_tool(client, tool_name, arguments, timeout: timeout)
  end

  def call_tool(client, tool_name, arguments, opts) when is_list(opts) do
    Tools.call_tool(client, tool_name, arguments, opts)
  end

  @doc """
  Sends a batch of requests to the server.

  This function allows sending multiple requests in a single batch, which can
  be more efficient than sending them individually. The server processes the
  requests and returns a batch of responses.

  ## Parameters

  - `client` - Client process reference
  - `requests` - A list of `{method, params}` tuples for each request.
  - `timeout` - Timeout for the entire batch operation (default: 30_000).

  ## Returns

  - `{:ok, results}` - On success, where `results` is a list of `{:ok, result}`
    or `{:error, error}` tuples, in the same order as the original requests.
  - `{:error, reason}` - If the batch request fails (e.g., timeout).

  ## Example

      requests = [
        {"tools/list", %{}},
        {"prompts/list", %{}}
      ]
      {:ok, [tools_result, prompts_result]} = ExMCP.Client.batch_request(client, requests)
  """
  @spec batch_request(t(), [{String.t(), map()}], timeout()) ::
          {:ok, [any()]} | {:error, any()}
  def batch_request(client, requests, timeout \\ 30_000) do
    GenServer.call(client, {:batch_request, requests}, timeout)
  end

  @doc """
  Convenience alias for batch_request/3.

  Sends a batch of JSON-RPC requests. Available in protocol version 2025-03-26 only.
  """
  @spec send_batch(t(), [map()], timeout()) :: {:ok, [any()]} | {:error, any()}
  def send_batch(client, requests, timeout \\ 30_000) do
    batch_request(client, requests, timeout)
  end

  @doc """
  Convenience alias for call_tool/4.
  """
  @spec call(t(), String.t(), map(), keyword()) :: {:ok, any()} | {:error, any()}
  def call(client, tool_name, args \\ %{}, opts \\ []) do
    call_tool(client, tool_name, args, opts)
  end

  @doc """
  Finds a tool by name or pattern.

  ## Options

  - `:fuzzy` - Enable fuzzy matching (default: false)
  - `:timeout` - Request timeout (default: 5000)
  """
  @spec find_tool(t(), String.t() | nil, keyword()) ::
          {:ok, map()} | {:error, :not_found} | {:error, any()}
  def find_tool(client, name_or_pattern \\ nil, opts \\ []) do
    Tools.find_tool(client, name_or_pattern, opts)
  end

  @doc """
  Lists available resources.
  """
  @spec list_resources(t(), keyword() | timeout()) ::
          {:ok, %{String.t() => [map()]}} | {:error, any()}
  def list_resources(client, timeout_or_opts \\ [])

  def list_resources(client, timeout) when is_integer(timeout) do
    list_resources(client, timeout: timeout)
  end

  def list_resources(client, opts) when is_list(opts) do
    {params, opts} = RequestParams.take_cursor(opts)
    make_request(client, "resources/list", params, opts, 5_000)
  end

  @doc """
  Lists available roots.

  Sends a `roots/list` request to the server to retrieve the list of
  available root URIs.
  """
  @spec list_roots(t(), keyword() | timeout()) ::
          {:ok, %{String.t() => [map()]}} | {:error, any()}
  def list_roots(client, timeout_or_opts \\ [])

  def list_roots(client, timeout) when is_integer(timeout) do
    list_roots(client, timeout: timeout)
  end

  def list_roots(client, opts) when is_list(opts) do
    make_request(client, "roots/list", %{}, opts, 5_000)
  end

  @doc """
  Lists available resource templates.

  Sends a `resources/templates/list` request to the server to retrieve the list of
  available resource templates.
  """
  @spec list_resource_templates(t(), keyword() | timeout()) ::
          {:ok, %{String.t() => [map()]}} | {:error, any()}
  def list_resource_templates(client, timeout_or_opts \\ [])

  def list_resource_templates(client, timeout) when is_integer(timeout) do
    list_resource_templates(client, timeout: timeout)
  end

  def list_resource_templates(client, opts) when is_list(opts) do
    {params, opts} = RequestParams.take_cursor(opts)
    make_request(client, "resources/templates/list", params, opts, 5_000)
  end

  @doc """
  Reads a resource by URI.
  """
  @spec read_resource(t(), String.t(), keyword() | timeout()) :: {:ok, any()} | {:error, any()}
  def read_resource(client, uri, timeout_or_opts \\ [])

  def read_resource(client, uri, timeout) when is_integer(timeout) do
    read_resource(client, uri, timeout: timeout)
  end

  def read_resource(client, uri, opts) when is_list(opts) do
    Resources.read_resource(client, uri, opts)
  end

  @doc """
  Subscribes to notifications for a resource.

  Sends a `resources/subscribe` request to receive notifications when the
  specified resource changes. The server will send `notifications/resources/updated`
  messages when the subscribed resource is modified.

  ## Parameters

  - `client` - Client process reference
  - `uri` - Resource URI to subscribe to (e.g., "file:///path/to/file")

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Return format (:map or :struct, default: :struct)

  ## Returns

  - `{:ok, result}` - Subscription successful
  - `{:error, error}` - Subscription failed with error details

  ## Examples

      {:ok, _result} = ExMCP.Client.subscribe_resource(client, "file:///config.json")
  """
  @spec subscribe_resource(t(), String.t(), keyword()) :: {:ok, map()} | {:error, any()}
  def subscribe_resource(client, uri, opts \\ []) do
    Resources.subscribe_resource(client, uri, opts)
  end

  @doc """
  Unsubscribes from notifications for a resource.

  Sends a `resources/unsubscribe` request to stop receiving notifications
  for the specified resource.

  ## Parameters

  - `client` - Client process reference
  - `uri` - Resource URI to unsubscribe from

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Return format (:map or :struct, default: :struct)

  ## Returns

  - `{:ok, result}` - Unsubscription successful
  - `{:error, error}` - Unsubscription failed with error details

  ## Examples

      {:ok, _result} = ExMCP.Client.unsubscribe_resource(client, "file:///config.json")
  """
  @spec unsubscribe_resource(t(), String.t(), keyword()) :: {:ok, map()} | {:error, any()}
  def unsubscribe_resource(client, uri, opts \\ []) do
    Resources.unsubscribe_resource(client, uri, opts)
  end

  @doc """
  Lists available prompts.
  """
  @spec list_prompts(t(), keyword() | timeout()) ::
          {:ok, %{String.t() => [map()]}} | {:error, any()}
  def list_prompts(client, timeout_or_opts \\ [])

  def list_prompts(client, timeout) when is_integer(timeout) do
    list_prompts(client, timeout: timeout)
  end

  def list_prompts(client, opts) when is_list(opts) do
    Prompts.list_prompts(client, opts)
  end

  @doc """
  Gets a prompt with the given arguments.
  """
  @spec get_prompt(t(), String.t(), map(), keyword() | timeout()) ::
          {:ok, any()} | {:error, any()}
  def get_prompt(client, prompt_name, arguments \\ %{}, timeout_or_opts \\ [])

  def get_prompt(client, prompt_name, arguments, timeout) when is_integer(timeout) do
    get_prompt(client, prompt_name, arguments, timeout: timeout)
  end

  def get_prompt(client, prompt_name, arguments, opts) when is_list(opts) do
    Prompts.get_prompt(client, prompt_name, arguments, opts)
  end

  @doc """
  Gets the client status.
  """
  @spec get_status(t()) :: {:ok, map()}
  def get_status(client) do
    GenServer.call(client, :get_status)
  end

  @doc """
  Gets the list of pending request IDs.

  Returns a list of request IDs for requests that are currently in progress.
  This can be used with `send_cancelled/3` to cancel specific requests.

  ## Examples

      {:ok, client} = ExMCP.Client.connect("http://localhost:8080/mcp")
      
      # Start a long-running request
      task = Task.async(fn -> 
        ExMCP.Client.call_tool(client, "slow_tool", %{})
      end)
      
      # Get pending requests  
      pending = ExMCP.Client.get_pending_requests(client)
      # => ["req_123", "req_456"]
      
      # Cancel a specific request
      ExMCP.Client.send_cancelled(client, "req_123", "User cancelled")
  """
  @spec get_pending_requests(t()) :: [ExMCP.Types.request_id()]
  def get_pending_requests(client) do
    GenServer.call(client, :get_pending_requests)
  end

  @doc """
  Gets server information.
  """
  @spec server_info(t()) :: {:ok, map()} | {:error, any()}
  def server_info(client) do
    case get_status(client) do
      {:ok, %{server_info: info}} -> {:ok, info}
      _ -> {:error, :not_connected}
    end
  end

  @doc """
  Gets server capabilities.
  """
  @spec server_capabilities(t()) :: {:ok, map()} | {:error, any()}
  def server_capabilities(client) do
    case get_status(client) do
      {:ok, %{server_capabilities: caps}} -> {:ok, caps}
      _ -> {:error, :not_connected}
    end
  end

  @doc """
  Gets the negotiated protocol version with the server.
  """
  @spec negotiated_version(t()) :: {:ok, String.t()} | {:error, any()}
  def negotiated_version(client) do
    case get_status(client) do
      {:ok, %{protocol_version: version}} -> {:ok, version}
      _ -> {:error, :not_connected}
    end
  end

  @doc """
  Pings the server.
  """
  @spec ping(t(), keyword() | integer()) :: {:ok, map()} | {:error, any()}
  def ping(client, opts_or_timeout \\ []) do
    # Handle both ping(client, timeout) and ping(client, opts) patterns
    timeout =
      case opts_or_timeout do
        timeout when is_integer(timeout) -> timeout
        opts when is_list(opts) -> Keyword.get(opts, :timeout, 5_000)
      end

    opts = if is_list(opts_or_timeout), do: opts_or_timeout, else: []
    make_request(client, "ping", %{}, opts, timeout)
  end

  @doc """
  Sends a notification to the server.

  Notifications are fire-and-forget messages that don't expect a response.

  ## Parameters

  - `client` - Client process reference
  - `method` - The method name to notify
  - `params` - Parameters for the notification (map)

  ## Returns

  - `:ok` - Notification sent

  ## Examples

      :ok = ExMCP.Client.notify(client, "resource_updated", %{"uri" => "file://test.txt"})
  """
  @spec notify(t(), String.t(), map()) :: :ok
  def notify(client, method, params \\ %{}) do
    GenServer.cast(client, {:notification, method, params})
  end

  @doc """
  Sends a cancellation notification for a pending request.

  This function sends a `notifications/cancelled` message to inform the server
  that a previously-sent request should be cancelled. The server MAY stop
  processing the request if it hasn't completed yet.

  ## Parameters

  - `client` - Client process reference
  - `request_id` - The ID of the request to cancel
  - `reason` - Optional human-readable reason for cancellation

  ## Returns

  - `:ok` - Cancellation notification sent
  - `{:error, :cannot_cancel_initialize}` - Cannot cancel initialize request

  ## Examples

      :ok = ExMCP.Client.send_cancelled(client, "req_123", "User cancelled")
      :ok = ExMCP.Client.send_cancelled(client, 12345, nil)
  """
  @spec send_cancelled(t(), ExMCP.Types.request_id(), String.t() | nil) ::
          :ok | {:error, :cannot_cancel_initialize}
  def send_cancelled(client, request_id, reason \\ nil) do
    case Protocol.encode_cancelled(request_id, reason) do
      {:ok, notification} ->
        # Extract method and params from the notification
        %{"method" => method, "params" => params} = notification
        GenServer.call(client, {:send_cancelled, request_id, method, params})

      {:error, :cannot_cancel_initialize} = error ->
        error
    end
  end

  @doc """
  Disconnects the client gracefully, cleaning up all resources.

  This function performs a clean shutdown by:
  - Closing the transport connection
  - Cancelling health checks
  - Stopping the receiver task
  - Replying to any pending requests with an error

  ## Examples

      {:ok, client} = ExMCP.Client.connect("http://localhost:8080/mcp")
      :ok = ExMCP.Client.disconnect(client)
  """
  @spec disconnect(t()) :: :ok
  def disconnect(client) do
    GenServer.call(client, :disconnect, 10_000)
  end

  @doc """
  Stops the client.
  """
  @spec stop(t(), term()) :: :ok
  def stop(client, reason \\ :normal) do
    GenServer.stop(client, reason)
  end

  # GenServer callbacks

  @impl GenServer
  def init(opts) do
    # Set up process
    Process.flag(:trap_exit, true)

    # Build initial state from options
    state = build_initial_state(opts)

    # Check if we should skip connection (for testing)
    if Keyword.get(opts, :_skip_connect, false) do
      {:ok, %{state | connection_status: :disconnected}}
    else
      # Start connection process
      establish_connection(state, opts)
    end
  end

  # Build initial client state from options
  defp build_initial_state(opts) do
    %__MODULE__{
      transport_opts: opts,
      pending_requests: %{},
      pending_batches: %{},
      cancelled_requests: MapSet.new(),
      health_check_interval: Keyword.get(opts, :health_check_interval, 30_000),
      connection_status: :connecting,
      last_activity: System.system_time(:second),
      reconnect_attempts: 0,
      reconnect_enabled: Keyword.get(opts, :reconnect, true),
      max_reconnect_attempts:
        Keyword.get(opts, :max_reconnect_attempts, @default_max_reconnect_attempts),
      reconnect_backoff: build_reconnect_backoff(opts),
      reconnect_timer: nil,
      manual_disconnect: false,
      client_info: build_client_info(),
      server_capabilities: %{},
      initialized: false,
      default_retry_policy: Keyword.get(opts, :retry_policy, []),
      default_timeout: Keyword.get(opts, :timeout, 5_000),
      async_post_tasks: %{}
    }
  end

  defp build_reconnect_backoff(opts) do
    configured = Keyword.get(opts, :reconnect_backoff, [])

    %{
      initial: Keyword.get(configured, :initial, @default_reconnect_backoff[:initial]),
      max: Keyword.get(configured, :max, @default_reconnect_backoff[:max]),
      multiplier: Keyword.get(configured, :multiplier, @default_reconnect_backoff[:multiplier])
    }
  end

  # Establish connection with the server
  defp establish_connection(state, opts) do
    connection_opts = Keyword.put(opts, :retry_policy, state.default_retry_policy)

    case ConnectionManager.establish_connection(state, connection_opts) do
      {:ok, updated_state} ->
        # Update connection status to ready after successful handshake
        :telemetry.execute(
          [:ex_mcp, :client, :connected],
          %{},
          %{transport: updated_state.transport_mod}
        )

        final_state = %{updated_state | connection_status: :ready, initialized: true}
        {:ok, final_state}

      {:error, reason} ->
        handle_connection_error(reason)
    end
  end

  # Handle connection errors with proper normalization
  defp handle_connection_error(reason) do
    Logger.error("Failed to initialize MCP client: #{inspect(reason)}")
    {:stop, normalize_connection_error(reason)}
  end

  # Normalize various error formats to consistent structure
  defp normalize_connection_error(:handshake_timeout), do: :handshake_timeout

  defp normalize_connection_error(:invalid_request) do
    {:initialize_error, %{"code" => ErrorCodes.invalid_request()}}
  end

  defp normalize_connection_error(:connection_refused) do
    {:transport_connect_failed, :connection_refused}
  end

  defp normalize_connection_error({:transport_error, details}) do
    {:transport_connect_failed, details}
  end

  defp normalize_connection_error({:method_not_found, message}) do
    {:initialize_error, %{"code" => ErrorCodes.method_not_found(), "message" => message}}
  end

  defp normalize_connection_error(error) when is_binary(error) do
    if String.contains?(error, "Handshake failed") do
      {:initialize_error, %{"code" => ErrorCodes.invalid_request()}}
    else
      {:transport_connect_failed, error}
    end
  end

  defp normalize_connection_error(reason) do
    # Handle nested errors and other formats
    normalized = extract_inner_reason(reason)
    {:transport_connect_failed, normalized}
  end

  # Extract inner reason from nested structures
  defp extract_inner_reason(%{"code" => _, "message" => _} = err_map), do: err_map
  defp extract_inner_reason({:error, inner_reason}), do: inner_reason
  defp extract_inner_reason(atom) when is_atom(atom), do: to_string(atom)
  defp extract_inner_reason(other), do: inspect(other)

  @impl GenServer
  def handle_call({:request, method, params}, from, state) do
    # Legacy request shape: the caller enforces its own GenServer.call timeout.
    :telemetry.execute(
      [:ex_mcp, :client, :request, :sent],
      %{},
      %{method: method}
    )

    RequestHandler.handle_request(method, params, from, state)
  end

  def handle_call({:request, method, params, meta}, from, state) when is_map(meta) do
    # Request shape used by make_request/5. Default timeout and retry policy
    # are resolved from this process's own state (single GenServer.call per
    # request); explicit per-call options win and are enforced caller-side.
    :telemetry.execute(
      [:ex_mcp, :client, :request, :sent],
      %{},
      %{method: method}
    )

    RequestHandler.handle_request(method, params, from, state, meta)
  end

  def handle_call(:get_default_retry_policy, _from, state) do
    {:reply, {:ok, state.default_retry_policy}, state}
  end

  def handle_call(:get_default_timeout, _from, state) do
    {:reply, {:ok, state.default_timeout}, state}
  end

  def handle_call({:batch_request, requests}, from, state) do
    RequestHandler.handle_batch_request(requests, from, state)
  end

  def handle_call(:disconnect, _from, state) do
    :telemetry.execute(
      [:ex_mcp, :client, :disconnected],
      %{},
      %{}
    )

    # Cancel health check timer
    if state.health_check_ref do
      Process.cancel_timer(state.health_check_ref)
    end

    # Cancel any scheduled reconnection attempt
    if state.reconnect_timer do
      Process.cancel_timer(state.reconnect_timer)
    end

    # Stop receiver task by killing the process directly
    if state.receiver_task && is_struct(state.receiver_task, Task) do
      if Process.alive?(state.receiver_task.pid) do
        Process.exit(state.receiver_task.pid, :shutdown)
      end
    end

    # Reply to all pending requests with connection error
    connection_error = Error.connection_error("Client disconnected")

    state.pending_requests
    |> Enum.each(fn
      {_id, {from, :single}} ->
        GenServer.reply(from, {:error, connection_error})

      {_id, {pid, ref}} when is_pid(pid) and is_reference(ref) ->
        # Handle simple {pid, ref} tuples from older test code
        # Use consistent error format
        GenServer.reply({pid, ref}, {:error, connection_error})

      {_batch_id, {from, :batch, ordered_ids, received_responses}}
      when is_map(received_responses) ->
        # For batch requests, we need to handle them specially. The reply is
        # wrapped in {:ok, responses} to match the batch_request/3 contract;
        # each element is an individual {:ok, _} | {:error, _} result.
        missing_responses =
          ordered_ids
          |> Enum.reject(&Map.has_key?(received_responses, &1))
          |> Enum.map(fn id -> {id, {:error, connection_error}} end)
          |> Map.new()

        all_responses = Map.merge(received_responses, missing_responses)
        ordered_responses = Enum.map(ordered_ids, &Map.get(all_responses, &1))
        GenServer.reply(from, {:ok, ordered_responses})

      {_id, batch_id} when is_binary(batch_id) ->
        # This is a request that's part of a batch
        :ok
    end)

    # Close transport connection
    if state.transport_mod && state.transport_state do
      try do
        state.transport_mod.close(state.transport_state)
      rescue
        # Ignore errors during cleanup
        _ -> :ok
      end
    end

    # Update state to disconnected. The manual_disconnect flag ensures a
    # late {:transport_closed, _} message does not trigger auto-reconnection.
    new_state = %{
      state
      | connection_status: :disconnected,
        pending_requests: %{},
        pending_batches: %{},
        cancelled_requests: MapSet.new(),
        receiver_task: nil,
        health_check_ref: nil,
        reconnect_timer: nil,
        manual_disconnect: true,
        async_post_tasks: %{}
    }

    {:reply, :ok, new_state}
  end

  def handle_call(:get_status, _from, state) do
    status = %{
      connection_status: state.connection_status,
      server_info: state.server_info,
      server_capabilities: state.server_capabilities,
      protocol_version: state.protocol_version,
      transport: state.transport_mod,
      reconnect_attempts: state.reconnect_attempts,
      last_activity: state.last_activity,
      pending_requests: map_size(state.pending_requests)
    }

    {:reply, {:ok, status}, state}
  end

  def handle_call(:get_pending_requests, _from, state) do
    # Return list of pending request IDs from the state
    pending_ids = Map.keys(state.pending_requests)
    {:reply, pending_ids, state}
  end

  def handle_call({:send_cancelled, request_id, method, params}, _from, state) do
    # Track the cancelled request
    updated_state = %{
      state
      | cancelled_requests: MapSet.put(state.cancelled_requests, request_id)
    }

    # Send the cancellation notification
    RequestHandler.handle_cast_notification(method, params, updated_state)

    # Check if this request is still pending and complete it with :cancelled error
    case Map.get(state.pending_requests, request_id) do
      nil ->
        # Request already completed or doesn't exist
        {:reply, :ok, updated_state}

      {from, :single} ->
        # Reply with cancelled error and remove from pending
        GenServer.reply(from, {:error, :cancelled})
        new_pending = Map.delete(state.pending_requests, request_id)
        {:reply, :ok, %{updated_state | pending_requests: new_pending}}

      _ ->
        # Other types of requests (batch, etc.) - just track as cancelled
        {:reply, :ok, updated_state}
    end
  end

  @impl GenServer
  def handle_cast({:notification, method, params}, state) do
    RequestHandler.handle_cast_notification(method, params, state)
  end

  @impl GenServer
  def handle_info({:transport_message, message}, state) do
    RequestHandler.parse_transport_message(message, state)
  end

  # Async POST result — the HTTP transport spawns a monitored task for POST
  # requests in SSE mode to avoid blocking the GenServer during bidirectional
  # flows. `meta` carries the request id the task served plus the durable
  # transport-state fields the POST changed (session rotation, OAuth token
  # refresh), which are merged back into our copy of the transport state.
  def handle_info({:async_post_result, result, meta}, state) when is_map(meta) do
    state = merge_async_transport_state(state, meta)
    handle_async_post_result(result, Map.get(meta, :request_id), state)
  end

  # Legacy 2-tuple shape (no metadata) kept for compatibility.
  def handle_info({:async_post_result, result}, state) do
    handle_async_post_result(result, nil, state)
  end

  # Async POST task registration: maps the task's monitor ref to the request
  # id it serves so a crashed task can fail that request.
  def handle_info({:async_post_task, ref, request_id}, state) when is_reference(ref) do
    tasks = Map.put(state.async_post_tasks || %{}, ref, request_id)
    {:noreply, %{state | async_post_tasks: tasks}}
  end

  # Async POST task exited. A :normal exit just clears the bookkeeping (its
  # result was delivered separately); an abnormal exit fails the pending
  # request the task was serving instead of leaving it to hang until timeout.
  def handle_info({:DOWN, ref, :process, _pid, reason}, %{async_post_tasks: tasks} = state)
      when is_map(tasks) and is_map_key(tasks, ref) do
    {request_id, remaining} = Map.pop(tasks, ref)
    state = %{state | async_post_tasks: remaining}

    if reason == :normal do
      {:noreply, state}
    else
      Logger.error("Async POST task exited: #{inspect(reason)}")
      {:noreply, fail_async_post_request(state, request_id, reason)}
    end
  end

  # A server-request handler task (sampling/elicitation/custom) finished.
  def handle_info({:server_request_result, task_pid, outcome}, state)
      when is_pid(task_pid) do
    RequestHandler.handle_server_request_completion(task_pid, outcome, state)
  end

  # A server-request handler task died before delivering a result.
  def handle_info(
        {:DOWN, _ref, :process, pid, reason},
        %{server_request_tasks: tasks} = state
      )
      when is_map(tasks) and is_map_key(tasks, pid) do
    RequestHandler.handle_server_request_down(pid, reason, state)
  end

  # Push model: transport sends pre-parsed messages directly
  def handle_info({:transport_event, message}, state) do
    RequestHandler.parse_transport_message(message, state)
  end

  # Push model: event ID tracking (for SSE resumability)
  def handle_info({:transport_event_id, _event_id}, state) do
    # Event IDs are tracked by the transport internally
    {:noreply, state}
  end

  # Push model: transport error
  def handle_info({:transport_error, reason}, state) do
    Logger.warning("Transport error (push): #{inspect(reason)}")
    {:noreply, state}
  end

  def handle_info(:health_check, state) do
    # Perform health check
    # Note: Health check logic could ping server or check connection status

    # Schedule next health check
    health_check_ref = schedule_health_check(state.health_check_interval)
    {:noreply, %{state | health_check_ref: health_check_ref}}
  end

  # Default-timeout enforcement for requests made without an explicit
  # :timeout option (scheduled by RequestHandler). Stale timers for requests
  # that already completed find no pending entry and are ignored.
  def handle_info({:request_timeout, request_id}, state) do
    case Map.get(state.pending_requests, request_id) do
      {from, :single} ->
        GenServer.reply(from, {:error, :timeout})
        {:noreply, %{state | pending_requests: Map.delete(state.pending_requests, request_id)}}

      _ ->
        {:noreply, state}
    end
  end

  def handle_info({:EXIT, pid, reason}, %{receiver_task: %Task{pid: task_pid}} = state)
      when pid == task_pid do
    Logger.error("Receiver task died: #{inspect(reason)}")
    {:noreply, handle_transport_down({:receiver_task_died, reason}, state)}
  end

  # Push mode: forwarder process died
  def handle_info({:EXIT, _pid, reason}, %{receiver_task: :push} = state)
      when reason != :normal do
    Logger.error("Transport forwarder died: #{inspect(reason)}")
    {:noreply, handle_transport_down({:transport_forwarder_died, reason}, state)}
  end

  def handle_info({:transport_closed, reason}, state) do
    Logger.error("Transport closed: #{inspect(reason)}")
    {:noreply, handle_transport_down(reason, state)}
  end

  def handle_info(:attempt_reconnect, %{connection_status: :reconnecting} = state) do
    {:noreply, attempt_reconnect(%{state | reconnect_timer: nil})}
  end

  def handle_info(:attempt_reconnect, state) do
    # Stale timer (e.g. the user disconnected while a reconnect was scheduled)
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  # Async POST support (HTTP/SSE transport)

  defp handle_async_post_result({:ok, _new_ts, response_data}, _request_id, state) do
    # POST response contains data — parse it as a transport message
    RequestHandler.parse_transport_message(response_data, state)
  end

  defp handle_async_post_result({:ok, _new_ts}, _request_id, state) do
    # POST returned but no inline data — result will come via SSE stream
    {:noreply, state}
  end

  defp handle_async_post_result({:error, reason}, request_id, state) do
    Logger.error("Async POST failed: #{inspect(reason)}")
    {:noreply, fail_async_post_request(state, request_id, reason)}
  end

  # Merge the durable transport-state changes computed by an async POST task
  # (session id rotation, OAuth token/auth state, SSE retry metadata) into the
  # client's current transport state. Only the fields the task actually
  # changed are merged — see ExMCP.Transport.HTTP.async_state_changes/2 — so
  # concurrent updates to unrelated fields are preserved. The merge is skipped
  # when the task is no longer tracked (the transport was torn down or
  # reconnected after the task started), which keeps stale results from an old
  # connection out of the new connection's state.
  defp merge_async_transport_state(state, meta) do
    changes = Map.get(meta, :state_changes) || %{}

    if map_size(changes) > 0 and is_map(state.transport_state) and
         known_async_post_request?(state, Map.get(meta, :request_id)) do
      %{state | transport_state: Map.merge(state.transport_state, changes)}
    else
      state
    end
  end

  defp known_async_post_request?(%{async_post_tasks: tasks}, request_id) when is_map(tasks) do
    Enum.any?(tasks, fn {_ref, id} -> id == request_id end)
  end

  defp known_async_post_request?(_state, _request_id), do: false

  # Fail the pending request an async POST task was serving. Batch members,
  # notifications (nil id), and already-completed requests are left to the
  # normal timeout/cleanup path.
  defp fail_async_post_request(state, request_id, reason) do
    case request_id && Map.get(state.pending_requests, request_id) do
      {from, :single} ->
        GenServer.reply(from, {:error, {:transport_error, reason}})
        %{state | pending_requests: Map.delete(state.pending_requests, request_id)}

      _ ->
        state
    end
  end

  # Transport teardown and reconnection

  # Already reconnecting with an attempt scheduled — nothing left to tear down.
  defp handle_transport_down(
         _reason,
         %{connection_status: :reconnecting, reconnect_timer: timer} = state
       )
       when timer != nil do
    state
  end

  defp handle_transport_down(reason, state) do
    reply_pending_with_close_error(reason, state)

    # Stop any lingering receiver task for the old transport so it cannot
    # deliver stale close events after a successful reconnection
    if is_struct(state.receiver_task, Task) && Process.alive?(state.receiver_task.pid) do
      Process.exit(state.receiver_task.pid, :shutdown)
    end

    previous_status = state.connection_status

    cleared_state = %{
      state
      | connection_status: :disconnected,
        transport_mod: nil,
        transport_state: nil,
        receiver_task: nil,
        pending_requests: %{},
        pending_batches: %{},
        cancelled_requests: MapSet.new(),
        async_post_tasks: %{}
    }

    if reconnect_allowed?(cleared_state, previous_status) do
      schedule_reconnect(cleared_state)
    else
      cleared_state
    end
  end

  # Reply to all pending requests with connection error or cancelled error
  defp reply_pending_with_close_error(reason, state) do
    connection_error = Error.connection_error("Transport closed: #{inspect(reason)}")

    Enum.each(state.pending_requests, fn
      {id, {from, :single}} ->
        GenServer.reply(from, {:error, close_error_for(id, state, connection_error)})

      {id, {pid, ref}} when is_pid(pid) and is_reference(ref) ->
        # Handle simple {pid, ref} tuples from older test code
        GenServer.reply({pid, ref}, {:error, close_error_for(id, state, connection_error)})

      {_batch_id, {from, :batch, ordered_ids, received_responses}}
      when is_map(received_responses) ->
        # For batch requests, we need to handle them specially. The reply is
        # wrapped in {:ok, responses} to match the batch_request/3 contract;
        # each element is an individual {:ok, _} | {:error, _} result.
        missing_responses =
          ordered_ids
          |> Enum.reject(&Map.has_key?(received_responses, &1))
          |> Enum.map(fn id -> {id, {:error, connection_error}} end)
          |> Map.new()

        all_responses = Map.merge(received_responses, missing_responses)
        ordered_responses = Enum.map(ordered_ids, &Map.get(all_responses, &1))
        GenServer.reply(from, {:ok, ordered_responses})

      {_id, batch_id} when is_binary(batch_id) ->
        # This is a request that's part of a batch
        :ok
    end)
  end

  defp close_error_for(id, state, connection_error) do
    if MapSet.member?(state.cancelled_requests, id) do
      # Use proper error map for cancelled requests
      %{
        "code" => ErrorCodes.request_cancelled(),
        "message" => "Request cancelled"
      }
    else
      connection_error
    end
  end

  defp reconnect_allowed?(state, previous_status) do
    state.reconnect_enabled == true and
      state.manual_disconnect != true and
      previous_status == :ready and
      state.reconnect_attempts < state.max_reconnect_attempts
  end

  defp schedule_reconnect(state) do
    attempt = state.reconnect_attempts + 1
    delay = reconnect_delay(state.reconnect_backoff, attempt)
    timer = Process.send_after(self(), :attempt_reconnect, delay)

    :telemetry.execute(
      [:ex_mcp, :client, :reconnect, :attempt],
      %{attempt: attempt, delay_ms: delay},
      %{transport: configured_transport(state), pid: self()}
    )

    Logger.info("Scheduling MCP reconnection attempt #{attempt} in #{delay}ms")

    %{
      state
      | connection_status: :reconnecting,
        reconnect_attempts: attempt,
        reconnect_timer: timer
    }
  end

  defp attempt_reconnect(state) do
    attempt = state.reconnect_attempts

    case ConnectionManager.establish_connection(state, reconnect_opts(state)) do
      {:ok, connected_state} ->
        :telemetry.execute(
          [:ex_mcp, :client, :reconnect, :success],
          %{attempt: attempt},
          %{transport: connected_state.transport_mod, pid: self()}
        )

        :telemetry.execute(
          [:ex_mcp, :client, :connected],
          %{},
          %{transport: connected_state.transport_mod}
        )

        %{
          connected_state
          | connection_status: :ready,
            initialized: true,
            reconnect_attempts: 0
        }

      {:error, reason} ->
        :telemetry.execute(
          [:ex_mcp, :client, :reconnect, :error],
          %{attempt: attempt},
          %{reason: reason, transport: configured_transport(state), pid: self()}
        )

        handle_reconnect_failure(state, reason)
    end
  end

  defp handle_reconnect_failure(state, reason) do
    if state.reconnect_attempts < state.max_reconnect_attempts do
      schedule_reconnect(state)
    else
      :telemetry.execute(
        [:ex_mcp, :client, :reconnect, :timeout],
        %{attempt: state.reconnect_attempts},
        %{
          max_attempts: state.max_reconnect_attempts,
          reason: reason,
          transport: configured_transport(state),
          pid: self()
        }
      )

      Logger.error(
        "Giving up on reconnection after #{state.reconnect_attempts} attempts: " <>
          inspect(reason)
      )

      %{state | connection_status: :disconnected}
    end
  end

  # Each reconnection attempt is a single try; the reconnect scheduler owns
  # retry/backoff, so disable the nested connection retry policy.
  defp reconnect_opts(state) do
    Keyword.put(state.transport_opts, :retry_policy, [])
  end

  defp reconnect_delay(%{initial: initial, max: max, multiplier: multiplier}, attempt) do
    base = min(round(initial * :math.pow(multiplier, attempt - 1)), max)
    add_reconnect_jitter(base)
  end

  # +/-25% jitter (same policy as ExMCP.Reliability.Retry) to avoid
  # synchronized reconnection storms.
  defp add_reconnect_jitter(delay) do
    jitter_range = div(delay, 4)

    if jitter_range > 0 do
      delay + :rand.uniform(jitter_range * 2) - jitter_range
    else
      delay
    end
  end

  defp configured_transport(state) do
    Keyword.get(state.transport_opts, :transport) ||
      Keyword.get(state.transport_opts, :transports)
  end

  # Private functions (some exposed for testing)

  @doc false
  def parse_connection_spec(spec) do
    do_parse_connection_spec(spec)
  catch
    :throw, {:transport_config_error, reason} ->
      {:error, {:invalid_transport_config, reason}}
  end

  @doc false
  def prepare_transport_config(opts), do: ConnectionManager.prepare_transport_config(opts)

  # Delegate to ConnectionManager for consistent transport spec normalization
  defp normalize_transport_spec(transport_spec, opts) do
    case ConnectionManager.prepare_transport_config([transport: transport_spec] ++ opts) do
      {:ok, [transports: [normalized_spec]]} -> normalized_spec
      {:error, reason} -> throw({:transport_config_error, reason})
    end
  end

  defp do_parse_connection_spec(url) when is_binary(url) do
    uri = URI.parse(url)

    case uri.scheme do
      "http" -> [transport: :http, url: url]
      "https" -> [transport: :http, url: url]
      "stdio" -> [transport: :stdio, command: uri.path || uri.host]
      "file" -> [transport: :stdio, command: uri.path]
      _ -> [transport: :http, url: url]
    end
  end

  defp do_parse_connection_spec({transport, opts}) do
    [transport: transport] ++ opts
  end

  defp do_parse_connection_spec(specs) when is_list(specs) do
    transports =
      Enum.map(specs, fn
        url when is_binary(url) ->
          opts = do_parse_connection_spec(url)
          transport_atom = Keyword.fetch!(opts, :transport)
          normalize_transport_spec(transport_atom, opts)

        {transport, opts} ->
          normalize_transport_spec(transport, opts)
      end)

    [transports: transports]
  end

  defp build_client_info do
    VersionInfo.client_info()
  end

  defp schedule_health_check(interval) do
    Process.send_after(self(), :health_check, interval)
  end

  defp format_response(response, :struct, opts) do
    # Use the proper Response.from_raw_response/2 constructor
    response_opts = [
      tool_name: Keyword.get(opts, :tool_name),
      request_id: Keyword.get(opts, :request_id),
      server_info: Keyword.get(opts, :server_info)
    ]

    structured_response = Response.from_raw_response(response, response_opts)
    {:ok, structured_response}
  end

  # Issues a single GenServer.call per request in the common path. Default
  # timeout and retry policy are resolved by the client process from its own
  # state instead of dedicated pre-flight calls:
  #
  # - Explicit :timeout opts are enforced caller-side via the GenServer.call
  #   timeout (explicit opts win).
  # - Without an explicit timeout, the client process schedules its own
  #   default-timeout timer for the request and replies {:error, :timeout};
  #   the call itself waits without a caller-side deadline (the call monitor
  #   still detects a dead client).
  # - The default retry policy is only fetched (one extra call) after a
  #   failed first attempt, so successful requests never pay for it.
  @doc false
  @spec make_request(t(), String.t(), map(), keyword(), pos_integer()) ::
          {:ok, any()} | {:error, any()}
  def make_request(client, method, params, opts, _default_timeout) do
    explicit_timeout = Keyword.get(opts, :timeout)
    call_timeout = explicit_timeout || :infinity

    operation = fn ->
      try do
        GenServer.call(
          client,
          {:request, method, params, %{timeout: explicit_timeout}},
          call_timeout
        )
      catch
        :exit, {:timeout, _} -> {:error, :timeout}
      end
    end

    retry_policy = Keyword.get(opts, :retry_policy, :use_default)

    result = execute_with_retry_policy(operation, client, retry_policy)
    handle_request_result(result, opts)
  end

  defp execute_with_retry_policy(operation, client, retry_policy) do
    case retry_policy do
      :use_default ->
        execute_with_lazy_default_retry(operation, client)

      false ->
        operation.()

      [] ->
        operation.()

      policy when is_list(policy) ->
        Retry.with_retry(operation, Retry.mcp_defaults(policy))
    end
  end

  # First attempt runs without any pre-flight calls; the client's default
  # retry policy is only fetched when that attempt fails.
  defp execute_with_lazy_default_retry(operation, client) do
    case operation.() do
      {:error, reason} = error ->
        retry_remaining_attempts(operation, client, reason, error)

      result ->
        result
    end
  end

  defp retry_remaining_attempts(operation, client, reason, original_error) do
    case fetch_default_retry_policy(client) do
      [] ->
        original_error

      policy ->
        retry_opts = Retry.mcp_defaults(policy)
        should_retry? = Keyword.fetch!(retry_opts, :should_retry?)
        max_attempts = Keyword.get(retry_opts, :max_attempts, 0)

        if max_attempts > 1 and should_retry?.(reason) do
          # The first attempt already ran; honor its backoff delay, then run
          # the remaining attempts through the shared retry infrastructure.
          Process.sleep(Retry.calculate_delay(1, retry_opts))
          Retry.with_retry(operation, Keyword.put(retry_opts, :max_attempts, max_attempts - 1))
        else
          original_error
        end
    end
  end

  defp fetch_default_retry_policy(client) do
    case GenServer.call(client, :get_default_retry_policy, 5_000) do
      {:ok, policy} when is_list(policy) -> policy
      _ -> []
    end
  catch
    :exit, _ -> []
  end

  defp handle_request_result({:ok, response}, opts) do
    case Keyword.get(opts, :format, :struct) do
      :map -> {:ok, response}
      format -> format_response(response, format, opts)
    end
  end

  defp handle_request_result({:error, %{__struct__: mod}} = error, _opts)
       when mod in [
              Error.ProtocolError,
              Error.TransportError,
              Error.ToolError,
              Error.ResourceError,
              Error.ValidationError
            ] do
    # Already an ExMCP.Error struct, return as-is
    error
  end

  defp handle_request_result({:error, error_data}, opts) when is_map(error_data) do
    case Keyword.get(opts, :format, :struct) do
      :map ->
        # Return error data as map when format is :map
        {:error, error_data}

      _ ->
        # Convert JSON-RPC errors to ProtocolError for client responses
        code = Map.get(error_data, "code")
        message = Map.get(error_data, "message", "Unknown error")
        data = Map.get(error_data, "data")

        # For JSON-RPC standard errors, return ProtocolError
        error_struct =
          if code && code >= -32768 && code <= -32000 do
            %Error.ProtocolError{
              code: code,
              message: message,
              data: data
            }
          else
            # For non-standard errors, use the helper function for compatibility
            Error.from_json_rpc_error(error_data, request_id: Keyword.get(opts, :request_id))
          end

        {:error, error_struct}
    end
  end

  defp handle_request_result({:error, :not_connected}, _opts) do
    # Preserve :not_connected atom for backward compatibility
    {:error, :not_connected}
  end

  defp handle_request_result({:error, :timeout}, opts) do
    case Keyword.get(opts, :format, :struct) do
      :map ->
        # Return timeout as atom when format is :map
        {:error, :timeout}

      _ ->
        # Convert timeout to proper ExMCP.Error
        {:error,
         %Error.ProtocolError{
           code: -32603,
           message: "Request timeout",
           data: nil
         }}
    end
  end

  defp handle_request_result(error, _opts), do: error

  @doc """
  Requests completion suggestions from the server.

  Sends a `completion/complete` request to get completion suggestions based on
  a reference (prompt or resource) and partial input.

  ## Parameters

  - `client` - Client process reference
  - `ref` - Reference map describing what to complete:
    - For prompts: `%{"type" => "ref/prompt", "name" => "prompt_name"}`
    - For resources: `%{"type" => "ref/resource", "uri" => "resource_uri"}`
  - `argument` - Argument map with completion context:
    - `%{"name" => "argument_name", "value" => "partial_value"}`

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Return format (:map or :struct, default: :struct)

  ## Returns

  - `{:ok, result}` - Success with completion suggestions:
    ```
    %{
      completion: %{
        values: ["suggestion1", "suggestion2", ...],
        total: 10,
        hasMore: false
      }
    }
    ```
  - `{:error, error}` - Request failed with error details

  ## Examples

      # Complete prompt argument
      {:ok, result} = ExMCP.Client.complete(
        client,
        %{"type" => "ref/prompt", "name" => "code_generator"},
        %{"name" => "language", "value" => "java"}
      )

      # Complete resource URI
      {:ok, result} = ExMCP.Client.complete(
        client,
        %{"type" => "ref/resource", "uri" => "file:///"},
        %{"name" => "path", "value" => "/src"}
      )
  """
  @spec complete(t(), map(), map(), keyword()) :: {:ok, map()} | {:error, any()}
  def complete(client, ref, argument, opts \\ []) do
    params =
      ref
      |> RequestParams.completion(argument)
      |> RequestParams.with_opts_meta(opts)

    make_request(client, "completion/complete", params, opts, 5_000)
  end

  @doc """
  Sets the log level for the server.

  Sends a `logging/setLevel` request to configure the server's log verbosity.
  This is part of the MCP specification for controlling server logging behavior.

  ## Parameters

  - `client` - Client process reference
  - `level` - Log level string: "debug", "info", "warning", or "error"

  ## Returns

  - `{:ok, result}` - Success with any server response data
  - `{:error, error}` - Request failed with error details

  ## Example

      {:ok, client} = ExMCP.Client.start_link(transport: :http, url: "...")
      {:ok, _} = ExMCP.Client.set_log_level(client, "debug")
  """
  @spec set_log_level(GenServer.server(), String.t()) :: {:ok, map()} | {:error, any()}
  def set_log_level(client, level) when is_binary(level) do
    params = %{"level" => level}

    case make_request(client, "logging/setLevel", params, [], 30_000) do
      {:ok, response} -> {:ok, response}
      error -> error
    end
  end

  @doc """
  Sends a log message to the server as a notification.

  This function sends log messages from the client to the server for centralized
  logging and monitoring. The message is sent as a notification (fire-and-forget)
  following the MCP specification.

  ## Parameters

  - `client` - Client process reference
  - `level` - Log level string (e.g., "debug", "info", "warning", "error")
  - `message` - Log message text

  ## Returns

  - `:ok` - Message sent successfully
  - `{:error, reason}` - Failed to send message

  ## Example

      {:ok, client} = ExMCP.Client.start_link(transport: :http, url: "...")
      :ok = ExMCP.Client.log_message(client, "info", "Operation completed")
  """
  @spec log_message(t(), String.t(), String.t()) :: :ok | {:error, any()}
  def log_message(client, level, message) when is_binary(level) and is_binary(message) do
    log_message(client, level, message, nil)
  end

  @doc """
  Sends a log message with additional data to the server as a notification.

  This function sends detailed log messages from the client to the server for
  centralized logging and monitoring. The message is sent as a notification
  (fire-and-forget) following the MCP specification.

  ## Parameters

  - `client` - Client process reference
  - `level` - Log level string (e.g., "debug", "info", "warning", "error")
  - `message` - Log message text
  - `data` - Optional additional data (map or any JSON-serializable value)

  ## Supported Log Levels

  Standard RFC 5424 levels: "debug", "info", "notice", "warning", "error",
  "critical", "alert", "emergency"

  ## Returns

  - `:ok` - Message sent successfully
  - `{:error, reason}` - Failed to send message

  ## Examples

      {:ok, client} = ExMCP.Client.start_link(transport: :http, url: "...")

      # Simple log message
      :ok = ExMCP.Client.log_message(client, "info", "User logged in")

      # Log message with additional context
      :ok = ExMCP.Client.log_message(client, "error", "Database connection failed", %{
        host: "db.example.com",
        port: 5432,
        error_code: "CONNECTION_TIMEOUT"
      })
  """
  @spec log_message(t(), String.t(), String.t(), any()) :: :ok | {:error, any()}
  def log_message(client, level, message, data) when is_binary(level) and is_binary(message) do
    GenServer.cast(
      client,
      {:notification, "notifications/message",
       %{
         "level" => level,
         "message" => message,
         "data" => data
       }}
    )
  end

  @doc """
  Finds a matching tool from a list of tools.

  ## Parameters

  - `tools` - List of tool maps
  - `name` - Tool name to find (exact match) or pattern (fuzzy match)
  - `opts` - Options including :fuzzy for fuzzy matching

  ## Examples

      tools = [%{"name" => "calculator"}, %{"name" => "weather"}]
      {:ok, tool} = ExMCP.Client.find_matching_tool(tools, "calculator", [])
      {:ok, tool} = ExMCP.Client.find_matching_tool(tools, "calc", fuzzy: true)
  """
  @spec find_matching_tool(list(map()), String.t() | nil, keyword()) ::
          {:ok, map()} | {:error, :not_found}
  def find_matching_tool(tools, name, opts \\ [])

  def find_matching_tool(tools, nil, _opts) when is_list(tools) do
    case List.first(tools) do
      nil -> {:error, :not_found}
      tool -> {:ok, tool}
    end
  end

  def find_matching_tool(tools, name, opts) when is_list(tools) and is_binary(name) do
    fuzzy? = Keyword.get(opts, :fuzzy, false)

    # Try exact match first
    case Enum.find(tools, fn tool -> tool["name"] == name end) do
      nil when fuzzy? ->
        # Try fuzzy match
        case Enum.find(tools, fn tool -> String.contains?(tool["name"], name) end) do
          nil -> {:error, :not_found}
          tool -> {:ok, tool}
        end

      nil ->
        {:error, :not_found}

      tool ->
        {:ok, tool}
    end
  end
end
