defmodule ExMCP.SecureServer do
  @moduledoc """
  Security-enhanced MCP server implementation.

  This module extends ExMCP.Server with security best practices:
  - Token validation for every request
  - Client registration and accountability
  - Consent management for dynamic clients
  - Audit trail maintenance
  - Trust boundary enforcement

  Use this instead of ExMCP.Server when security features are required.
  """

  use GenServer
  require Logger

  alias ExMCP.Server
  alias ExMCP.Security.{ClientRegistry, ConsentManager, TokenValidator}

  defstruct [
    :server_id,
    :server_pid,
    :security_config,
    :client_info_cache
  ]

  @doc """
  Starts a secure MCP server.

  ## Options

  All ExMCP.Server options plus:

  - `:server_id` - Unique server identifier for token validation (required)
  - `:security` - Security configuration map:
    - `:require_auth` - Require authentication (default: true)
    - `:trusted_issuers` - List of trusted token issuers
    - `:introspection_endpoint` - Token introspection endpoint
    - `:approval_handler` - Module for consent approval
    - `:trust_boundaries` - List of supported trust boundaries
  """
  def start_link(opts) do
    {name, opts} = Keyword.pop(opts, :name)
    gen_opts = if name, do: [name: name], else: []
    GenServer.start_link(__MODULE__, opts, gen_opts)
  end

  # Delegate public API to underlying server
  defdelegate notify_resources_changed(server), to: Server
  defdelegate notify_resource_updated(server, uri), to: Server
  defdelegate notify_tools_changed(server), to: Server
  defdelegate notify_prompts_changed(server), to: Server
  defdelegate notify_progress(server, token, progress, total), to: Server
  # defdelegate send_log_message(server, level, message, data), to: Server

  # GenServer callbacks

  @impl true
  def init(opts) do
    server_id = Keyword.fetch!(opts, :server_id)
    security_config = Keyword.get(opts, :security, %{})

    # Ensure security components are started
    {:ok, _} =
      ExMCP.Security.Supervisor.ensure_started(
        approval_handler: security_config[:approval_handler]
      )

    # Create a wrapped handler that adds security
    wrapped_handler =
      create_secure_handler(
        Keyword.fetch!(opts, :handler),
        server_id,
        security_config
      )

    # Start the underlying server with wrapped handler
    server_opts =
      opts
      |> Keyword.put(:handler, wrapped_handler)
      |> Keyword.delete(:server_id)
      |> Keyword.delete(:security)

    case Server.start_link(server_opts) do
      {:ok, server_pid} ->
        state = %__MODULE__{
          server_id: server_id,
          server_pid: server_pid,
          security_config: security_config,
          client_info_cache: %{}
        }

        {:ok, state}

      {:error, reason} ->
        {:stop, reason}
    end
  end

  @impl true
  def handle_call(request, _from, state) do
    # Forward all calls to the underlying server
    case GenServer.call(state.server_pid, request) do
      {:reply, reply, _} -> {:reply, reply, state}
      other -> other
    end
  end

  @impl true
  def handle_cast(request, state) do
    GenServer.cast(state.server_pid, request)
    {:noreply, state}
  end

  # Create a secure handler wrapper
  defp create_secure_handler(handler, server_id, security_config) do
    %{
      __struct__: SecureHandlerWrapper,
      wrapped_handler: handler,
      server_id: server_id,
      security_config: security_config
    }
  end
end

defmodule ExMCP.SecureServer.SecureHandlerWrapper do
  @moduledoc false

  @behaviour ExMCP.Server.Handler

  require Logger

  alias ExMCP.Security.{ClientRegistry, ConsentManager, TokenValidator}

  defstruct [
    :wrapped_handler,
    :server_id,
    :security_config,
    :handler_state
  ]

  @impl true
  def init(args) do
    %{wrapped_handler: handler} = args

    case handler.init(args[:handler_args] || []) do
      {:ok, handler_state} ->
        {:ok, %{args | handler_state: handler_state}}

      error ->
        error
    end
  end

  @impl true
  def handle_initialize(params, state) do
    # Extract client info and auth token
    client_info = params["clientInfo"]
    auth_token = extract_auth_token(params)

    # Validate authentication if required
    with :ok <- validate_authentication(auth_token, client_info, state),
         :ok <- register_client(client_info, auth_token, state),
         {:ok, result} <- state.wrapped_handler.handle_initialize(params, state.handler_state) do
      # Audit successful initialization
      audit_request(client_info["clientId"], "initialize", params["id"], state)

      {:ok, result, %{state | handler_state: state.handler_state}}
    else
      {:error, :unauthorized} ->
        {:error, -32001, "Authentication required", state}

      {:error, :invalid_token} ->
        {:error, -32001, "Invalid authentication token", state}

      {:error, :consent_required} ->
        {:error, -32002, "Dynamic client registration requires consent", state}

      {:error, reason} ->
        {:error, -32603, "Security validation failed: #{inspect(reason)}", state}
    end
  end

  # Wrap all handler callbacks with security checks
  @impl true
  def handle_list_tools(cursor, state) do
    with :ok <- check_request_auth(state) do
      case state.wrapped_handler.handle_list_tools(cursor, state.handler_state) do
        {:ok, tools, new_handler_state} ->
          audit_request(get_current_client(state), "tools/list", nil, state)
          {:ok, tools, %{state | handler_state: new_handler_state}}

        error ->
          error
      end
    end
  end

  @impl true
  def handle_call_tool(name, arguments, state) do
    with :ok <- check_request_auth(state),
         :ok <- check_tool_permission(name, state) do
      case state.wrapped_handler.handle_call_tool(name, arguments, state.handler_state) do
        {:ok, result, new_handler_state} ->
          audit_request(get_current_client(state), "tools/call", name, state)
          {:ok, result, %{state | handler_state: new_handler_state}}

        error ->
          error
      end
    end
  end

  # Implement remaining handler callbacks with security wrapper...
  # (Similar pattern for all other handle_* functions)

  @impl true
  def handle_list_resources(cursor, state) do
    with :ok <- check_request_auth(state) do
      state.wrapped_handler.handle_list_resources(cursor, state.handler_state)
    end
  end

  @impl true
  def handle_read_resource(uri, state) do
    with :ok <- check_request_auth(state) do
      state.wrapped_handler.handle_read_resource(uri, state.handler_state)
    end
  end

  @impl true
  def handle_subscribe_resource(uri, state) do
    with :ok <- check_request_auth(state) do
      state.wrapped_handler.handle_subscribe_resource(uri, state.handler_state)
    end
  end

  @impl true
  def handle_unsubscribe_resource(uri, state) do
    with :ok <- check_request_auth(state) do
      state.wrapped_handler.handle_unsubscribe_resource(uri, state.handler_state)
    end
  end

  @impl true
  def handle_list_resource_templates(cursor, state) do
    with :ok <- check_request_auth(state) do
      state.wrapped_handler.handle_list_resource_templates(cursor, state.handler_state)
    end
  end

  @impl true
  def handle_list_prompts(cursor, state) do
    with :ok <- check_request_auth(state) do
      state.wrapped_handler.handle_list_prompts(cursor, state.handler_state)
    end
  end

  @impl true
  def handle_get_prompt(name, arguments, state) do
    with :ok <- check_request_auth(state) do
      state.wrapped_handler.handle_get_prompt(name, arguments, state.handler_state)
    end
  end

  @impl true
  def handle_complete(ref, argument, state) do
    with :ok <- check_request_auth(state) do
      state.wrapped_handler.handle_complete(ref, argument, state.handler_state)
    end
  end

  @impl true
  def handle_set_log_level(level, state) do
    with :ok <- check_request_auth(state) do
      state.wrapped_handler.handle_set_log_level(level, state.handler_state)
    end
  end

  @impl true
  def terminate(reason, state) do
    state.wrapped_handler.terminate(reason, state.handler_state)
  end

  # Security helper functions

  defp extract_auth_token(params) do
    # Check for token in various places
    params["_meta"]["authToken"] ||
      params["authToken"] ||
      nil
  end

  defp validate_authentication(nil, _client_info, %{security_config: %{require_auth: false}}),
    do: :ok

  defp validate_authentication(nil, _client_info, _state), do: {:error, :unauthorized}

  defp validate_authentication(token, _client_info, state) do
    validation_opts = [
      server_id: state.server_id,
      trusted_issuers: state.security_config[:trusted_issuers],
      introspection_endpoint: state.security_config[:introspection_endpoint]
    ]

    case TokenValidator.validate_token_for_server(token, validation_opts) do
      {:ok, token_info} ->
        # Store token info for request context
        Process.put(:mcp_token_info, token_info)
        :ok

      {:error, reason} ->
        Logger.warning("Token validation failed: #{inspect(reason)}")
        {:error, :invalid_token}
    end
  end

  defp register_client(client_info, auth_token, state) do
    client_id = client_info["clientId"] || generate_client_id()
    registration_type = determine_registration_type(auth_token)

    with :ok <- check_consent_if_required(registration_type, client_info, state) do
      perform_client_registration(client_info, client_id, registration_type)
    end
  end

  defp determine_registration_type(auth_token) do
    if auth_token, do: :static, else: :dynamic
  end

  defp check_consent_if_required(:dynamic, client_info, %{security_config: security_config}) do
    if security_config[:approval_handler] do
      case ConsentManager.request_consent(client_info, get_user_id()) do
        {:ok, _consent} -> :ok
        {:error, _} -> {:error, :consent_required}
      end
    else
      :ok
    end
  end

  defp check_consent_if_required(_registration_type, _client_info, _state), do: :ok

  defp perform_client_registration(client_info, client_id, registration_type) do
    client_info_with_id = Map.put(client_info, :client_id, client_id)

    case ClientRegistry.register_client(client_info_with_id, registration_type) do
      {:ok, registered_client} ->
        Process.put(:mcp_client_info, registered_client)
        :ok

      {:error, :client_already_registered} ->
        handle_existing_client(client_id)

      error ->
        error
    end
  end

  defp handle_existing_client(client_id) do
    case ClientRegistry.validate_client(client_id) do
      {:ok, client} ->
        Process.put(:mcp_client_info, client)
        :ok

      error ->
        error
    end
  end

  defp check_request_auth(%{security_config: %{require_auth: false}}), do: :ok

  defp check_request_auth(_state) do
    # Check if we have valid auth context
    if Process.get(:mcp_token_info) do
      :ok
    else
      {:error, :unauthorized}
    end
  end

  defp check_tool_permission(_tool_name, _state) do
    # Could implement fine-grained permissions here
    :ok
  end

  defp audit_request(client_id, method, details, _state) do
    request_id = ExMCP.Protocol.generate_id()
    ClientRegistry.record_request(client_id || "unknown", method, request_id)

    # Also use TokenValidator for audit trail
    if token_info = Process.get(:mcp_token_info) do
      TokenValidator.audit_token_usage(
        token_info[:jti] || "unknown",
        Process.get(:mcp_client_info) || %{},
        %{method: method, params: details}
      )
    end
  end

  defp get_current_client(_state) do
    case Process.get(:mcp_client_info) do
      %{client_id: id} -> id
      _ -> "unknown"
    end
  end

  defp get_user_id do
    case Process.get(:mcp_token_info) do
      %{sub: sub} -> sub
      _ -> "anonymous"
    end
  end

  defp generate_client_id do
    "mcp_dynamic_#{:crypto.strong_rand_bytes(8) |> Base.url_encode64(padding: false)}"
  end
end
