defmodule ExMCP.Client.LegacyAdapter do
  @moduledoc """
  Adapter that wraps the existing GenServer-based ExMCP.Client implementation.

  This adapter provides a consistent interface while delegating to the
  original implementation. It's used as the default adapter to maintain
  backward compatibility.
  """

  @behaviour ExMCP.Client.Adapter

  # Connection Management

  @impl true
  def start_link(config, opts \\ []) when is_map(config) do
    # Convert map config to keyword list for legacy client
    config_kw = Enum.to_list(config) ++ opts
    ExMCP.Client.start_link(config_kw)
  end

  @impl true
  def connect(_client) do
    # Legacy client doesn't have a separate connect step
    :ok
  end

  @impl true
  def disconnect(_client) do
    # Legacy client doesn't have a separate disconnect step
    :ok
  end

  @impl true
  def stop(client) do
    GenServer.stop(client)
  end

  # Request Operations

  @impl true
  def call(client, method, params, opts \\ []) do
    ExMCP.Client.call(client, method, params, opts)
  end

  @impl true
  def notify(client, method, params) do
    ExMCP.Client.notify(client, method, params)
  end

  @impl true
  def batch_request(client, requests, opts \\ []) do
    timeout = opts[:timeout] || 5000
    ExMCP.Client.batch_request(client, requests, timeout)
  end

  @impl true
  def complete(client, request, token) do
    # Legacy client doesn't have progress completion - this is for progress notifications
    # Stub implementation since legacy client doesn't support this
    params = Map.put(request, "progressToken", token)
    _result = notify(client, "notifications/progress", params)
    {:ok, %{}}
  end

  # Tool Operations

  @impl true
  def list_tools(client, opts \\ []) do
    ExMCP.Client.list_tools(client, opts)
  end

  @impl true
  def call_tool(client, name, arguments, opts \\ []) do
    ExMCP.Client.call_tool(client, name, arguments, opts)
  end

  @impl true
  def find_tool(client, name) do
    ExMCP.Client.find_tool(client, name)
  end

  @impl true
  def find_matching_tool(client, pattern) do
    ExMCP.Client.find_matching_tool(client, pattern)
  end

  # Resource Operations

  @impl true
  def list_resources(client, opts \\ []) do
    ExMCP.Client.list_resources(client, opts)
  end

  @impl true
  def read_resource(client, uri, opts \\ []) do
    ExMCP.Client.read_resource(client, uri, opts)
  end

  @impl true
  def list_resource_templates(client, opts \\ []) do
    ExMCP.Client.list_resource_templates(client, opts)
  end

  @impl true
  def subscribe_resource(client, uri, opts \\ []) do
    ExMCP.Client.subscribe_resource(client, uri, opts)
  end

  @impl true
  def unsubscribe_resource(client, uri, opts \\ []) do
    ExMCP.Client.unsubscribe_resource(client, uri, opts)
  end

  # Prompt Operations

  @impl true
  def list_prompts(client, opts \\ []) do
    ExMCP.Client.list_prompts(client, opts)
  end

  @impl true
  def get_prompt(client, name, arguments, opts \\ []) do
    ExMCP.Client.get_prompt(client, name, arguments, opts)
  end

  # Logging Operations

  @impl true
  def set_log_level(client, level, _opts \\ []) do
    # Legacy client doesn't take opts for set_log_level
    ExMCP.Client.set_log_level(client, level)
  end

  @impl true
  def log_message(client, level, message, data \\ nil, _opts \\ []) do
    # Legacy client has different signatures for log_message
    case data do
      nil -> ExMCP.Client.log_message(client, level, message)
      _ -> ExMCP.Client.log_message(client, level, message, data)
    end
  end

  # Server Information

  @impl true
  def ping(client, opts \\ []) do
    ExMCP.Client.ping(client, opts)
  end

  @impl true
  def server_info(client) do
    case ExMCP.Client.server_info(client) do
      {:ok, info} -> info
      _ -> nil
    end
  end

  @impl true
  def server_capabilities(client) do
    case ExMCP.Client.server_capabilities(client) do
      {:ok, caps} -> caps
      _ -> %{}
    end
  end

  @impl true
  def negotiated_version(client) do
    case ExMCP.Client.negotiated_version(client) do
      {:ok, version} -> version
      _ -> nil
    end
  end

  @impl true
  def list_roots(client, opts \\ []) do
    ExMCP.Client.list_roots(client, opts)
  end

  # Client State

  @impl true
  def get_status(_client) do
    # Legacy client status - simplified
    %{connected: true, state: :ready}
  end

  @impl true
  def get_pending_requests(_client) do
    # Legacy client doesn't track pending requests the same way
    %{pending: []}
  end

  # Legacy compatibility

  @impl true
  def make_request(client, method, params, opts \\ []) do
    # Legacy client expects 5 args for make_request
    default_timeout = opts[:timeout] || 5000
    ExMCP.Client.make_request(client, method, params, opts, default_timeout)
  end

  @impl true
  def send_batch(client, requests) do
    ExMCP.Client.send_batch(client, requests)
  end

  @impl true
  def send_cancelled(client, request_id) do
    ExMCP.Client.send_cancelled(client, request_id)
  end

  @impl true
  def tools(client) do
    ExMCP.Client.tools(client)
  end
end
