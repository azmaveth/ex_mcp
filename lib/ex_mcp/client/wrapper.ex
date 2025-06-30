defmodule ExMCP.Client.Wrapper do
  @moduledoc """
  Wrapper module that provides a unified interface to different client implementations.

  This module allows runtime switching between the legacy GenServer implementation
  and the new StateMachine implementation via configuration.

  ## Configuration

      # Use state machine implementation (opt-in)
      config :ex_mcp, :client_adapter, ExMCP.Client.StateMachineAdapter
      
      # Use legacy implementation (default)
      config :ex_mcp, :client_adapter, ExMCP.Client.LegacyAdapter
      
  ## Per-client configuration

      # Force specific implementation
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: "mcp-server",
        adapter: ExMCP.Client.StateMachineAdapter
      )
  """

  @default_adapter ExMCP.Client.LegacyAdapter

  # Store adapter module with client reference
  defmodule State do
    @moduledoc false
    defstruct [:client, :adapter]
  end

  @doc """
  Starts a client with the configured adapter.
  """
  def start_link(config, opts \\ []) do
    # Check for adapter in options, then config, then application env
    adapter = get_adapter(config, opts)

    # Extract GenServer options
    {_gs_opts, adapter_opts} = split_options(opts)

    # Start the underlying client
    case adapter.start_link(config, adapter_opts) do
      {:ok, client} ->
        # Wrap the client reference with adapter info
        wrapped = %State{client: client, adapter: adapter}
        {:ok, wrapped}

      error ->
        error
    end
  end

  @doc """
  Gets the adapter module for a client or from configuration.
  """
  def get_adapter(config, opts) do
    cond do
      # Explicit adapter in opts (non-nil)
      (adapter = opts[:adapter]) && adapter != nil ->
        adapter

      # Adapter in config (non-nil)
      is_map(config) && Map.has_key?(config, :adapter) && config.adapter != nil ->
        config.adapter

      # From application environment (non-nil)
      (adapter = Application.get_env(:ex_mcp, :client_adapter)) && adapter != nil ->
        adapter

      # Default
      true ->
        @default_adapter
    end
  end

  # Delegate all calls to the appropriate adapter

  def connect(%State{client: client, adapter: adapter}) do
    adapter.connect(client)
  end

  def connect(client) when is_pid(client) or is_atom(client) do
    # For raw client PIDs, legacy clients don't have separate connect
    :ok
  end

  def disconnect(%State{client: client, adapter: adapter}) do
    adapter.disconnect(client)
  end

  def disconnect(client) when is_pid(client) or is_atom(client) do
    # For raw client PIDs, legacy clients don't have separate disconnect
    :ok
  end

  def stop(%State{client: client, adapter: adapter}) do
    adapter.stop(client)
  end

  def call(%State{client: client, adapter: adapter}, method, params, opts \\ []) do
    adapter.call(client, method, params, opts)
  end

  def notify(%State{client: client, adapter: adapter}, method, params) do
    adapter.notify(client, method, params)
  end

  def batch_request(%State{client: client, adapter: adapter}, requests, opts \\ []) do
    adapter.batch_request(client, requests, opts)
  end

  def complete(%State{client: client, adapter: adapter}, request, token) do
    adapter.complete(client, request, token)
  end

  def list_tools(%State{client: client, adapter: adapter}, opts \\ []) do
    adapter.list_tools(client, opts)
  end

  def call_tool(%State{client: client, adapter: adapter}, name, arguments, opts \\ []) do
    adapter.call_tool(client, name, arguments, opts)
  end

  def find_tool(%State{client: client, adapter: adapter}, name) do
    adapter.find_tool(client, name)
  end

  def find_matching_tool(%State{client: client, adapter: adapter}, pattern) do
    adapter.find_matching_tool(client, pattern)
  end

  def list_resources(%State{client: client, adapter: adapter}, opts \\ []) do
    adapter.list_resources(client, opts)
  end

  def read_resource(%State{client: client, adapter: adapter}, uri, opts \\ []) do
    adapter.read_resource(client, uri, opts)
  end

  def list_resource_templates(%State{client: client, adapter: adapter}, opts \\ []) do
    adapter.list_resource_templates(client, opts)
  end

  def subscribe_resource(%State{client: client, adapter: adapter}, uri, opts \\ []) do
    adapter.subscribe_resource(client, uri, opts)
  end

  def unsubscribe_resource(%State{client: client, adapter: adapter}, uri, opts \\ []) do
    adapter.unsubscribe_resource(client, uri, opts)
  end

  def list_prompts(%State{client: client, adapter: adapter}, opts \\ []) do
    adapter.list_prompts(client, opts)
  end

  def get_prompt(%State{client: client, adapter: adapter}, name, arguments, opts \\ []) do
    adapter.get_prompt(client, name, arguments, opts)
  end

  def set_log_level(%State{client: client, adapter: adapter}, level, opts \\ []) do
    adapter.set_log_level(client, level, opts)
  end

  def log_message(
        %State{client: client, adapter: adapter},
        level,
        message,
        data \\ nil,
        opts \\ []
      ) do
    adapter.log_message(client, level, message, data, opts)
  end

  def ping(%State{client: client, adapter: adapter}, opts \\ []) do
    adapter.ping(client, opts)
  end

  def server_info(%State{client: client, adapter: adapter}) do
    adapter.server_info(client)
  end

  def server_capabilities(%State{client: client, adapter: adapter}) do
    adapter.server_capabilities(client)
  end

  def negotiated_version(%State{client: client, adapter: adapter}) do
    adapter.negotiated_version(client)
  end

  def list_roots(%State{client: client, adapter: adapter}, opts \\ []) do
    adapter.list_roots(client, opts)
  end

  def get_status(%State{client: client, adapter: adapter}) do
    adapter.get_status(client)
  end

  def get_pending_requests(%State{client: client, adapter: adapter}) do
    adapter.get_pending_requests(client)
  end

  def make_request(%State{client: client, adapter: adapter}, method, params, opts \\ []) do
    adapter.make_request(client, method, params, opts)
  end

  def send_batch(%State{client: client, adapter: adapter}, requests) do
    adapter.send_batch(client, requests)
  end

  def send_cancelled(%State{client: client, adapter: adapter}, request_id) do
    adapter.send_cancelled(client, request_id)
  end

  def tools(%State{client: client, adapter: adapter}) do
    adapter.tools(client)
  end

  # Private functions

  defp split_options(opts) do
    {gs_opts, adapter_opts} = Keyword.split(opts, [:name, :timeout, :debug, :spawn_opt])
    {gs_opts, adapter_opts}
  end
end
