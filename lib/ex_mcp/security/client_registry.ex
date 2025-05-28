defmodule ExMCP.Security.ClientRegistry do
  @moduledoc """
  Secure client registry for MCP server accountability.

  Implements security best practices for:
  - Client identification and distinction
  - Request traceability
  - Audit trail maintenance
  - Trust boundary enforcement

  This ensures MCP servers can properly identify clients and maintain
  accurate security audit trails as required by the specification.
  """

  use GenServer
  require Logger

  defstruct [
    :clients,
    :request_log,
    :trust_boundaries,
    :options
  ]

  @type client_info :: %{
          client_id: String.t(),
          name: String.t(),
          version: String.t(),
          registered_at: DateTime.t(),
          registration_type: :static | :dynamic,
          trust_boundary: String.t() | nil,
          metadata: map()
        }

  @type request_record :: %{
          client_id: String.t(),
          method: String.t(),
          timestamp: DateTime.t(),
          request_id: String.t(),
          trust_boundary: String.t() | nil
        }

  # Client API

  @doc """
  Starts the client registry.

  Options:
  - `:max_request_log` - Maximum request log entries (default: 10_000)
  - `:persist_clients` - Whether to persist client registrations
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Registers a client with the MCP server.

  Registration types:
  - `:static` - Pre-configured clients (no consent needed)
  - `:dynamic` - Runtime registered clients (require consent)
  """
  @spec register_client(map(), :static | :dynamic) ::
          {:ok, client_info()} | {:error, atom()}
  def register_client(client_data, registration_type) do
    GenServer.call(__MODULE__, {:register_client, client_data, registration_type})
  end

  @doc """
  Validates a client's identity for a request.

  Ensures the client is registered and authorized for the trust boundary.
  """
  @spec validate_client(String.t(), String.t() | nil) ::
          {:ok, client_info()} | {:error, atom()}
  def validate_client(client_id, trust_boundary \\ nil) do
    GenServer.call(__MODULE__, {:validate_client, client_id, trust_boundary})
  end

  @doc """
  Records a client request for audit trail.

  Maintains request traceability as required by security best practices.
  """
  @spec record_request(String.t(), String.t(), String.t()) :: :ok
  def record_request(client_id, method, request_id) do
    GenServer.cast(__MODULE__, {:record_request, client_id, method, request_id})
  end

  @doc """
  Gets audit trail for a specific client.
  """
  @spec get_client_audit_trail(String.t(), keyword()) :: [request_record()]
  def get_client_audit_trail(client_id, opts \\ []) do
    GenServer.call(__MODULE__, {:get_audit_trail, client_id, opts})
  end

  @doc """
  Lists all registered clients.
  """
  @spec list_clients(keyword()) :: [client_info()]
  def list_clients(opts \\ []) do
    GenServer.call(__MODULE__, {:list_clients, opts})
  end

  @doc """
  Revokes a client registration.
  """
  @spec revoke_client(String.t(), String.t()) :: :ok
  def revoke_client(client_id, reason) do
    GenServer.call(__MODULE__, {:revoke_client, client_id, reason})
  end

  @doc """
  Sets trust boundary for a client.

  Used to enforce token audience separation and prevent cross-service token reuse.
  """
  @spec set_client_trust_boundary(String.t(), String.t()) :: :ok | {:error, atom()}
  def set_client_trust_boundary(client_id, trust_boundary) do
    GenServer.call(__MODULE__, {:set_trust_boundary, client_id, trust_boundary})
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    state = %__MODULE__{
      clients: %{},
      request_log: :queue.new(),
      trust_boundaries: %{},
      options: opts
    }

    # Load persisted clients if configured
    state = maybe_load_persisted_clients(state)

    {:ok, state}
  end

  @impl true
  def handle_call({:register_client, client_data, registration_type}, _from, state) do
    client_id = client_data[:client_id] || generate_client_id()

    if Map.has_key?(state.clients, client_id) do
      {:reply, {:error, :client_already_registered}, state}
    else
      client_info = %{
        client_id: client_id,
        name: client_data[:name] || "Unknown Client",
        version: client_data[:version] || "0.0.0",
        registered_at: DateTime.utc_now(),
        registration_type: registration_type,
        trust_boundary: client_data[:trust_boundary],
        metadata: Map.drop(client_data, [:client_id, :name, :version, :trust_boundary])
      }

      new_state = %{state | clients: Map.put(state.clients, client_id, client_info)}

      # Persist if configured
      maybe_persist_client(client_info, state.options)

      # Audit registration
      Logger.info("Client registered: #{client_id} (#{registration_type})",
        tag: :client_registry,
        client_id: client_id,
        registration_type: registration_type
      )

      {:reply, {:ok, client_info}, new_state}
    end
  end

  def handle_call({:validate_client, client_id, trust_boundary}, _from, state) do
    case Map.get(state.clients, client_id) do
      nil ->
        {:reply, {:error, :unknown_client}, state}

      client_info ->
        # Validate trust boundary if specified
        if trust_boundary && client_info.trust_boundary &&
             client_info.trust_boundary != trust_boundary do
          {:reply, {:error, :trust_boundary_violation}, state}
        else
          {:reply, {:ok, client_info}, state}
        end
    end
  end

  def handle_call({:get_audit_trail, client_id, opts}, _from, state) do
    limit = Keyword.get(opts, :limit, 100)

    # Filter request log for this client
    requests =
      :queue.to_list(state.request_log)
      |> Enum.filter(fn record -> record.client_id == client_id end)
      # Most recent
      |> Enum.take(-limit)

    {:reply, requests, state}
  end

  def handle_call({:list_clients, opts}, _from, state) do
    clients = Map.values(state.clients)

    # Apply filters if provided
    clients =
      clients
      |> maybe_filter_by_type(opts[:registration_type])
      |> maybe_filter_by_boundary(opts[:trust_boundary])

    {:reply, clients, state}
  end

  def handle_call({:revoke_client, client_id, reason}, _from, state) do
    case Map.pop(state.clients, client_id) do
      {nil, _} ->
        {:reply, {:error, :client_not_found}, state}

      {_client_info, remaining_clients} ->
        # Audit revocation
        Logger.warning("Client revoked: #{client_id} - #{reason}",
          tag: :client_revocation,
          client_id: client_id,
          reason: reason
        )

        # Remove from persistence if configured
        maybe_remove_persisted_client(client_id, state.options)

        new_state = %{state | clients: remaining_clients}
        {:reply, :ok, new_state}
    end
  end

  def handle_call({:set_trust_boundary, client_id, trust_boundary}, _from, state) do
    case Map.get(state.clients, client_id) do
      nil ->
        {:reply, {:error, :unknown_client}, state}

      client_info ->
        updated_client = %{client_info | trust_boundary: trust_boundary}
        new_clients = Map.put(state.clients, client_id, updated_client)

        {:reply, :ok, %{state | clients: new_clients}}
    end
  end

  @impl true
  def handle_cast({:record_request, client_id, method, request_id}, state) do
    record = %{
      client_id: client_id,
      method: method,
      timestamp: DateTime.utc_now(),
      request_id: request_id,
      trust_boundary: get_client_boundary(state.clients, client_id)
    }

    # Add to request log with size limit
    max_log = Keyword.get(state.options, :max_request_log, 10_000)
    new_log = add_to_bounded_queue(state.request_log, record, max_log)

    {:noreply, %{state | request_log: new_log}}
  end

  # Private functions

  defp generate_client_id do
    "mcp_client_#{:crypto.strong_rand_bytes(16) |> Base.url_encode64(padding: false)}"
  end

  defp get_client_boundary(clients, client_id) do
    case Map.get(clients, client_id) do
      nil -> nil
      client_info -> client_info.trust_boundary
    end
  end

  defp add_to_bounded_queue(queue, item, max_size) do
    new_queue = :queue.in(item, queue)

    if :queue.len(new_queue) > max_size do
      {_, trimmed} = :queue.out(new_queue)
      trimmed
    else
      new_queue
    end
  end

  defp maybe_filter_by_type(clients, nil), do: clients

  defp maybe_filter_by_type(clients, type) do
    Enum.filter(clients, fn client -> client.registration_type == type end)
  end

  defp maybe_filter_by_boundary(clients, nil), do: clients

  defp maybe_filter_by_boundary(clients, boundary) do
    Enum.filter(clients, fn client -> client.trust_boundary == boundary end)
  end

  defp maybe_load_persisted_clients(state) do
    if Keyword.get(state.options, :persist_clients, false) do
      # In production, load from persistent storage
      Logger.info("Client persistence not implemented, starting with empty registry")
    end

    state
  end

  defp maybe_persist_client(client_info, opts) do
    if Keyword.get(opts, :persist_clients, false) do
      # In production, save to persistent storage
      Logger.debug("Would persist client: #{client_info.client_id}")
    end
  end

  defp maybe_remove_persisted_client(client_id, opts) do
    if Keyword.get(opts, :persist_clients, false) do
      # In production, remove from persistent storage
      Logger.debug("Would remove persisted client: #{client_id}")
    end
  end
end
