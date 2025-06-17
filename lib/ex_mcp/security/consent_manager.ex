defmodule ExMCP.Security.ConsentManager do
  @moduledoc """
  Manages user consent for MCP security operations.

  Implements the MCP security requirement:
  "MCP proxy servers... MUST obtain user consent for each dynamically
  registered client before forwarding to third-party authorization servers"

  This module provides:
  - Consent tracking and persistence
  - Revocation management
  - Audit trail for consent decisions
  """

  use GenServer
  require Logger

  defstruct [
    :consent_store,
    :approval_handler,
    :audit_log,
    :options
  ]

  @type consent_record :: %{
          client_id: String.t(),
          user_id: String.t(),
          granted_at: DateTime.t(),
          expires_at: DateTime.t() | nil,
          scopes: [String.t()],
          revoked: boolean()
        }

  # Client API

  @doc """
  Starts the consent manager.

  Options:
  - `:approval_handler` - Module implementing ExMCP.Approval behaviour
  - `:consent_ttl` - How long consent is valid (seconds, default: 30 days)
  - `:store_backend` - Storage backend (:ets or :persistent)
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Checks if user has consented to a dynamic client registration.

  Returns:
  - `{:ok, :valid}` - Valid consent exists
  - `{:error, :no_consent}` - No consent record found
  - `{:error, :expired}` - Consent has expired
  - `{:error, :revoked}` - Consent was revoked
  """
  @spec check_consent(String.t(), String.t(), [String.t()]) ::
          {:ok, :valid} | {:error, atom()}
  def check_consent(client_id, user_id, requested_scopes) do
    GenServer.call(__MODULE__, {:check_consent, client_id, user_id, requested_scopes})
  end

  @doc """
  Requests user consent for dynamic client registration.

  This implements the MCP requirement for user consent before
  forwarding to third-party authorization servers.
  """
  @spec request_consent(map(), String.t()) ::
          {:ok, consent_record()} | {:error, atom()}
  def request_consent(client_metadata, user_id) do
    GenServer.call(__MODULE__, {:request_consent, client_metadata, user_id})
  end

  @doc """
  Revokes consent for a client.
  """
  @spec revoke_consent(String.t(), String.t()) :: :ok
  def revoke_consent(client_id, user_id) do
    GenServer.call(__MODULE__, {:revoke_consent, client_id, user_id})
  end

  @doc """
  Lists all active consents for a user.
  """
  @spec list_user_consents(String.t()) :: [consent_record()]
  def list_user_consents(user_id) do
    GenServer.call(__MODULE__, {:list_consents, user_id})
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    approval_handler = Keyword.get(opts, :approval_handler)
    store_backend = Keyword.get(opts, :store_backend, :ets)

    consent_store = init_store(store_backend)

    state = %__MODULE__{
      consent_store: consent_store,
      approval_handler: approval_handler,
      audit_log: [],
      options: opts
    }

    # Schedule periodic cleanup of expired consents
    schedule_cleanup()

    {:ok, state}
  end

  @impl true
  def handle_call({:check_consent, client_id, user_id, requested_scopes}, _from, state) do
    result =
      case lookup_consent(state.consent_store, client_id, user_id) do
        nil ->
          {:error, :no_consent}

        consent ->
          validate_consent(consent, requested_scopes)
      end

    {:reply, result, state}
  end

  def handle_call({:request_consent, client_metadata, user_id}, _from, state) do
    # Check if valid consent already exists
    case lookup_consent(state.consent_store, client_metadata.client_id, user_id) do
      nil ->
        # Request new consent
        handle_new_consent_request(client_metadata, user_id, state)

      existing ->
        case validate_consent(existing, client_metadata.scope || []) do
          {:ok, :valid} ->
            {:reply, {:ok, existing}, state}

          _ ->
            # Existing consent is invalid, request new
            handle_new_consent_request(client_metadata, user_id, state)
        end
    end
  end

  def handle_call({:revoke_consent, client_id, user_id}, _from, state) do
    revoke_consent_record(state.consent_store, client_id, user_id)

    # Audit the revocation
    audit_entry = %{
      action: :consent_revoked,
      client_id: client_id,
      user_id: user_id,
      timestamp: DateTime.utc_now()
    }

    new_state = add_audit_log(state, audit_entry)
    {:reply, :ok, new_state}
  end

  def handle_call({:list_consents, user_id}, _from, state) do
    consents = list_active_consents(state.consent_store, user_id)
    {:reply, consents, state}
  end

  @impl true
  def handle_info(:cleanup_expired, state) do
    cleanup_expired_consents(state.consent_store)
    schedule_cleanup()
    {:noreply, state}
  end

  # Private functions

  defp init_store(:ets) do
    :ets.new(:mcp_consent_store, [:set, :private])
  end

  defp init_store(:persistent) do
    # In production, use a persistent store like DETS or database
    Logger.warning("Persistent consent store not implemented, using ETS")
    init_store(:ets)
  end

  defp handle_new_consent_request(client_metadata, user_id, state) do
    if state.approval_handler do
      approval_data = %{
        client_id: client_metadata[:client_id],
        client_name: client_metadata[:client_name] || "Unknown Client",
        client_uri: client_metadata[:client_uri],
        scopes: client_metadata[:scope] || [],
        redirect_uris: client_metadata[:redirect_uris] || [],
        user_id: user_id
      }

      case state.approval_handler.request_approval(
             :dynamic_client_consent,
             approval_data,
             []
           ) do
        {:approved, approved_data} ->
          consent =
            create_consent_record(
              client_metadata.client_id,
              user_id,
              approved_data[:scopes] || client_metadata.scope || [],
              state.options
            )

          store_consent(state.consent_store, consent)

          # Audit the approval
          audit_entry = %{
            action: :consent_granted,
            client_id: client_metadata.client_id,
            user_id: user_id,
            scopes: consent.scopes,
            timestamp: DateTime.utc_now()
          }

          new_state = add_audit_log(state, audit_entry)
          {:reply, {:ok, consent}, new_state}

        {:denied, reason} ->
          # Audit the denial
          audit_entry = %{
            action: :consent_denied,
            client_id: client_metadata.client_id,
            user_id: user_id,
            reason: reason,
            timestamp: DateTime.utc_now()
          }

          new_state = add_audit_log(state, audit_entry)
          {:reply, {:error, :consent_denied}, new_state}

        {:modified, modified_data} ->
          # Handle modified consent (reduced scopes, etc.)
          consent =
            create_consent_record(
              client_metadata.client_id,
              user_id,
              modified_data[:scopes] || [],
              state.options
            )

          store_consent(state.consent_store, consent)
          {:reply, {:ok, consent}, state}
      end
    else
      {:reply, {:error, :no_approval_handler}, state}
    end
  end

  defp create_consent_record(client_id, user_id, scopes, opts) do
    # 30 days default
    ttl = Keyword.get(opts, :consent_ttl, 30 * 24 * 60 * 60)

    %{
      client_id: client_id,
      user_id: user_id,
      granted_at: DateTime.utc_now(),
      expires_at: DateTime.add(DateTime.utc_now(), ttl, :second),
      scopes: scopes,
      revoked: false
    }
  end

  defp validate_consent(consent, requested_scopes) do
    cond do
      consent.revoked ->
        {:error, :revoked}

      consent.expires_at && DateTime.compare(DateTime.utc_now(), consent.expires_at) == :gt ->
        {:error, :expired}

      !scopes_satisfied?(consent.scopes, requested_scopes) ->
        {:error, :insufficient_scope}

      true ->
        {:ok, :valid}
    end
  end

  defp scopes_satisfied?(granted_scopes, requested_scopes) do
    MapSet.subset?(
      MapSet.new(requested_scopes),
      MapSet.new(granted_scopes)
    )
  end

  defp lookup_consent(store, client_id, user_id) do
    key = {client_id, user_id}

    case :ets.lookup(store, key) do
      [{^key, consent}] -> consent
      [] -> nil
    end
  end

  defp store_consent(store, consent) do
    key = {consent.client_id, consent.user_id}
    :ets.insert(store, {key, consent})
  end

  defp revoke_consent_record(store, client_id, user_id) do
    case lookup_consent(store, client_id, user_id) do
      nil ->
        :ok

      consent ->
        revoked = %{consent | revoked: true}
        store_consent(store, revoked)
    end
  end

  defp list_active_consents(store, user_id) do
    :ets.foldl(
      fn {{_client_id, uid}, consent}, acc ->
        if uid == user_id && !consent.revoked do
          [consent | acc]
        else
          acc
        end
      end,
      [],
      store
    )
  end

  defp cleanup_expired_consents(store) do
    now = DateTime.utc_now()

    :ets.foldl(
      fn {key, consent}, acc ->
        if consent.expires_at && DateTime.compare(now, consent.expires_at) == :gt do
          :ets.delete(store, key)
        end

        acc
      end,
      :ok,
      store
    )
  end

  defp schedule_cleanup do
    # Run cleanup every hour
    Process.send_after(self(), :cleanup_expired, 60 * 60 * 1000)
  end

  defp add_audit_log(state, entry) do
    Logger.info("Consent audit: #{inspect(entry)}", tag: :consent_audit)

    # Keep last 1000 audit entries in memory
    audit_log = [entry | state.audit_log] |> Enum.take(1000)

    %{state | audit_log: audit_log}
  end
end
