defmodule ExMCP.Authorization.TokenManager do
  @moduledoc """
  Manages OAuth tokens with automatic refresh and expiration tracking.

  This GenServer maintains tokens, monitors their expiration, and automatically
  refreshes them before they expire, following MCP authorization best practices.

  ## Features

  - Automatic token refresh before expiration
  - Thread-safe token storage
  - Configurable refresh window
  - Token rotation support
  """

  use GenServer
  require Logger

  alias ExMCP.Authorization

  # Refresh tokens 5 minutes before expiration by default
  @default_refresh_window 300

  defstruct [
    :access_token,
    :refresh_token,
    :expires_at,
    :token_type,
    :scope,
    :auth_config,
    :refresh_timer,
    :subscribers
  ]

  @type t :: %__MODULE__{
          access_token: String.t() | nil,
          refresh_token: String.t() | nil,
          expires_at: DateTime.t() | nil,
          token_type: String.t(),
          scope: String.t() | nil,
          auth_config: map(),
          refresh_timer: reference() | nil,
          subscribers: MapSet.t(pid())
        }

  # Client API

  @doc """
  Starts a token manager process.

  Options:
  - `:auth_config` - Authorization configuration (client_id, client_secret, token_endpoint, etc.)
  - `:refresh_window` - Seconds before expiration to refresh (default: 300)
  - `:name` - Optional process name
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    {name_opts, init_opts} =
      case Keyword.pop(opts, :name) do
        {nil, opts} -> {[], opts}
        {name, opts} -> {[name: name], opts}
      end

    GenServer.start_link(__MODULE__, init_opts, name_opts)
  end

  @doc """
  Sets a new token in the manager.
  """
  @spec set_token(GenServer.server(), map()) :: :ok
  def set_token(manager, token_response) do
    GenServer.call(manager, {:set_token, token_response})
  end

  @doc """
  Gets the current valid access token.

  Returns `{:ok, token}` or `{:error, reason}` if token is expired/missing.
  """
  @spec get_token(GenServer.server()) :: {:ok, String.t()} | {:error, atom()}
  def get_token(manager) do
    GenServer.call(manager, :get_token)
  end

  @doc """
  Gets full token information including metadata.
  """
  @spec get_token_info(GenServer.server()) :: {:ok, map()} | {:error, atom()}
  def get_token_info(manager) do
    GenServer.call(manager, :get_token_info)
  end

  @doc """
  Forces an immediate token refresh.
  """
  @spec refresh_now(GenServer.server()) :: {:ok, map()} | {:error, any()}
  def refresh_now(manager) do
    GenServer.call(manager, :refresh_now, 30_000)
  end

  @doc """
  Subscribes to token update notifications.

  Subscribers receive `{:token_updated, manager, token_info}` messages.
  """
  @spec subscribe(GenServer.server()) :: :ok
  def subscribe(manager) do
    GenServer.call(manager, {:subscribe, self()})
  end

  @doc """
  Unsubscribes from token update notifications.
  """
  @spec unsubscribe(GenServer.server()) :: :ok
  def unsubscribe(manager) do
    GenServer.call(manager, {:unsubscribe, self()})
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    auth_config = Keyword.fetch!(opts, :auth_config)
    refresh_window = Keyword.get(opts, :refresh_window, @default_refresh_window)

    state = %__MODULE__{
      auth_config: Map.put(auth_config, :refresh_window, refresh_window),
      token_type: "Bearer",
      subscribers: MapSet.new()
    }

    # If initial token provided, set it
    case Keyword.get(opts, :initial_token) do
      nil ->
        {:ok, state}

      token ->
        {:ok, state, {:continue, {:set_token, token}}}
    end
  end

  @impl true
  def handle_continue({:set_token, token}, state) do
    {:noreply, process_token_response(token, state)}
  end

  @impl true
  def handle_call({:set_token, token_response}, _from, state) do
    new_state = process_token_response(token_response, state)
    {:reply, :ok, new_state}
  end

  def handle_call(:get_token, _from, state) do
    case validate_token(state) do
      :valid ->
        {:reply, {:ok, state.access_token}, state}

      :expired ->
        {:reply, {:error, :token_expired}, state}

      :missing ->
        {:reply, {:error, :no_token}, state}
    end
  end

  def handle_call(:get_token_info, _from, state) do
    case validate_token(state) do
      :valid ->
        info = %{
          access_token: state.access_token,
          token_type: state.token_type,
          expires_at: state.expires_at,
          scope: state.scope
        }

        {:reply, {:ok, info}, state}

      status ->
        {:reply, {:error, status}, state}
    end
  end

  def handle_call(:refresh_now, from, state) do
    if state.refresh_token do
      # Spawn refresh task to avoid blocking
      Task.start(fn ->
        result = refresh_token(state)
        GenServer.reply(from, result)
      end)

      {:noreply, state}
    else
      {:reply, {:error, :no_refresh_token}, state}
    end
  end

  def handle_call({:subscribe, pid}, _from, state) do
    Process.monitor(pid)
    new_state = %{state | subscribers: MapSet.put(state.subscribers, pid)}
    {:reply, :ok, new_state}
  end

  def handle_call({:unsubscribe, pid}, _from, state) do
    new_state = %{state | subscribers: MapSet.delete(state.subscribers, pid)}
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_info(:refresh_token, state) do
    case refresh_token(state) do
      {:ok, new_token} ->
        new_state = process_token_response(new_token, state)
        {:noreply, new_state}

      {:error, reason} ->
        Logger.error("Token refresh failed: #{inspect(reason)}")
        # Schedule retry in 30 seconds
        Process.send_after(self(), :refresh_token, 30_000)
        {:noreply, state}
    end
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    new_state = %{state | subscribers: MapSet.delete(state.subscribers, pid)}
    {:noreply, new_state}
  end

  def handle_info({:token_refreshed, new_token}, state) do
    new_state = process_token_response(new_token, state)
    {:noreply, new_state}
  end

  # Private functions

  defp process_token_response(token_response, state) do
    # Cancel existing timer
    if state.refresh_timer do
      Process.cancel_timer(state.refresh_timer)
    end

    # Calculate expiration
    expires_at = calculate_expiration(token_response)

    # Schedule refresh
    refresh_timer = schedule_refresh(expires_at, state.auth_config.refresh_window)

    new_state = %{
      state
      | access_token: token_response["access_token"],
        refresh_token: token_response["refresh_token"] || state.refresh_token,
        expires_at: expires_at,
        scope: token_response["scope"] || state.scope,
        refresh_timer: refresh_timer
    }

    # Notify subscribers
    notify_subscribers(new_state)

    new_state
  end

  defp calculate_expiration(%{"expires_in" => expires_in}) when is_integer(expires_in) do
    DateTime.utc_now()
    |> DateTime.add(expires_in, :second)
  end

  defp calculate_expiration(_) do
    # Default to 1 hour if not specified
    DateTime.utc_now()
    |> DateTime.add(3600, :second)
  end

  defp schedule_refresh(expires_at, refresh_window) do
    now = DateTime.utc_now()
    diff = DateTime.diff(expires_at, now, :second)

    # Schedule refresh before expiration
    refresh_in = max(diff - refresh_window, 1)

    Logger.debug("Scheduling token refresh in #{refresh_in} seconds")
    Process.send_after(self(), :refresh_token, refresh_in * 1000)
  end

  defp validate_token(%{access_token: nil}), do: :missing
  # No expiration info
  defp validate_token(%{expires_at: nil}), do: :valid

  defp validate_token(%{expires_at: expires_at}) do
    if DateTime.compare(DateTime.utc_now(), expires_at) == :lt do
      :valid
    else
      :expired
    end
  end

  defp refresh_token(state) do
    Logger.info("Refreshing access token")

    config =
      Map.merge(state.auth_config, %{
        grant_type: "refresh_token",
        refresh_token: state.refresh_token
      })

    Authorization.token_request(config)
  end

  defp notify_subscribers(state) do
    token_info = %{
      access_token: state.access_token,
      token_type: state.token_type,
      expires_at: state.expires_at,
      scope: state.scope
    }

    Enum.each(state.subscribers, fn pid ->
      send(pid, {:token_updated, self(), token_info})
    end)
  end

  @impl true
  def terminate(_reason, state) do
    if state.refresh_timer do
      Process.cancel_timer(state.refresh_timer)
    end

    :ok
  end
end
