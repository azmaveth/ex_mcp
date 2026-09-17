defmodule ExMCP.Client.NotificationListener do
  @moduledoc """
  Local delivery of legacy-era MCP server notifications.

  MCP revisions `2024-11-05` through `2025-11-25` deliver
  `notifications/tools/list_changed`, `notifications/prompts/list_changed`,
  `notifications/resources/list_changed`, and `notifications/resources/updated`
  on the connection itself. Nothing on the wire correlates them with a request,
  so the client decides locally which process wants them. A listener is that
  decision: `ExMCP.Client.subscribe_notifications/3` registers a filter for a
  subscriber process, and the client forwards every matching notification to
  it as

      {:ex_mcp_notification, listener, method, params}

  This is the legacy counterpart of `ExMCP.Client.listen/3`, not a substitute
  for it. Both accept the same filter vocabulary, but the guarantees differ:

  | | `listen/3` (MCP 2026-07-28) | `subscribe_notifications/3` (legacy) |
  |---|---|---|
  | Wire request | `subscriptions/listen`, acknowledged by the server | None for list changes; `resources/subscribe` per URI |
  | Correlation | Server-supplied subscription id on every event | None; the client matches on method and URI |
  | Reconnect | Re-opened by the subscription process with a resync snapshot | Resource URIs re-subscribed; no snapshot |
  | Task events | `taskIds` filter | Not available |
  | Owner | An `ExMCP.Client.Subscription` process | State inside the client |

  A listener can only be registered on a legacy peer. On a modern peer
  `ExMCP.Client.subscribe_notifications/3` returns `{:error, :use_listen}`.

  ## Filter

  The filter uses the notification-filter keys of MCP 2026-07-28 so hosts can
  share one vocabulary across eras:

    * `"toolsListChanged"`, `"promptsListChanged"`, `"resourcesListChanged"`
      (booleans) enable the matching `list_changed` notification;
    * `"resourceSubscriptions"` (a list of URIs) enables
      `notifications/resources/updated` for those URIs only.

  The requested filter is authoritative. A `resources/updated` notification for
  a URI that is not listed is never delivered, even if the server sends it,
  and a disabled `list_changed` category is dropped. `"taskIds"` is rejected
  because legacy peers have no task notifications.

  ## Resource subscriptions

  Every URI in `"resourceSubscriptions"` needs a server-side subscription. The
  client sends `resources/subscribe` once per URI, shared across all listeners
  that name it, and `resources/unsubscribe` when the last listener naming it
  goes away. A failed `resources/subscribe` fails the whole registration with
  `{:error, {:subscribe_failed, uri, reason}}` and rolls back any subscription
  the call had already made.

  Direct calls to `ExMCP.Client.subscribe_resource/3` and
  `ExMCP.Client.unsubscribe_resource/3` are not refcounted with listeners. The
  server keeps one subscription per URI per session, so an explicit
  `unsubscribe_resource/3` also silences a listener that names the same URI.

  ## Lifecycle

  The client monitors the subscriber. When the subscriber exits, the listener
  is removed and any URI no longer named by another listener is unsubscribed.
  `ExMCP.Client.unsubscribe_notifications/2` does the same explicitly.

  When the transport closes and the client reconnects, the listener survives.
  After the client has re-initialized, it re-sends `resources/subscribe` for
  every listened URI and then tells each subscriber what happened:

      {:ex_mcp_notification_reconnected, listener, %{resubscribed: uris, failed: [{uri, reason}]}}

  Delivery pauses between the transport loss and that message. A listener is
  closed, and its subscriber receives

      {:ex_mcp_notification_closed, listener, reason}

  when the client gives up reconnecting (`{:reconnect_exhausted, reason}`),
  when the transport closes with reconnection disabled
  (`{:transport_closed, reason}`), when the peer turns out to be modern after
  a reconnect (`{:era_changed, :modern}`), on `ExMCP.Client.disconnect/1`
  (`:disconnected`), and when the client process stops normally
  (`{:shutdown, reason}`). A client that crashes cannot send that message;
  subscribers that need to notice should monitor `listener.client`.
  """

  alias ExMCP.Internal.RequestParams
  alias ExMCP.SubscriptionFilter

  defmodule Ref do
    @moduledoc "A reference to a registered legacy notification listener."

    @enforce_keys [:id, :client, :subscriber, :filter]
    defstruct [:id, :client, :subscriber, :filter]

    @type t :: %__MODULE__{
            id: reference(),
            client: pid(),
            subscriber: pid(),
            filter: map()
          }
  end

  @typedoc "Messages a subscriber receives for a listener."
  @type message ::
          {:ex_mcp_notification, Ref.t(), String.t(), map()}
          | {:ex_mcp_notification_reconnected, Ref.t(),
             %{resubscribed: [String.t()], failed: [{String.t(), term()}]}}
          | {:ex_mcp_notification_closed, Ref.t(), term()}

  @typedoc "Client-side registry: listener id => entry."
  @type registry :: %{reference() => %{ref: Ref.t(), monitor: reference()}}

  @default_timeout 5_000
  @category_keys ["toolsListChanged", "promptsListChanged", "resourcesListChanged"]

  @doc """
  Registers a listener on `client`. See `ExMCP.Client.subscribe_notifications/3`.
  """
  @spec subscribe(GenServer.server(), map(), keyword()) :: {:ok, Ref.t()} | {:error, term()}
  def subscribe(client, filter, opts \\ []) when is_list(opts) do
    subscriber = Keyword.get(opts, :subscriber, self())
    timeout = Keyword.get(opts, :timeout, @default_timeout)

    with {:ok, normalized} <- normalize_filter(filter),
         {:ok, ref, new_uris} <-
           GenServer.call(client, {:register_notification_listener, normalized, subscriber}) do
      case subscribe_uris(client, new_uris, timeout) do
        :ok ->
          {:ok, ref}

        {:error, uri, reason} ->
          _ = unsubscribe(ref, timeout: timeout)
          {:error, {:subscribe_failed, uri, reason}}
      end
    end
  end

  @doc """
  Removes a listener. See `ExMCP.Client.unsubscribe_notifications/2`.
  """
  @spec unsubscribe(Ref.t(), keyword()) :: :ok | {:error, :not_found}
  def unsubscribe(%Ref{} = ref, opts \\ []) when is_list(opts) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)

    case GenServer.call(ref.client, {:deregister_notification_listener, ref.id}) do
      {:ok, released_uris} ->
        unsubscribe_uris(ref.client, released_uris, timeout)
        :ok

      {:error, :not_found} = error ->
        error
    end
  catch
    # A client that is already gone has no listener left to remove.
    :exit, _reason -> :ok
  end

  ## Registry helpers used inside the client process.

  @doc false
  @spec register(registry(), Ref.t(), reference()) :: {registry(), [String.t()]}
  def register(listeners, %Ref{} = ref, monitor) when is_map(listeners) do
    new_uris = uris_of(ref.filter) -- uris(listeners)
    {Map.put(listeners, ref.id, %{ref: ref, monitor: monitor}), new_uris}
  end

  @doc false
  @spec deregister(registry(), reference()) ::
          {:ok, %{ref: Ref.t(), monitor: reference()}, registry(), [String.t()]} | :not_found
  def deregister(listeners, id) when is_map(listeners) do
    case Map.pop(listeners, id) do
      {nil, _listeners} -> :not_found
      {entry, rest} -> {:ok, entry, rest, uris_of(entry.ref.filter) -- uris(rest)}
    end
  end

  @doc false
  @spec uris(registry()) :: [String.t()]
  def uris(listeners) when is_map(listeners) do
    listeners
    |> Map.values()
    |> Enum.flat_map(&uris_of(&1.ref.filter))
    |> Enum.uniq()
    |> Enum.sort()
  end

  @doc false
  @spec dispatch(registry(), String.t(), map()) :: non_neg_integer()
  def dispatch(listeners, method, params) when is_map(listeners) do
    Enum.count(listeners, fn {_id, %{ref: ref}} ->
      if SubscriptionFilter.event_allowed?(method, params, ref.filter) do
        send(ref.subscriber, {:ex_mcp_notification, ref, method, params})
        true
      else
        false
      end
    end)
  end

  @doc false
  @spec notify_reconnected(registry(), %{String.t() => :ok | {:error, term()}}) :: :ok
  def notify_reconnected(listeners, results) when is_map(listeners) and is_map(results) do
    Enum.each(listeners, fn {_id, %{ref: ref}} ->
      {resubscribed, failed} =
        Enum.reduce(uris_of(ref.filter), {[], []}, fn uri, {ok, bad} ->
          case Map.get(results, uri, {:error, :not_attempted}) do
            :ok -> {[uri | ok], bad}
            {:error, reason} -> {ok, [{uri, reason} | bad]}
          end
        end)

      send(
        ref.subscriber,
        {:ex_mcp_notification_reconnected, ref,
         %{resubscribed: Enum.reverse(resubscribed), failed: Enum.reverse(failed)}}
      )
    end)
  end

  @doc false
  @spec close_all(registry(), term()) :: :ok
  def close_all(listeners, reason) when is_map(listeners) do
    Enum.each(listeners, fn {_id, %{ref: ref, monitor: monitor}} ->
      Process.demonitor(monitor, [:flush])
      send(ref.subscriber, {:ex_mcp_notification_closed, ref, reason})
    end)
  end

  ## Wire helpers. These run in the caller or in a task, never in the client.

  @doc false
  @spec subscribe_uris(GenServer.server(), [String.t()], timeout()) ::
          :ok | {:error, String.t(), term()}
  def subscribe_uris(client, uris, timeout) do
    Enum.reduce_while(uris, :ok, fn uri, :ok ->
      case request(client, "resources/subscribe", uri, timeout) do
        {:ok, _result} -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, uri, reason}}
      end
    end)
  end

  @doc false
  @spec unsubscribe_uris(GenServer.server(), [String.t()], timeout()) :: :ok
  def unsubscribe_uris(client, uris, timeout) do
    Enum.each(uris, fn uri -> _ = request(client, "resources/unsubscribe", uri, timeout) end)
  end

  @doc false
  @spec resubscribe(GenServer.server(), [String.t()], timeout()) ::
          %{String.t() => :ok | {:error, term()}}
  def resubscribe(client, uris, timeout) do
    Map.new(uris, fn uri ->
      case request(client, "resources/subscribe", uri, timeout) do
        {:ok, _result} -> {uri, :ok}
        {:error, reason} -> {uri, {:error, reason}}
      end
    end)
  end

  defp request(client, method, uri, timeout) do
    ExMCP.Client.make_request(
      client,
      method,
      RequestParams.uri(uri),
      [timeout: timeout, format: :map],
      timeout
    )
  catch
    :exit, reason -> {:error, {:exit, reason}}
  end

  defp normalize_filter(filter) do
    with {:ok, normalized} <- SubscriptionFilter.normalize(filter) do
      cond do
        Map.has_key?(normalized, "taskIds") ->
          {:error, :task_subscriptions_require_mcp_2026_07_28}

        uris_of(normalized) == [] and not Enum.any?(@category_keys, &(normalized[&1] == true)) ->
          {:error, :empty_subscription_filter}

        true ->
          {:ok, normalized}
      end
    end
  end

  defp uris_of(filter), do: Map.get(filter, "resourceSubscriptions", [])
end
