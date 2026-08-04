defmodule ExMCP.Server.CancellationTracker do
  @moduledoc """
  Strategy for propagating `notifications/cancelled` into handler state.

  `ExMCP.Server.HandlerServer` marks the request cancelled in its own state
  and then hands the notification to a tracker module so that servers can
  observe cancellation the way their handler is written. The module is chosen
  at start-up with the `:cancellation_tracker` option:

      ExMCP.Server.HandlerServer.start_link(
        handler: MyHandler,
        cancellation_tracker: MyApp.CancellationTracker
      )

  This keeps `HandlerServer` free of handler-shape-specific branches: it just
  calls the configured module.

  The default implementation is `ExMCP.Server.CancellationTracker.Default`.
  """

  @doc """
  Records `request_id` as cancelled and returns the (possibly updated) handler
  state.

  Implementations run inside the server process, so they must not block.
  """
  @callback mark_cancelled(request_id :: term(), handler_state :: term()) ::
              handler_state :: term()

  defmodule Default do
    @moduledoc """
    Default cancellation tracker.

    Supports the two conventions ExMCP handlers use to observe cancellation:

    * a `:cancelled_requests` `MapSet` in the handler state, which is updated
      in place, and
    * an `:active_requests` map of `request_id => pid`, whose worker process is
      sent `{:cancelled, request_id}` so long-running work can stop early.

    Handler states that use neither are returned untouched.
    """

    @behaviour ExMCP.Server.CancellationTracker

    require Logger

    @impl true
    def mark_cancelled(request_id, handler_state) when is_map(handler_state) do
      handler_state
      |> update_cancelled_requests(request_id)
      |> notify_worker(request_id)
    end

    def mark_cancelled(_request_id, handler_state), do: handler_state

    defp update_cancelled_requests(%{cancelled_requests: set} = handler_state, request_id) do
      %{handler_state | cancelled_requests: MapSet.put(set, request_id)}
    end

    defp update_cancelled_requests(handler_state, _request_id), do: handler_state

    defp notify_worker(%{active_requests: active} = handler_state, request_id) do
      case Map.get(active, request_id) do
        pid when is_pid(pid) ->
          send(pid, {:cancelled, request_id})
          handler_state

        _other ->
          handler_state
      end
    end

    defp notify_worker(handler_state, _request_id), do: handler_state
  end
end
