defmodule ExMCP.Security.Consent do
  @moduledoc """
  Manages user consent for various operations.

  This module is responsible for ensuring that user consent has been
  obtained before proceeding with sensitive actions.
  """

  alias ExMCP.Internal.ConsentCache
  alias ExMCP.Security.TokenHandler

  @doc """
  Ensures user consent is obtained before accessing an external resource.
  """
  @spec ensure_user_consent(
          ExMCP.ConsentHandler.user_id(),
          String.t(),
          atom(),
          module(),
          map()
        ) :: :ok | {:error, :consent_denied | :consent_required | :consent_error}
  def ensure_user_consent(user_id, url, transport, handler, config) do
    trusted_origins = Map.get(config, :trusted_origins, [])

    with {:ok, origin} <- TokenHandler.extract_origin(url),
         :external <- TokenHandler.classify_url(url, trusted_origins) do
      do_ensure_user_consent(user_id, origin, transport, handler, config)
    else
      :internal ->
        :ok

      {:error, _reason} ->
        # If URL is invalid or has no origin, we can't check consent.
        # Let other parts of the system handle the invalid URL.
        # From a consent perspective, we don't block it.
        :ok
    end
  end

  defp do_ensure_user_consent(user_id, origin, transport, handler, config) do
    case ConsentCache.check_consent(user_id, origin) do
      {:ok, _expires_at} ->
        :ok

      {:not_found} ->
        handle_consent_request(user_id, origin, transport, handler, config)

      {:expired} ->
        # Consent expired, so revoke it from cache and re-request.
        ConsentCache.revoke_consent(user_id, origin)
        handle_consent_request(user_id, origin, transport, handler, config)
    end
  end

  defp compute_expires_at(context) do
    ttl = Map.get(context, :consent_ttl, 3600)
    DateTime.add(DateTime.utc_now(), ttl, :second)
  end

  defp handle_consent_request(user_id, origin, transport, handler, config) do
    # Include any handler-specific options
    handler_opts = Map.get(config, :consent_handler_opts, [])

    context =
      %{
        transport: transport,
        consent_ttl: Map.get(config, :consent_ttl, 3600)
      }
      |> Map.merge(Enum.into(handler_opts, %{}))

    case handler.request_consent(user_id, origin, context) do
      {:ok, expires_at} ->
        # Convert DateTime or integer to monotonic time
        monotonic_expires = convert_to_monotonic_time(expires_at)
        ConsentCache.store_consent(user_id, origin, monotonic_expires)
        :ok

      {:approved, opts} ->
        expires_at = Keyword.get(opts, :expires_at, compute_expires_at(context))
        # Convert DateTime to monotonic time
        monotonic_expires = convert_to_monotonic_time(expires_at)
        ConsentCache.store_consent(user_id, origin, monotonic_expires)
        :ok

      {:denied, _opts} ->
        # Don't cache denials - let the handler be called each time
        # This ensures fresh consent checks for denied requests
        {:error, :consent_denied}

      {:error, :denied} ->
        {:error, :consent_denied}

      {:error, :consent_required} ->
        {:error, :consent_required}

      {:error, opts} when is_list(opts) ->
        # Handle error with options
        if Keyword.get(opts, :reason) == "Consent required" do
          {:error, :consent_required}
        else
          {:error, :consent_denied}
        end
    end
  end

  defp convert_to_monotonic_time(%DateTime{} = datetime) do
    # Convert DateTime to monotonic time
    unix_seconds = DateTime.to_unix(datetime)
    now_unix = DateTime.to_unix(DateTime.utc_now())
    diff_seconds = unix_seconds - now_unix
    System.monotonic_time(:second) + diff_seconds
  end

  defp convert_to_monotonic_time(seconds) when is_integer(seconds) do
    # Already in monotonic time format
    seconds
  end
end
