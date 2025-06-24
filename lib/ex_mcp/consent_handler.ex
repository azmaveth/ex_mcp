defmodule ExMCP.ConsentHandler do
  @moduledoc """
  A behaviour for handling user consent for accessing external resources.
  """

  @typedoc "The user identifier."
  @type user_id :: String.t() | atom()

  @typedoc "The origin of the resource being accessed (e.g., \"https://api.example.com\")."
  @type resource_origin :: String.t()

  @typedoc "Context about the consent request."
  @type request_context :: map()

  @typedoc """
  The result of a consent request.
  - `{:ok, expires_at}`: Consent granted, valid until the given monotonic time in seconds.
  - `{:error, :denied}`: Consent explicitly denied.
  - `{:error, :consent_required}`: Consent needs to be obtained through another channel (e.g., a web UI).
  """
  @type consent_result ::
          {:ok, expires_at :: non_neg_integer()} | {:error, :denied | :consent_required}

  @doc """
  Requests user consent to access a resource.

  The `request_context` map can contain transport-specific information and
  configuration like `:consent_ttl`.
  """
  @callback request_consent(
              user_id :: user_id(),
              resource_origin :: resource_origin(),
              request_context :: request_context()
            ) :: consent_result()

  @doc """
  Checks if a valid consent already exists.

  This callback is primarily for handlers that might have their own persistent
  storage, separate from the global `ConsentCache`. Most handlers can simply
  return `{:not_found}` and rely on the cache.
  """
  @callback check_existing_consent(
              user_id :: user_id(),
              resource_origin :: resource_origin()
            ) :: {:ok, expires_at :: non_neg_integer()} | {:not_found} | {:expired}

  @doc """
  Revokes any existing consent for a user and resource.
  """
  @callback revoke_consent(user_id :: user_id(), resource_origin :: resource_origin()) ::
              :ok | {:error, String.t()}
end
