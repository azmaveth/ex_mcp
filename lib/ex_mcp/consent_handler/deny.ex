defmodule ExMCP.ConsentHandler.Deny do
  @moduledoc """
  A consent handler that denies all requests for external resource access.
  This is the default, production-safe handler.
  """

  @behaviour ExMCP.ConsentHandler

  @impl ExMCP.ConsentHandler
  def request_consent(_user_id, _resource_origin, _request_context) do
    {:error, :denied}
  end

  @impl ExMCP.ConsentHandler
  def check_existing_consent(_user_id, _resource_origin) do
    {:not_found}
  end

  @impl ExMCP.ConsentHandler
  def revoke_consent(_user_id, _resource_origin) do
    :ok
  end
end
