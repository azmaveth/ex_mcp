defmodule ExMCP.Authorization.Provider.OAuth do
  @moduledoc """
  OAuth 2.1 authorization provider with PKCE, PRM discovery, and scope step-up.

  This is the default provider when OAuth configuration is provided to the transport.
  It handles the complete lifecycle:

  1. On 401 — discovers PRM, AS metadata, optionally registers dynamically, runs PKCE flow
  2. On 403 with insufficient_scope — re-authorizes with broader scopes
  3. Prevents auth loops via `auth_completed` flag

  ## Configuration

      # Minimal (browser-based PKCE flow)
      {ExMCP.Authorization.Provider.OAuth, %{resource_url: "http://localhost:3000/mcp"}}

      # With pre-existing credentials
      {ExMCP.Authorization.Provider.OAuth, %{
        resource_url: "http://localhost:3000/mcp",
        client_id: "my-client",
        client_secret: "secret"
      }}
  """

  @behaviour ExMCP.Authorization.Provider

  require Logger

  alias ExMCP.Authorization.FullOAuthFlow

  defstruct [
    :access_token,
    :resource_url,
    :auth_config,
    auth_completed: false
  ]

  @impl true
  def init(config) do
    config = if is_list(config), do: Map.new(config), else: config

    state = %__MODULE__{
      resource_url: config[:resource_url],
      auth_config: config
    }

    {:ok, state}
  end

  @impl true
  def get_token(%__MODULE__{access_token: token} = state) do
    {:ok, token, state}
  end

  @impl true
  def handle_unauthorized(www_authenticate, scopes, %__MODULE__{} = state) do
    if state.auth_completed && state.access_token do
      # Already authenticated once and still getting 401 — don't loop
      Logger.warning("Auth failed after successful OAuth flow, not retrying")
      {:error, :auth_loop_detected, state}
    else
      do_authenticate(www_authenticate, scopes, state)
    end
  end

  @impl true
  def handle_forbidden(www_authenticate, scopes, %__MODULE__{} = state) do
    if www_authenticate && String.contains?(to_string(www_authenticate), "insufficient_scope") do
      # Scope step-up: clear token and re-auth with broader scope
      Logger.info("Scope step-up required, re-authorizing")

      :telemetry.execute(
        [:ex_mcp, :auth, :provider, :scope_stepup],
        %{system_time: System.system_time()},
        %{scopes: scopes}
      )

      cleared = %{state | access_token: nil, auth_completed: false}
      do_authenticate(www_authenticate, scopes, cleared)
    else
      {:error, :forbidden, state}
    end
  end

  defp do_authenticate(www_authenticate, scopes, state) do
    config =
      (state.auth_config || %{})
      |> Map.put(:resource_url, state.resource_url)
      |> Map.put(:www_authenticate, www_authenticate)

    config =
      if scopes != [] do
        Map.put(config, :scopes, scopes)
      else
        config
      end

    case FullOAuthFlow.execute(config) do
      {:ok, token_result} ->
        access_token = token_result[:access_token] || token_result["access_token"]
        Logger.info("OAuth token obtained")

        :telemetry.execute(
          [:ex_mcp, :auth, :provider, :token_obtained],
          %{system_time: System.system_time()},
          %{}
        )

        new_state = %{state | access_token: access_token, auth_completed: true}
        {:ok, access_token, new_state}

      {:error, reason} ->
        Logger.warning("OAuth flow failed: #{inspect(reason)}")
        {:error, {:oauth_failed, reason}, state}
    end
  end
end
