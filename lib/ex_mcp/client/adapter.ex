defmodule ExMCP.Client.Adapter do
  @moduledoc """
  Behaviour for client adapters.

  This behaviour defines the interface that all client implementations
  must satisfy. It allows for different backends (GenServer-based,
  StateMachine-based, etc.) while maintaining a consistent API.
  """

  @type client :: term()
  @type request_opts :: keyword()
  @type batch_request :: %{method: String.t(), params: map()}
  @type error :: {:error, term()}

  # Connection Management
  @callback start_link(config :: map(), opts :: keyword()) :: {:ok, client()} | error()
  @callback connect(client()) :: :ok | error()
  @callback disconnect(client()) :: :ok | error()
  @callback stop(client()) :: :ok

  # Request Operations
  @callback call(client(), method :: String.t(), params :: map(), opts :: request_opts()) ::
              {:ok, term()} | error()
  @callback notify(client(), method :: String.t(), params :: map()) :: :ok | error()
  @callback batch_request(client(), requests :: [batch_request()], opts :: request_opts()) ::
              {:ok, [term()]} | error()
  @callback complete(client(), request :: map(), token :: String.t()) ::
              {:ok, term()} | error()

  # Tool Operations
  @callback list_tools(client(), opts :: request_opts()) :: {:ok, map()} | error()
  @callback call_tool(client(), name :: String.t(), arguments :: map(), opts :: request_opts()) ::
              {:ok, term()} | error()
  @callback find_tool(client(), name :: String.t()) :: {:ok, map()} | {:error, :not_found}
  @callback find_matching_tool(client(), pattern :: String.t() | Regex.t()) ::
              {:ok, [map()]} | error()

  # Resource Operations
  @callback list_resources(client(), opts :: request_opts()) :: {:ok, map()} | error()
  @callback read_resource(client(), uri :: String.t(), opts :: request_opts()) ::
              {:ok, term()} | error()
  @callback list_resource_templates(client(), opts :: request_opts()) :: {:ok, map()} | error()
  @callback subscribe_resource(client(), uri :: String.t(), opts :: request_opts()) ::
              {:ok, map()} | error()
  @callback unsubscribe_resource(client(), uri :: String.t(), opts :: request_opts()) ::
              {:ok, map()} | error()

  # Prompt Operations
  @callback list_prompts(client(), opts :: request_opts()) :: {:ok, map()} | error()
  @callback get_prompt(client(), name :: String.t(), arguments :: map(), opts :: request_opts()) ::
              {:ok, map()} | error()

  # Logging Operations
  @callback set_log_level(client(), level :: String.t(), opts :: request_opts()) ::
              {:ok, map()} | error()
  @callback log_message(
              client(),
              level :: String.t(),
              message :: String.t(),
              data :: map() | nil,
              opts :: request_opts()
            ) :: :ok | error()

  # Server Information
  @callback ping(client(), opts :: request_opts()) :: {:ok, map()} | error()
  @callback server_info(client()) :: map() | nil
  @callback server_capabilities(client()) :: map()
  @callback negotiated_version(client()) :: String.t() | nil
  @callback list_roots(client(), opts :: request_opts()) :: {:ok, map()} | error()

  # Client State
  @callback get_status(client()) :: map()
  @callback get_pending_requests(client()) :: map()

  # Legacy compatibility
  @callback make_request(client(), method :: String.t(), params :: map(), opts :: request_opts()) ::
              {:ok, term()} | error()
  @callback send_batch(client(), requests :: [batch_request()]) ::
              {:ok, [term()]} | error()
  @callback send_cancelled(client(), request_id :: term()) :: :ok
  @callback tools(client()) :: {:ok, [map()]} | error()
end
