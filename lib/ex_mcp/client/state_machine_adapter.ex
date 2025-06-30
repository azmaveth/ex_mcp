defmodule ExMCP.Client.StateMachineAdapter do
  @moduledoc """
  Adapter that wraps the StateMachine implementation to provide
  the standard ExMCP.Client interface.

  This adapter translates between the ExMCP.Client API and the
  StateMachine implementation, handling state conversions and
  maintaining backward compatibility.
  """

  @behaviour ExMCP.Client.Adapter

  alias ExMCP.Client.StateMachine
  require Logger

  # Connection Management

  @impl true
  def start_link(config, opts \\ []) do
    # Convert config format if needed
    sm_config = convert_config(config)
    StateMachine.start_link(sm_config, opts)
  end

  @impl true
  def connect(client) do
    StateMachine.connect(client)
  end

  @impl true
  def disconnect(client) do
    StateMachine.disconnect(client)
  end

  @impl true
  def stop(client) do
    GenStateMachine.stop(client)
  end

  # Request Operations

  @impl true
  def call(client, method, params, opts \\ []) do
    StateMachine.request(client, method, params, opts)
  end

  @impl true
  def notify(client, method, params) do
    # Notifications don't expect a response
    case StateMachine.request(client, method, params, notify: true) do
      {:ok, _} -> :ok
      error -> error
    end
  end

  @impl true
  def batch_request(client, requests, opts \\ []) do
    # Convert batch requests to individual requests
    # TODO: Implement proper batch support in StateMachine
    results =
      Enum.map(requests, fn %{method: method, params: params} ->
        StateMachine.request(client, method, params, opts)
      end)

    # Check if all succeeded
    case Enum.find(results, &match?({:error, _}, &1)) do
      nil ->
        {:ok, Enum.map(results, fn {:ok, result} -> result end)}

      error ->
        error
    end
  end

  @impl true
  def complete(client, request, token) do
    # Send completion notification
    params = Map.put(request, "progressToken", token)
    call(client, "notifications/progress", params, [])
  end

  # Tool Operations

  @impl true
  def list_tools(client, opts \\ []) do
    call(client, "tools/list", %{}, opts)
  end

  @impl true
  def call_tool(client, name, arguments, opts \\ []) do
    params = %{
      "name" => name,
      "arguments" => arguments
    }

    call(client, "tools/call", params, opts)
  end

  @impl true
  def find_tool(client, name) do
    case list_tools(client) do
      {:ok, %{"tools" => tools}} ->
        case Enum.find(tools, fn tool -> tool["name"] == name end) do
          nil -> {:error, :not_found}
          tool -> {:ok, tool}
        end

      error ->
        error
    end
  end

  @impl true
  def find_matching_tool(client, pattern) do
    regex =
      case pattern do
        %Regex{} = r -> r
        str when is_binary(str) -> ~r/#{str}/
      end

    case list_tools(client) do
      {:ok, %{"tools" => tools}} ->
        matching =
          Enum.filter(tools, fn tool ->
            Regex.match?(regex, tool["name"]) or
              (tool["description"] && Regex.match?(regex, tool["description"]))
          end)

        {:ok, matching}

      error ->
        error
    end
  end

  # Resource Operations

  @impl true
  def list_resources(client, opts \\ []) do
    call(client, "resources/list", %{}, opts)
  end

  @impl true
  def read_resource(client, uri, opts \\ []) do
    params = %{"uri" => uri}
    call(client, "resources/read", params, opts)
  end

  @impl true
  def list_resource_templates(client, opts \\ []) do
    call(client, "resources/templates/list", %{}, opts)
  end

  @impl true
  def subscribe_resource(client, uri, opts \\ []) do
    params = %{"uri" => uri}
    call(client, "resources/subscribe", params, opts)
  end

  @impl true
  def unsubscribe_resource(client, uri, opts \\ []) do
    params = %{"uri" => uri}
    call(client, "resources/unsubscribe", params, opts)
  end

  # Prompt Operations

  @impl true
  def list_prompts(client, opts \\ []) do
    call(client, "prompts/list", %{}, opts)
  end

  @impl true
  def get_prompt(client, name, arguments, opts \\ []) do
    params = %{
      "name" => name,
      "arguments" => arguments
    }

    call(client, "prompts/get", params, opts)
  end

  # Logging Operations

  @impl true
  def set_log_level(client, level, opts \\ []) do
    params = %{"level" => level}
    call(client, "logging/setLevel", params, opts)
  end

  @impl true
  def log_message(client, level, message, data \\ nil, _opts \\ []) do
    params = %{
      "level" => level,
      "message" => message
    }

    params = if data, do: Map.put(params, "data", data), else: params

    case notify(client, "logging/message", params) do
      :ok -> :ok
      error -> error
    end
  end

  # Server Information

  @impl true
  def ping(client, opts \\ []) do
    call(client, "ping", %{}, opts)
  end

  @impl true
  def server_info(client) do
    case StateMachine.get_internal_state(client) do
      {:ok, %{server_info: server_info}} when is_map(server_info) ->
        # Extract the nested serverInfo map if present
        case server_info do
          %{"serverInfo" => info} -> info
          _ -> server_info
        end

      _ ->
        nil
    end
  end

  @impl true
  def server_capabilities(client) do
    case StateMachine.get_internal_state(client) do
      {:ok, %{server_info: %{"capabilities" => capabilities}}} -> capabilities
      _ -> %{}
    end
  end

  @impl true
  def negotiated_version(client) do
    case StateMachine.get_internal_state(client) do
      {:ok, %{server_info: %{"protocolVersion" => version}}} -> version
      _ -> nil
    end
  end

  @impl true
  def list_roots(client, opts \\ []) do
    call(client, "roots/list", %{}, opts)
  end

  # Client State

  @impl true
  def get_status(client) do
    StateMachine.get_state(client)
  end

  @impl true
  def get_pending_requests(client) do
    case StateMachine.get_internal_state(client) do
      {:ok, %{pending_requests: requests}} -> requests
      _ -> %{}
    end
  rescue
    _ -> %{}
  end

  # Legacy compatibility

  @impl true
  def make_request(client, method, params, opts \\ []) do
    call(client, method, params, opts)
  end

  @impl true
  def send_batch(client, requests) do
    batch_request(client, requests)
  end

  @impl true
  def send_cancelled(_client, _request_id) do
    # TODO: Implement request cancellation in StateMachine
    Logger.warning("Request cancellation not yet implemented in StateMachine adapter")
    :ok
  end

  @impl true
  def tools(client) do
    case list_tools(client) do
      {:ok, %{"tools" => tools}} -> {:ok, tools}
      error -> error
    end
  end

  # Private functions

  defp convert_config(config) when is_list(config) do
    Enum.into(config, %{})
  end

  defp convert_config(config) when is_map(config) do
    config
  end

  defp convert_config(url) when is_binary(url) do
    %{url: url}
  end
end
