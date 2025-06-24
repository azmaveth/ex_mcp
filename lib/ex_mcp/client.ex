defmodule ExMCP.Client do
  @moduledoc """
  Unified MCP client combining the best features of all implementations.

  This module provides a clean, consistent API for interacting with MCP servers
  while maintaining backward compatibility with existing code.

  ## Features

  - Simple connection with URL strings or transport specs
  - Automatic transport fallback via TransportManager
  - Consistent return values with optional normalization
  - Convenience methods for common operations
  - Clean separation of concerns

  ## Examples

      # Connect with URL
      {:ok, client} = ExMCP.Client.connect("http://localhost:8080/mcp")
      
      # Connect with transport spec
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: "mcp-server"
      )
      
      # List and call tools
      {:ok, %{"tools" => tools}} = ExMCP.Client.list_tools(client)
      {:ok, result} = ExMCP.Client.call_tool(client, "weather", %{location: "NYC"})
  """

  use GenServer
  require Logger

  alias ExMCP.{Response, Error, TransportManager}
  alias ExMCP.Internal.Protocol

  # Client state
  defstruct [
    :transport_mod,
    :transport_state,
    :server_info,
    :transport_opts,
    :pending_requests,
    :receiver_task,
    :health_check_ref,
    :health_check_interval,
    :connection_status,
    :last_activity,
    :reconnect_attempts,
    :client_info,
    :raw_terms_enabled,
    :batch_tracking
  ]

  @type t :: GenServer.server()
  @type connection_spec :: String.t() | {atom(), keyword()} | [{atom(), keyword()}]

  # Public API

  @doc """
  Starts a client process with the given options.

  ## Options

  - `:transport` - Transport type (:stdio, :http, :sse, etc.)
  - `:transports` - List of transports for fallback
  - `:name` - Optional GenServer name
  - `:health_check_interval` - Interval for health checks (default: 30_000)
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    {name_opts, start_opts} = Keyword.split(opts, [:name])
    GenServer.start_link(__MODULE__, start_opts, name_opts)
  end

  @doc """
  Connects to an MCP server using a URL or connection spec.

  ## Examples

      # URL string
      {:ok, client} = ExMCP.Client.connect("http://localhost:8080/mcp")
      
      # Transport spec
      {:ok, client} = ExMCP.Client.connect({:stdio, command: "mcp-server"})
      
      # Multiple transports with fallback
      {:ok, client} = ExMCP.Client.connect([
        "http://localhost:8080/mcp",
        "stdio://mcp-server"
      ])
  """
  @spec connect(connection_spec(), keyword()) :: {:ok, t()} | {:error, any()}
  def connect(connection_spec, opts \\ []) do
    transport_opts = do_parse_connection_spec(connection_spec)
    start_link(Keyword.merge(transport_opts, opts))
  end

  @doc """
  Lists available tools from the server.

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Return format (:map or :struct, default: :map)
  """
  @spec list_tools(t(), keyword()) :: {:ok, %{String.t() => [map()]}} | {:error, any()}
  def list_tools(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)
    format = Keyword.get(opts, :format, :map)

    case GenServer.call(client, {:request, "tools/list", %{}}, timeout) do
      {:ok, response} -> format_response(response, format)
      error -> error
    end
  end

  @doc """
  Convenience alias for list_tools/2.
  """
  @spec tools(t(), keyword()) :: {:ok, %{String.t() => [map()]}} | {:error, any()}
  def tools(client, opts \\ []), do: list_tools(client, opts)

  @doc """
  Calls a tool with the given arguments.

  ## Options

  - `:timeout` - Request timeout (default: 30000)
  - `:format` - Return format (:map or :struct, default: :map)
  """
  @spec call_tool(t(), String.t(), map(), keyword() | timeout()) ::
          {:ok, any()} | {:error, any()}
  def call_tool(client, tool_name, arguments, timeout_or_opts \\ 30_000)

  def call_tool(client, tool_name, arguments, timeout) when is_integer(timeout) do
    call_tool(client, tool_name, arguments, timeout: timeout)
  end

  def call_tool(client, tool_name, arguments, opts) when is_list(opts) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    format = Keyword.get(opts, :format, :map)

    params = %{
      "name" => tool_name,
      "arguments" => arguments
    }

    case GenServer.call(client, {:request, "tools/call", params}, timeout) do
      {:ok, response} -> format_response(response, format)
      error -> error
    end
  end

  @doc """
  Convenience alias for call_tool/4.
  """
  @spec call(t(), String.t(), map(), keyword()) :: {:ok, any()} | {:error, any()}
  def call(client, tool_name, args \\ %{}, opts \\ []) do
    call_tool(client, tool_name, args, opts)
  end

  @doc """
  Finds a tool by name or pattern.

  ## Options

  - `:fuzzy` - Enable fuzzy matching (default: false)
  - `:timeout` - Request timeout (default: 5000)
  """
  @spec find_tool(t(), String.t() | nil, keyword()) ::
          {:ok, map()} | {:error, :not_found} | {:error, any()}
  def find_tool(client, name_or_pattern \\ nil, opts \\ []) do
    case list_tools(client, opts) do
      {:ok, %{"tools" => tools}} ->
        do_find_matching_tool(tools, name_or_pattern, opts)

      error ->
        error
    end
  end

  @doc """
  Lists available resources.
  """
  @spec list_resources(t(), keyword()) :: {:ok, %{String.t() => [map()]}} | {:error, any()}
  def list_resources(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)
    format = Keyword.get(opts, :format, :map)

    case GenServer.call(client, {:request, "resources/list", %{}}, timeout) do
      {:ok, response} -> format_response(response, format)
      error -> error
    end
  end

  @doc """
  Reads a resource by URI.
  """
  @spec read_resource(t(), String.t(), keyword()) :: {:ok, any()} | {:error, any()}
  def read_resource(client, uri, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 10_000)
    format = Keyword.get(opts, :format, :map)

    params = %{"uri" => uri}

    case GenServer.call(client, {:request, "resources/read", params}, timeout) do
      {:ok, response} -> format_response(response, format)
      error -> error
    end
  end

  @doc """
  Subscribes to notifications for a resource.

  Sends a `resources/subscribe` request to receive notifications when the 
  specified resource changes. The server will send `notifications/resources/updated`
  messages when the subscribed resource is modified.

  ## Parameters

  - `client` - Client process reference  
  - `uri` - Resource URI to subscribe to (e.g., "file:///path/to/file")

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Return format (:map or :struct, default: :map)

  ## Returns

  - `{:ok, result}` - Subscription successful 
  - `{:error, error}` - Subscription failed with error details

  ## Examples

      {:ok, _result} = ExMCP.Client.subscribe_resource(client, "file:///config.json")
  """
  @spec subscribe_resource(t(), String.t(), keyword()) :: {:ok, map()} | {:error, any()}
  def subscribe_resource(client, uri, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)
    format = Keyword.get(opts, :format, :map)

    params = %{"uri" => uri}

    case GenServer.call(client, {:request, "resources/subscribe", params}, timeout) do
      {:ok, response} -> format_response(response, format)
      error -> error
    end
  end

  @doc """
  Unsubscribes from notifications for a resource.

  Sends a `resources/unsubscribe` request to stop receiving notifications 
  for the specified resource.

  ## Parameters

  - `client` - Client process reference
  - `uri` - Resource URI to unsubscribe from

  ## Options

  - `:timeout` - Request timeout (default: 5000) 
  - `:format` - Return format (:map or :struct, default: :map)

  ## Returns

  - `{:ok, result}` - Unsubscription successful
  - `{:error, error}` - Unsubscription failed with error details

  ## Examples

      {:ok, _result} = ExMCP.Client.unsubscribe_resource(client, "file:///config.json")
  """
  @spec unsubscribe_resource(t(), String.t(), keyword()) :: {:ok, map()} | {:error, any()}
  def unsubscribe_resource(client, uri, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)
    format = Keyword.get(opts, :format, :map)

    params = %{"uri" => uri}

    case GenServer.call(client, {:request, "resources/unsubscribe", params}, timeout) do
      {:ok, response} -> format_response(response, format)
      error -> error
    end
  end

  @doc """
  Lists available prompts.
  """
  @spec list_prompts(t(), keyword()) :: {:ok, %{String.t() => [map()]}} | {:error, any()}
  def list_prompts(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)
    format = Keyword.get(opts, :format, :map)

    case GenServer.call(client, {:request, "prompts/list", %{}}, timeout) do
      {:ok, response} -> format_response(response, format)
      error -> error
    end
  end

  @doc """
  Gets a prompt with the given arguments.
  """
  @spec get_prompt(t(), String.t(), map(), keyword()) :: {:ok, any()} | {:error, any()}
  def get_prompt(client, prompt_name, arguments \\ %{}, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)
    format = Keyword.get(opts, :format, :map)

    params = %{
      "name" => prompt_name,
      "arguments" => arguments
    }

    case GenServer.call(client, {:request, "prompts/get", params}, timeout) do
      {:ok, response} -> format_response(response, format)
      error -> error
    end
  end

  @doc """
  Gets the client status.
  """
  @spec get_status(t()) :: {:ok, map()}
  def get_status(client) do
    GenServer.call(client, :get_status)
  end

  @doc """
  Gets server information.
  """
  @spec server_info(t()) :: {:ok, map()} | {:error, any()}
  def server_info(client) do
    case get_status(client) do
      {:ok, %{server_info: info}} -> {:ok, info}
      _ -> {:error, :not_connected}
    end
  end

  @doc """
  Pings the server.
  """
  @spec ping(t(), keyword() | integer()) :: {:ok, map()} | {:error, any()}
  def ping(client, opts_or_timeout \\ []) do
    # Handle both ping(client, timeout) and ping(client, opts) patterns
    timeout =
      case opts_or_timeout do
        timeout when is_integer(timeout) -> timeout
        opts when is_list(opts) -> Keyword.get(opts, :timeout, 5_000)
      end

    case GenServer.call(client, {:request, "ping", %{}}, timeout) do
      {:ok, result} -> {:ok, result}
      error -> error
    end
  end

  @doc """
  Sends a notification to the server.

  Notifications are fire-and-forget messages that don't expect a response.

  ## Parameters

  - `client` - Client process reference
  - `method` - The method name to notify
  - `params` - Parameters for the notification (map)

  ## Returns

  - `:ok` - Notification sent

  ## Examples

      :ok = ExMCP.Client.notify(client, "resource_updated", %{"uri" => "file://test.txt"})
  """
  @spec notify(t(), String.t(), map()) :: :ok
  def notify(client, method, params \\ %{}) do
    GenServer.cast(client, {:notification, method, params})
  end

  @doc """
  Sends a batch of JSON-RPC requests to the server.

  Takes a list of request tuples in the format `{method, params}` and sends
  them as a single JSON-RPC batch. Returns a list of results in the same order
  as the requests.

  ## Parameters

  - `client` - The client process
  - `requests` - List of `{method, params}` tuples
  - `timeout` - Timeout in milliseconds (default: 30_000)

  ## Returns

  - `{:ok, results}` - List of `{:ok, result}` or `{:error, error}` tuples
  - `{:error, reason}` - If the batch request fails entirely

  ## Examples

      requests = [
        {"tools/list", %{}},
        {"resources/list", %{}},
        {"ping", %{}}
      ]
      
      {:ok, results} = ExMCP.Client.batch_request(client, requests, 5000)
      
      # results will be like:
      # [
      #   {:ok, %{"tools" => [...]}},
      #   {:ok, %{"resources" => [...]}},
      #   {:ok, %{}}
      # ]
  """
  @spec batch_request(t(), [{String.t(), map()}], timeout()) ::
          {:ok, [{:ok, any()} | {:error, any()}]} | {:error, any()}
  def batch_request(client, requests, timeout \\ 30_000) when is_list(requests) do
    GenServer.call(client, {:batch_request, requests}, timeout)
  end

  @doc """
  Disconnects the client gracefully, cleaning up all resources.

  This function performs a clean shutdown by:
  - Closing the transport connection
  - Cancelling health checks
  - Stopping the receiver task
  - Replying to any pending requests with an error

  ## Examples

      {:ok, client} = ExMCP.Client.connect("http://localhost:8080/mcp")
      :ok = ExMCP.Client.disconnect(client)
  """
  @spec disconnect(t()) :: :ok
  def disconnect(client) do
    GenServer.call(client, :disconnect, 10_000)
  end

  @doc """
  Stops the client.
  """
  @spec stop(t(), term()) :: :ok
  def stop(client, reason \\ :normal) do
    GenServer.stop(client, reason)
  end

  # GenServer callbacks

  @impl GenServer
  def init(opts) do
    # Set up process
    Process.flag(:trap_exit, true)

    # Extract options
    health_check_interval = Keyword.get(opts, :health_check_interval, 30_000)
    client_info = build_client_info()

    # Initial state
    state = %__MODULE__{
      transport_opts: opts,
      pending_requests: %{},
      batch_tracking: %{},
      health_check_interval: health_check_interval,
      connection_status: :connecting,
      last_activity: System.system_time(:second),
      reconnect_attempts: 0,
      client_info: client_info
    }

    # Check if we should skip connection (for testing)
    if Keyword.get(opts, :_skip_connect, false) do
      {:ok, %{state | connection_status: :disconnected}}
    else
      # Start connection process
      case establish_connection(opts) do
        {:ok, transport_mod, transport_state, server_info} ->
          # Check transport capabilities for raw terms support
          raw_terms_enabled = check_transport_capabilities(transport_mod, transport_state)

          # Start receiver task
          receiver_task = start_receiver_task(transport_mod, transport_state)

          # Schedule health check
          health_check_ref = schedule_health_check(health_check_interval)

          new_state = %{
            state
            | transport_mod: transport_mod,
              transport_state: transport_state,
              server_info: server_info,
              receiver_task: receiver_task,
              health_check_ref: health_check_ref,
              connection_status: :connected,
              raw_terms_enabled: raw_terms_enabled
          }

          {:ok, new_state}

        {:error, reason} ->
          Logger.error("Failed to establish connection: #{inspect(reason)}")
          {:stop, {:connection_failed, reason}}
      end
    end
  end

  @impl GenServer
  def handle_call({:request, method, params}, from, state) do
    case state.connection_status do
      :connected ->
        # Generate request
        id = generate_id()

        request = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "method" => method,
          "params" => params
        }

        # Send request
        case send_message(state, request) do
          {:ok, new_transport_state} ->
            # Track pending request
            pending = Map.put(state.pending_requests, id, from)

            new_state = %{
              state
              | transport_state: new_transport_state,
                pending_requests: pending,
                last_activity: System.system_time(:second)
            }

            {:noreply, new_state}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end

      _ ->
        {:reply, {:error, :not_connected}, state}
    end
  end

  def handle_call({:batch_request, requests}, from, state) do
    case state.connection_status do
      :connected ->
        # Transform requests into JSON-RPC format with unique IDs
        {batch_requests, id_mapping} =
          Enum.reduce(requests, {[], %{}}, fn {method, params}, {acc_reqs, acc_map} ->
            id = generate_id()

            request = %{
              "jsonrpc" => "2.0",
              "id" => id,
              "method" => method,
              "params" => params
            }

            {[request | acc_reqs], Map.put(acc_map, id, length(acc_reqs))}
          end)

        batch_requests = Enum.reverse(batch_requests)

        # Send batch as array
        case send_message(state, batch_requests) do
          {:ok, new_transport_state} ->
            # Track the batch request with special handling
            batch_info = %{
              type: :batch,
              from: from,
              id_mapping: id_mapping,
              responses: %{},
              expected_count: length(requests)
            }

            # Add all IDs to pending requests
            pending =
              Enum.reduce(Map.keys(id_mapping), state.pending_requests, fn id, acc ->
                Map.put(acc, id, {:batch, from, id_mapping})
              end)

            # Store batch info
            batch_tracking = Map.put(state.batch_tracking || %{}, from, batch_info)

            new_state = %{
              state
              | transport_state: new_transport_state,
                pending_requests: pending,
                batch_tracking: batch_tracking,
                last_activity: System.system_time(:second)
            }

            {:noreply, new_state}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end

      _ ->
        {:reply, {:error, :not_connected}, state}
    end
  end

  def handle_call(:disconnect, _from, state) do
    # Cancel health check timer
    if state.health_check_ref do
      Process.cancel_timer(state.health_check_ref)
    end

    # Stop receiver task by killing the process directly
    if state.receiver_task do
      if Process.alive?(state.receiver_task.pid) do
        Process.exit(state.receiver_task.pid, :shutdown)
      end
    end

    # Reply to all pending requests with error
    for {_id, from} <- state.pending_requests do
      GenServer.reply(from, {:error, :disconnected})
    end

    # Close transport connection
    if state.transport_mod && state.transport_state do
      try do
        state.transport_mod.close(state.transport_state)
      rescue
        # Ignore errors during cleanup
        _ -> :ok
      end
    end

    # Update state to disconnected
    new_state = %{
      state
      | connection_status: :disconnected,
        pending_requests: %{},
        receiver_task: nil,
        health_check_ref: nil
    }

    {:reply, :ok, new_state}
  end

  def handle_call(:get_status, _from, state) do
    status = %{
      connection_status: state.connection_status,
      server_info: state.server_info,
      transport: state.transport_mod,
      reconnect_attempts: state.reconnect_attempts,
      last_activity: state.last_activity,
      pending_requests: map_size(state.pending_requests)
    }

    {:reply, {:ok, status}, state}
  end

  @impl GenServer
  def handle_cast({:notification, method, params}, state) do
    case state.connection_status do
      :connected ->
        # Create notification message (no id field)
        notification = %{
          "jsonrpc" => "2.0",
          "method" => method,
          "params" => params
        }

        # Send notification
        case send_message(state, notification) do
          {:ok, new_transport_state} ->
            new_state = %{
              state
              | transport_state: new_transport_state,
                last_activity: System.system_time(:second)
            }

            {:noreply, new_state}

          {:error, _reason} ->
            # For notifications, we don't reply with errors
            # Just log and continue
            {:noreply, state}
        end

      _ ->
        # Not connected, ignore notification
        {:noreply, state}
    end
  end

  @impl GenServer
  def handle_info({:transport_message, message}, state) do
    # Update last activity
    new_state = %{state | last_activity: System.system_time(:second)}

    # Parse message based on transport capabilities
    parsed_message =
      if state.raw_terms_enabled do
        # Message is already a map from raw terms transport
        {:ok, message}
      else
        # Decode JSON message
        case message do
          "" -> {:error, :empty_message}
          m when is_binary(m) -> Jason.decode(m)
          _ -> {:error, :invalid_message}
        end
      end

    case parsed_message do
      {:ok, responses} when is_list(responses) ->
        # Handle batch response array
        {updated_state, _} =
          Enum.reduce(responses, {new_state, state.pending_requests}, fn response,
                                                                         {acc_state, acc_pending} ->
            case response do
              %{"id" => id} when is_map_key(acc_pending, id) ->
                # Process each response in the batch
                {from_info, new_pending} = Map.pop(acc_pending, id)

                case from_info do
                  {:batch, batch_from, id_mapping} ->
                    # Store batch member response
                    batch_info = Map.get(acc_state.batch_tracking, batch_from)
                    position = Map.get(id_mapping, id)

                    result =
                      if Map.has_key?(response, "error") do
                        {:error, format_error(response["error"])}
                      else
                        {:ok, response["result"]}
                      end

                    updated_responses = Map.put(batch_info.responses, position, result)
                    updated_batch_info = %{batch_info | responses: updated_responses}

                    if map_size(updated_responses) == batch_info.expected_count do
                      # All responses received
                      ordered_results =
                        for i <- 0..(batch_info.expected_count - 1) do
                          Map.get(updated_responses, i)
                        end

                      GenServer.reply(batch_from, {:ok, ordered_results})

                      batch_tracking = Map.delete(acc_state.batch_tracking, batch_from)

                      {%{
                         acc_state
                         | pending_requests: new_pending,
                           batch_tracking: batch_tracking
                       }, new_pending}
                    else
                      batch_tracking =
                        Map.put(acc_state.batch_tracking, batch_from, updated_batch_info)

                      {%{
                         acc_state
                         | pending_requests: new_pending,
                           batch_tracking: batch_tracking
                       }, new_pending}
                    end

                  _ ->
                    # Should not happen in batch but handle anyway
                    {acc_state, acc_pending}
                end

              _ ->
                # Response without ID or unknown ID, skip
                {acc_state, acc_pending}
            end
          end)

        {:noreply, updated_state}

      {:ok, %{"id" => id} = response} when is_map_key(state.pending_requests, id) ->
        # Handle response to our request
        {from_info, pending} = Map.pop(state.pending_requests, id)

        case from_info do
          {:batch, batch_from, id_mapping} ->
            # This is part of a batch request
            batch_info = Map.get(state.batch_tracking, batch_from)

            # Store this response
            position = Map.get(id_mapping, id)

            result =
              if Map.has_key?(response, "error") do
                {:error, format_error(response["error"])}
              else
                {:ok, response["result"]}
              end

            updated_responses = Map.put(batch_info.responses, position, result)
            updated_batch_info = %{batch_info | responses: updated_responses}

            # Check if we have all responses
            if map_size(updated_responses) == batch_info.expected_count do
              # All responses received, send ordered results
              ordered_results =
                for i <- 0..(batch_info.expected_count - 1) do
                  Map.get(updated_responses, i)
                end

              GenServer.reply(batch_from, {:ok, ordered_results})

              # Clean up batch tracking
              batch_tracking = Map.delete(state.batch_tracking, batch_from)
              {:noreply, %{new_state | pending_requests: pending, batch_tracking: batch_tracking}}
            else
              # Still waiting for more responses
              batch_tracking = Map.put(state.batch_tracking, batch_from, updated_batch_info)
              {:noreply, %{new_state | pending_requests: pending, batch_tracking: batch_tracking}}
            end

          regular_from ->
            # Regular single request
            result =
              if Map.has_key?(response, "error") do
                {:error, format_error(response["error"])}
              else
                {:ok, response["result"]}
              end

            GenServer.reply(regular_from, result)
            {:noreply, %{new_state | pending_requests: pending}}
        end

      {:ok, %{"method" => method, "params" => params} = _notification} ->
        # Handle server notification
        case method do
          "notifications/resources/updated" ->
            # Handle resource update notification
            # The notification params contain the updated resource URI
            # For now, just log the notification - users can override this
            if Application.get_env(:ex_mcp, :debug_logging, false) do
              Logger.debug("Resource updated: #{inspect(params)}")
            end

            {:noreply, new_state}

          "notifications/roots/list_changed" ->
            # Handle roots list changed notification
            if Application.get_env(:ex_mcp, :debug_logging, false) do
              Logger.debug("Roots list changed: #{inspect(params)}")
            end

            {:noreply, new_state}

          "notifications/message" ->
            # Handle log message notification from server
            if Application.get_env(:ex_mcp, :debug_logging, false) do
              Logger.debug("Server log message: #{inspect(params)}")
            end

            {:noreply, new_state}

          _ ->
            # Unknown notification type
            if Application.get_env(:ex_mcp, :debug_logging, false) do
              Logger.debug("Unknown notification: #{method} - #{inspect(params)}")
            end

            {:noreply, new_state}
        end

      {:ok, %{"method" => method} = notification} ->
        # Handle notification without params
        if Application.get_env(:ex_mcp, :debug_logging, false) do
          Logger.debug("Notification without params: #{method} - #{inspect(notification)}")
        end

        {:noreply, new_state}

      _ ->
        # Unknown message
        {:noreply, new_state}
    end
  end

  def handle_info(:health_check, state) do
    # Perform health check
    # Note: Health check logic could ping server or check connection status

    # Schedule next health check
    health_check_ref = schedule_health_check(state.health_check_interval)
    {:noreply, %{state | health_check_ref: health_check_ref}}
  end

  def handle_info({:EXIT, pid, reason}, %{receiver_task: %{pid: pid}} = state) do
    Logger.error("Receiver task died: #{inspect(reason)}")
    # Note: Automatic reconnection logic could be implemented here
    {:noreply, %{state | connection_status: :disconnected}}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  # Private functions (some exposed for testing)

  @doc false
  def parse_connection_spec(spec), do: do_parse_connection_spec(spec)

  @doc false
  def prepare_transport_config(opts), do: do_prepare_transport_config(opts)

  @doc false
  def find_matching_tool(tools, name, opts), do: do_find_matching_tool(tools, name, opts)

  defp establish_connection(opts) do
    case do_prepare_transport_config(opts) do
      {:single, transport_mod, transport_opts} ->
        # Single transport mode
        case transport_mod.connect(transport_opts) do
          {:ok, transport_state} ->
            case do_handshake(transport_mod, transport_state) do
              {:ok, server_info} ->
                {:ok, transport_mod, transport_state, server_info}

              {:error, reason} ->
                transport_mod.close(transport_state)
                {:error, reason}
            end

          {:error, reason} ->
            {:error, reason}
        end

      {:multiple, transport_manager_opts} ->
        # Multiple transport mode with fallback
        case TransportManager.connect(transport_manager_opts) do
          {:ok, {transport_mod, transport_state}} ->
            case do_handshake(transport_mod, transport_state) do
              {:ok, server_info} ->
                {:ok, transport_mod, transport_state, server_info}

              {:error, reason} ->
                transport_mod.close(transport_state)
                {:error, reason}
            end

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp do_prepare_transport_config(opts) do
    cond do
      Keyword.has_key?(opts, :transports) ->
        # Multiple transports specified
        transport_manager_opts =
          Keyword.take(opts, [
            :transports,
            :fallback_strategy,
            :max_retries,
            :retry_interval
          ])

        {:multiple, transport_manager_opts}

      Keyword.has_key?(opts, :transport) ->
        # Single transport specified
        transport = Keyword.get(opts, :transport)
        {transport_mod, transport_opts} = normalize_transport_spec(transport, opts)
        {:single, transport_mod, transport_opts}

      true ->
        # Default to stdio
        {:single, ExMCP.Transport.Stdio, opts}
    end
  end

  defp normalize_transport_spec(transport, opts) when is_atom(transport) do
    {transport_mod, mode_opts} =
      case transport do
        :native -> {ExMCP.Transport.Local, [mode: :native]}
        :beam -> {ExMCP.Transport.Local, [mode: :beam]}
        :stdio -> {ExMCP.Transport.Stdio, []}
        :http -> {ExMCP.Transport.HTTP, []}
        :sse -> {ExMCP.Transport.SSE, []}
        :test -> {ExMCP.Transport.Test, []}
        mod -> {mod, []}
      end

    transport_opts = Keyword.merge(Keyword.delete(opts, :transport), mode_opts)
    {transport_mod, transport_opts}
  end

  defp normalize_transport_spec({transport, transport_opts}, _opts) do
    normalize_transport_spec(transport, transport_opts)
  end

  defp do_parse_connection_spec(url) when is_binary(url) do
    uri = URI.parse(url)

    case uri.scheme do
      "http" -> [transport: :http, url: url]
      "https" -> [transport: :http, url: url]
      "stdio" -> [transport: :stdio, command: uri.path || uri.host]
      "file" -> [transport: :stdio, command: uri.path]
      _ -> [transport: :http, url: url]
    end
  end

  defp do_parse_connection_spec({transport, opts}) do
    [transport: transport] ++ opts
  end

  defp do_parse_connection_spec(specs) when is_list(specs) do
    transports =
      Enum.map(specs, fn
        url when is_binary(url) ->
          opts = do_parse_connection_spec(url)
          transport_atom = Keyword.fetch!(opts, :transport)
          normalize_transport_spec(transport_atom, opts)

        {transport, opts} ->
          normalize_transport_spec(transport, opts)
      end)

    [transports: transports]
  end

  defp do_handshake(transport_mod, transport_state) do
    # Check transport capabilities before handshake
    raw_terms_enabled = check_transport_capabilities(transport_mod, transport_state)

    # Build initialize request
    client_info = build_client_info()

    init_msg =
      Protocol.encode_initialize(
        client_info,
        %{
          "tools" => %{},
          "resources" => %{},
          "prompts" => %{}
        }
      )

    # Send initialize request (encode based on transport capabilities)
    message_to_send = if raw_terms_enabled, do: init_msg, else: Jason.encode!(init_msg)

    case transport_mod.send_message(message_to_send, transport_state) do
      {:ok, new_state} ->
        # Receive response
        case transport_mod.receive_message(new_state, 5_000) do
          {:ok, response_data, next_state} ->
            # Parse response based on transport capabilities
            parsed_response =
              if raw_terms_enabled do
                {:ok, response_data}
              else
                Jason.decode(response_data)
              end

            case parsed_response do
              {:ok, %{"result" => result}} ->
                # Send initialized notification
                case send_initialized(transport_mod, next_state, raw_terms_enabled) do
                  {:ok, final_state} ->
                    {:ok, Map.put(result, :transport_state, final_state)}

                  {:error, reason} ->
                    {:error, {:initialized_failed, reason}}
                end

              {:ok, %{"error" => error}} ->
                {:error, {:handshake_error, error}}

              _ ->
                {:error, :invalid_handshake_response}
            end

          {:error, reason} ->
            {:error, {:handshake_failed, reason}}
        end

      {:error, reason} ->
        {:error, {:send_failed, reason}}
    end
  end

  defp send_initialized(transport_mod, transport_state, raw_terms_enabled) do
    msg = Protocol.encode_initialized()
    message_to_send = if raw_terms_enabled, do: msg, else: Jason.encode!(msg)
    transport_mod.send_message(message_to_send, transport_state)
  end

  defp build_client_info do
    %{
      "name" => "ExMCP",
      "version" => "0.8.0"
    }
  end

  defp send_message(state, message) do
    # Check if transport supports raw terms
    message_to_send =
      if state.raw_terms_enabled do
        # Send raw map for performance
        message
      else
        # Encode to JSON for spec compliance
        Jason.encode!(message)
      end

    state.transport_mod.send_message(message_to_send, state.transport_state)
  end

  defp start_receiver_task(transport_mod, transport_state) do
    parent = self()

    Task.async(fn ->
      receive_loop(parent, transport_mod, transport_state)
    end)
  end

  defp receive_loop(parent, transport_mod, transport_state) do
    case transport_mod.receive_message(transport_state) do
      {:ok, message, new_state} ->
        send(parent, {:transport_message, message})
        receive_loop(parent, transport_mod, new_state)

      {:error, :closed} ->
        send(parent, {:transport_closed, :normal})
        :ok

      {:error, :no_message} ->
        # No message available right now - continue listening
        # Skip sleep for high-performance local transports to achieve sub-millisecond latency
        if transport_mod == ExMCP.Transport.Local do
          # Local transports use Agent queues - no need to sleep
          receive_loop(parent, transport_mod, transport_state)
        else
          # Network transports need brief sleep to prevent busy-waiting
          :timer.sleep(1)
          receive_loop(parent, transport_mod, transport_state)
        end

      {:error, reason} ->
        send(parent, {:transport_closed, reason})
        :ok
    end
  end

  defp schedule_health_check(interval) do
    Process.send_after(self(), :health_check, interval)
  end

  defp format_response(response, :map) do
    {:ok, response}
  end

  defp format_response(response, :struct) do
    # For now, just wrap in a Response struct
    {:ok, %Response{content: response}}
  end

  defp generate_id do
    System.unique_integer([:positive])
    |> Integer.to_string()
  end

  defp format_error(error) when is_map(error) do
    if Map.get(error, "code") == -32000 and Map.get(error, "message") == "Request timed out" do
      :timeout
    else
      %Error{
        code: error["code"],
        message: error["message"],
        data: error["data"]
      }
    end
  end

  defp format_error(error) do
    %Error{message: inspect(error)}
  end

  defp do_find_matching_tool(tools, nil, _opts), do: {:ok, List.first(tools)}

  defp do_find_matching_tool(tools, name, opts) do
    fuzzy? = Keyword.get(opts, :fuzzy, false)

    result =
      if fuzzy? do
        Enum.find(tools, fn tool ->
          String.contains?(
            String.downcase(tool["name"] || ""),
            String.downcase(name)
          )
        end)
      else
        Enum.find(tools, &(&1["name"] == name))
      end

    case result do
      nil -> {:error, :not_found}
      tool -> {:ok, tool}
    end
  end

  @doc """
  Requests completion suggestions from the server.

  Sends a `completion/complete` request to get completion suggestions based on
  a reference (prompt or resource) and partial input.

  ## Parameters

  - `client` - Client process reference
  - `ref` - Reference map describing what to complete:
    - For prompts: `%{"type" => "ref/prompt", "name" => "prompt_name"}`
    - For resources: `%{"type" => "ref/resource", "uri" => "resource_uri"}`
  - `argument` - Argument map with completion context:
    - `%{"name" => "argument_name", "value" => "partial_value"}`

  ## Options

  - `:timeout` - Request timeout (default: 5000)
  - `:format` - Return format (:map or :struct, default: :map)

  ## Returns

  - `{:ok, result}` - Success with completion suggestions:
    ```
    %{
      completion: %{
        values: ["suggestion1", "suggestion2", ...],
        total: 10,
        hasMore: false
      }
    }
    ```
  - `{:error, error}` - Request failed with error details

  ## Examples

      # Complete prompt argument
      {:ok, result} = ExMCP.Client.complete(
        client,
        %{"type" => "ref/prompt", "name" => "code_generator"},
        %{"name" => "language", "value" => "java"}
      )

      # Complete resource URI
      {:ok, result} = ExMCP.Client.complete(
        client,
        %{"type" => "ref/resource", "uri" => "file:///"},
        %{"name" => "path", "value" => "/src"}
      )
  """
  @spec complete(t(), map(), map(), keyword()) :: {:ok, map()} | {:error, any()}
  def complete(client, ref, argument, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)
    format = Keyword.get(opts, :format, :map)

    params = %{
      "ref" => ref,
      "argument" => argument
    }

    case GenServer.call(client, {:request, "completion/complete", params}, timeout) do
      {:ok, response} -> format_response(response, format)
      error -> error
    end
  end

  @doc """
  Sets the log level for the server.

  Sends a `logging/setLevel` request to configure the server's log verbosity.
  This is part of the MCP specification for controlling server logging behavior.

  ## Parameters

  - `client` - Client process reference
  - `level` - Log level string: "debug", "info", "warning", or "error"

  ## Returns

  - `{:ok, result}` - Success with any server response data
  - `{:error, error}` - Request failed with error details

  ## Example

      {:ok, client} = ExMCP.Client.start_link(transport: :http, url: "...")
      {:ok, _} = ExMCP.Client.set_log_level(client, "debug")
  """
  @spec set_log_level(GenServer.server(), String.t()) :: {:ok, map()} | {:error, any()}
  def set_log_level(client, level) when is_binary(level) do
    params = %{"level" => level}

    case GenServer.call(client, {:request, "logging/setLevel", params}, 30_000) do
      {:ok, response} -> {:ok, response}
      error -> error
    end
  end

  @doc """
  Sends a log message to the server as a notification.

  This function sends log messages from the client to the server for centralized
  logging and monitoring. The message is sent as a notification (fire-and-forget)
  following the MCP specification.

  ## Parameters

  - `client` - Client process reference
  - `level` - Log level string (e.g., "debug", "info", "warning", "error")
  - `message` - Log message text

  ## Returns

  - `:ok` - Message sent successfully
  - `{:error, reason}` - Failed to send message

  ## Example

      {:ok, client} = ExMCP.Client.start_link(transport: :http, url: "...")
      :ok = ExMCP.Client.log_message(client, "info", "Operation completed")
  """
  @spec log_message(t(), String.t(), String.t()) :: :ok | {:error, any()}
  def log_message(client, level, message) when is_binary(level) and is_binary(message) do
    log_message(client, level, message, nil)
  end

  @doc """
  Sends a log message with additional data to the server as a notification.

  This function sends detailed log messages from the client to the server for
  centralized logging and monitoring. The message is sent as a notification
  (fire-and-forget) following the MCP specification.

  ## Parameters

  - `client` - Client process reference  
  - `level` - Log level string (e.g., "debug", "info", "warning", "error")
  - `message` - Log message text
  - `data` - Optional additional data (map or any JSON-serializable value)

  ## Supported Log Levels

  Standard RFC 5424 levels: "debug", "info", "notice", "warning", "error", 
  "critical", "alert", "emergency"

  ## Returns

  - `:ok` - Message sent successfully
  - `{:error, reason}` - Failed to send message

  ## Examples

      {:ok, client} = ExMCP.Client.start_link(transport: :http, url: "...")
      
      # Simple log message
      :ok = ExMCP.Client.log_message(client, "info", "User logged in")
      
      # Log message with additional context
      :ok = ExMCP.Client.log_message(client, "error", "Database connection failed", %{
        host: "db.example.com",
        port: 5432,
        error_code: "CONNECTION_TIMEOUT"
      })
  """
  @spec log_message(t(), String.t(), String.t(), any()) :: :ok | {:error, any()}
  def log_message(client, level, message, data) when is_binary(level) and is_binary(message) do
    GenServer.cast(
      client,
      {:notification, "notifications/message",
       %{
         "level" => level,
         "message" => message,
         "data" => data
       }}
    )
  end

  # Private helper to check transport capabilities
  defp check_transport_capabilities(transport_mod, transport_state) do
    if function_exported?(transport_mod, :capabilities, 1) do
      capabilities = transport_mod.capabilities(transport_state)
      :raw_terms in capabilities
    else
      false
    end
  end
end
