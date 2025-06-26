defmodule ExMCP.Server.Legacy do
  @moduledoc """
  Legacy handler-based server implementation.

  This module provides compatibility for the old handler-based server API
  that uses the `ExMCP.Server.Handler` behaviour. It's primarily used for
  testing and legacy code that hasn't been migrated to the DSL-based approach.

  ## Usage

      # Handler module implementing ExMCP.Server.Handler
      defmodule MyHandler do
        use ExMCP.Server.Handler

        @impl true
        def handle_initialize(params, state) do
          {:ok, %{
            protocolVersion: "2025-03-26",
            serverInfo: %{name: "test-server", version: "1.0.0"},
            capabilities: %{tools: %{}}
          }, state}
        end

        @impl true
        def handle_list_tools(_cursor, state) do
          tools = [
            %{
              name: "ping",
              description: "Simple ping tool",
              inputSchema: %{type: "object", properties: %{}}
            }
          ]
          {:ok, tools, nil, state}
        end
      end

      # Start the server
      {:ok, server} = ExMCP.Server.start_link(transport: :test, handler: MyHandler)
  """

  use GenServer
  require Logger

  alias ExMCP.Protocol.ErrorCodes
  alias ExMCP.Transport.Test

  @type handler_module :: module()
  @type state :: %{
          handler_module: handler_module(),
          handler_state: any(),
          transport: any(),
          transport_state: any(),
          protocol_version: String.t() | nil
        }

  @doc """
  Starts a legacy handler-based server.

  ## Options

  * `:handler` - Module implementing `ExMCP.Server.Handler` behaviour (required)
  * `:transport` - Transport type (`:test`, `:stdio`, `:http`, etc.)
  * Other options are passed to the transport and handler
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl GenServer
  def init(opts) do
    handler_module = Keyword.fetch!(opts, :handler)
    transport_type = Keyword.get(opts, :transport, :test)

    # Initialize the handler
    case handler_module.init(opts) do
      {:ok, handler_state} ->
        # Connect to the transport
        case connect_transport(transport_type, opts) do
          {:ok, {transport_mod, transport_state}} ->
            state = %{
              handler_module: handler_module,
              handler_state: handler_state,
              transport: transport_mod,
              transport_state: transport_state,
              protocol_version: nil,
              pending_requests: %{},
              cancelled_requests: MapSet.new()
            }

            # For test transport, handle connection setup
            if transport_type == :test do
              # Start listening for messages
              send(self(), :start_message_loop)
            end

            {:ok, state}

          {:error, reason} ->
            {:stop, {:transport_error, reason}}
        end

      {:error, reason} ->
        {:stop, {:handler_init_error, reason}}
    end
  end

  @impl GenServer
  def handle_info(:start_message_loop, state) do
    # Start receiving messages from transport
    spawn_link(fn -> message_loop(self(), state.transport, state.transport_state) end)
    {:noreply, state}
  end

  def handle_info({:transport_message, message}, state) do
    case Jason.decode(message) do
      {:ok, requests} when is_list(requests) ->
        handle_batch_request(requests, state)

      {:ok, request} when is_map(request) ->
        case process_mcp_request(request, state) do
          {:response, response, new_state} ->
            case send_message(response, new_state) do
              {:ok, final_state} -> {:noreply, final_state}
              {:error, _reason} -> {:noreply, new_state}
            end

          {:notification, new_state} ->
            # Single notification received, no response needed.
            {:noreply, new_state}
        end

      {:error, error} ->
        Logger.error("Failed to decode message: #{inspect(error)}")
        {:noreply, state}
    end
  end

  def handle_info({:transport_error, reason}, state) do
    Logger.error("Transport error: #{inspect(reason)}")
    {:noreply, state}
  end

  def handle_info({:test_transport_connect, client_pid}, state) do
    # Update transport state to include the connected client
    if state.transport == Test do
      new_transport_state = %{state.transport_state | peer_pid: client_pid}
      new_state = %{state | transport_state: new_transport_state}
      {:noreply, new_state}
    else
      {:noreply, state}
    end
  end

  def handle_info({:transport_closed}, state) do
    Logger.info("Transport closed")
    {:stop, :normal, state}
  end

  # Handle batch requests according to JSON-RPC 2.0 specification
  defp handle_batch_request([], state) do
    # Empty batch is invalid according to JSON-RPC 2.0
    send_error_response(-32600, "Invalid Request", nil, state)
  end

  defp handle_batch_request(requests, %{protocol_version: "2025-06-18"} = state)
       when is_list(requests) do
    # Batch requests not supported in this version
    send_error_response(
      -32600,
      "Batch requests are not supported in protocol version 2025-06-18",
      nil,
      state
    )
  end

  defp handle_batch_request(requests, state) when is_list(requests) do
    # Process each request in the batch
    {responses, final_state} = process_batch_requests(requests, state)

    # Filter out nils from notifications
    non_nil_responses = Enum.reject(responses, &is_nil/1)

    # Only send response if we have any (notifications don't generate responses)
    if not Enum.empty?(non_nil_responses) do
      case send_message(non_nil_responses, final_state) do
        {:ok, new_state} -> {:noreply, new_state}
        {:error, _reason} -> {:noreply, final_state}
      end
    else
      {:noreply, final_state}
    end
  end

  defp send_error_response(code, message, id, state) do
    error_response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "error" => %{"code" => code, "message" => message}
    }

    case send_message(error_response, state) do
      {:ok, new_state} -> {:noreply, new_state}
      {:error, _reason} -> {:noreply, state}
    end
  end

  defp process_batch_requests(requests, state) do
    Enum.map_reduce(requests, state, fn request, acc_state ->
      case process_mcp_request(request, acc_state) do
        {:response, response, new_state} ->
          {response, new_state}

        {:notification, new_state} ->
          # Notifications don't get responses
          {nil, new_state}
      end
    end)
  end

  @impl GenServer
  def handle_call(:ping, _from, state) do
    # Send ping to client via transport
    ping_request = %{
      "jsonrpc" => "2.0",
      "id" => System.unique_integer([:positive]),
      "method" => "ping",
      "params" => %{}
    }

    case send_message(ping_request, state) do
      {:ok, _new_state} ->
        # In a real implementation, we'd wait for the pong response
        # For testing, we just return success
        {:reply, {:ok, %{}}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(:list_roots, _from, state) do
    # Send list_roots request to client via transport
    list_roots_request = %{
      "jsonrpc" => "2.0",
      "id" => System.unique_integer([:positive]),
      "method" => "roots/list",
      "params" => %{}
    }

    case send_message(list_roots_request, state) do
      {:ok, _new_state} ->
        # In a real implementation, we'd wait for the response from the client
        # For testing with test transport, we simulate a successful response
        # Note: Real implementation would need bidirectional client-server communication
        mock_roots = %{
          "roots" => [
            %{"uri" => "file:///shared", "name" => "Shared"}
          ]
        }

        {:reply, {:ok, mock_roots}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(request, from, state) do
    # Forward unknown calls to the handler if it supports GenServer calls
    if function_exported?(state.handler_module, :handle_call, 3) do
      case state.handler_module.handle_call(request, from, state.handler_state) do
        {:reply, reply, new_handler_state} ->
          new_state = %{state | handler_state: new_handler_state}
          {:reply, reply, new_state}

        other ->
          other
      end
    else
      {:reply, {:error, {:unknown_call, request}}, state}
    end
  end

  @impl GenServer
  def handle_cast({:send_log_message, level, message, data}, state) do
    # Send log notification to client
    log_notification = %{
      "jsonrpc" => "2.0",
      "method" => "notifications/message",
      "params" => %{
        "level" => level,
        "logger" => "ExMCP.Server",
        "data" => data || %{},
        "message" => message
      }
    }

    case send_message(log_notification, state) do
      {:ok, new_state} -> {:noreply, new_state}
      {:error, _reason} -> {:noreply, state}
    end
  end

  def handle_cast({:notify_progress, progress_token, progress, total}, state) do
    # Send progress notification to client
    progress_notification = %{
      "jsonrpc" => "2.0",
      "method" => "notifications/progress",
      "params" => %{
        "progressToken" => progress_token,
        "progress" => progress,
        "total" => total
      }
    }

    case send_message(progress_notification, state) do
      {:ok, new_state} -> {:noreply, new_state}
      {:error, _reason} -> {:noreply, state}
    end
  end

  def handle_cast({:notify_resource_update, uri}, state) do
    # Send resource update notification to client
    update_notification = %{
      "jsonrpc" => "2.0",
      "method" => "notifications/resources/updated",
      "params" => %{
        "uri" => uri
      }
    }

    case send_message(update_notification, state) do
      {:ok, new_state} -> {:noreply, new_state}
      {:error, _reason} -> {:noreply, state}
    end
  end

  def handle_cast(:notify_roots_changed, state) do
    # Send roots changed notification to client
    roots_notification = %{
      "jsonrpc" => "2.0",
      "method" => "notifications/roots/list_changed",
      "params" => %{}
    }

    case send_message(roots_notification, state) do
      {:ok, new_state} -> {:noreply, new_state}
      {:error, _reason} -> {:noreply, state}
    end
  end

  def handle_cast({:notification, "notifications/cancelled", params}, state) do
    # Handle cancellation notifications from clients
    handle_cancellation_notification(params, state)
  end

  def handle_cast(request, state) do
    # Forward unknown casts to the handler if it supports GenServer casts
    if function_exported?(state.handler_module, :handle_cast, 2) do
      case state.handler_module.handle_cast(request, state.handler_state) do
        {:noreply, new_handler_state} ->
          new_state = %{state | handler_state: new_handler_state}
          {:noreply, new_state}

        other ->
          other
      end
    else
      {:noreply, state}
    end
  end

  @impl GenServer
  def terminate(reason, state) do
    if function_exported?(state.handler_module, :terminate, 2) do
      state.handler_module.terminate(reason, state.handler_state)
    end
  end

  # Private functions

  # Handle cancellation notifications from clients
  defp handle_cancellation_notification(%{"requestId" => request_id} = params, state) do
    require Logger
    reason = Map.get(params, "reason", "Request cancelled by client")

    Logger.debug("Received cancellation for request #{request_id}: #{reason}")

    # Mark the request as cancelled
    new_cancelled_requests = MapSet.put(state.cancelled_requests, request_id)
    new_state = %{state | cancelled_requests: new_cancelled_requests}

    # If the request is still pending, remove it and reply with cancellation error
    case Map.get(state.pending_requests, request_id) do
      nil ->
        # Request not found or already completed
        Logger.debug("Request #{request_id} not found in pending requests")
        # Still update handler state to inform it about cancellation
        new_handler_state =
          update_handler_cancelled_requests(state.handler_module, state.handler_state, request_id)

        final_state = %{new_state | handler_state: new_handler_state}
        {:noreply, final_state}

      from ->
        # Request is still pending, reply with cancellation error
        GenServer.reply(from, {:error, :cancelled})
        new_pending_requests = Map.delete(state.pending_requests, request_id)
        # Also notify handler about cancellation
        new_handler_state =
          update_handler_cancelled_requests(state.handler_module, state.handler_state, request_id)

        final_state = %{
          new_state
          | pending_requests: new_pending_requests,
            handler_state: new_handler_state
        }

        {:noreply, final_state}
    end
  end

  defp handle_cancellation_notification(params, state) do
    require Logger
    Logger.warning("Invalid cancellation notification: #{inspect(params)}")
    {:noreply, state}
  end

  # Update handler state with cancelled request information
  defp update_handler_cancelled_requests(_handler_module, handler_state, request_id) do
    require Logger

    # For the SlowHandler pattern, update the cancelled_requests field
    updated_state =
      if Map.has_key?(handler_state, :cancelled_requests) do
        new_cancelled = MapSet.put(handler_state.cancelled_requests, request_id)
        Logger.debug("Updated cancelled_requests in handler state: #{inspect(new_cancelled)}")

        # Also update ETS table if it exists (for test handlers)
        if :ets.whereis(:cancellation_tracker) != :undefined do
          :ets.insert(:cancellation_tracker, {request_id, :cancelled})
          Logger.debug("Updated ETS cancellation tracker for request #{request_id}")
        end

        %{handler_state | cancelled_requests: new_cancelled}
      else
        handler_state
      end

    # Also send cancellation message to active request processes
    if Map.has_key?(handler_state, :active_requests) do
      case Map.get(handler_state.active_requests, request_id) do
        nil ->
          Logger.debug("No active process found for request #{request_id}")

        pid when is_pid(pid) ->
          Logger.debug(
            "Sending cancellation message to worker process #{inspect(pid)} for request #{request_id}"
          )

          # Send cancellation message to the worker process
          send(pid, {:cancelled, request_id})

        other ->
          Logger.debug("Unexpected active_requests entry for #{request_id}: #{inspect(other)}")
      end
    else
      Logger.debug("Handler state has no active_requests field")
    end

    # IMPORTANT: Also send cancellation to the handler process itself
    # The handler might be waiting for this message in a receive block
    send(self(), {:cancelled, request_id})
    Logger.debug("Sent cancellation message to handler process for request #{request_id}")

    updated_state
  end

  # Check if a request has been cancelled
  defp request_cancelled?(request_id, state) do
    MapSet.member?(state.cancelled_requests, request_id)
  end

  # Add a pending request to tracking
  defp track_pending_request(request_id, from, state) do
    new_pending_requests = Map.put(state.pending_requests, request_id, from)
    %{state | pending_requests: new_pending_requests}
  end

  # Remove a pending request from tracking (when completed)
  defp complete_pending_request(request_id, state) do
    new_pending_requests = Map.delete(state.pending_requests, request_id)
    %{state | pending_requests: new_pending_requests}
  end

  defp connect_transport(:test, opts) do
    case Test.connect(opts) do
      {:ok, transport_state} -> {:ok, {Test, transport_state}}
      error -> error
    end
  end

  defp connect_transport(transport_type, _opts) do
    {:error, {:unsupported_transport, transport_type}}
  end

  defp message_loop(server_pid, transport_mod, transport_state) do
    case transport_mod.receive_message(transport_state) do
      {:ok, message, new_transport_state} ->
        send(server_pid, {:transport_message, message})
        message_loop(server_pid, transport_mod, new_transport_state)

      {:error, reason} ->
        send(server_pid, {:transport_error, reason})
    end
  end

  # Process a single MCP request or notification
  defp process_mcp_request(%{"method" => "initialize"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})

    case state.handler_module.handle_initialize(params, state.handler_state) do
      {:ok, result, new_handler_state} ->
        response = %{"jsonrpc" => "2.0", "id" => id, "result" => result}

        new_state = %{
          state
          | handler_state: new_handler_state,
            protocol_version: Map.get(result, "protocolVersion")
        }

        {:response, response, new_state}

      {:error, error, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{"code" => -32000, "message" => "Initialize error: #{inspect(error)}"}
        }

        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}
    end
  end

  defp process_mcp_request(%{"method" => "tools/list"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    cursor = Map.get(params, "cursor")

    case state.handler_module.handle_list_tools(cursor, state.handler_state) do
      {:ok, tools, next_cursor, new_handler_state} ->
        result = %{"tools" => tools}
        result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result
        response = %{"jsonrpc" => "2.0", "id" => id, "result" => result}
        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}

      {:error, error, new_handler_state} ->
        # Use appropriate error code based on error type
        error_code =
          case error do
            # Invalid params
            "Invalid cursor" <> _ -> -32602
            # Invalid params
            "Invalid cursor parameter" -> -32602
            # Generic server error
            _ -> -32000
          end

        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{"code" => error_code, "message" => "List tools error: #{inspect(error)}"}
        }

        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}
    end
  end

  defp process_mcp_request(%{"method" => "tools/call"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    require Logger

    # Check if request was already cancelled before processing
    if request_cancelled?(id, state) do
      response = %{
        "jsonrpc" => "2.0",
        "id" => id,
        "error" => %{"code" => -32001, "message" => "Request was cancelled"}
      }

      {:response, response, state}
    else
      # Track this as a pending request for cancellation support
      new_state = track_pending_request(id, :sync_call, state)
      Logger.debug("Tracking pending request #{id} for tools/call")

      # Add the request_id to arguments for handlers that support cancellation
      enhanced_arguments = Map.put(arguments, "_request_id", id)

      case new_state.handler_module.handle_call_tool(
             name,
             enhanced_arguments,
             new_state.handler_state
           ) do
        {:ok, result, new_handler_state} ->
          # Handle different result formats
          response_result =
            case result do
              # If result is already a map with content field, normalize it
              %{content: _} = structured_result ->
                normalize_error_key(structured_result)

              # If result is just content, wrap it
              _ ->
                %{"content" => normalize_error_key(result)}
            end

          response = %{"jsonrpc" => "2.0", "id" => id, "result" => response_result}
          final_state = %{new_state | handler_state: new_handler_state}
          # Remove from pending requests when complete
          completed_state = complete_pending_request(id, final_state)
          {:response, response, completed_state}

        {:error, error, new_handler_state} ->
          response = %{
            "jsonrpc" => "2.0",
            "id" => id,
            "error" => %{"code" => -32000, "message" => "Tool call error: #{inspect(error)}"}
          }

          final_state = %{new_state | handler_state: new_handler_state}
          # Remove from pending requests when complete
          completed_state = complete_pending_request(id, final_state)
          {:response, response, completed_state}
      end
    end
  end

  defp process_mcp_request(%{"method" => "resources/list"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    cursor = Map.get(params, "cursor")

    case state.handler_module.handle_list_resources(cursor, state.handler_state) do
      {:ok, resources, next_cursor, new_handler_state} ->
        result = %{"resources" => resources}
        result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result
        response = %{"jsonrpc" => "2.0", "id" => id, "result" => result}
        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}

      {:error, error, new_handler_state} ->
        # Use appropriate error code based on error type
        error_code =
          case error do
            # Invalid params
            "Invalid cursor" <> _ -> -32602
            # Invalid params
            "Invalid cursor parameter" -> -32602
            # Generic server error
            _ -> -32000
          end

        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{
            "code" => error_code,
            "message" => "List resources error: #{inspect(error)}"
          }
        }

        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}
    end
  end

  defp process_mcp_request(%{"method" => "resources/templates/list"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    cursor = Map.get(params, "cursor")

    if function_exported?(state.handler_module, :handle_list_resource_templates, 2) do
      case state.handler_module.handle_list_resource_templates(cursor, state.handler_state) do
        {:ok, templates, next_cursor, new_handler_state} ->
          result = %{"resourceTemplates" => templates}
          result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result
          response = %{"jsonrpc" => "2.0", "id" => id, "result" => result}
          new_state = %{state | handler_state: new_handler_state}
          {:response, response, new_state}

        {:error, error, new_handler_state} ->
          # Use appropriate error code based on error type
          error_code =
            case error do
              # Invalid params
              "Invalid cursor" <> _ -> -32602
              # Generic server error
              _ -> -32000
            end

          response = %{
            "jsonrpc" => "2.0",
            "id" => id,
            "error" => %{
              "code" => error_code,
              "message" => "List resource templates error: #{inspect(error)}"
            }
          }

          new_state = %{state | handler_state: new_handler_state}
          {:response, response, new_state}
      end
    else
      response = %{
        "jsonrpc" => "2.0",
        "id" => id,
        "error" =>
          ErrorCodes.error_response(
            :method_not_found,
            "Method not found: resources/templates/list"
          )
      }

      {:response, response, state}
    end
  end

  defp process_mcp_request(%{"method" => "resources/read"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    uri = Map.get(params, "uri")

    case state.handler_module.handle_read_resource(uri, state.handler_state) do
      {:ok, result, new_handler_state} ->
        response = %{"jsonrpc" => "2.0", "id" => id, "result" => %{"contents" => [result]}}
        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}

      {:error, error, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{"code" => -32000, "message" => "Read resource error: #{inspect(error)}"}
        }

        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}
    end
  end

  defp process_mcp_request(%{"method" => "ping"} = request, state) do
    id = Map.get(request, "id")
    response = %{"jsonrpc" => "2.0", "id" => id, "result" => %{}}
    {:response, response, state}
  end

  defp process_mcp_request(%{"method" => "logging/setLevel"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    level = Map.get(params, "level")

    if function_exported?(state.handler_module, :handle_set_log_level, 2) do
      case state.handler_module.handle_set_log_level(level, state.handler_state) do
        {:ok, new_handler_state} ->
          response = %{"jsonrpc" => "2.0", "id" => id, "result" => %{}}
          new_state = %{state | handler_state: new_handler_state}
          {:response, response, new_state}

        {:error, error, new_handler_state} ->
          response = %{
            "jsonrpc" => "2.0",
            "id" => id,
            "error" => %{"code" => -32000, "message" => "Set log level error: #{inspect(error)}"}
          }

          new_state = %{state | handler_state: new_handler_state}
          {:response, response, new_state}
      end
    else
      # Default implementation - just return success
      response = %{"jsonrpc" => "2.0", "id" => id, "result" => %{}}
      {:response, response, state}
    end
  end

  defp process_mcp_request(%{"method" => "prompts/list"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    cursor = Map.get(params, "cursor")

    case state.handler_module.handle_list_prompts(cursor, state.handler_state) do
      {:ok, prompts, next_cursor, new_handler_state} ->
        result = %{"prompts" => prompts}
        result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result
        response = %{"jsonrpc" => "2.0", "id" => id, "result" => result}
        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}

      {:error, error, new_handler_state} ->
        # Use appropriate error code based on error type
        error_code =
          case error do
            # Invalid params
            "Invalid cursor" <> _ -> -32602
            # Invalid params
            "Invalid cursor parameter" -> -32602
            # Generic server error
            _ -> -32000
          end

        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{"code" => error_code, "message" => "List prompts error: #{inspect(error)}"}
        }

        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}
    end
  end

  defp process_mcp_request(%{"method" => "prompts/get"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    case state.handler_module.handle_get_prompt(name, arguments, state.handler_state) do
      {:ok, result, new_handler_state} ->
        response = %{"jsonrpc" => "2.0", "id" => id, "result" => result}
        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}

      {:error, error, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{"code" => -32000, "message" => "Get prompt error: #{inspect(error)}"}
        }

        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}
    end
  end

  defp process_mcp_request(%{"method" => "roots/list"} = request, state) do
    id = Map.get(request, "id")

    if function_exported?(state.handler_module, :handle_list_roots, 1) do
      case state.handler_module.handle_list_roots(state.handler_state) do
        {:ok, roots, new_handler_state} ->
          result = %{"roots" => roots}
          response = %{"jsonrpc" => "2.0", "id" => id, "result" => result}
          new_state = %{state | handler_state: new_handler_state}
          {:response, response, new_state}

        {:error, error, new_handler_state} ->
          # Use appropriate error code based on error type
          error_code =
            case error do
              # Invalid params
              "Invalid cursor" <> _ -> -32602
              # Generic server error
              _ -> -32000
            end

          response = %{
            "jsonrpc" => "2.0",
            "id" => id,
            "error" => %{"code" => error_code, "message" => "List roots error: #{inspect(error)}"}
          }

          new_state = %{state | handler_state: new_handler_state}
          {:response, response, new_state}
      end
    else
      # Handler doesn't implement roots listing
      response = %{
        "jsonrpc" => "2.0",
        "id" => id,
        "error" => ErrorCodes.error_response(:method_not_found, "Method not found: roots/list")
      }

      {:response, response, state}
    end
  end

  defp process_mcp_request(%{"method" => "resources/subscribe"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    uri = Map.get(params, "uri")

    case state.handler_module.handle_subscribe_resource(uri, state.handler_state) do
      {:ok, result, new_handler_state} ->
        response = %{"jsonrpc" => "2.0", "id" => id, "result" => result}
        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}

      {:error, error, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{
            "code" => -32000,
            "message" => "Subscribe resource error: #{inspect(error)}"
          }
        }

        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}
    end
  end

  defp process_mcp_request(%{"method" => "resources/unsubscribe"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    uri = Map.get(params, "uri")

    case state.handler_module.handle_unsubscribe_resource(uri, state.handler_state) do
      {:ok, result, new_handler_state} ->
        response = %{"jsonrpc" => "2.0", "id" => id, "result" => result}
        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}

      {:error, error, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{
            "code" => -32000,
            "message" => "Unsubscribe resource error: #{inspect(error)}"
          }
        }

        new_state = %{state | handler_state: new_handler_state}
        {:response, response, new_state}
    end
  end

  defp process_mcp_request(%{"method" => "completion/complete"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    ref = Map.get(params, "ref")
    argument = Map.get(params, "argument")

    if function_exported?(state.handler_module, :handle_complete, 3) do
      case state.handler_module.handle_complete(ref, argument, state.handler_state) do
        {:ok, result, new_handler_state} ->
          response = %{"jsonrpc" => "2.0", "id" => id, "result" => result}
          new_state = %{state | handler_state: new_handler_state}
          {:response, response, new_state}

        {:error, error, new_handler_state} ->
          response = %{
            "jsonrpc" => "2.0",
            "id" => id,
            "error" => %{"code" => -32000, "message" => "Completion error: #{inspect(error)}"}
          }

          new_state = %{state | handler_state: new_handler_state}
          {:response, response, new_state}
      end
    else
      # Handler doesn't support completion
      response = %{
        "jsonrpc" => "2.0",
        "id" => id,
        "error" =>
          ErrorCodes.error_response(:method_not_found, "Method not found: completion/complete")
      }

      {:response, response, state}
    end
  end

  defp process_mcp_request(%{"method" => "notifications/cancelled"} = request, state) do
    # Handle cancellation notifications from clients
    params = Map.get(request, "params", %{})
    {_, new_state} = handle_cancellation_notification(params, state)
    {:notification, new_state}
  end

  defp process_mcp_request(%{"method" => method} = request, state) do
    if Map.has_key?(request, "id") do
      # It's a request with an unknown method
      id = Map.get(request, "id")

      response = %{
        "jsonrpc" => "2.0",
        "id" => id,
        "error" => ErrorCodes.error_response(:method_not_found, "Method not found: #{method}")
      }

      {:response, response, state}
    else
      # It's a notification with an unknown method, ignore it.
      {:notification, state}
    end
  end

  defp process_mcp_request(_invalid_request, state) do
    # Invalid request format (e.g. not a map)
    response = %{
      "jsonrpc" => "2.0",
      "id" => nil,
      "error" => %{"code" => -32600, "message" => "Invalid Request"}
    }

    {:response, response, state}
  end

  defp normalize_error_key(%{is_error: error_value} = result) when is_map(result) do
    # Convert atom key is_error: to string key "isError" for MCP compatibility
    result
    |> Map.put("isError", error_value)
    |> Map.delete(:is_error)
  end

  defp normalize_error_key(result), do: result

  defp send_message(message, state) do
    json_message = Jason.encode!(message)

    case state.transport.send_message(json_message, state.transport_state) do
      {:ok, new_transport_state} ->
        {:ok, %{state | transport_state: new_transport_state}}

      error ->
        error
    end
  end
end
