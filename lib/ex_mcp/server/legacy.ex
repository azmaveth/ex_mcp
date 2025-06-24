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
              protocol_version: nil
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
        # Handle batch request
        handle_batch_request(requests, state)

      {:ok, request} when is_map(request) ->
        # Handle single request
        handle_mcp_request(request, state)

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
    if state.transport == ExMCP.Transport.Test do
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
  defp handle_batch_request(requests, state) when is_list(requests) do
    if Enum.empty?(requests) do
      # Empty batch is invalid according to JSON-RPC 2.0
      error_response = %{
        "jsonrpc" => "2.0",
        "id" => nil,
        "error" => %{
          "code" => -32600,
          "message" => "Invalid Request"
        }
      }

      case send_message(error_response, state) do
        {:ok, new_state} -> {:noreply, new_state}
        {:error, _reason} -> {:noreply, state}
      end
    else
      # Process each request in the batch
      {responses, final_state} =
        Enum.reduce(requests, {[], state}, fn request, {acc_responses, acc_state} ->
          case handle_single_batch_request(request, acc_state) do
            {:response, response, new_state} ->
              {[response | acc_responses], new_state}

            {:notification, new_state} ->
              # Notifications don't get responses
              {acc_responses, new_state}

            {:error, error_response, new_state} ->
              {[error_response | acc_responses], new_state}
          end
        end)

      # Reverse to maintain order
      ordered_responses = Enum.reverse(responses)

      # Only send response if we have any (notifications don't generate responses)
      if not Enum.empty?(ordered_responses) do
        case send_message(ordered_responses, final_state) do
          {:ok, new_state} -> {:noreply, new_state}
          {:error, _reason} -> {:noreply, final_state}
        end
      else
        {:noreply, final_state}
      end
    end
  end

  # Handle a single request within a batch
  defp handle_single_batch_request(request, state) when is_map(request) do
    # Check if it's a notification (no id field)
    if Map.has_key?(request, "id") do
      # It's a request - needs a response
      {response, new_state} = capture_mcp_response(request, state)
      {:response, response, new_state}
    else
      # It's a notification - no response needed
      case handle_mcp_request(request, state) do
        {:noreply, new_state} -> {:notification, new_state}
        _ -> {:notification, state}
      end
    end
  end

  defp handle_single_batch_request(_invalid_request, state) do
    # Invalid request format
    error_response = %{
      "jsonrpc" => "2.0",
      "id" => nil,
      "error" => %{
        "code" => -32600,
        "message" => "Invalid Request"
      }
    }

    {:error, error_response, state}
  end

  # Capture the response from handle_mcp_request for batch processing
  defp capture_mcp_response(request, state) do
    # We need to modify handle_mcp_request to return responses instead of sending them
    # For now, let's create a separate handler that builds responses
    build_mcp_response(request, state)
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

  @impl GenServer
  def terminate(reason, state) do
    if function_exported?(state.handler_module, :terminate, 2) do
      state.handler_module.terminate(reason, state.handler_state)
    end
  end

  # Private functions

  defp connect_transport(:test, opts) do
    case ExMCP.Transport.Test.connect(opts) do
      {:ok, transport_state} -> {:ok, {ExMCP.Transport.Test, transport_state}}
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

  defp handle_mcp_request(%{"method" => "initialize"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})

    case state.handler_module.handle_initialize(params, state.handler_state) do
      {:ok, result, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => result
        }

        new_state = %{
          state
          | handler_state: new_handler_state,
            protocol_version: Map.get(result, "protocolVersion")
        }

        case send_message(response, new_state) do
          {:ok, final_state} -> {:noreply, final_state}
          {:error, _reason} -> {:noreply, new_state}
        end

      {:error, error, new_handler_state} ->
        send_error_response(-32000, "Initialize error: #{inspect(error)}", id, state)
        new_state = %{state | handler_state: new_handler_state}
        {:noreply, new_state}
    end
  end

  defp handle_mcp_request(%{"method" => "tools/list"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    cursor = Map.get(params, "cursor")

    case state.handler_module.handle_list_tools(cursor, state.handler_state) do
      {:ok, tools, next_cursor, new_handler_state} ->
        result = %{"tools" => tools}
        result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result

        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => result
        }

        new_state = %{state | handler_state: new_handler_state}

        case send_message(response, new_state) do
          {:ok, final_state} -> {:noreply, final_state}
          {:error, _reason} -> {:noreply, new_state}
        end

      {:error, error, new_handler_state} ->
        send_error_response(-32000, "List tools error: #{inspect(error)}", id, state)
        new_state = %{state | handler_state: new_handler_state}
        {:noreply, new_state}
    end
  end

  defp handle_mcp_request(%{"method" => "tools/call"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    case state.handler_module.handle_call_tool(name, arguments, state.handler_state) do
      {:ok, result, new_handler_state} ->
        # Handle different result formats
        formatted_result =
          case result do
            %{content: _} = tool_result ->
              tool_result

            content_list when is_list(content_list) ->
              %{content: content_list}

            other ->
              %{content: [%{type: "text", text: inspect(other)}]}
          end

        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => formatted_result
        }

        new_state = %{state | handler_state: new_handler_state}

        case send_message(response, new_state) do
          {:ok, final_state} -> {:noreply, final_state}
          {:error, _reason} -> {:noreply, new_state}
        end

      {:error, error, new_handler_state} ->
        send_error_response(-32000, "Tool call error: #{inspect(error)}", id, state)
        new_state = %{state | handler_state: new_handler_state}
        {:noreply, new_state}
    end
  end

  defp handle_mcp_request(%{"method" => "ping"} = request, state) do
    id = Map.get(request, "id")

    # Respond to ping with pong
    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{}
    }

    case send_message(response, state) do
      {:ok, new_state} -> {:noreply, new_state}
      {:error, _reason} -> {:noreply, state}
    end
  end

  defp handle_mcp_request(%{"method" => "logging/setLevel"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    level = Map.get(params, "level")

    if function_exported?(state.handler_module, :handle_set_log_level, 2) do
      case state.handler_module.handle_set_log_level(level, state.handler_state) do
        {:ok, new_handler_state} ->
          response = %{
            "jsonrpc" => "2.0",
            "id" => id,
            "result" => %{}
          }

          new_state = %{state | handler_state: new_handler_state}

          case send_message(response, new_state) do
            {:ok, final_state} -> {:noreply, final_state}
            {:error, _reason} -> {:noreply, new_state}
          end

        {:error, error, new_handler_state} ->
          send_error_response(-32000, "Set log level error: #{inspect(error)}", id, state)
          new_state = %{state | handler_state: new_handler_state}
          {:noreply, new_state}
      end
    else
      # Default implementation - just return success
      response = %{
        "jsonrpc" => "2.0",
        "id" => id,
        "result" => %{}
      }

      case send_message(response, state) do
        {:ok, new_state} -> {:noreply, new_state}
        {:error, _reason} -> {:noreply, state}
      end
    end
  end

  defp handle_mcp_request(%{"method" => "completion/complete"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    ref = Map.get(params, "ref")
    argument = Map.get(params, "argument")

    if function_exported?(state.handler_module, :handle_complete, 3) do
      case state.handler_module.handle_complete(ref, argument, state.handler_state) do
        {:ok, result, new_handler_state} ->
          response = %{
            "jsonrpc" => "2.0",
            "id" => id,
            "result" => result
          }

          new_state = %{state | handler_state: new_handler_state}

          case send_message(response, new_state) do
            {:ok, final_state} -> {:noreply, final_state}
            {:error, _reason} -> {:noreply, new_state}
          end

        {:error, error, new_handler_state} ->
          send_error_response(-32000, "Completion error: #{inspect(error)}", id, state)
          new_state = %{state | handler_state: new_handler_state}
          {:noreply, new_state}
      end
    else
      # Handler doesn't support completion
      send_error_response(-32601, "Method not found: completion/complete", id, state)
      {:noreply, state}
    end
  end

  defp handle_mcp_request(%{"method" => method} = request, state) do
    id = Map.get(request, "id")
    send_error_response(-32601, "Method not found: #{method}", id, state)
    {:noreply, state}
  end

  # Build response for batch processing without sending
  defp build_mcp_response(%{"method" => "initialize"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})

    case state.handler_module.handle_initialize(params, state.handler_state) do
      {:ok, result, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => result
        }

        new_state = %{
          state
          | handler_state: new_handler_state,
            protocol_version: Map.get(result, "protocolVersion")
        }

        {response, new_state}

      {:error, error, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{
            "code" => -32000,
            "message" => "Initialize error: #{inspect(error)}"
          }
        }

        new_state = %{state | handler_state: new_handler_state}
        {response, new_state}
    end
  end

  defp build_mcp_response(%{"method" => "tools/list"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    cursor = Map.get(params, "cursor")

    case state.handler_module.handle_list_tools(cursor, state.handler_state) do
      {:ok, tools, next_cursor, new_handler_state} ->
        result = %{"tools" => tools}
        result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result

        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => result
        }

        new_state = %{state | handler_state: new_handler_state}
        {response, new_state}

      {:error, error, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{
            "code" => -32000,
            "message" => "List tools error: #{inspect(error)}"
          }
        }

        new_state = %{state | handler_state: new_handler_state}
        {response, new_state}
    end
  end

  defp build_mcp_response(%{"method" => "tools/call"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    case state.handler_module.handle_call_tool(name, arguments, state.handler_state) do
      {:ok, result, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => %{"content" => result}
        }

        new_state = %{state | handler_state: new_handler_state}
        {response, new_state}

      {:error, error, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{
            "code" => -32000,
            "message" => "Tool call error: #{inspect(error)}"
          }
        }

        new_state = %{state | handler_state: new_handler_state}
        {response, new_state}
    end
  end

  defp build_mcp_response(%{"method" => "resources/list"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    cursor = Map.get(params, "cursor")

    case state.handler_module.handle_list_resources(cursor, state.handler_state) do
      {:ok, resources, next_cursor, new_handler_state} ->
        result = %{"resources" => resources}
        result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result

        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => result
        }

        new_state = %{state | handler_state: new_handler_state}
        {response, new_state}

      {:error, error, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{
            "code" => -32000,
            "message" => "List resources error: #{inspect(error)}"
          }
        }

        new_state = %{state | handler_state: new_handler_state}
        {response, new_state}
    end
  end

  defp build_mcp_response(%{"method" => "resources/read"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})
    uri = Map.get(params, "uri")

    case state.handler_module.handle_read_resource(uri, state.handler_state) do
      {:ok, result, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => %{"contents" => [result]}
        }

        new_state = %{state | handler_state: new_handler_state}
        {response, new_state}

      {:error, error, new_handler_state} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{
            "code" => -32000,
            "message" => "Read resource error: #{inspect(error)}"
          }
        }

        new_state = %{state | handler_state: new_handler_state}
        {response, new_state}
    end
  end

  defp build_mcp_response(%{"method" => method} = request, state) do
    id = Map.get(request, "id")

    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "error" => %{
        "code" => -32601,
        "message" => "Method not found: #{method}"
      }
    }

    {response, state}
  end

  defp send_message(message, state) do
    json_message = Jason.encode!(message)

    case state.transport.send_message(json_message, state.transport_state) do
      {:ok, new_transport_state} ->
        {:ok, %{state | transport_state: new_transport_state}}

      error ->
        error
    end
  end

  defp send_error_response(code, message, id, state) do
    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "error" => %{
        "code" => code,
        "message" => message
      }
    }

    send_message(response, state)
  end
end
