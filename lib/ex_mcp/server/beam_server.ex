defmodule ExMCP.Server.BeamServer do
  @moduledoc """
  BEAM transport server for MCP protocol.

  This server handles the MCP protocol layer for BEAM/native transport,
  routing requests to services registered with ExMCP.Native.
  """

  use GenServer
  require Logger

  alias ExMCP.Internal.Protocol
  alias ExMCP.Transport.Local

  defstruct [
    :transport_state,
    :handler_module,
    :handler_state,
    :server_info,
    :protocol_version,
    :capabilities,
    :initialized
  ]

  @doc """
  Starts a BEAM server with the given options.

  Options:
  - `:handler` - The handler module (required)
  - `:name` - Process name (optional)
  - `:transport` - Transport options (optional)
  """
  def start_link(opts) do
    name = Keyword.get(opts, :name)
    gen_opts = if name, do: [name: name], else: []
    GenServer.start_link(__MODULE__, opts, gen_opts)
  end

  @impl true
  def init(opts) do
    handler_module = Keyword.fetch!(opts, :handler)
    transport_opts = Keyword.get(opts, :transport, [])

    # Initialize transport in server mode
    transport_opts =
      case transport_opts do
        opts when is_list(opts) -> Keyword.put(opts, :mode, :beam)
        atom when is_atom(atom) -> [type: atom, mode: :beam]
        _ -> [mode: :beam]
      end

    case Local.connect(transport_opts) do
      {:ok, transport_state} ->
        # Initialize handler with initial state from options or default
        init_arg = Keyword.get(opts, :handler_state, %{})

        case handler_module.init(init_arg) do
          {:ok, handler_state} ->
            state = %__MODULE__{
              transport_state: transport_state,
              handler_module: handler_module,
              handler_state: handler_state,
              server_info: %{
                "name" => to_string(handler_module),
                "version" => "1.0.0"
              },
              protocol_version: Application.get_env(:ex_mcp, :protocol_version, "2025-06-18"),
              capabilities: %{},
              initialized: false
            }

            # Start receiver loop
            Task.start_link(fn -> receive_loop(self(), transport_state) end)

            {:ok, state}

          {:error, reason} ->
            {:stop, {:handler_init_failed, reason}}
        end

      {:error, reason} ->
        {:stop, {:transport_connect_failed, reason}}
    end
  end

  @impl true
  def handle_info({:transport_message, message}, state) do
    # First try to decode the message as it might be a JSON string
    decoded =
      case Jason.decode(message) do
        {:ok, decoded_msg} -> decoded_msg
        {:error, _} -> message
      end

    # Check if decoded message is a batch (array)
    case decoded do
      messages when is_list(messages) ->
        # Handle batch request
        handle_batch_request(messages, state)

      _ ->
        # Handle single message - use the original message for parse_message
        case Protocol.parse_message(message) do
          {:request, method, params, id} ->
            handle_request(method, params, id, state)

          {:notification, method, params} ->
            handle_notification(method, params, state)

          {:error, :invalid_message} ->
            Logger.warning("Received invalid message format")
            {:noreply, state}

          {:error, :validation_failed, _error} ->
            Logger.warning("Message validation failed")
            {:noreply, state}

          _ ->
            {:noreply, state}
        end
    end
  end

  def handle_info({:test_transport_connect, client_pid}, state) do
    # Handle test transport connection
    new_transport_state = %{state.transport_state | server_pid: client_pid, connected: true}
    {:noreply, %{state | transport_state: new_transport_state}}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def handle_call(:get_notifications, _from, state) do
    # Forward to handler if it supports this
    if function_exported?(state.handler_module, :handle_call, 3) do
      case state.handler_module.handle_call(:get_notifications, nil, state.handler_state) do
        {:reply, notifications, new_handler_state} ->
          {:reply, notifications, %{state | handler_state: new_handler_state}}

        _ ->
          {:reply, [], state}
      end
    else
      {:reply, [], state}
    end
  end

  def handle_call(_request, _from, state) do
    {:reply, {:error, :not_implemented}, state}
  end

  # Private functions

  defp receive_loop(parent, transport_state) do
    case Local.receive_message(transport_state) do
      {:ok, message, new_state} when message != nil ->
        send(parent, {:transport_message, message})
        receive_loop(parent, new_state)

      {:ok, nil, new_state} ->
        # No message, continue
        receive_loop(parent, new_state)

      {:error, {:transport_error, _}} ->
        :ok

      {:error, {:connection_error, _}} ->
        :ok
    end
  end

  defp handle_request("initialize", _params, id, state) do
    result = %{
      "protocolVersion" => state.protocol_version,
      "capabilities" => state.capabilities,
      "serverInfo" => state.server_info
    }

    response = %{
      "jsonrpc" => "2.0",
      "result" => result,
      "id" => id
    }

    send_response(response, state)

    {:noreply, %{state | initialized: true}}
  end

  defp handle_request(method, params, id, state) do
    # Route to handler or service
    {result, new_state} =
      case method do
        "tools/list" ->
          handle_tools_list(state)

        "tools/call" ->
          handle_tools_call(params, state)

        _ ->
          # Try to route to service via Native
          case route_to_service(method, params, state) do
            {:ok, result} ->
              # Check if handler state was updated via Process dictionary
              new_handler_state =
                Process.get({:handler_state, state.handler_module}, state.handler_state)

              Process.delete({:handler_state, state.handler_module})
              {{:ok, result}, %{state | handler_state: new_handler_state}}

            {:error, reason} ->
              {{:error, reason}, state}
          end
      end

    response =
      case result do
        {:ok, data} ->
          %{
            "jsonrpc" => "2.0",
            "result" => data,
            "id" => id
          }

        {:error, error} ->
          %{
            "jsonrpc" => "2.0",
            "error" => error,
            "id" => id
          }
      end

    send_response(response, new_state)
    {:noreply, new_state}
  end

  defp handle_notification("notifications/initialized", _params, state) do
    {:noreply, state}
  end

  defp handle_notification(method, params, state) do
    # Route notifications to service and update handler state
    case route_to_service(method, params, state) do
      {:ok, _result} ->
        # Check if handler state was updated
        new_handler_state =
          Process.get({:handler_state, state.handler_module}, state.handler_state)

        Process.delete({:handler_state, state.handler_module})
        {:noreply, %{state | handler_state: new_handler_state}}

      _ ->
        {:noreply, state}
    end
  end

  defp handle_tools_list(state) do
    # Check if handler implements handle_mcp_request
    if function_exported?(state.handler_module, :handle_mcp_request, 3) do
      case state.handler_module.handle_mcp_request("tools/list", %{}, state.handler_state) do
        {:ok, result, new_handler_state} ->
          {{:ok, result}, %{state | handler_state: new_handler_state}}

        {:error, error, new_handler_state} ->
          {{:error, error}, %{state | handler_state: new_handler_state}}
      end
    else
      {{:ok, %{"tools" => []}}, state}
    end
  end

  defp handle_tools_call(%{"name" => tool_name, "arguments" => args}, state) do
    # Check if handler implements handle_mcp_request
    if function_exported?(state.handler_module, :handle_mcp_request, 3) do
      case state.handler_module.handle_mcp_request(
             "tools/call",
             %{"name" => tool_name, "arguments" => args},
             state.handler_state
           ) do
        {:ok, result, new_handler_state} ->
          {{:ok, result}, %{state | handler_state: new_handler_state}}

        {:error, error, new_handler_state} ->
          {{:error, error}, %{state | handler_state: new_handler_state}}
      end
    else
      {{:error, %{"code" => -32601, "message" => "Tool not found: #{tool_name}"}}, state}
    end
  end

  defp route_to_service(method, params, state) do
    # For notifications, we need to call the handler directly since they update state
    if function_exported?(state.handler_module, :handle_mcp_request, 3) do
      case state.handler_module.handle_mcp_request(method, params, state.handler_state) do
        {:ok, result, new_handler_state} ->
          # Update handler state for notifications
          Process.put({:handler_state, state.handler_module}, new_handler_state)
          {:ok, result}

        {:error, error, _new_state} ->
          {:error, error}
      end
    else
      {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}}
    end
  end

  defp send_response(response, state) do
    {:ok, encoded} = Protocol.encode_to_string(response)
    {:ok, new_transport_state} = Local.send_message(encoded, state.transport_state)
    %{state | transport_state: new_transport_state}
  end

  defp handle_batch_request(messages, state) when is_list(messages) do
    # Process each message in the batch, threading state through
    {responses, final_state} =
      Enum.map_reduce(messages, state, fn msg, current_state ->
        # Messages are already decoded maps
        case msg do
          %{"jsonrpc" => "2.0", "method" => method, "params" => params, "id" => id} ->
            # Get the result for this request
            {result, new_state} = process_request(method, params, current_state)

            # Format response
            response =
              case result do
                {:ok, data} ->
                  %{
                    "jsonrpc" => "2.0",
                    "result" => data,
                    "id" => id
                  }

                {:error, error} ->
                  %{
                    "jsonrpc" => "2.0",
                    "error" => error,
                    "id" => id
                  }
              end

            {response, new_state}

          _ ->
            # Invalid message in batch
            response = %{
              "jsonrpc" => "2.0",
              "error" => %{"code" => -32600, "message" => "Invalid Request"},
              "id" => Map.get(msg, "id", nil)
            }

            {response, current_state}
        end
      end)

    # Send batch response
    send_response(responses, final_state)
    {:noreply, final_state}
  end

  defp process_request(method, params, state) do
    case method do
      "tools/list" ->
        handle_tools_list(state)

      "tools/call" ->
        handle_tools_call(params, state)

      _ ->
        # Try to route to service via Native
        case route_to_service(method, params, state) do
          {:ok, result} ->
            # Check if handler state was updated via Process dictionary
            new_handler_state =
              Process.get({:handler_state, state.handler_module}, state.handler_state)

            Process.delete({:handler_state, state.handler_module})
            {{:ok, result}, %{state | handler_state: new_handler_state}}

          {:error, reason} ->
            {{:error, reason}, state}
        end
    end
  end
end
