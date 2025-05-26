defmodule ExMCP.Server do
  @moduledoc """
  MCP server implementation.

  This module handles the protocol layer for MCP servers, delegating
  the actual functionality to a handler module that implements the
  `ExMCP.Server.Handler` behaviour.

  ## Features

  - Multiple transport support (stdio, SSE, BEAM)
  - Tool, resource, and prompt management
  - Progress notifications for long operations
  - Change notifications for dynamic content
  - Sampling/LLM integration support

  ## Example

      defmodule MyHandler do
        use ExMCP.Server.Handler
        
        @impl true
        def handle_initialize(_params, state) do
          {:ok, %{
            name: "my-server", 
            version: "1.0.0",
            capabilities: %{
              tools: %{},
              resources: %{},
              prompts: %{},
              sampling: %{}  # Enable LLM features
            }
          }, state}
        end
        
        @impl true
        def handle_call_tool("process", params, state) do
          # Use progress notifications for long operations
          if progress_token = params["_progressToken"] do
            Task.start(fn ->
              for i <- 1..100 do
                ExMCP.Server.notify_progress(self(), progress_token, i, 100)
                Process.sleep(100)
              end
            end)
          end
          
          {:ok, [%{type: "text", text: "Processing..."}], state}
        end
        
        # Implement sampling for LLM integration
        @impl true
        def handle_create_message(params, state) do
          # Your LLM integration here
          {:ok, %{content: %{type: "text", text: "Response"}}, state}
        end
      end
      
      # Start server
      {:ok, server} = ExMCP.Server.start_link(
        handler: MyHandler,
        transport: :stdio  # or :beam, :sse
      )

  ## Notifications

  The server can send various notifications to connected clients:

      # Notify about resource changes
      ExMCP.Server.notify_resources_changed(server)
      ExMCP.Server.notify_resource_updated(server, "file:///path")
      
      # Notify about tool changes
      ExMCP.Server.notify_tools_changed(server)
      
      # Notify about prompt changes  
      ExMCP.Server.notify_prompts_changed(server)
      
      # Notify about roots changes
      ExMCP.Server.notify_roots_changed(server)
      
      # Send progress updates
      ExMCP.Server.notify_progress(server, "token", 50, 100)
  """

  use GenServer
  require Logger

  alias ExMCP.{Protocol, Transport}
  alias ExMCP.Transport.Beam, as: BeamTransport

  defstruct [
    :handler,
    :handler_state,
    :transport_mod,
    :transport_state,
    :initialized
  ]

  @doc """
  Starts an MCP server.

  ## Options

  - `:handler` - Module implementing `ExMCP.Server.Handler` behaviour (required)
  - `:transport` - Transport type (:stdio, :sse, or module)
  - `:handler_args` - Arguments passed to handler's init/1 callback
  - `:name` - GenServer name (optional)

  Transport-specific options are passed through.
  """
  def start_link(opts) do
    {name, opts} = Keyword.pop(opts, :name)
    gen_opts = if name, do: [name: name], else: []
    GenServer.start_link(__MODULE__, opts, gen_opts)
  end

  @doc """
  Notifies connected clients that the resources list has changed.
  """
  def notify_resources_changed(server) do
    GenServer.cast(server, :notify_resources_changed)
  end

  @doc """
  Notifies connected clients that a specific resource has been updated.
  """
  def notify_resource_updated(server, uri) do
    GenServer.cast(server, {:notify_resource_updated, uri})
  end

  @doc """
  Notifies connected clients that the tools list has changed.
  """
  def notify_tools_changed(server) do
    GenServer.cast(server, :notify_tools_changed)
  end

  @doc """
  Notifies connected clients that the prompts list has changed.
  """
  def notify_prompts_changed(server) do
    GenServer.cast(server, :notify_prompts_changed)
  end

  @doc """
  Notifies connected clients that the roots list has changed.
  """
  def notify_roots_changed(server) do
    GenServer.cast(server, :notify_roots_changed)
  end

  @doc """
  Sends a progress notification to connected clients.

  ## Parameters
  - `progress_token` - Token identifying the operation
  - `progress` - Current progress value (0-100 for percentage, or any positive number)
  - `total` - Optional total value for the operation
  """
  def notify_progress(server, progress_token, progress, total \\ nil) do
    GenServer.cast(server, {:notify_progress, progress_token, progress, total})
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    handler = Keyword.fetch!(opts, :handler)
    handler_args = Keyword.get(opts, :handler_args, [])
    transport_type = Keyword.fetch!(opts, :transport)
    transport_mod = Transport.get_transport(transport_type)

    with {:ok, handler_state} <- handler.init(handler_args),
         {:ok, transport_state} <- init_transport(transport_mod, transport_type, opts) do
      state = %__MODULE__{
        handler: handler,
        handler_state: handler_state,
        transport_mod: transport_mod,
        transport_state: transport_state,
        initialized: false
      }

      # Start receiving messages
      {:ok, state, {:continue, :start_receiver}}
    else
      {:error, reason} ->
        {:stop, reason}
    end
  end

  @impl true
  def handle_continue(:start_receiver, state) do
    start_receiver(state.transport_state, state.transport_mod)
    {:noreply, state}
  end

  @impl true
  def handle_info({:transport_message, message}, state) do
    case Protocol.parse_message(message) do
      {:request, method, params, id} ->
        handle_request(method, params, id, state)

      {:notification, method, params} ->
        handle_notification(method, params, state)

      _ ->
        Logger.warning("Invalid message received: #{inspect(message)}")
        {:noreply, state}
    end
  end

  def handle_info({:transport_closed, reason}, state) do
    Logger.info("Transport closed: #{inspect(reason)}")
    {:stop, :normal, state}
  end

  def handle_info({:beam_connect, client_mailbox}, state) do
    # Handle BEAM transport connection
    if state.transport_mod == BeamTransport do
      {:ok, new_transport_state} =
        BeamTransport.handle_connection_request(client_mailbox, state.transport_state)

      {:noreply, %{state | transport_state: new_transport_state}}
    else
      Logger.warning("Received BEAM connection request but not using BEAM transport")
      {:noreply, state}
    end
  end

  @impl true
  def handle_cast(:notify_resources_changed, state) do
    send_notification(Protocol.encode_resources_changed(), state)
  end

  def handle_cast({:notify_resource_updated, uri}, state) do
    send_notification(Protocol.encode_resource_updated(uri), state)
  end

  def handle_cast(:notify_tools_changed, state) do
    send_notification(Protocol.encode_tools_changed(), state)
  end

  def handle_cast(:notify_prompts_changed, state) do
    send_notification(Protocol.encode_prompts_changed(), state)
  end

  def handle_cast(:notify_roots_changed, state) do
    send_notification(Protocol.encode_roots_changed(), state)
  end

  def handle_cast({:notify_progress, progress_token, progress, total}, state) do
    send_notification(Protocol.encode_progress(progress_token, progress, total), state)
  end

  @impl true
  def terminate(reason, state) do
    state.handler.terminate(reason, state.handler_state)

    if state.transport_state do
      state.transport_mod.close(state.transport_state)
    end
  end

  # Private functions

  defp start_receiver(transport_state, transport_mod) do
    parent = self()

    spawn_link(fn ->
      receive_loop(transport_state, transport_mod, parent)
    end)
  end

  defp receive_loop(transport_state, transport_mod, parent) do
    case transport_mod.receive_message(transport_state) do
      {:ok, message, new_state} ->
        send(parent, {:transport_message, message})
        receive_loop(new_state, transport_mod, parent)

      {:error, reason} ->
        send(parent, {:transport_closed, reason})
    end
  end

  defp handle_request("initialize", params, id, state) do
    case state.handler.handle_initialize(params, state.handler_state) do
      {:ok, result, new_handler_state} ->
        response = Protocol.encode_response(result, id)
        send_message(response, %{state | handler_state: new_handler_state, initialized: true})

      {:error, reason, new_handler_state} ->
        error =
          Protocol.encode_error(
            Protocol.internal_error(),
            format_error(reason),
            nil,
            id
          )

        send_message(error, %{state | handler_state: new_handler_state})
    end
  end

  defp handle_request("tools/list", _params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_list_tools(state.handler_state) do
        {:ok, tools, new_handler_state} ->
          response = Protocol.encode_response(%{tools: tools}, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error =
            Protocol.encode_error(
              Protocol.internal_error(),
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("tools/call", %{"name" => name, "arguments" => args}, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_call_tool(name, args, state.handler_state) do
        {:ok, result, new_handler_state} ->
          response = Protocol.encode_response(%{content: result}, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error =
            Protocol.encode_error(
              Protocol.internal_error(),
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("resources/list", _params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_list_resources(state.handler_state) do
        {:ok, resources, new_handler_state} ->
          response = Protocol.encode_response(%{resources: resources}, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error =
            Protocol.encode_error(
              Protocol.internal_error(),
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("resources/read", %{"uri" => uri}, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_read_resource(uri, state.handler_state) do
        {:ok, content, new_handler_state} ->
          response = Protocol.encode_response(%{contents: [content]}, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error =
            Protocol.encode_error(
              Protocol.internal_error(),
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("prompts/list", _params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_list_prompts(state.handler_state) do
        {:ok, prompts, new_handler_state} ->
          response = Protocol.encode_response(%{prompts: prompts}, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error =
            Protocol.encode_error(
              Protocol.internal_error(),
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("prompts/get", %{"name" => name} = params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      arguments = Map.get(params, "arguments", %{})

      case state.handler.handle_get_prompt(name, arguments, state.handler_state) do
        {:ok, messages, new_handler_state} ->
          response = Protocol.encode_response(%{messages: messages}, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error =
            Protocol.encode_error(
              Protocol.internal_error(),
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("sampling/createMessage", params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_create_message(params, state.handler_state) do
        {:ok, result, new_handler_state} ->
          response = Protocol.encode_response(result, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error =
            Protocol.encode_error(
              Protocol.internal_error(),
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("roots/list", _params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_list_roots(state.handler_state) do
        {:ok, roots, new_handler_state} ->
          response = Protocol.encode_response(%{roots: roots}, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error =
            Protocol.encode_error(
              Protocol.internal_error(),
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("resources/subscribe", %{"uri" => uri} = _params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_subscribe_resource(uri, state.handler_state) do
        {:ok, result, new_handler_state} ->
          response = Protocol.encode_response(result, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error =
            Protocol.encode_error(
              Protocol.internal_error(),
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("resources/unsubscribe", %{"uri" => uri} = _params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_unsubscribe_resource(uri, state.handler_state) do
        {:ok, result, new_handler_state} ->
          response = Protocol.encode_response(result, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error =
            Protocol.encode_error(
              Protocol.internal_error(),
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("resources/templates/list", _params, id, state) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_list_resource_templates(state.handler_state) do
        {:ok, templates, new_handler_state} ->
          response = Protocol.encode_response(%{resourceTemplates: templates}, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error =
            Protocol.encode_error(
              Protocol.internal_error(),
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request("ping", _params, id, state) do
    # Ping always responds with empty result
    response = Protocol.encode_pong(id)
    send_message(response, state)
  end

  defp handle_request(
         "completion/complete",
         %{"ref" => ref, "argument" => argument} = _params,
         id,
         state
       ) do
    if not state.initialized do
      error =
        Protocol.encode_error(
          Protocol.invalid_request(),
          "Not initialized",
          nil,
          id
        )

      send_message(error, state)
    else
      case state.handler.handle_complete(ref, argument, state.handler_state) do
        {:ok, result, new_handler_state} ->
          response = Protocol.encode_response(result, id)
          send_message(response, %{state | handler_state: new_handler_state})

        {:error, reason, new_handler_state} ->
          error =
            Protocol.encode_error(
              Protocol.internal_error(),
              format_error(reason),
              nil,
              id
            )

          send_message(error, %{state | handler_state: new_handler_state})
      end
    end
  end

  defp handle_request(method, _params, id, state) do
    error =
      Protocol.encode_error(
        Protocol.method_not_found(),
        "Method not found: #{method}",
        nil,
        id
      )

    send_message(error, state)
  end

  defp handle_notification("notifications/initialized", _params, state) do
    Logger.debug("Client initialized")
    {:noreply, state}
  end

  defp handle_notification(
         "notifications/cancelled",
         %{"requestId" => request_id} = params,
         state
       ) do
    reason = Map.get(params, "reason")
    Logger.info("Request #{request_id} cancelled: #{reason || "no reason given"}")
    # TODO: Actually cancel the request if it's still running
    {:noreply, state}
  end

  defp handle_notification("notifications/log", params, state) do
    level = Map.get(params, "level", "info")
    message = Map.get(params, "message", "")
    data = Map.get(params, "data")

    log_data = if data, do: " - #{inspect(data)}", else: ""

    case level do
      "debug" -> Logger.debug("Client: #{message}#{log_data}")
      "info" -> Logger.info("Client: #{message}#{log_data}")
      "notice" -> Logger.info("Client: #{message}#{log_data}")
      "warning" -> Logger.warning("Client: #{message}#{log_data}")
      "error" -> Logger.error("Client: #{message}#{log_data}")
      "critical" -> Logger.error("Client [CRITICAL]: #{message}#{log_data}")
      "alert" -> Logger.error("Client [ALERT]: #{message}#{log_data}")
      "emergency" -> Logger.error("Client [EMERGENCY]: #{message}#{log_data}")
      _ -> Logger.info("Client [#{level}]: #{message}#{log_data}")
    end

    {:noreply, state}
  end

  defp handle_notification(method, params, state) do
    Logger.warning("Unhandled notification: #{method} with params: #{inspect(params)}")
    {:noreply, state}
  end

  defp send_message(message, state) do
    case Protocol.encode_to_string(message) do
      {:ok, json} ->
        case state.transport_mod.send_message(json, state.transport_state) do
          {:ok, new_transport_state} ->
            {:noreply, %{state | transport_state: new_transport_state}}

          {:error, reason} ->
            Logger.error("Failed to send message: #{inspect(reason)}")
            {:noreply, state}
        end

      {:error, reason} ->
        Logger.error("Failed to encode message: #{inspect(reason)}")
        {:noreply, state}
    end
  end

  defp send_notification(notification, state) do
    if state.initialized do
      send_message(notification, state)
    else
      {:noreply, state}
    end
  end

  defp format_error(reason) when is_binary(reason), do: reason
  defp format_error(reason), do: inspect(reason)

  defp init_transport(transport_mod, transport_type, opts) do
    # Special handling for BEAM transport on server side
    if transport_type == :beam do
      transport_mod.accept(opts)
    else
      transport_mod.connect(opts)
    end
  end
end
