defmodule ExMCP.Server do
  @moduledoc """
  MCP server implementation.

  This module handles the protocol layer for MCP servers, delegating
  the actual functionality to a handler module that implements the
  `ExMCP.Server.Handler` behaviour.

  ## Example

      defmodule MyHandler do
        use ExMCP.Server.Handler
        
        @impl true
        def handle_initialize(_params, state) do
          {:ok, %{
            server_info: %{name: "my-server", version: "1.0.0"},
            capabilities: %{tools: %{}}
          }, state}
        end
        
        # ... implement other callbacks ...
      end
      
      # Start server on stdio
      {:ok, server} = ExMCP.Server.start_link(
        handler: MyHandler,
        transport: :stdio
      )
  """

  use GenServer
  require Logger

  alias ExMCP.{Protocol, Transport}

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

  # GenServer callbacks

  @impl true
  def init(opts) do
    handler = Keyword.fetch!(opts, :handler)
    handler_args = Keyword.get(opts, :handler_args, [])
    transport_type = Keyword.fetch!(opts, :transport)
    transport_mod = Transport.get_transport(transport_type)

    with {:ok, handler_state} <- handler.init(handler_args),
         {:ok, transport_state} <- transport_mod.connect(opts) do
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

  defp handle_notification(method, params, state) do
    Logger.debug("Received notification: #{method} #{inspect(params)}")
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

  defp format_error(reason) when is_binary(reason), do: reason
  defp format_error(reason), do: inspect(reason)
end
