defmodule ExMCP.Transport.Local do
  @moduledoc """
  Unified BEAM transport for ExMCP, supporting both raw terms and JSON.

  This module provides a high-performance transport for BEAM-based communication.
  It operates in two modes:

  - **`:native` mode**: Passes raw Elixir terms directly for maximum performance.
    This is ideal for trusted, local BEAM communication. It reports the
    `:raw_terms` capability.

  - **`:beam` mode**: Enforces MCP specification compliance by serializing all
    messages to/from JSON. This is safer for distributed or less-trusted
    scenarios.

  The mode is selected automatically by the `ExMCP.Client` based on the
  transport specified (`:native` or `:beam`).

  ## Features

  - Dual-mode operation (raw terms or JSON)
  - Automatic service discovery via Horde.Registry
  - Built-in fault tolerance
  - Low latency for local and cross-node calls

  ## Security Notice

  > ⚠️ **SECURITY WARNING**: The `:native` mode is designed ONLY for trusted,
  > local BEAM communication. It bypasses JSON validation. For distributed or
  > untrusted scenarios, the `:beam` mode should be used.

  ## Configuration

  This transport is configured via `ExMCP.Client.start_link/1`:

      # For native (raw term) mode
      {:ok, client} = ExMCP.Client.start_link(
        transport: :native,
        service_name: :my_service
      )

      # For beam (JSON) mode
      {:ok, client} = ExMCP.Client.start_link(
        transport: :beam,
        service_name: :my_service
      )

  Options:
  - `:service_name` - Required. The service to connect to.
  - `:timeout` - Optional. Call timeout in milliseconds (default: 5000).
  - `:node` - Optional. The specific node where the service is running.
  """

  @behaviour ExMCP.Transport

  require Logger

  defstruct [:service_name, :node, :timeout, :connected, :queue_agent, :mode]

  @type t :: %__MODULE__{
          service_name: atom(),
          node: node() | nil,
          timeout: pos_integer(),
          connected: boolean(),
          queue_agent: pid() | nil,
          mode: :beam | :native
        }

  @default_timeout 5_000

  @impl true
  def connect(opts) do
    service_name =
      Keyword.get(opts, :service_name) ||
        raise ArgumentError, "Missing required option :service_name"

    mode =
      Keyword.get(opts, :mode) ||
        raise ArgumentError,
              "Missing required option :mode for Local transport. This should be set by ExMCP.Client."

    node = Keyword.get(opts, :node)
    timeout = Keyword.get(opts, :timeout, @default_timeout)

    # Build service ID (either local atom or {atom, node})
    service_id = if node, do: {service_name, node}, else: service_name

    # Verify the service is available
    case ExMCP.Native.service_available?(service_id) do
      true ->
        # Start an Agent to hold the shared message queue
        {:ok, queue_agent} = Agent.start_link(fn -> :queue.new() end)

        transport = %__MODULE__{
          service_name: service_name,
          node: node,
          timeout: timeout,
          connected: true,
          queue_agent: queue_agent,
          mode: mode
        }

        {:ok, transport}

      false ->
        {:error, {:service_not_available, service_id}}
    end
  end

  @impl true
  def close(%__MODULE__{queue_agent: queue_agent}) do
    if queue_agent && Process.alive?(queue_agent) do
      Agent.stop(queue_agent)
    end

    :ok
  end

  @impl true
  def connected?(%__MODULE__{connected: connected}), do: connected

  @impl true
  def capabilities(%__MODULE__{mode: :native}), do: [:raw_terms]
  def capabilities(_state), do: []

  @impl true
  def send_message(message, %__MODULE__{queue_agent: queue_agent} = transport) do
    # Store the message in the shared queue
    Agent.update(queue_agent, fn queue -> :queue.in(message, queue) end)
    {:ok, transport}
  end

  @impl true
  def receive_message(%__MODULE__{queue_agent: queue_agent, mode: mode} = transport) do
    # Get and remove a message from the shared queue
    result =
      Agent.get_and_update(queue_agent, fn queue ->
        case :queue.out(queue) do
          {{:value, message}, new_queue} -> {{:ok, message}, new_queue}
          {:empty, queue} -> {:empty, queue}
        end
      end)

    case result do
      :empty ->
        # No messages to process - client loop will handle polling.
        {:error, :no_message}

      {:ok, message} ->
        response =
          case mode do
            :beam -> handle_beam_message(message, transport)
            :native -> handle_native_message(message, transport)
          end

        {:ok, response, transport}
    end
  end

  # Overload with timeout parameter (ignored for synchronous operation)
  def receive_message(%__MODULE__{} = transport, _timeout) do
    receive_message(transport)
  end

  # Compatibility methods
  def send(message, %__MODULE__{} = transport) do
    send_message(message, transport)
  end

  def recv(%__MODULE__{} = transport, _timeout) do
    # For simplicity, ignore timeout in synchronous mode
    receive_message(transport)
  end

  def receive(%__MODULE__{} = transport) do
    receive_message(transport)
  end

  # Private functions

  defp handle_beam_message(json_message, transport) do
    case Jason.decode(json_message) do
      {:ok, %{"jsonrpc" => "2.0", "method" => method, "params" => params, "id" => id}} ->
        # Handle request
        response_map = process_request(transport, method, params, id)
        Jason.encode!(response_map)

      {:ok, %{"jsonrpc" => "2.0", "method" => method, "params" => params}} ->
        # Handle notification
        process_notification(transport, method, params)
        # For notifications, return empty map which will be ignored by client
        %{}

      {:error, _reason} ->
        # Invalid JSON
        error_response = %{
          "jsonrpc" => "2.0",
          "error" => %{
            "code" => -32700,
            "message" => "Parse error"
          },
          "id" => nil
        }

        Jason.encode!(error_response)
    end
  end

  defp handle_native_message(message, transport) do
    case message do
      %{"jsonrpc" => "2.0", "method" => method, "params" => params, "id" => id} ->
        process_request(transport, method, params, id)

      %{"jsonrpc" => "2.0", "method" => method, "params" => params} ->
        process_notification(transport, method, params)
        # For notifications, we don't return a response
        %{}

      _ ->
        # Invalid request format
        %{
          "jsonrpc" => "2.0",
          "error" => %{
            "code" => -32700,
            "message" => "Parse error"
          },
          "id" => nil
        }
    end
  end

  defp process_request(transport, method, params, id) do
    service_id = build_service_id(transport)

    # Make the native call
    case ExMCP.Native.call(service_id, method, params, timeout: transport.timeout) do
      {:ok, response} ->
        %{
          "jsonrpc" => "2.0",
          "result" => response,
          "id" => id
        }

      {:error, :timeout} ->
        %{
          "jsonrpc" => "2.0",
          "error" => %{
            "code" => -32000,
            "message" => "Request timed out"
          },
          "id" => id
        }

      {:error, %{"code" => _code} = error} ->
        # Already formatted as JSON-RPC error
        %{
          "jsonrpc" => "2.0",
          "error" => error,
          "id" => id
        }

      {:error, reason} ->
        # Convert other errors to JSON-RPC format
        %{
          "jsonrpc" => "2.0",
          "error" => %{
            "code" => -32603,
            "message" => "Internal error",
            "data" => inspect(reason)
          },
          "id" => id
        }
    end
  rescue
    exception ->
      %{
        "jsonrpc" => "2.0",
        "error" => %{
          "code" => -32603,
          "message" => "Internal error",
          "data" => Exception.message(exception)
        },
        "id" => id
      }
  end

  defp process_notification(transport, method, params) do
    service_id = build_service_id(transport)

    # Fire and forget
    :ok = ExMCP.Native.notify(service_id, method, params)
    :ok
  rescue
    exception ->
      Logger.error("Local transport notification failed: #{Exception.message(exception)}")
      :ok
  end

  defp build_service_id(%__MODULE__{service_name: name, node: nil}), do: name
  defp build_service_id(%__MODULE__{service_name: name, node: node}), do: {name, node}
end
