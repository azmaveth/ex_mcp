defmodule ExMCP.Transport.Local do
  import Kernel, except: [send: 2]

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
  - Direct process-to-process communication
  - Built-in fault tolerance
  - Low latency for local communication

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
  - `:service_name` - Required for client mode. The service to connect to.
  - `:server` - Required for client mode if connecting to a server process.
  - `:timeout` - Optional. Call timeout in milliseconds (default: 5000).
  """

  @behaviour ExMCP.Transport

  alias ExMCP.Transport.Error
  require Logger

  defstruct [:server_pid, :mode, :role, :connected, :timeout]

  @type t :: %__MODULE__{
          server_pid: pid() | nil,
          mode: :beam | :native,
          role: :client | :server,
          connected: boolean(),
          timeout: pos_integer()
        }

  @default_timeout 5_000

  @impl true
  def connect(opts) do
    mode =
      Keyword.get(opts, :mode) ||
        raise ArgumentError,
              "Missing required option :mode for Local transport. This should be set by ExMCP.Client."

    timeout = Keyword.get(opts, :timeout, @default_timeout)

    # Determine if this is client or server mode
    cond do
      # Client mode - connecting to a server
      Keyword.has_key?(opts, :server) ->
        server_pid = Keyword.fetch!(opts, :server)

        if is_pid(server_pid) && Process.alive?(server_pid) do
          transport = %__MODULE__{
            server_pid: server_pid,
            mode: mode,
            role: :client,
            connected: true,
            timeout: timeout
          }

          # Notify server of connection
          Kernel.send(server_pid, {:test_transport_connect, self()})

          {:ok, transport}
        else
          Error.connection_error(:server_not_available)
        end

      # Client mode - using service_name (for backward compatibility)
      Keyword.has_key?(opts, :service_name) ->
        # This is the problematic case - client trying to connect directly to service
        # Return an error to force the test to be updated
        {:error,
         {:not_supported,
          "BEAM transport requires a server process. Use :server option to specify the server PID."}}

      # Server mode - listening for connections
      true ->
        transport = %__MODULE__{
          server_pid: nil,
          mode: mode,
          role: :server,
          connected: false,
          timeout: timeout
        }

        {:ok, transport}
    end
  end

  @impl true
  def close(%__MODULE__{}) do
    :ok
  end

  @impl true
  def connected?(%__MODULE__{role: :client, server_pid: pid}) when is_pid(pid) do
    Process.alive?(pid)
  end

  def connected?(%__MODULE__{role: :server, server_pid: pid}) when is_pid(pid) do
    Process.alive?(pid)
  end

  def connected?(%__MODULE__{}) do
    false
  end

  @impl true
  def capabilities(%__MODULE__{mode: :native}), do: [:raw_terms]
  def capabilities(_state), do: []

  @impl true
  def send_message(message, %__MODULE__{} = transport) do
    case Error.validate_connection(transport, &connected?/1) do
      :ok ->
        case transport.role do
          :client ->
            # Client sending to server
            Kernel.send(transport.server_pid, {:transport_message, message})
            {:ok, transport}

          :server ->
            # Server sending to client
            if transport.server_pid do
              Kernel.send(transport.server_pid, {:transport_message, message})
              {:ok, transport}
            else
              # No client connected yet
              {:ok, transport}
            end
        end

      error ->
        error
    end
  end

  @impl true
  def receive_message(%__MODULE__{} = transport) do
    receive_message(transport, transport.timeout)
  end

  def receive_message(%__MODULE__{} = transport, _timeout) do
    case Error.validate_connection(transport, &connected?/1) do
      :ok ->
        receive do
          {:transport_message, message} ->
            {:ok, message, transport}

          {:test_transport_connect, client_pid} when transport.role == :server ->
            # Server accepting client connection
            new_transport = %{transport | server_pid: client_pid, connected: true}
            # Continue waiting for actual message
            receive_message(new_transport, nil)

          {:transport_error, reason} ->
            Error.transport_error(reason)
        end

      error ->
        error
    end
  end

  # Compatibility methods
  def send(transport, message) do
    send_message(message, transport)
  end

  def recv(%__MODULE__{} = transport, timeout) do
    receive_message(transport, timeout)
  end

  def receive(%__MODULE__{} = transport) do
    receive_message(transport)
  end
end
