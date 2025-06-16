defmodule ExMCP.Transport do
  @moduledoc """
  This module implements the standard MCP specification.

  Behaviour for MCP transport implementations.

  A transport is responsible for sending and receiving MCP protocol
  messages over a specific communication channel (stdio, Streamable HTTP, WebSocket, etc).

  While the behaviour itself is an implementation detail, it enables the
  official MCP transports (stdio and Streamable HTTP) as well as custom transports.

  ## Implementing a Transport

  To implement a custom transport, create a module that implements
  all the callbacks defined in this behaviour:

      defmodule MyTransport do
        @behaviour ExMCP.Transport
        
        @impl true
        def connect(opts) do
          # Establish connection
          {:ok, state}
        end
        
        @impl true
        def send_message(message, state) do
          # Send the message
          {:ok, state}
        end
        
        @impl true
        def receive_message(state) do
          # Receive a message (blocking)
          {:ok, message, state}
        end
        
        @impl true
        def close(state) do
          # Clean up
          :ok
        end
      end
  """

  @type state :: any()
  @type message :: String.t()
  @type opts :: keyword()

  @doc """
  Establishes a connection for the transport.

  Options are transport-specific. Should return `{:ok, state}`
  where state contains any necessary connection information.
  """
  @callback connect(opts()) :: {:ok, state()} | {:error, any()}

  @doc """
  Sends a message through the transport.

  The message will be a JSON-encoded string. Should return
  `{:ok, new_state}` on success.
  """
  @callback send_message(message(), state()) :: {:ok, state()} | {:error, any()}

  @doc """
  Receives a message from the transport.

  This should block until a message is available. Returns
  `{:ok, message, new_state}` where message is a JSON string.
  """
  @callback receive_message(state()) :: {:ok, message(), state()} | {:error, any()}

  @doc """
  Closes the transport connection.

  Should clean up any resources and return `:ok`.
  """
  @callback close(state()) :: :ok

  @doc """
  Optional callback to check if the transport is still connected.

  Default implementation always returns true.
  """
  @callback connected?(state()) :: boolean()
  @optional_callbacks connected?: 1

  @doc """
  Helper to get the appropriate transport module for an atom identifier.

  ## Transport identifiers:
  - `:stdio` - Standard I/O transport (official MCP transport)
  - `:http` - Streamable HTTP transport with SSE (official MCP transport)
  - `:websocket` - WebSocket transport (ExMCP extension)

  Note: The `:beam` identifier previously pointed to a transport implementation
  but has been replaced with ExMCP.Native for direct service dispatch.
  """
  @spec get_transport(:stdio | :http | :websocket | module()) :: module()
  def get_transport(:stdio), do: ExMCP.Transport.Stdio
  def get_transport(:http), do: ExMCP.Transport.HTTP
  def get_transport(:websocket), do: ExMCP.Transport.WebSocket
  def get_transport(module) when is_atom(module), do: module
end
