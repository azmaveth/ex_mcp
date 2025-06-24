defmodule ExMCP.Transport do
  @moduledoc """
  Behaviour definition for MCP transport implementations.

  A transport is responsible for sending and receiving MCP protocol messages
  over a specific communication channel. ExMCP includes implementations for
  the standard MCP transports and provides this behaviour for custom implementations.

  ## Built-in Transports

  ExMCP provides these standard transports:

  - **`:stdio`** - Standard I/O communication (MCP specification)
  - **`:http`** - HTTP with optional SSE streaming (MCP specification)
  - **`:test`** - In-memory transport for testing
  - **`:beam`** - JSON-compliant transport for BEAM-to-BEAM communication.
  - **`:native`** - High-performance transport for BEAM-to-BEAM communication using raw Elixir terms.

  ## Using Transports

  Transports are specified when starting clients or servers:

      # stdio transport
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: ["python", "mcp-server.py"]
      )

      # HTTP transport
      {:ok, client} = ExMCP.Client.start_link(
        transport: :http,
        url: "https://api.example.com"
      )

  ## Custom Transport Implementation

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
  @type message :: String.t() | map()
  @type opts :: keyword()

  @doc """
  Establishes a connection for the transport.

  Options are transport-specific. Should return `{:ok, state}`
  where state contains any necessary connection information.
  """
  @callback connect(opts()) :: {:ok, state()} | {:error, any()}

  @doc """
  Sends a message through the transport.

  The message will be a JSON-encoded string, or a raw map if the
  transport supports the `:raw_terms` capability. Should return
  `{:ok, new_state}` on success.
  """
  @callback send_message(message(), state()) :: {:ok, state()} | {:error, any()}

  @doc """
  Receives a message from the transport.

  This should block until a message is available. Returns
  `{:ok, message, new_state}` where message is a JSON string or raw map.
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

  @doc """
  Optional callback to declare transport capabilities.

  Returns a list of capability atoms that indicate special features
  supported by this transport. Clients can use this information to
  optimize their communication strategy.

  ## Capabilities

  - `:raw_terms` - Transport can handle raw Elixir terms without JSON serialization
  - `:compression` - Transport supports message compression (future)
  - `:encryption` - Transport supports message encryption (future)

  ## Examples

      # Transport that supports raw term passing
      def capabilities(_state), do: [:raw_terms]

      # Transport with multiple capabilities
      def capabilities(_state), do: [:raw_terms, :compression]

      # Transport with no special capabilities (default)
      def capabilities(_state), do: []

  Default implementation returns an empty list (no special capabilities).
  """
  @callback capabilities(state()) :: [atom()]
  @optional_callbacks connected?: 1, capabilities: 1

  @doc """
  Helper to get the appropriate transport module for an atom identifier.

  ## Transport identifiers:
  - `:stdio` - Standard I/O transport (official MCP transport)
  - `:http` - Streamable HTTP transport with SSE (official MCP transport)
  - `:test` - In-memory transport for testing (non-standard)
  - `:beam` - JSON-compliant transport for BEAM-to-BEAM communication.
  - `:native` - High-performance transport for BEAM-to-BEAM communication using raw Elixir terms.

  Note: For direct Elixir service communication, use ExMCP.Native for service registration.
  """
  @spec get_transport(:stdio | :http | :test | :beam | :native | module()) :: module()
  def get_transport(:stdio), do: ExMCP.Transport.Stdio
  def get_transport(:http), do: ExMCP.Transport.HTTP
  def get_transport(:test), do: ExMCP.Transport.Test
  def get_transport(:beam), do: ExMCP.Transport.Local
  def get_transport(:native), do: ExMCP.Transport.Local
  def get_transport(module) when is_atom(module), do: module
end
