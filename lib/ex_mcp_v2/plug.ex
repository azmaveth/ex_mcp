defmodule ExMCP.Plug do
  @moduledoc """
  Core message processing abstraction for ExMCP v2.

  The Plug provides a simple, composable interface for processing MCP messages.
  It follows the Plug specification pattern used throughout the Elixir ecosystem.
  """

  @type t :: module()
  @type opts :: term()
  @type conn :: %__MODULE__.Conn{}

  defmodule Conn do
    @moduledoc """
    Connection struct representing an MCP message processing context.
    """

    defstruct [
      # The incoming MCP request
      :request,
      # The outgoing MCP response (if any)
      :response,
      # Processing state
      :state,
      # User-defined assigns
      :assigns,
      # Transport information
      :transport,
      # Session identifier
      :session_id,
      # Whether processing should stop
      :halted
    ]

    @type t :: %__MODULE__{
            request: map() | nil,
            response: map() | nil,
            state: term(),
            assigns: map(),
            transport: atom(),
            session_id: String.t() | nil,
            halted: boolean()
          }
  end

  @doc """
  Callback for initializing the plug with options.
  """
  @callback init(opts) :: opts

  @doc """
  Callback for processing the connection.
  """
  @callback call(conn, opts) :: conn

  @doc """
  Creates a new connection.
  """
  @spec new(map(), keyword()) :: Conn.t()
  def new(request, opts \\ []) do
    %Conn{
      request: request,
      response: nil,
      state: nil,
      assigns: %{},
      transport: Keyword.get(opts, :transport),
      session_id: Keyword.get(opts, :session_id),
      halted: false
    }
  end

  @doc """
  Assigns a value to the connection.
  """
  @spec assign(Conn.t(), atom(), term()) :: Conn.t()
  def assign(%Conn{} = conn, key, value) do
    %{conn | assigns: Map.put(conn.assigns, key, value)}
  end

  @doc """
  Halts the plug pipeline.
  """
  @spec halt(Conn.t()) :: Conn.t()
  def halt(%Conn{} = conn) do
    %{conn | halted: true}
  end

  @doc """
  Sets the response on the connection.
  """
  @spec put_response(Conn.t(), map()) :: Conn.t()
  def put_response(%Conn{} = conn, response) do
    %{conn | response: response}
  end

  @doc """
  Runs a list of plugs on the connection.
  """
  @spec run([{module(), opts}], Conn.t()) :: Conn.t()
  def run(plugs, %Conn{} = conn) do
    Enum.reduce_while(plugs, conn, fn {plug_module, opts}, acc ->
      if acc.halted do
        {:halt, acc}
      else
        result = plug_module.call(acc, plug_module.init(opts))
        {:cont, result}
      end
    end)
  end
end
