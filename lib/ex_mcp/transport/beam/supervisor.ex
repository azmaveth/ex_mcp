defmodule ExMCP.Transport.Beam.Supervisor do
  @moduledoc """
  Supervisor for the enhanced BEAM transport with ranch integration.
  
  This supervisor manages the entire BEAM transport stack including:
  - Ranch listener for TCP connections
  - Registry for request correlation
  - Connection pools and supervision
  
  ## Usage
  
      # Start the BEAM transport supervisor
      {:ok, pid} = ExMCP.Transport.Beam.Supervisor.start_link([
        port: 9999,
        handler: MyApp.MCPHandler,
        max_connections: 1000
      ])
      
      # Get connection statistics
      stats = ExMCP.Transport.Beam.Supervisor.get_stats()
  """

  use Supervisor
  require Logger

  alias ExMCP.Transport.Beam.{Acceptor, Correlation}

  @default_ref :beam_transport_listener
  @default_port 9999

  @doc """
  Starts the BEAM transport supervisor.
  """
  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    Supervisor.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Starts a BEAM transport listener with the given options.
  """
  @spec start_listener(keyword()) :: {:ok, pid()} | {:error, term()}
  def start_listener(opts \\ []) do
    ref = Keyword.get(opts, :ref, @default_ref)
    port = Keyword.get(opts, :port, @default_port)
    max_connections = Keyword.get(opts, :max_connections, 1000)
    handler = Keyword.fetch!(opts, :handler)
    auth_config = Keyword.get(opts, :auth_config)
    transport_opts = Keyword.get(opts, :transport_opts, [])

    acceptor_opts = [
      port: port,
      max_connections: max_connections,
      handler: handler,
      auth_config: auth_config,
      transport_opts: transport_opts
    ]

    Acceptor.start_listener(ref, acceptor_opts)
  end

  @doc """
  Stops a BEAM transport listener.
  """
  @spec stop_listener(atom()) :: :ok | {:error, term()}
  def stop_listener(ref \\ @default_ref) do
    Acceptor.stop_listener(ref)
  end

  @doc """
  Gets statistics for the BEAM transport.
  """
  @spec get_stats(atom()) :: map()
  def get_stats(ref \\ @default_ref) do
    listener_info = case Acceptor.get_info(ref) do
      {:error, :not_found} -> %{status: :not_running}
      info when is_map(info) -> info
    end

    correlation_stats = case Process.whereis(Correlation) do
      nil -> %{correlation_service: :not_running}
      _pid -> Correlation.get_stats()
    end

    Map.merge(listener_info, correlation_stats)
  end

  @doc """
  Gets the port number for a listener.
  """
  @spec get_port(atom()) :: {:ok, non_neg_integer()} | {:error, term()}
  def get_port(ref \\ @default_ref) do
    case Acceptor.get_info(ref) do
      {:error, reason} -> {:error, reason}
      %{port: port} -> {:ok, port}
    end
  end

  @doc """
  Gets the number of active connections.
  """
  @spec get_connection_count(atom()) :: {:ok, non_neg_integer()} | {:error, term()}
  def get_connection_count(ref \\ @default_ref) do
    case Acceptor.get_info(ref) do
      {:error, reason} -> {:error, reason}
      %{active_connections: count} -> {:ok, count}
    end
  end

  # Supervisor callbacks

  @impl true
  def init(_opts) do
    children = [
      # Start the correlation service
      {Correlation, []},
      
      # Note: Ranch listeners are started separately via start_listener/1
      # to allow for dynamic configuration
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  @doc """
  Adds a ranch listener child to the supervisor dynamically.
  
  This is used internally when start_listener/1 is called to ensure
  proper supervision of the ranch processes.
  """
  def add_listener_child(ref, opts) do
    child_spec = %{
      id: {:ranch_listener, ref},
      start: {Acceptor, :start_listener, [ref, opts]},
      type: :supervisor,
      restart: :permanent,
      shutdown: 5000
    }

    case Supervisor.start_child(__MODULE__, child_spec) do
      {:ok, pid} -> {:ok, pid}
      {:error, {:already_started, pid}} -> {:ok, pid}
      error -> error
    end
  end

  @doc """
  Removes a ranch listener child from the supervisor.
  """
  def remove_listener_child(ref) do
    case Supervisor.terminate_child(__MODULE__, {:ranch_listener, ref}) do
      :ok -> Supervisor.delete_child(__MODULE__, {:ranch_listener, ref})
      error -> error
    end
  end
end