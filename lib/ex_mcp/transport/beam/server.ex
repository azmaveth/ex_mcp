defmodule ExMCP.Transport.Beam.Server do
  @moduledoc """
  High-level server API for the enhanced BEAM transport.

  This module provides a simple interface for starting BEAM transport servers
  with the new ranch-based architecture. It handles all the complexity of
  setting up the supervision tree, ranch listeners, and connection management.

  ## Usage

      defmodule MyMCPHandler do
        @behaviour ExMCP.Server.Handler
        
        def init(_opts), do: {:ok, %{}}
        
        def handle_request(request, state) do
          # Handle MCP request
          response = %{"result" => "pong"}
          {:ok, response, state}
        end
        
        def handle_notification(notification, state) do
          # Handle MCP notification
          {:ok, state}
        end
      end
      
      # Start the server
      {:ok, pid} = ExMCP.Transport.Beam.Server.start_link([
        port: 9999,
        handler: MyMCPHandler,
        max_connections: 1000
      ])
  """

  use GenServer
  require Logger

  alias ExMCP.Transport.Beam.{Supervisor, Acceptor}

  @default_port 9999
  @default_max_connections 1000

  defstruct [
    :ref,
    :supervisor_pid,
    :listener_pid,
    :options
  ]

  @type t :: %__MODULE__{}
  @type server_opts :: [
          ref: atom(),
          port: non_neg_integer(),
          handler: module(),
          max_connections: pos_integer(),
          auth_config: map() | nil,
          transport_opts: keyword()
        ]

  # Public API

  @doc """
  Starts a BEAM transport server.
  """
  @spec start_link(server_opts()) :: {:ok, pid()} | {:error, term()}
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Starts a named BEAM transport server.
  """
  @spec start_link(atom(), server_opts()) :: {:ok, pid()} | {:error, term()}
  def start_link(name, opts) when is_atom(name) do
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Stops a BEAM transport server.
  """
  @spec stop(pid() | atom()) :: :ok
  def stop(server) do
    GenServer.call(server, :stop)
  end

  @doc """
  Gets server information and statistics.
  """
  @spec get_info(pid() | atom()) :: map()
  def get_info(server) do
    GenServer.call(server, :get_info)
  end

  @doc """
  Gets the port number the server is listening on.
  """
  @spec get_port(pid() | atom()) :: {:ok, non_neg_integer()} | {:error, term()}
  def get_port(server) do
    GenServer.call(server, :get_port)
  end

  @doc """
  Gets the number of active connections.
  """
  @spec get_connection_count(pid() | atom()) :: {:ok, non_neg_integer()} | {:error, term()}
  def get_connection_count(server) do
    GenServer.call(server, :get_connection_count)
  end

  @doc """
  Convenience function to start a simple server for testing.
  """
  @spec start_test_server(keyword()) :: {:ok, pid(), non_neg_integer()} | {:error, term()}
  def start_test_server(opts \\ []) do
    # Use a random available port for testing
    port = Keyword.get(opts, :port, 0)
    handler = Keyword.get(opts, :handler, __MODULE__.TestHandler)

    server_opts =
      [
        port: port,
        handler: handler,
        max_connections: 10
      ] ++ opts

    case start_link(server_opts) do
      {:ok, pid} ->
        case get_port(pid) do
          {:ok, actual_port} -> {:ok, pid, actual_port}
          error -> error
        end

      error ->
        error
    end
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    ref = Keyword.get(opts, :ref, make_ref())
    port = Keyword.get(opts, :port, @default_port)
    handler = Keyword.fetch!(opts, :handler)
    max_connections = Keyword.get(opts, :max_connections, @default_max_connections)
    auth_config = Keyword.get(opts, :auth_config)
    transport_opts = Keyword.get(opts, :transport_opts, [])

    # Start the supervisor with unique name
    supervisor_name = :"beam_supervisor_#{:erlang.unique_integer([:positive])}"

    case Supervisor.start_link(name: supervisor_name) do
      {:ok, supervisor_pid} ->
        # Start the listener
        listener_opts = [
          port: port,
          max_connections: max_connections,
          handler: handler,
          auth_config: auth_config,
          transport_opts: transport_opts
        ]

        case Acceptor.start_listener(ref, listener_opts) do
          {:ok, listener_pid} ->
            state = %__MODULE__{
              ref: ref,
              supervisor_pid: supervisor_pid,
              listener_pid: listener_pid,
              options: %{
                port: port,
                handler: handler,
                max_connections: max_connections,
                auth_config: auth_config
              }
            }

            Logger.info("BEAM transport server started on port #{port}")
            {:ok, state}

          {:error, reason} ->
            Logger.error("Failed to start BEAM transport listener: #{inspect(reason)}")
            {:stop, {:listener_start_failed, reason}}
        end

      {:error, reason} ->
        Logger.error("Failed to start BEAM transport supervisor: #{inspect(reason)}")
        {:stop, {:supervisor_start_failed, reason}}
    end
  end

  @impl true
  def handle_call(:stop, _from, state) do
    Logger.info("Stopping BEAM transport server")
    {:stop, :normal, :ok, state}
  end

  def handle_call(:get_info, _from, state) do
    info = %{
      ref: state.ref,
      supervisor: state.supervisor_pid,
      listener: state.listener_pid,
      options: state.options
    }

    # Add listener statistics
    listener_stats =
      case Acceptor.get_info(state.ref) do
        {:error, _} -> %{}
        stats -> stats
      end

    full_info = Map.merge(info, listener_stats)
    {:reply, full_info, state}
  end

  def handle_call(:get_port, _from, state) do
    result = Acceptor.get_info(state.ref)

    case result do
      {:error, reason} -> {:reply, {:error, reason}, state}
      %{port: port} -> {:reply, {:ok, port}, state}
    end
  end

  def handle_call(:get_connection_count, _from, state) do
    result = Acceptor.get_info(state.ref)

    case result do
      {:error, reason} -> {:reply, {:error, reason}, state}
      %{active_connections: count} -> {:reply, {:ok, count}, state}
    end
  end

  @impl true
  def terminate(reason, state) do
    Logger.debug("BEAM transport server terminating: #{inspect(reason)}")

    # Stop the listener
    if state.ref do
      Acceptor.stop_listener(state.ref)
    end

    :ok
  end

  # Test handler module for convenience
  defmodule TestHandler do
    @moduledoc """
    Simple test handler for the BEAM transport server.

    Responds to ping requests and logs notifications.
    """

    def init(_opts) do
      {:ok, %{requests_handled: 0, notifications_received: 0}}
    end

    def handle_request(%{"method" => "ping"} = _request, state) do
      response = %{
        "result" => %{
          "message" => "pong",
          "timestamp" => System.system_time(:millisecond),
          "requests_handled" => state.requests_handled + 1
        }
      }

      new_state = %{state | requests_handled: state.requests_handled + 1}
      {:ok, response, new_state}
    end

    def handle_request(%{"method" => "echo", "params" => params} = _request, state) do
      response = %{
        "result" => %{
          "echo" => params,
          "timestamp" => System.system_time(:millisecond)
        }
      }

      new_state = %{state | requests_handled: state.requests_handled + 1}
      {:ok, response, new_state}
    end

    def handle_request(%{"method" => "stats"} = _request, state) do
      response = %{
        "result" => %{
          "requests_handled" => state.requests_handled,
          "notifications_received" => state.notifications_received
        }
      }

      new_state = %{state | requests_handled: state.requests_handled + 1}
      {:ok, response, new_state}
    end

    def handle_request(request, state) do
      error_response = %{
        "error" => %{
          "code" => -32601,
          "message" => "Method not found",
          "data" => %{"method" => Map.get(request, "method", "unknown")}
        }
      }

      {:error, error_response, state}
    end

    def handle_notification(notification, state) do
      Logger.debug("Received notification: #{inspect(notification)}")
      new_state = %{state | notifications_received: state.notifications_received + 1}
      {:ok, new_state}
    end

    def terminate(_reason, _state) do
      :ok
    end
  end
end
