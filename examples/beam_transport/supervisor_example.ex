defmodule Examples.BeamTransport.SupervisorExample do
  @moduledoc """
  Example of integrating BEAM transport MCP servers and clients with OTP supervisors.
  
  This demonstrates:
  - Starting MCP components under supervision
  - Automatic restart on crashes
  - Building fault-tolerant MCP services
  - Managing multiple MCP connections
  """
  
  defmodule MCPSupervisor do
    use Supervisor
    require Logger
    
    def start_link(opts) do
      Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
    end
    
    @impl true
    def init(_opts) do
      Logger.info("Starting MCP Supervisor...")
      
      children = [
        # Start multiple MCP servers
        {ExMCP.Server, 
         id: :calc_server,
         transport: :beam,
         name: :calc_server,
         handler: Examples.BeamTransport.CalculatorServer},
        
        {ExMCP.Server,
         id: :weather_server,
         transport: :beam,
         name: :weather_server,
         handler: Examples.BeamTransport.DistributedExample.WeatherHandler},
        
        # Start a monitoring client
        {MonitoringClient,
         servers: [:calc_server, :weather_server]}
      ]
      
      Supervisor.init(children, strategy: :one_for_one)
    end
  end
  
  defmodule MonitoringClient do
    @moduledoc """
    A GenServer that monitors multiple MCP servers and logs their status.
    """
    use GenServer
    require Logger
    
    defmodule State do
      defstruct clients: %{}, check_interval: 5000
    end
    
    def start_link(opts) do
      GenServer.start_link(__MODULE__, opts, name: __MODULE__)
    end
    
    @impl true
    def init(opts) do
      servers = Keyword.get(opts, :servers, [])
      check_interval = Keyword.get(opts, :check_interval, 5000)
      
      # Schedule first check
      Process.send_after(self(), :check_servers, 1000)
      
      {:ok, %State{clients: %{}, check_interval: check_interval}, {:continue, {:connect_servers, servers}}}
    end
    
    @impl true
    def handle_continue({:connect_servers, servers}, state) do
      clients = Enum.reduce(servers, %{}, fn server, acc ->
        case connect_to_server(server) do
          {:ok, client} ->
            Logger.info("Connected to #{server}")
            Map.put(acc, server, client)
          {:error, reason} ->
            Logger.error("Failed to connect to #{server}: #{reason}")
            acc
        end
      end)
      
      {:noreply, %{state | clients: clients}}
    end
    
    @impl true
    def handle_info(:check_servers, state) do
      Logger.debug("Checking MCP servers...")
      
      Enum.each(state.clients, fn {server, client} ->
        case check_server_status(client) do
          :ok ->
            Logger.debug("#{server} is healthy")
          {:error, reason} ->
            Logger.warning("#{server} health check failed: #{reason}")
            # Try to reconnect
            handle_reconnect(server, state)
        end
      end)
      
      # Schedule next check
      Process.send_after(self(), :check_servers, state.check_interval)
      
      {:noreply, state}
    end
    
    defp connect_to_server(server) do
      ExMCP.Client.start_link(
        transport: :beam,
        server: server
      )
    end
    
    defp check_server_status(client) do
      case ExMCP.Client.server_info(client, 1000) do
        {:ok, _info} -> :ok
        error -> error
      end
    end
    
    defp handle_reconnect(server, state) do
      # In a real system, you might want more sophisticated reconnection logic
      Logger.info("Attempting to reconnect to #{server}...")
      
      # Close old client if it exists
      if client = Map.get(state.clients, server) do
        GenServer.stop(client, :normal)
      end
      
      # Try to reconnect
      case connect_to_server(server) do
        {:ok, new_client} ->
          Logger.info("Reconnected to #{server}")
          put_in(state.clients[server], new_client)
        {:error, reason} ->
          Logger.error("Reconnection to #{server} failed: #{reason}")
          Map.delete(state.clients, server)
      end
    end
  end
  
  defmodule ResilientApp do
    @moduledoc """
    Example application that uses MCP services with fault tolerance.
    """
    use GenServer
    require Logger
    
    def start_link(opts) do
      GenServer.start_link(__MODULE__, opts, name: __MODULE__)
    end
    
    def calculate(expression) do
      GenServer.call(__MODULE__, {:calculate, expression})
    end
    
    def get_weather(city) do
      GenServer.call(__MODULE__, {:weather, city})
    end
    
    @impl true
    def init(_opts) do
      # The app will connect to services lazily
      {:ok, %{}}
    end
    
    @impl true
    def handle_call({:calculate, expression}, _from, state) do
      result = with_service(:calc_server, fn client ->
        # Parse a simple expression like "5 + 3"
        case parse_expression(expression) do
          {:ok, {op, a, b}} ->
            ExMCP.Client.call_tool(client, op, %{"a" => a, "b" => b})
          error ->
            error
        end
      end)
      
      {:reply, result, state}
    end
    
    def handle_call({:weather, city}, _from, state) do
      result = with_service(:weather_server, fn client ->
        ExMCP.Client.call_tool(client, "get_weather", %{"city" => city})
      end)
      
      {:reply, result, state}
    end
    
    defp with_service(server, fun) do
      # Try to use existing client or create new one
      case get_or_create_client(server) do
        {:ok, client} ->
          fun.(client)
        error ->
          error
      end
    end
    
    defp get_or_create_client(server) do
      # In a real app, you'd cache these connections
      ExMCP.Client.start_link(
        transport: :beam,
        server: server
      )
    end
    
    defp parse_expression(expr) do
      # Simple expression parser for demo
      cond do
        String.contains?(expr, "+") ->
          [a, b] = String.split(expr, "+") |> Enum.map(&String.trim/1) |> Enum.map(&String.to_integer/1)
          {:ok, {"add", a, b}}
        String.contains?(expr, "*") ->
          [a, b] = String.split(expr, "*") |> Enum.map(&String.trim/1) |> Enum.map(&String.to_integer/1)
          {:ok, {"multiply", a, b}}
        true ->
          {:error, "Unsupported expression"}
      end
    rescue
      _ -> {:error, "Invalid expression"}
    end
  end
  
  @doc """
  Start the complete supervised system.
  """
  def start do
    # Start the supervisor tree
    {:ok, sup} = MCPSupervisor.start_link([])
    
    # Start the resilient app
    {:ok, app} = ResilientApp.start_link([])
    
    # Give everything time to initialize
    Process.sleep(1000)
    
    {:ok, sup, app}
  end
  
  @doc """
  Demonstrate the supervised system with automatic recovery.
  """
  def demo do
    Logger.info("Starting supervised MCP system...")
    
    {:ok, _sup, app} = start()
    
    # Use the services
    Logger.info("\nTesting calculation service...")
    case ResilientApp.calculate("10 + 20") do
      {:ok, %{"content" => content}} ->
        text = content |> List.first() |> Map.get("text")
        Logger.info("Result: #{text}")
      error ->
        Logger.error("Calculation failed: #{inspect(error)}")
    end
    
    Logger.info("\nTesting weather service...")
    case ResilientApp.get_weather("Berlin") do
      {:ok, %{"content" => content}} ->
        text = content |> List.first() |> Map.get("text")
        Logger.info("Weather: #{text}")
      error ->
        Logger.error("Weather query failed: #{inspect(error)}")
    end
    
    # Simulate a crash
    Logger.info("\nSimulating calculator server crash...")
    Process.whereis(:calc_server) |> Process.exit(:kill)
    
    # Wait for supervisor to restart it
    Process.sleep(1000)
    
    # Try using it again
    Logger.info("\nTrying calculation after crash...")
    case ResilientApp.calculate("15 + 25") do
      {:ok, %{"content" => content}} ->
        text = content |> List.first() |> Map.get("text")
        Logger.info("Result after recovery: #{text}")
      error ->
        Logger.error("Still failing: #{inspect(error)}")
    end
    
    Logger.info("\nSupervised MCP system demonstration complete!")
    :ok
  end
end