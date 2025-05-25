defmodule ExMCP.Client do
  @moduledoc """
  MCP client for connecting to Model Context Protocol servers.
  
  The client handles:
  - Connection management with automatic reconnection
  - Request/response correlation
  - Protocol message encoding/decoding
  - Concurrent request handling
  
  ## Example
  
      # Connect to a filesystem server
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: ["npx", "-y", "@modelcontextprotocol/server-filesystem", "/tmp"],
        name: :fs_client
      )
      
      # List available tools
      {:ok, tools} = ExMCP.Client.list_tools(client)
      
      # Call a tool
      {:ok, result} = ExMCP.Client.call_tool(client, "read_file", %{
        "path" => "/tmp/example.txt"
      })
  """

  use GenServer
  require Logger

  alias ExMCP.{Protocol, Transport}

  @reconnect_interval 5_000
  @request_timeout 30_000

  # Client state
  defstruct [
    :transport_mod,
    :transport_opts,
    :transport_state,
    :server_info,
    :server_capabilities,
    :pending_requests,
    :initialized,
    :client_info
  ]

  # Public API

  @doc """
  Starts an MCP client.
  
  ## Options
  
  - `:transport` - Transport type (:stdio, :sse, or module)
  - `:name` - GenServer name (optional)
  - `:client_info` - Client information map with :name and :version
  
  Transport-specific options are passed through.
  """
  def start_link(opts) do
    {name, opts} = Keyword.pop(opts, :name)
    gen_opts = if name, do: [name: name], else: []
    GenServer.start_link(__MODULE__, opts, gen_opts)
  end

  @doc """
  Lists available tools from the server.
  """
  @spec list_tools(GenServer.server(), timeout()) :: {:ok, [ExMCP.Types.tool()]} | {:error, any()}
  def list_tools(client, timeout \\ @request_timeout) do
    GenServer.call(client, :list_tools, timeout)
  end

  @doc """
  Calls a tool with the given arguments.
  """
  @spec call_tool(GenServer.server(), String.t(), map(), timeout()) :: 
    {:ok, ExMCP.Types.tool_result()} | {:error, any()}
  def call_tool(client, name, arguments, timeout \\ @request_timeout) do
    GenServer.call(client, {:call_tool, name, arguments}, timeout)
  end

  @doc """
  Lists available resources from the server.
  """
  @spec list_resources(GenServer.server(), timeout()) :: {:ok, [ExMCP.Types.resource()]} | {:error, any()}
  def list_resources(client, timeout \\ @request_timeout) do
    GenServer.call(client, :list_resources, timeout)
  end

  @doc """
  Reads a resource by URI.
  """
  @spec read_resource(GenServer.server(), String.t(), timeout()) :: 
    {:ok, ExMCP.Types.resource_content()} | {:error, any()}
  def read_resource(client, uri, timeout \\ @request_timeout) do
    GenServer.call(client, {:read_resource, uri}, timeout)
  end

  @doc """
  Lists available prompts from the server.
  """
  @spec list_prompts(GenServer.server(), timeout()) :: {:ok, [ExMCP.Types.prompt()]} | {:error, any()}
  def list_prompts(client, timeout \\ @request_timeout) do
    GenServer.call(client, :list_prompts, timeout)
  end

  @doc """
  Gets a prompt with the given arguments.
  """
  @spec get_prompt(GenServer.server(), String.t(), map(), timeout()) :: 
    {:ok, ExMCP.Types.prompt_message()} | {:error, any()}
  def get_prompt(client, name, arguments \\ %{}, timeout \\ @request_timeout) do
    GenServer.call(client, {:get_prompt, name, arguments}, timeout)
  end

  @doc """
  Gets server information.
  """
  @spec server_info(GenServer.server()) :: {:ok, ExMCP.Types.server_info()} | {:error, :not_initialized}
  def server_info(client) do
    GenServer.call(client, :server_info)
  end

  @doc """
  Gets server capabilities.
  """
  @spec server_capabilities(GenServer.server()) :: {:ok, ExMCP.Types.capabilities()} | {:error, :not_initialized}
  def server_capabilities(client) do
    GenServer.call(client, :server_capabilities)
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    transport_type = Keyword.fetch!(opts, :transport)
    transport_mod = Transport.get_transport(transport_type)
    
    client_info = Keyword.get(opts, :client_info, %{
      name: "ex_mcp",
      version: ExMCP.version()
    })
    
    state = %__MODULE__{
      transport_mod: transport_mod,
      transport_opts: opts,
      transport_state: nil,
      pending_requests: %{},
      initialized: false,
      client_info: client_info
    }
    
    {:ok, state, {:continue, :connect}}
  end

  @impl true
  def handle_continue(:connect, state) do
    case connect_and_initialize(state) do
      {:ok, new_state} ->
        {:noreply, new_state}
        
      {:error, reason} ->
        Logger.error("Failed to connect to MCP server: #{inspect(reason)}")
        Process.send_after(self(), :reconnect, @reconnect_interval)
        {:noreply, state}
    end
  end

  @impl true
  def handle_call(:list_tools, from, state) do
    send_request(Protocol.encode_list_tools(), from, state)
  end

  def handle_call({:call_tool, name, arguments}, from, state) do
    send_request(Protocol.encode_call_tool(name, arguments), from, state)
  end

  def handle_call(:list_resources, from, state) do
    send_request(Protocol.encode_list_resources(), from, state)
  end

  def handle_call({:read_resource, uri}, from, state) do
    send_request(Protocol.encode_read_resource(uri), from, state)
  end

  def handle_call(:list_prompts, from, state) do
    send_request(Protocol.encode_list_prompts(), from, state)
  end

  def handle_call({:get_prompt, name, arguments}, from, state) do
    send_request(Protocol.encode_get_prompt(name, arguments), from, state)
  end

  def handle_call(:server_info, _from, state) do
    if state.initialized do
      {:reply, {:ok, state.server_info}, state}
    else
      {:reply, {:error, :not_initialized}, state}
    end
  end

  def handle_call(:server_capabilities, _from, state) do
    if state.initialized do
      {:reply, {:ok, state.server_capabilities}, state}
    else
      {:reply, {:error, :not_initialized}, state}
    end
  end

  @impl true
  def handle_info(:reconnect, state) do
    handle_continue(:connect, state)
  end
  
  # Handle port messages that might arrive before ownership transfer
  def handle_info({port, {:data, _data}}, state) when is_port(port) do
    # Ignore port messages - they should be handled by the transport
    {:noreply, state}
  end
  
  def handle_info({port, {:exit_status, _status}}, state) when is_port(port) do
    # Port exited - treat as transport closed
    {:noreply, state}
  end

  def handle_info({:transport_message, message}, state) do
    case Protocol.parse_message(message) do
      {:result, result, id} ->
        handle_response(result, id, state)
        
      {:error, error, id} ->
        handle_error_response(error, id, state)
        
      {:notification, method, params} ->
        handle_notification(method, params, state)
        
      _ ->
        Logger.warning("Unexpected message from server: #{inspect(message)}")
        {:noreply, state}
    end
  end

  def handle_info({:transport_closed, reason}, state) do
    Logger.warning("Transport closed: #{inspect(reason)}")
    Process.send_after(self(), :reconnect, @reconnect_interval)
    {:noreply, %{state | transport_state: nil, initialized: false}}
  end

  # Private functions

  defp connect_and_initialize(state) do
    with {:ok, transport_state} <- state.transport_mod.connect(state.transport_opts),
         :ok <- start_receiver(transport_state, state.transport_mod),
         {:ok, init_result} <- initialize_connection(transport_state, state) do
      
      new_state = %{state | 
        transport_state: transport_state,
        server_info: init_result["serverInfo"],
        server_capabilities: init_result["capabilities"],
        initialized: true
      }
      
      {:ok, new_state}
    end
  end

  defp initialize_connection(transport_state, state) do
    init_msg = Protocol.encode_initialize(state.client_info)
    
    with {:ok, json} <- Protocol.encode_to_string(init_msg),
         {:ok, _} <- state.transport_mod.send_message(json, transport_state) do
      
      # Wait for initialize response
      receive do
        {:transport_message, message} ->
          case Protocol.parse_message(message) do
            {:result, result, _id} ->
              # Send initialized notification
              notif = Protocol.encode_initialized()
              {:ok, json} = Protocol.encode_to_string(notif)
              state.transport_mod.send_message(json, transport_state)
              {:ok, result}
              
            {:error, error, _id} ->
              {:error, error}
              
            _ ->
              {:error, :unexpected_response}
          end
      after
        10_000 ->
          {:error, :initialize_timeout}
      end
    end
  end

  defp start_receiver(transport_state, transport_mod) do
    parent = self()
    
    spawn_link(fn ->
      receive_loop(transport_state, transport_mod, parent)
    end)
    
    :ok
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

  defp send_request(message, from, state) do
    if state.initialized && state.transport_state do
      id = message["id"]
      
      case Protocol.encode_to_string(message) do
        {:ok, json} ->
          case state.transport_mod.send_message(json, state.transport_state) do
            {:ok, new_transport_state} ->
              pending = Map.put(state.pending_requests, id, from)
              new_state = %{state | 
                transport_state: new_transport_state,
                pending_requests: pending
              }
              {:noreply, new_state}
              
            {:error, reason} ->
              {:reply, {:error, reason}, state}
          end
          
        {:error, reason} ->
          {:reply, {:error, reason}, state}
      end
    else
      {:reply, {:error, :not_connected}, state}
    end
  end

  defp handle_response(result, id, state) do
    case Map.pop(state.pending_requests, id) do
      {nil, _} ->
        Logger.warning("Received response for unknown request #{id}")
        {:noreply, state}
        
      {from, pending} ->
        GenServer.reply(from, {:ok, result})
        {:noreply, %{state | pending_requests: pending}}
    end
  end

  defp handle_error_response(error, id, state) do
    case Map.pop(state.pending_requests, id) do
      {nil, _} ->
        Logger.warning("Received error for unknown request #{id}")
        {:noreply, state}
        
      {from, pending} ->
        GenServer.reply(from, {:error, error})
        {:noreply, %{state | pending_requests: pending}}
    end
  end

  defp handle_notification(method, params, state) do
    Logger.debug("Received notification: #{method} #{inspect(params)}")
    {:noreply, state}
  end
end