defmodule ExMCP.Transport.Stdio do
  @moduledoc """
  stdio transport implementation for MCP.
  
  This transport communicates with MCP servers over standard input/output,
  typically by spawning a subprocess.
  
  ## Options
  
  - `:command` - Command and arguments to spawn (required)
  - `:cd` - Working directory for the process
  - `:env` - Environment variables as a list of {"KEY", "VALUE"} tuples
  
  ## Example
  
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: ["node", "my-mcp-server.js"],
        cd: "/path/to/server",
        env: [{"NODE_ENV", "production"}]
      )
  """

  @behaviour ExMCP.Transport

  require Logger

  defstruct [:port, :buffer, :line_buffer]

  @impl true
  def connect(opts) do
    command = Keyword.fetch!(opts, :command)
    
    port_opts = [
      :binary,
      :exit_status,
      :use_stdio,
      :hide,
      :stream,
      line: 1_000_000,
      args: tl(command)
    ]
    
    port_opts = 
      case Keyword.get(opts, :cd) do
        nil -> port_opts
        dir -> [{:cd, to_charlist(dir)} | port_opts]
      end
      
    port_opts =
      case Keyword.get(opts, :env) do
        nil -> port_opts
        env -> [{:env, format_env(env)} | port_opts]
      end
    
    executable = hd(command)
    
    # Try to find the executable in common locations if it's not a full path
    executable_path = 
      if Path.type(executable) == :absolute do
        executable
      else
        case System.find_executable(executable) do
          nil ->
            # Try common locations for node/npm/npx on macOS
            common_paths = [
              "/opt/homebrew/bin/#{executable}",
              "/usr/local/bin/#{executable}",
              "/usr/bin/#{executable}",
              "#{System.get_env("HOME")}/.nvm/versions/node/#{System.get_env("NODE_VERSION", "*")}/bin/#{executable}"
            ]
            
            Enum.find(common_paths, executable, &File.exists?/1)
            
          path ->
            path
        end
      end
    
    try do
      port = Port.open({:spawn_executable, to_charlist(executable_path)}, port_opts)
      
      state = %__MODULE__{
        port: port,
        buffer: "",
        line_buffer: ""
      }
      
      {:ok, state}
    catch
      :error, reason ->
        {:error, {:spawn_failed, reason}}
    end
  end

  @impl true
  def send_message(message, %__MODULE__{port: port} = state) do
    # MCP uses newline-delimited JSON
    data = message <> "\n"
    
    try do
      Port.command(port, data)
      {:ok, state}
    catch
      :error, reason ->
        {:error, {:send_failed, reason}}
    end
  end

  @impl true
  def receive_message(%__MODULE__{} = state) do
    receive_loop(state)
  end

  @impl true
  def close(%__MODULE__{port: port}) do
    Port.close(port)
    :ok
  end

  @impl true
  def connected?(%__MODULE__{port: port}) do
    Port.info(port) != nil
  end

  # Private functions

  defp receive_loop(state) do
    receive do
      {port, {:data, data}} when port == state.port ->
        process_data(data, state)
        
      {port, {:exit_status, status}} when port == state.port ->
        {:error, {:process_exited, status}}
        
      {port, :eof} when port == state.port ->
        {:error, :eof}
    end
  end

  defp process_data(data, state) do
    # Accumulate data until we have a complete line
    new_buffer = state.line_buffer <> data
    
    case String.split(new_buffer, "\n", parts: 2) do
      [line, rest] ->
        # We have a complete line
        case String.trim(line) do
          "" ->
            # Empty line, continue
            receive_loop(%{state | line_buffer: rest})
            
          json_line ->
            # Return the line and update state
            {:ok, json_line, %{state | line_buffer: rest}}
        end
        
      [partial] ->
        # No complete line yet, keep buffering
        receive_loop(%{state | line_buffer: partial})
    end
  end

  defp format_env(env) do
    Enum.map(env, fn {key, value} ->
      {to_charlist(key), to_charlist(value)}
    end)
  end
end