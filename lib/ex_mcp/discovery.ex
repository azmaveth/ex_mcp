defmodule ExMCP.Discovery do
  @moduledoc """
  MCP server discovery functionality.
  
  Provides mechanisms to discover available MCP servers through:
  - Environment variables
  - Configuration files
  - Well-known locations
  - Service registration
  """
  
  @doc """
  Discovers available MCP servers from various sources.
  
  Returns a list of server configurations that can be used
  to establish connections.
  """
  @spec discover_servers() :: [map()]
  def discover_servers do
    []
    |> discover_from_env()
    |> discover_from_config()
    |> discover_from_well_known()
    |> Enum.uniq_by(& &1.name)
  end
  
  @doc """
  Discovers servers from environment variables.
  
  Looks for MCP_SERVERS environment variable containing
  a JSON array of server configurations.
  """
  @spec discover_from_env(list()) :: [map()]
  def discover_from_env(servers \\ []) do
    case System.get_env("MCP_SERVERS") do
      nil ->
        servers
        
      json ->
        case Jason.decode(json) do
          {:ok, env_servers} when is_list(env_servers) ->
            servers ++ normalize_servers(env_servers)
            
          _ ->
            servers
        end
    end
  end
  
  @doc """
  Discovers servers from configuration files.
  
  Looks for mcp.json or .mcp/config.json in:
  - Current directory
  - Home directory
  - XDG config directory
  """
  @spec discover_from_config(list()) :: [map()]
  def discover_from_config(servers \\ []) do
    config_paths = [
      "mcp.json",
      ".mcp/config.json",
      Path.join([System.user_home!(), ".mcp", "config.json"]),
      Path.join([xdg_config_home(), "mcp", "config.json"])
    ]
    
    config_servers = 
      config_paths
      |> Enum.map(&read_config_file/1)
      |> Enum.reject(&is_nil/1)
      |> List.flatten()
      |> normalize_servers()
    
    servers ++ config_servers
  end
  
  @doc """
  Discovers servers from well-known locations.
  
  Checks standard locations where MCP servers might be installed:
  - System paths
  - User local directories
  - Application bundles
  """
  @spec discover_from_well_known(list()) :: [map()]
  def discover_from_well_known(servers \\ []) do
    well_known_paths = [
      "/usr/local/lib/mcp-servers",
      "/opt/mcp-servers",
      Path.join([System.user_home!(), ".local", "lib", "mcp-servers"]),
      Path.join([System.user_home!(), ".mcp", "servers"])
    ]
    
    well_known_servers =
      well_known_paths
      |> Enum.filter(&File.dir?/1)
      |> Enum.flat_map(&scan_directory/1)
      |> normalize_servers()
    
    servers ++ well_known_servers
  end
  
  @doc """
  Registers a server for discovery.
  
  This allows programmatic registration of servers that
  may not be discoverable through other means.
  """
  @spec register_server(map()) :: :ok
  def register_server(server_config) do
    # In a real implementation, this might write to a registry
    # For now, we'll just validate the config
    if valid_server_config?(server_config) do
      :ok
    else
      {:error, :invalid_config}
    end
  end
  
  # Private functions
  
  defp xdg_config_home do
    System.get_env("XDG_CONFIG_HOME", Path.join(System.user_home!(), ".config"))
  end
  
  defp read_config_file(path) do
    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, %{"servers" => servers}} when is_list(servers) ->
            servers
          {:ok, servers} when is_list(servers) ->
            servers
          _ ->
            nil
        end
      _ ->
        nil
    end
  end
  
  defp scan_directory(dir) do
    dir
    |> File.ls!()
    |> Enum.filter(&File.dir?(Path.join(dir, &1)))
    |> Enum.map(fn subdir ->
      manifest_path = Path.join([dir, subdir, "mcp.json"])
      if File.exists?(manifest_path) do
        read_manifest(manifest_path, Path.join(dir, subdir))
      else
        nil
      end
    end)
    |> Enum.reject(&is_nil/1)
  end
  
  defp read_manifest(path, base_dir) do
    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, manifest} ->
            manifest
            |> Map.put("base_dir", base_dir)
            |> resolve_command_path()
          _ ->
            nil
        end
      _ ->
        nil
    end
  end
  
  defp resolve_command_path(%{"command" => command} = manifest) when is_binary(command) do
    base_dir = Map.get(manifest, "base_dir", ".")
    resolved_command = 
      if Path.type(command) == :absolute do
        command
      else
        Path.join(base_dir, command)
      end
    
    Map.put(manifest, "command", resolved_command)
  end
  defp resolve_command_path(manifest), do: manifest
  
  defp normalize_servers(servers) do
    Enum.map(servers, &normalize_server/1)
  end
  
  defp normalize_server(%{"name" => name} = server) do
    %{
      name: name,
      transport: Map.get(server, "transport", "stdio"),
      command: Map.get(server, "command"),
      args: Map.get(server, "args", []),
      env: Map.get(server, "env", %{}),
      url: Map.get(server, "url"),
      headers: Map.get(server, "headers", %{})
    }
  end
  defp normalize_server(server) do
    # Generate a name if not provided
    name = 
      Map.get(server, "command", "unknown")
      |> Path.basename()
      |> String.replace(~r/\.[^.]+$/, "")
    
    normalize_server(Map.put(server, "name", name))
  end
  
  defp valid_server_config?(%{name: name, transport: transport}) 
       when is_binary(name) and is_binary(transport) do
    case transport do
      "stdio" -> true
      "sse" -> true
      "websocket" -> true
      _ -> false
    end
  end
  defp valid_server_config?(_), do: false
end