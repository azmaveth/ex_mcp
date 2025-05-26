defmodule ExMCP.Specification do
  @moduledoc """
  @exmcp_extension

  Module for tracking MCP specification compliance.

  This module provides compile-time attributes and runtime functions
  to identify which features are part of the MCP specification versus
  ExMCP extensions.
  """

  @doc """
  Module attribute to mark functions as MCP specification compliant.

  ## Usage

      @mcp_spec true
      def list_tools(client, timeout \\ 5000) do
        # Implementation
      end
  """
  defmacro mcp_spec(opts \\ true) do
    quote do
      Module.register_attribute(__MODULE__, :mcp_spec, persist: true)
      @mcp_spec unquote(opts)
    end
  end

  @doc """
  Module attribute to mark functions as ExMCP extensions.

  ## Usage

      @exmcp_extension "Auto-reconnection logic"  
      defp handle_reconnect(state) do
        # Implementation
      end
  """
  defmacro exmcp_extension(description \\ true) do
    quote do
      Module.register_attribute(__MODULE__, :exmcp_extension, persist: true)
      @exmcp_extension unquote(description)
    end
  end

  @doc """
  Check if a module is MCP specification compliant.
  """
  def spec_compliant?(module) when is_atom(module) do
    case module.__info__(:attributes)[:mcp_spec] do
      [true] -> true
      _ -> false
    end
  rescue
    _ -> false
  end

  @doc """
  Check if a module contains ExMCP extensions.
  """
  def has_extensions?(module) when is_atom(module) do
    case module.__info__(:attributes)[:exmcp_extension] do
      nil -> false
      [] -> false
      _ -> true
    end
  rescue
    _ -> false
  end

  @doc """
  List all MCP specification modules.
  """
  def spec_modules do
    [
      ExMCP.Protocol,
      ExMCP.Types,
      ExMCP.Server,
      ExMCP.Server.Handler,
      ExMCP.Transport.Stdio,
      ExMCP.Transport.SSE
    ]
  end

  @doc """
  List all ExMCP extension modules.
  """
  def extension_modules do
    [
      ExMCP.Transport.Beam,
      ExMCP.Discovery,
      ExMCP.ServerManager,
      __MODULE__
    ]
  end

  @doc """
  List modules with mixed spec and extension features.
  """
  def mixed_modules do
    [
      ExMCP.Client
    ]
  end
end
