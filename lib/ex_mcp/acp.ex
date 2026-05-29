defmodule ExMCP.ACP do
  @moduledoc """
  Facade for the Agent Client Protocol (ACP).

  ACP lets clients control coding agents over stdio using JSON-RPC 2.0 — the same
  wire format as MCP. Many coding agents speak ACP natively (Gemini CLI,
  OpenCode, Qwen Code, etc.), and Elixir applications can expose native ACP
  agents with `ExMCP.ACP.Agent`.

  ## Quick Start

      {:ok, client} = ExMCP.ACP.start_client(command: ["gemini", "--acp"])
      {:ok, %{"sessionId" => sid}} = ExMCP.ACP.Client.new_session(client, "/my/project")
      {:ok, %{"stopReason" => _}} = ExMCP.ACP.Client.prompt(client, sid, "Fix the bug")

      {:ok, agent} = ExMCP.ACP.start_agent(handler: MyApp.AgentHandler)

  ## Options

  See `ExMCP.ACP.Client` and `ExMCP.ACP.Agent` for the full option lists.
  """

  alias ExMCP.ACP.Agent
  alias ExMCP.ACP.Client

  @doc """
  Starts an ACP client connected to an agent subprocess.

  Shorthand for `ExMCP.ACP.Client.start_link/1`.
  """
  @spec start_client(keyword()) :: GenServer.on_start()
  def start_client(opts) do
    Client.start_link(opts)
  end

  @doc """
  Starts an ACP agent runtime.

  Shorthand for `ExMCP.ACP.Agent.start_link/1`.
  """
  @spec start_agent(keyword()) :: GenServer.on_start()
  def start_agent(opts) do
    Agent.start_link(opts)
  end

  @doc """
  Starts an ACP agent runtime and blocks until it exits.

  Shorthand for `ExMCP.ACP.Agent.run/1`.
  """
  @spec run_agent(keyword()) :: :ok | {:error, any()}
  def run_agent(opts) do
    Agent.run(opts)
  end
end
