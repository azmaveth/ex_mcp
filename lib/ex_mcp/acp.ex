defmodule ExMCP.ACP do
  @moduledoc """
  Facade for the Agent Client Protocol (ACP) client.

  ACP lets clients control coding agents over stdio using JSON-RPC 2.0 — the same
  wire format as MCP. 24 of 27 coding agents speak ACP natively (Gemini CLI,
  OpenCode, Qwen Code, etc.).

  ## Quick Start

      {:ok, client} = ExMCP.ACP.start_client(command: ["gemini", "--acp"])
      {:ok, %{"sessionId" => sid}} = ExMCP.ACP.Client.new_session(client, "/my/project")
      {:ok, %{"stopReason" => _}} = ExMCP.ACP.Client.prompt(client, sid, "Fix the bug")

  ## Options

  See `ExMCP.ACP.Client` for the full list of options.
  """

  alias ExMCP.ACP.Client

  @doc """
  Starts an ACP client connected to an agent subprocess.

  Shorthand for `ExMCP.ACP.Client.start_link/1`.
  """
  @spec start_client(keyword()) :: GenServer.on_start()
  def start_client(opts) do
    Client.start_link(opts)
  end
end
