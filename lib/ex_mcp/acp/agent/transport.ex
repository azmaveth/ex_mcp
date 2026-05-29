defmodule ExMCP.ACP.Agent.Transport do
  @moduledoc """
  Transport behaviour for ACP agent runtimes.

  This mirrors the pull-based `ExMCP.Transport` shape, but is named from the
  agent side because stdio agents read from this process' stdin/stdout instead
  of spawning a child process.
  """

  @type state :: any()
  @type message :: String.t()
  @type opts :: keyword()

  @callback connect(opts()) :: {:ok, state()} | {:error, any()}
  @callback send_message(message(), state()) :: {:ok, state()} | {:error, any()}
  @callback receive_message(state()) :: {:ok, message(), state()} | {:error, any()}
  @callback close(state()) :: :ok
  @callback connected?(state()) :: boolean()

  @optional_callbacks connected?: 1
end
