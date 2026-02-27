defmodule ExMCP.ACP.Adapter do
  @moduledoc """
  Behaviour for adapting non-native CLI agents to ACP.

  Agents like Claude Code and Codex CLI have their own protocols (NDJSON streams,
  one-shot JSON output). Adapters translate between ACP JSON-RPC messages and the
  agent's native format.

  ## Callbacks

  - `init/1` — initialize adapter state
  - `command/1` — return the executable and args to launch the agent
  - `translate_outbound/2` — convert ACP message to native CLI format
  - `translate_inbound/2` — convert native CLI output line to ACP messages
  - `capabilities/0` — return static agent capabilities (optional)
  """

  @type state :: term()

  @doc """
  Initialize adapter state from options.
  """
  @callback init(opts :: keyword()) :: {:ok, state()}

  @doc """
  Return the command and arguments to launch the agent subprocess.

  The bridge uses this to open a Port. For one-shot adapters that manage
  their own subprocess lifecycle, return `:one_shot` instead.
  """
  @callback command(opts :: keyword()) ::
              {executable :: String.t(), args :: [String.t()]} | :one_shot

  @doc """
  Translate an outbound ACP JSON-RPC message to the native CLI format.

  Returns `{:ok, iodata, new_state}` to write data to stdin,
  or `{:ok, :skip, new_state}` when no output is needed (e.g., initialize
  is handled internally by the bridge).
  """
  @callback translate_outbound(acp_message :: map(), state()) ::
              {:ok, iodata(), state()} | {:ok, :skip, state()}

  @doc """
  Translate one line of native CLI output to zero or more ACP messages.

  Returns:
  - `{:messages, [map()], new_state}` — one or more ACP JSON-RPC messages
  - `{:partial, new_state}` — line accumulated, no complete messages yet
  - `{:skip, new_state}` — line ignored (non-JSON, irrelevant event, etc.)
  """
  @callback translate_inbound(raw_line :: String.t(), state()) ::
              {:messages, [map()], state()}
              | {:partial, state()}
              | {:skip, state()}

  @doc """
  Return static agent capabilities for the initialize response.

  Optional — defaults to an empty map.
  """
  @callback capabilities() :: map()

  @optional_callbacks [capabilities: 0]
end
