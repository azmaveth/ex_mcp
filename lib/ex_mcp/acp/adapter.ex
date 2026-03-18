defmodule ExMCP.ACP.Adapter do
  @moduledoc """
  Behaviour for adapting non-native CLI agents to ACP.

  Agents like Claude Code and Codex CLI have their own protocols (NDJSON streams,
  one-shot JSON output). Adapters translate between ACP JSON-RPC messages and the
  agent's native format.

  ## Required Callbacks

  - `init/1` — initialize adapter state
  - `command/1` — return the executable and args to launch the agent
  - `translate_outbound/2` — convert ACP message to native CLI format
  - `translate_inbound/2` — convert native CLI output line to ACP messages

  ## Optional Callbacks

  - `capabilities/0` — return static agent capabilities
  - `post_connect/1` — called after Port is opened
  - `modes/0` — return supported operational modes
  - `config_options/0` — return supported config options
  - `list_sessions/1` — return available sessions (for `session/list`)
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
  - `{:messages_and_write, [map()], iodata(), new_state}` — messages + data to write back to port
  - `{:skip_and_write, iodata(), new_state}` — no messages, but write data back to port
  - `{:partial, new_state}` — line accumulated, no complete messages yet
  - `{:skip, new_state}` — line ignored (non-JSON, irrelevant event, etc.)
  """
  @callback translate_inbound(raw_line :: String.t(), state()) ::
              {:messages, [map()], state()}
              | {:messages_and_write, [map()], iodata(), state()}
              | {:skip_and_write, iodata(), state()}
              | {:partial, state()}
              | {:skip, state()}

  @doc """
  Called after the Port is opened, before any ACP messages are processed.

  Return `{:ok, iodata, new_state}` to write initial data to the port
  (e.g., a JSON-RPC initialize handshake), or `{:ok, state}` to do nothing.

  Optional — defaults to no-op.
  """
  @callback post_connect(state()) :: {:ok, iodata(), state()} | {:ok, state()}

  @doc """
  Return static agent capabilities for the initialize response.

  Optional — defaults to an empty map.
  """
  @callback capabilities() :: map()

  @doc """
  Return the operational modes this agent supports.

  Each mode is a map with `"id"`, `"name"`, and optional `"description"`.
  Returned in the `initialize` response under `agentCapabilities.modes`.

  Optional — defaults to an empty list.
  """
  @callback modes() :: [map()]

  @doc """
  Return the config options this agent supports.

  Each option is a map with `"id"`, `"name"`, `"category"` (mode/model/thought_level/other),
  and optional `"description"`, `"type"`, `"default"`, `"values"`.
  Returned in the `initialize` response under `agentCapabilities.configOptions`.

  Optional — defaults to an empty list.
  """
  @callback config_options() :: [map()]

  @doc """
  List available sessions for this agent.

  Returns `{:ok, sessions, new_state}` where sessions is a list of maps
  with `"sessionId"` and optional `"name"`, `"createdAt"`.

  Optional — defaults to returning an empty list.
  """
  @callback list_sessions(state()) :: {:ok, [map()], state()}

  @optional_callbacks [
    capabilities: 0,
    post_connect: 1,
    modes: 0,
    config_options: 0,
    list_sessions: 1
  ]
end
