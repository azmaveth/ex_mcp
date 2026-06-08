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
  - `handle_adapter_message/2` — handle process messages for adapter-managed subprocesses
  - `shutdown/1` — clean up adapter-managed resources when the bridge closes
  - `env/1` — return child-process environment variables
  - `modes/0` — return supported operational modes for session responses
  - `config_options/0` — return supported config options for session responses
  - `list_sessions/2` — return available sessions (for `session/list`)
  - `fork_session/2` — fork an existing session (for `session/fork`)
  - `auth_methods/1` — return initialize `authMethods` for adapter options
  """

  @type state :: term()

  @doc """
  Initialize adapter state from options.
  """
  @callback init(opts :: keyword()) :: {:ok, state()}

  @doc """
  Return the command and arguments to launch the agent subprocess.

  The bridge uses this to open a Port. For one-shot adapters that manage
  their own subprocess lifecycle, return `:one_shot` instead. For adapters
  that keep persistent subprocesses but need to own more than one Port, return
  `:adapter_managed` and implement `handle_adapter_message/2`.
  """
  @callback command(opts :: keyword()) ::
              {executable :: String.t(), args :: [String.t()]} | :one_shot | :adapter_managed

  @doc """
  Translate an outbound ACP JSON-RPC message to the native CLI format.

  Returns `{:ok, iodata, new_state}` to write data to stdin,
  or `{:ok, :skip, new_state}` when no output is needed (e.g., initialize
  is handled internally by the bridge), or `{:reply, result, new_state}`
  when the adapter can produce the JSON-RPC result directly, or
  `{:messages, messages, new_state}` when the adapter can emit ACP messages
  without writing to the native process, or
  `{:messages_and_reply, messages, result, new_state}` when the adapter can
  emit ACP messages before producing the JSON-RPC result directly, or
  `{:messages_and_write, messages, iodata, new_state}` when the adapter can
  emit ACP messages while also writing to the native process, or
  `{:reply_and_write, result, iodata, new_state}` when it can reply while
  also forwarding data to the agent process, or
  `{:error, reason, new_state}` when the request can't be honored (e.g., a
  config value outside the adapter's enum). The bridge translates
  `{:error, _, _}` into a JSON-RPC error response back to the ACP client.
  Adapter-managed implementations may return `{:ok, :pending, state}` after
  writing to one of their own subprocesses when the ACP response will be
  emitted later from `handle_adapter_message/2`.
  """
  @callback translate_outbound(acp_message :: map(), state()) ::
              {:ok, iodata(), state()}
              | {:ok, :skip, state()}
              | {:ok, :pending, state()}
              | {:reply, result :: map(), state()}
              | {:messages, messages :: [map()], state()}
              | {:messages_and_reply, messages :: [map()], result :: map(), state()}
              | {:messages_and_write, messages :: [map()], iodata(), state()}
              | {:reply_and_write, result :: map(), iodata(), state()}
              | {:error, reason :: any(), state()}
              | {:one_shot, function(), state()}

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
  Handle raw messages for adapter-managed subprocesses.

  The bridge calls this for messages it does not own, including Port data,
  exit-status, and close notifications. The adapter is responsible for routing
  writes back to the correct subprocess and may return ACP messages to emit.

  Optional — only used by adapters whose `command/1` returns
  `:adapter_managed`.
  """
  @callback handle_adapter_message(message :: term(), state()) ::
              {:messages, [map()], state()}
              | {:partial, state()}
              | {:skip, state()}

  @doc """
  Clean up adapter-managed resources before the bridge exits.

  Optional — defaults to no-op.
  """
  @callback shutdown(state()) :: state()

  @doc """
  Called after the Port is opened, before any ACP messages are processed.

  Return `{:ok, iodata, new_state}` to write initial data to the port
  (e.g., a JSON-RPC initialize handshake), or `{:ok, state}` to do nothing.

  Optional — defaults to no-op.
  """
  @callback post_connect(state()) :: {:ok, iodata(), state()} | {:ok, state()}

  @doc """
  Return environment variables for the child process.

  Values are merged with `adapter_opts[:env]`, with explicit adapter options
  taking precedence. The bridge still clears known session-secret variables
  before applying this environment.

  Optional — defaults to an empty map.
  """
  @callback env(opts :: keyword()) :: map() | keyword()

  @doc """
  Return static agent capabilities for the initialize response.

  Optional — defaults to an empty map.
  """
  @callback capabilities() :: map()

  @doc """
  Return the operational modes this agent supports.

  Each mode is a map with `"id"`, `"name"`, and optional `"description"`.
  Returned in session setup responses under `"modes"`.

  Optional — defaults to an empty list.
  """
  @callback modes() :: [map()]

  @doc """
  Return the config options this agent supports.

  Each option should follow the stable ACP select shape with `"id"`, `"name"`,
  `"type"`, `"currentValue"`, `"options"`, and optional `"category"` and
  `"description"`. Returned in session setup and config responses under
  `"configOptions"`.

  Optional — defaults to an empty list.
  """
  @callback config_options() :: [map()]

  @doc """
  Return authentication methods advertised in the initialize response.

  Optional — defaults to an empty list.
  """
  @callback auth_methods(opts :: keyword()) :: [map()]

  @doc """
  List available sessions for this agent.

  Returns `{:ok, sessions, new_state}` where sessions is a list of maps
  with `"sessionId"`, `"cwd"`, and optional `"title"`, `"updatedAt"`.
  `params` is the decoded `session/list` params map, including optional
  `"cwd"` and `"cursor"` values.

  Optional — adapters that implement it are automatically advertised as
  supporting `session/list`.
  """
  @callback list_sessions(params :: map(), state()) ::
              {:ok, [map()], state()} | {:error, any(), state()}

  @doc """
  Fork an existing session.

  Optional — adapters that implement it are automatically advertised as
  supporting `session/fork`.
  """
  @callback fork_session(params :: map(), state()) ::
              {:ok, map(), state()} | {:error, any(), state()}

  @optional_callbacks [
    capabilities: 0,
    post_connect: 1,
    env: 1,
    modes: 0,
    config_options: 0,
    auth_methods: 1,
    list_sessions: 2,
    fork_session: 2,
    handle_adapter_message: 2,
    shutdown: 1
  ]
end
