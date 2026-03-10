defmodule ExMCP.ACP.Client.Handler do
  @moduledoc """
  Behaviour for handling ACP session events and agent requests.

  Implement this behaviour to customize how your application responds to
  streaming session updates, permission requests, and file access requests
  from ACP agents.

  See `ExMCP.ACP.Client.DefaultHandler` for a reference implementation.
  """

  @type state :: any()

  @doc "Called when the handler is initialized."
  @callback init(opts :: keyword()) :: {:ok, state()}

  @doc """
  Called for each `session/update` notification from the agent.

  The `update` map contains a `"sessionUpdate"` discriminator field indicating
  the update type (e.g., `"agent_message_chunk"`, `"tool_call"`, `"plan"`, etc.).
  """
  @callback handle_session_update(session_id :: String.t(), update :: map(), state()) ::
              {:ok, state()}

  @doc """
  Called when the agent requests permission to use a tool.

  Must return an outcome map with an `"optionId"` matching one of the
  provided options.
  """
  @callback handle_permission_request(
              session_id :: String.t(),
              tool_call :: map(),
              options :: [map()],
              state()
            ) :: {:ok, outcome :: map(), state()}

  @doc """
  Called when the agent requests to read a file.

  Return `{:ok, content, state}` with the file contents, or
  `{:error, reason, state}` to deny access.
  """
  @callback handle_file_read(session_id :: String.t(), path :: String.t(), opts :: map(), state()) ::
              {:ok, content :: String.t(), state()} | {:error, reason :: String.t(), state()}

  @doc """
  Called when the agent requests to write a file.

  Return `{:ok, state}` to allow the write, or
  `{:error, reason, state}` to deny it.
  """
  @callback handle_file_write(
              session_id :: String.t(),
              path :: String.t(),
              content :: String.t(),
              state()
            ) :: {:ok, state()} | {:error, reason :: String.t(), state()}

  @doc "Called when the handler is being terminated."
  @callback terminate(reason :: any(), state()) :: :ok

  @optional_callbacks [handle_file_read: 4, handle_file_write: 4, terminate: 2]
end
