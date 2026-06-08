defmodule ExMCP.ACP.Agent.Handler do
  @moduledoc """
  Behaviour for native Elixir ACP agents.

  Implement this behaviour and run it with `ExMCP.ACP.Agent`. Prompt callbacks
  may either reply immediately or return `{:noreply, state}` and finish the
  prompt later with `ExMCP.ACP.Agent.finish_prompt/3` after streaming updates.
  """

  @type state :: any()
  @type context :: %{
          required(:agent) => GenServer.server(),
          required(:request_id) => integer() | String.t(),
          optional(:prompt_id) => integer() | String.t(),
          optional(:session_id) => String.t(),
          optional(:client_info) => map() | nil,
          optional(:client_capabilities) => map() | nil,
          optional(:protocol_version) => pos_integer()
        }

  @type callback_reply :: map() | [map()] | String.t() | nil
  @type callback_result ::
          {:reply, callback_reply(), state()}
          | {:ok, callback_reply(), state()}
          | {:noreply, state()}
          | {:error, any(), state()}

  @doc "Called when the handler starts."
  @callback init(opts :: keyword()) :: {:ok, state()} | {:error, any()}

  @doc """
  Called for `session/new`.

  Return either a session ID string or a full response map containing
  `"sessionId"`.
  """
  @callback handle_new_session(params :: map(), context(), state()) :: callback_result()

  @doc """
  Called for `session/prompt`.

  The `prompt_id` in the context is the ID to pass to
  `ExMCP.ACP.Agent.finish_prompt/3` when returning `{:noreply, state}`.
  """
  @callback handle_prompt(
              session_id :: String.t(),
              prompt :: [map()],
              context(),
              state()
            ) :: callback_result()

  @doc "Optionally customize the initialize response."
  @callback handle_initialize(params :: map(), context(), state()) :: callback_result()

  @doc "Optionally authenticate a client."
  @callback handle_authenticate(params :: map(), context(), state()) :: callback_result()

  @doc "Optionally log out a client."
  @callback handle_logout(context(), state()) :: callback_result()

  @doc "Optionally load an existing session and replay history."
  @callback handle_load_session(params :: map(), context(), state()) :: callback_result()

  @doc "Optionally list resumable sessions."
  @callback handle_list_sessions(params :: map(), context(), state()) :: callback_result()

  @doc "Optionally resume an existing session without replaying history."
  @callback handle_resume_session(params :: map(), context(), state()) :: callback_result()

  @doc "Optionally fork an existing session into a new independent session."
  @callback handle_fork_session(params :: map(), context(), state()) :: callback_result()

  @doc "Optionally close a session."
  @callback handle_close_session(session_id :: String.t(), context(), state()) ::
              callback_result()

  @doc "Optionally delete a session from session history."
  @callback handle_delete_session(session_id :: String.t(), context(), state()) ::
              callback_result()

  @doc "Optionally switch a session mode."
  @callback handle_set_mode(
              session_id :: String.t(),
              mode_id :: String.t(),
              context(),
              state()
            ) :: callback_result()

  @doc "Optionally switch a session model."
  @callback handle_set_model(
              session_id :: String.t(),
              model_id :: String.t(),
              context(),
              state()
            ) :: callback_result()

  @doc "Optionally update a session config option."
  @callback handle_set_config_option(
              session_id :: String.t(),
              config_id :: String.t(),
              value :: any(),
              context(),
              state()
            ) :: callback_result()

  @doc """
  Optionally handle `session/cancel`.

  If this callback is not implemented, the runtime immediately completes the
  active prompt with `"cancelled"`.
  """
  @callback handle_cancel(session_id :: String.t(), context(), state()) :: callback_result()

  @doc "Called when the handler runner terminates."
  @callback terminate(reason :: any(), state()) :: :ok

  @optional_callbacks [
    handle_initialize: 3,
    handle_authenticate: 3,
    handle_logout: 2,
    handle_load_session: 3,
    handle_list_sessions: 3,
    handle_resume_session: 3,
    handle_fork_session: 3,
    handle_close_session: 3,
    handle_delete_session: 3,
    handle_set_mode: 4,
    handle_set_model: 4,
    handle_set_config_option: 5,
    handle_cancel: 3,
    terminate: 2
  ]
end
