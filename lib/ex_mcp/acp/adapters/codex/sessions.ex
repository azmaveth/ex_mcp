defmodule ExMCP.ACP.Adapters.Codex.Sessions do
  @moduledoc """
  Pure session and thread helpers for the Codex ACP adapter.
  """

  alias ExMCP.ACP.Adapters.Codex.Config

  @type session_state :: %{
          required(:sessions) => map(),
          required(:model) => any(),
          required(:mode_id) => any(),
          required(:reasoning_effort) => any(),
          optional(any()) => any()
        }

  @type store_state :: %{required(:sessions) => map(), optional(any()) => any()}
  @type session :: map()

  @spec empty(String.t() | nil, session_state()) :: session()
  def empty(session_id, state) do
    %{
      id: session_id,
      model: state.model,
      model_id: nil,
      mode_id: state.mode_id || Config.default_mode(),
      reasoning_effort: state.reasoning_effort || Config.default_reasoning_effort(),
      accumulated_text: [],
      accumulated_thinking: [],
      accumulated_usage: nil
    }
  end

  @spec from_result(String.t(), map(), session_state(), (session() -> any())) :: session()
  def from_result(session_id, result, state, model_id_fun \\ fn _session -> nil end) do
    thread = result["thread"] || %{}

    session =
      empty(session_id, state)
      |> Map.merge(%{
        id: session_id,
        thread: thread,
        cwd: result["cwd"] || thread["cwd"],
        model: result["model"] || state.model,
        reasoning_effort:
          result["reasoningEffort"] || state.reasoning_effort || Config.default_reasoning_effort()
      })

    Map.put(session, :model_id, model_id_fun.(session))
  end

  @spec fetch_id(map()) :: {:ok, String.t()} | {:error, String.t()}
  def fetch_id(%{"sessionId" => session_id})
      when is_binary(session_id) and session_id != "",
      do: {:ok, session_id}

  def fetch_id(_params), do: {:error, "sessionId is required"}

  @spec fetch(store_state(), String.t()) :: {:ok, session()} | {:error, String.t()}
  def fetch(state, session_id) do
    case Map.fetch(state.sessions, session_id) do
      {:ok, session} -> {:ok, session}
      :error -> {:error, "Unknown Codex session: #{session_id}"}
    end
  end

  @spec put(state, String.t() | nil, session()) :: state when state: store_state()
  def put(state, nil, _session), do: state
  def put(state, "", _session), do: state

  def put(state, session_id, session),
    do: %{state | sessions: Map.put(state.sessions, session_id, session)}

  @spec update(state, String.t() | nil, (session() -> session())) :: state
        when state: session_state()
  def update(state, nil, _fun), do: state
  def update(state, "", _fun), do: state

  def update(state, session_id, fun) do
    session = Map.get(state.sessions, session_id, empty(session_id, state))
    put(state, session_id, fun.(session))
  end

  @spec current_id(store_state()) :: String.t() | nil
  def current_id(%{sessions: sessions}) when map_size(sessions) == 1 do
    sessions |> Map.keys() |> hd()
  end

  def current_id(_state), do: nil

  @spec id_from_params(map(), store_state()) :: String.t() | nil
  def id_from_params(params, state) do
    params["threadId"] || params["sessionId"] || get_in(params, ["turn", "threadId"]) ||
      current_id(state)
  end

  @spec thread_id(map(), map()) :: String.t()
  def thread_id(thread, result) do
    thread["id"] || thread["sessionId"] || result["threadId"] || result["sessionId"] || ""
  end
end
