defmodule ExMCP.ACP.Adapters.Pi.Sessions do
  @moduledoc false

  # Pure ACP session-lifecycle translation for the Pi ACP adapter: the state
  # transition a session/new or session/switch settles into, the session
  # response that announces it, and the session listing projection. The root
  # adapter keeps the Port, the `Pi.SessionStore` reads and writes, and the
  # startup banner, all of which touch the filesystem.

  alias ExMCP.ACP.Adapters.Pi.{Config, RPC}
  alias ExMCP.ACP.Envelope

  @page_size 50

  @typedoc """
  The subset of the adapter struct the session helpers read and update.
  """
  @type state :: %{
          required(:session_id) => String.t() | nil,
          required(:session_file) => String.t() | nil,
          required(:cwd) => String.t() | nil,
          required(:thinking_level) => String.t() | nil,
          required(:current_model_id) => String.t() | nil,
          required(:available_models) => list(),
          required(:file_commands) => list(),
          required(:available_commands) => list(),
          required(:settings) => map(),
          required(:last_session_cwd) => String.t() | nil,
          optional(any()) => any()
        }

  @typedoc """
  Everything a settled session/new or session/switch contributes to the state.
  """
  @type established :: %{
          session_id: String.t(),
          session_file: String.t() | nil,
          cwd: String.t() | nil,
          models: map() | nil,
          modes: map(),
          commands: list(),
          file_commands: list(),
          settings: map()
        }

  @doc """
  Applies a settled Pi session to the adapter state. Shared by session/new and
  by the session/load and session/resume switch, which settle identically.
  """
  @spec establish(state(), established()) :: state()
  def establish(state, fields) do
    %{
      state
      | session_id: fields.session_id,
        session_file: fields.session_file,
        cwd: fields.cwd,
        thinking_level: fields.modes["currentModeId"],
        current_model_id: get_in(fields.models, ["currentModelId"]),
        available_models: Map.get(fields.models || %{}, "availableModels", []),
        file_commands: fields.file_commands,
        available_commands: fields.commands,
        settings: fields.settings,
        last_session_cwd: fields.cwd
    }
  end

  @doc """
  The ACP response announcing a settled session.
  """
  @spec session_response(term(), String.t(), String.t() | nil, map() | nil, map()) :: map()
  def session_response(acp_id, session_id, session_file, models, modes) do
    Envelope.response(acp_id, %{
      "sessionId" => session_id,
      "models" => models,
      "modes" => modes,
      "configOptions" => Config.session_config_options(models, modes),
      "_meta" => %{"ex_mcp" => %{"pi" => RPC.compact(%{"sessionFile" => session_file})}}
    })
  end

  @doc """
  Folds an untracked `get_state` payload into the adapter state, leaving
  fields Pi did not report alone.
  """
  @spec update_session_state_from_pi(map(), state()) :: state()
  def update_session_state_from_pi(data, state) do
    state
    |> maybe_set(:session_id, data["sessionId"])
    |> maybe_set(:session_file, data["sessionFile"])
    |> maybe_set(:thinking_level, data["thinkingLevel"])
  end

  @doc """
  True when Pi reported an empty model catalog, which means the session could
  not authenticate rather than that it has no models.
  """
  @spec empty_models?(term()) :: boolean()
  def empty_models?(%{"models" => models}) when is_list(models), do: models == []
  def empty_models?(_data), do: false

  @spec require_absolute_cwd(term()) :: :ok | {:error, String.t()}
  def require_absolute_cwd(cwd) when is_binary(cwd) do
    if Path.type(cwd) == :absolute,
      do: :ok,
      else: {:error, "cwd must be an absolute path: #{cwd}"}
  end

  def require_absolute_cwd(_cwd), do: {:error, "cwd is required"}

  @doc """
  Projects a listing of stored Pi sessions into one `session/list` page,
  dropping the backing file path from every entry.
  """
  @spec page([map()], String.t() | nil, String.t() | nil) :: {[map()], String.t() | nil}
  def page(all_sessions, cwd, cursor) do
    offset = parse_cursor(cursor)
    filtered = filter_sessions_by_cwd(all_sessions, cwd)

    sessions =
      filtered
      |> Enum.slice(offset, @page_size)
      |> Enum.map(&Map.drop(&1, ["sessionFile"]))

    next_cursor =
      if offset + @page_size < length(filtered), do: Integer.to_string(offset + @page_size)

    {sessions, next_cursor}
  end

  defp filter_sessions_by_cwd(sessions, nil), do: sessions
  defp filter_sessions_by_cwd(sessions, cwd), do: Enum.filter(sessions, &(&1["cwd"] == cwd))

  defp parse_cursor(cursor) when is_binary(cursor) do
    case Integer.parse(cursor) do
      {offset, ""} when offset > 0 -> offset
      _ -> 0
    end
  end

  defp parse_cursor(_cursor), do: 0

  defp maybe_set(state, _key, nil), do: state
  defp maybe_set(state, key, value), do: Map.put(state, key, value)
end
