defmodule ExMCP.Test.PiGolden.Flows do
  @moduledoc """
  Step-list builders shared by the Pi golden scenarios.

  Everything here is sugar over `ExMCP.Test.PiGolden` steps: the builders
  only assemble ACP messages, Pi RPC events, and sandbox files. They make
  no assertions and reach every precondition through the adapter's public
  callbacks, so a scenario stays readable as "open a session, prompt,
  stream, settle" without repeating the five-message handshake each time.

  Paths use the `"<sandbox>"` placeholder documented in `ExMCP.Test.PiGolden`.
  """

  @default_session_id "pi-session"
  @default_cwd "<sandbox>/project"
  @default_models [
    %{"provider" => "anthropic", "id" => "claude-sonnet-4", "name" => "Claude Sonnet 4"},
    %{"provider" => "openai", "id" => "gpt-5.1", "name" => "GPT-5.1"}
  ]

  @doc "Default session id established by `open_session/2`."
  @spec session_id() :: String.t()
  def session_id, do: @default_session_id

  @doc "Default session file path (inside the sandbox session directory)."
  @spec session_file(String.t()) :: String.t()
  def session_file(session_id \\ @default_session_id),
    do: "<sandbox>/sessions/#{session_id}.jsonl"

  @doc "Default Pi model catalog returned by `get_available_models`."
  @spec models() :: [map()]
  def models, do: @default_models

  @doc "The `session/new` request."
  @spec session_new(term(), map()) :: ExMCP.Test.PiGolden.step()
  def session_new(acp_id, params \\ %{}) do
    {:outbound,
     %{
       "method" => "session/new",
       "id" => acp_id,
       "params" => Map.merge(%{"cwd" => @default_cwd}, params)
     }}
  end

  @doc "The `session/load` request."
  @spec session_load(term(), String.t(), map()) :: ExMCP.Test.PiGolden.step()
  def session_load(acp_id, session_id, params \\ %{}) do
    {:outbound,
     %{
       "method" => "session/load",
       "id" => acp_id,
       "params" => Map.merge(%{"sessionId" => session_id, "cwd" => @default_cwd}, params)
     }}
  end

  @doc "The `session/resume` request."
  @spec session_resume(term(), String.t(), map()) :: ExMCP.Test.PiGolden.step()
  def session_resume(acp_id, session_id, params \\ %{}) do
    {:outbound,
     %{
       "method" => "session/resume",
       "id" => acp_id,
       "params" => Map.merge(%{"sessionId" => session_id, "cwd" => @default_cwd}, params)
     }}
  end

  @doc "`get_state` data for a session, merged over sensible defaults."
  @spec state_data(map()) :: map()
  def state_data(overrides \\ %{}) do
    Map.merge(
      %{
        "sessionId" => @default_session_id,
        "sessionFile" => session_file(),
        "cwd" => @default_cwd,
        "thinkingLevel" => "medium",
        "model" => %{"provider" => "anthropic", "id" => "claude-sonnet-4"}
      },
      overrides
    )
  end

  @doc """
  Steps that open a session through `session/new` and answer all four
  control requests, leaving the adapter with an established session.

  Options: `:state` (get_state overrides), `:models` (catalog list),
  `:commands` (Pi command list), `:params` (session/new params).
  """
  @spec open_session(term(), keyword()) :: [ExMCP.Test.PiGolden.step()]
  def open_session(acp_id \\ 1, opts \\ []) do
    [
      session_new(acp_id, Keyword.get(opts, :params, %{})),
      {:respond, "new_session", %{}},
      {:respond, "get_state", state_data(Keyword.get(opts, :state, %{}))},
      {:respond, "get_available_models",
       %{"models" => Keyword.get(opts, :models, @default_models)}},
      {:respond, "get_commands", %{"commands" => Keyword.get(opts, :commands, [])}}
    ]
  end

  @doc "A session map file with one entry per `{session_id, cwd, session_file}` triple."
  @spec session_map([{String.t(), String.t(), String.t()}]) :: ExMCP.Test.PiGolden.step()
  def session_map(entries) do
    sessions =
      Map.new(entries, fn {id, cwd, file} ->
        {id, %{"sessionId" => id, "cwd" => cwd, "sessionFile" => file}}
      end)

    {:write_file, "<sandbox>/session-map.json", %{"version" => 1, "sessions" => sessions}}
  end

  @doc """
  A Pi session JSONL file under the sandbox session directory.

  Options: `:cwd`, `:first_prompt` (first user message), `:name`
  (`session_info` rename), `:extra` (additional lines appended verbatim),
  `:path` (defaults to `session_file(id)`).
  """
  @spec session_jsonl(String.t(), keyword()) :: ExMCP.Test.PiGolden.step()
  def session_jsonl(id, opts \\ []) do
    lines =
      [
        %{
          "type" => "session",
          "id" => id,
          "cwd" => Keyword.get(opts, :cwd, @default_cwd),
          "timestamp" => "2026-01-01T00:00:00Z"
        },
        case Keyword.get(opts, :first_prompt, "First prompt") do
          nil ->
            nil

          text ->
            %{
              "type" => "message",
              "timestamp" => "2026-01-01T00:00:01Z",
              "message" => %{"role" => "user", "content" => text}
            }
        end,
        case Keyword.get(opts, :name) do
          nil ->
            nil

          name ->
            %{"type" => "session_info", "name" => name, "timestamp" => "2026-01-01T00:00:02Z"}
        end
      ]
      |> Enum.reject(&is_nil/1)
      |> Kernel.++(Keyword.get(opts, :extra, []))

    {:write_file, Keyword.get(opts, :path, session_file(id)), lines}
  end

  @doc "A `session/prompt` request; `prompt` is a string or a content-block list."
  @spec prompt(term(), String.t() | [map()], map()) :: ExMCP.Test.PiGolden.step()
  def prompt(acp_id, prompt, params \\ %{}) do
    {:outbound,
     %{
       "method" => "session/prompt",
       "id" => acp_id,
       "params" => Map.merge(%{"sessionId" => @default_session_id, "prompt" => prompt}, params)
     }}
  end

  @doc "A `session/cancel` notification."
  @spec cancel(map()) :: ExMCP.Test.PiGolden.step()
  def cancel(params \\ %{"sessionId" => @default_session_id}),
    do: {:outbound, %{"method" => "session/cancel", "params" => params}}

  @doc "A `session/set_config_option` request."
  @spec set_config(term(), String.t(), term(), map()) :: ExMCP.Test.PiGolden.step()
  def set_config(acp_id, config_id, value, params \\ %{}) do
    {:outbound,
     %{
       "method" => "session/set_config_option",
       "id" => acp_id,
       "params" =>
         Map.merge(
           %{"sessionId" => @default_session_id, "configId" => config_id, "value" => value},
           params
         )
     }}
  end

  @doc "A `session/set_mode` request."
  @spec set_mode(term(), String.t(), map()) :: ExMCP.Test.PiGolden.step()
  def set_mode(acp_id, mode, params \\ %{}) do
    {:outbound,
     %{
       "method" => "session/set_mode",
       "id" => acp_id,
       "params" => Map.merge(%{"sessionId" => @default_session_id, "modeId" => mode}, params)
     }}
  end

  @doc "A `session/set_model` request."
  @spec set_model(term(), term(), map()) :: ExMCP.Test.PiGolden.step()
  def set_model(acp_id, model_id, params \\ %{}) do
    {:outbound,
     %{
       "method" => "session/set_model",
       "id" => acp_id,
       "params" => Map.merge(%{"sessionId" => @default_session_id, "modelId" => model_id}, params)
     }}
  end

  @doc "A Pi `message_update` stream event wrapping an assistant message event."
  @spec message_update(map()) :: ExMCP.Test.PiGolden.step()
  def message_update(assistant_event),
    do: {:inbound, %{"type" => "message_update", "assistantMessageEvent" => assistant_event}}

  @doc "An assistant `text_delta` stream event."
  @spec text_delta(String.t()) :: ExMCP.Test.PiGolden.step()
  def text_delta(text), do: message_update(%{"type" => "text_delta", "delta" => text})

  @doc "An assistant `thinking_delta` stream event."
  @spec thinking_delta(String.t()) :: ExMCP.Test.PiGolden.step()
  def thinking_delta(text), do: message_update(%{"type" => "thinking_delta", "delta" => text})

  @doc "A Pi `agent_end` event carrying the final assistant usage."
  @spec agent_end(map() | nil) :: ExMCP.Test.PiGolden.step()
  def agent_end(usage \\ %{"input" => 10, "output" => 4}) do
    messages =
      case usage do
        nil -> []
        usage -> [%{"role" => "assistant", "usage" => usage}]
      end

    {:inbound, %{"type" => "agent_end", "messages" => messages}}
  end

  @doc "A Pi `agent_settled` event."
  @spec agent_settled() :: ExMCP.Test.PiGolden.step()
  def agent_settled, do: {:inbound, %{"type" => "agent_settled"}}

  @doc "A Pi `tool_execution_start` event."
  @spec tool_start(String.t(), String.t(), map()) :: ExMCP.Test.PiGolden.step()
  def tool_start(tool_call_id, tool_name, args) do
    {:inbound,
     %{
       "type" => "tool_execution_start",
       "toolCallId" => tool_call_id,
       "toolName" => tool_name,
       "args" => args
     }}
  end

  @doc "A Pi `tool_execution_update` event."
  @spec tool_update(String.t(), term()) :: ExMCP.Test.PiGolden.step()
  def tool_update(tool_call_id, partial) do
    {:inbound,
     %{
       "type" => "tool_execution_update",
       "toolCallId" => tool_call_id,
       "partialResult" => partial
     }}
  end

  @doc "A Pi `tool_execution_end` event."
  @spec tool_end(String.t(), term(), boolean()) :: ExMCP.Test.PiGolden.step()
  def tool_end(tool_call_id, result, is_error \\ false) do
    {:inbound,
     %{
       "type" => "tool_execution_end",
       "toolCallId" => tool_call_id,
       "result" => result,
       "isError" => is_error
     }}
  end

  @doc "A Pi tool result payload with a single text content block."
  @spec text_result(String.t()) :: map()
  def text_result(text), do: %{"content" => [%{"type" => "text", "text" => text}]}
end
