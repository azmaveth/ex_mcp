defmodule ExMCP.Test.ClaudeGolden.Flows do
  @moduledoc """
  Step-list builders shared by the Claude golden scenarios.

  Everything here is sugar over `ExMCP.Test.ClaudeGolden` steps: the builders
  only assemble ACP messages, Claude SDK stream-json events, and sandbox
  files. They make no assertions and reach every precondition through the
  adapter's public callbacks, so a scenario stays readable as "open a
  session, prompt, stream, settle" without repeating the handshake each time.

  Paths use the `"<sandbox>"` and `"<sandbox-key>"` placeholders documented
  in `ExMCP.Test.ClaudeGolden`.
  """

  alias ExMCP.Test.ClaudeGolden

  @cwd "<sandbox>/project"

  @doc "The sandbox working directory every scenario runs in."
  @spec cwd() :: String.t()
  def cwd, do: @cwd

  @doc "Path of the session store transcript for `session_id` under the sandbox cwd."
  @spec session_path(String.t()) :: String.t()
  def session_path(session_id),
    do: "<sandbox>/claude/projects/<sandbox-key>-project/#{session_id}.jsonl"

  @doc "A stable session UUID; `n` picks one of the distinct ids."
  @spec session_uuid(pos_integer()) :: String.t()
  def session_uuid(n) when n in 1..9,
    do: "0000000#{n}-0000-4000-8000-00000000000#{n}"

  @doc "The `session/new` request."
  @spec session_new(term(), map()) :: ClaudeGolden.step()
  def session_new(acp_id \\ "acp-new", params \\ %{}) do
    {:outbound,
     %{
       "jsonrpc" => "2.0",
       "id" => acp_id,
       "method" => "session/new",
       "params" => Map.merge(%{"cwd" => @cwd}, params)
     }}
  end

  @doc "The `session/load` request."
  @spec session_load(term(), String.t(), map()) :: ClaudeGolden.step()
  def session_load(acp_id, session_id, params \\ %{}) do
    {:outbound,
     %{
       "jsonrpc" => "2.0",
       "id" => acp_id,
       "method" => "session/load",
       "params" => Map.merge(%{"sessionId" => session_id, "cwd" => @cwd}, params)
     }}
  end

  @doc "The `session/resume` request."
  @spec session_resume(term(), String.t(), map()) :: ClaudeGolden.step()
  def session_resume(acp_id, session_id, params \\ %{}) do
    {:outbound,
     %{
       "jsonrpc" => "2.0",
       "id" => acp_id,
       "method" => "session/resume",
       "params" => Map.merge(%{"sessionId" => session_id, "cwd" => @cwd}, params)
     }}
  end

  @doc "The `session/close` request for the session opened by `session_new/2`."
  @spec session_close(term()) :: ClaudeGolden.step()
  def session_close(acp_id) do
    request(acp_id, "session/close", fn transcript ->
      %{"sessionId" => session_id(transcript)}
    end)
  end

  @doc "The `initialize` request that captures client capabilities."
  @spec initialize(term(), map()) :: ClaudeGolden.step()
  def initialize(acp_id \\ "acp-init", capabilities \\ %{}) do
    {:outbound,
     %{
       "jsonrpc" => "2.0",
       "id" => acp_id,
       "method" => "initialize",
       "params" => %{"clientCapabilities" => capabilities}
     }}
  end

  @doc """
  Steps that open a session: `initialize` (capturing `capabilities`),
  `post_connect`, the SDK `initialize` control response, and `session/new`.

  Options: `:capabilities` (client capabilities), `:init_response` (the SDK
  initialize control response), `:params` (session/new params).
  """
  @spec open_session(keyword()) :: [ClaudeGolden.step()]
  def open_session(opts \\ []) do
    [
      initialize("acp-init", Keyword.get(opts, :capabilities, %{})),
      :post_connect,
      {:respond_control, "initialize", Keyword.get(opts, :init_response, %{})},
      session_new("acp-new", Keyword.get(opts, :params, %{}))
    ]
  end

  @doc "The ACP session id the adapter returned from the most recent `session/new`."
  @spec session_id(ClaudeGolden.transcript()) :: String.t()
  def session_id(transcript) do
    transcript
    |> Enum.flat_map(fn entry ->
      case entry do
        %{step: %{kind: :outbound, message: %{"method" => method}}, result: %{reply: reply}}
        when method in ["session/new", "session/load", "session/resume"] ->
          [reply["sessionId"]]

        _ ->
          []
      end
    end)
    |> List.last()
    |> Kernel.||("default")
  end

  @doc "An outbound request whose params are built from the transcript so far."
  @spec request(term(), String.t(), (ClaudeGolden.transcript() -> map())) :: ClaudeGolden.step()
  def request(acp_id, method, params_fun) when is_function(params_fun, 1) do
    {:outbound,
     fn transcript ->
       %{
         "jsonrpc" => "2.0",
         "id" => acp_id,
         "method" => method,
         "params" => params_fun.(transcript)
       }
     end}
  end

  @doc "A `session/prompt` request for the open session; `prompt` is a string or block list."
  @spec prompt(term(), String.t() | [map()] | nil, map()) :: ClaudeGolden.step()
  def prompt(acp_id, prompt, params \\ %{}) do
    request(acp_id, "session/prompt", fn transcript ->
      Map.merge(%{"sessionId" => session_id(transcript), "prompt" => prompt}, params)
    end)
  end

  @doc "A `session/cancel` notification for the open session."
  @spec cancel(map() | nil) :: ClaudeGolden.step()
  def cancel(params \\ nil) do
    {:outbound,
     fn transcript ->
       %{
         "jsonrpc" => "2.0",
         "method" => "session/cancel",
         "params" => params || %{"sessionId" => session_id(transcript)}
       }
     end}
  end

  @doc "A `session/set_mode` request."
  @spec set_mode(term(), String.t()) :: ClaudeGolden.step()
  def set_mode(acp_id, mode_id) do
    request(acp_id, "session/set_mode", fn transcript ->
      %{"sessionId" => session_id(transcript), "modeId" => mode_id}
    end)
  end

  @doc "A `session/set_config_option` request."
  @spec set_config(term(), String.t(), term()) :: ClaudeGolden.step()
  def set_config(acp_id, config_id, value) do
    request(acp_id, "session/set_config_option", fn transcript ->
      %{"sessionId" => session_id(transcript), "configId" => config_id, "value" => value}
    end)
  end

  @doc "An ACP client reply answering the adapter's `index`-th outstanding request."
  @spec reply_last(map(), integer()) :: ClaudeGolden.step()
  def reply_last(result, index \\ -1) do
    {:outbound,
     fn transcript ->
       %{
         "jsonrpc" => "2.0",
         "id" => transcript |> ClaudeGolden.request_ids() |> Enum.at(index),
         "result" => result
       }
     end}
  end

  @doc "An ACP client error reply answering the adapter's `index`-th outstanding request."
  @spec error_reply(map(), integer()) :: ClaudeGolden.step()
  def error_reply(error \\ %{"code" => -32_603, "message" => "client exploded"}, index \\ -1) do
    {:outbound,
     fn transcript ->
       %{
         "jsonrpc" => "2.0",
         "id" => transcript |> ClaudeGolden.request_ids() |> Enum.at(index),
         "error" => error
       }
     end}
  end

  @doc "An ACP permission reply selecting `option_id`."
  @spec select(String.t(), integer()) :: ClaudeGolden.step()
  def select(option_id, index \\ -1) do
    reply_last(%{"outcome" => %{"outcome" => "selected", "optionId" => option_id}}, index)
  end

  @doc "An ACP permission reply cancelling the request."
  @spec cancelled(integer()) :: ClaudeGolden.step()
  def cancelled(index \\ -1), do: reply_last(%{"outcome" => %{"outcome" => "cancelled"}}, index)

  @doc "A Claude SDK `system`/`init` event."
  @spec system_init(map()) :: ClaudeGolden.step()
  def system_init(overrides \\ %{}) do
    {:inbound,
     Map.merge(
       %{
         "type" => "system",
         "subtype" => "init",
         "session_id" => session_uuid(1),
         "cwd" => @cwd,
         "model" => "sonnet",
         "permissionMode" => "default",
         "tools" => ["Read", "Bash"],
         "mcp_servers" => [],
         "claude_code_version" => "2.1.215"
       },
       overrides
     )}
  end

  @doc "A Claude SDK `system` event of any other subtype."
  @spec system(String.t(), map()) :: ClaudeGolden.step()
  def system(subtype, overrides \\ %{}) do
    {:inbound, Map.merge(%{"type" => "system", "subtype" => subtype}, overrides)}
  end

  @doc "A Claude SDK partial `stream_event` wrapper."
  @spec stream_event(map()) :: ClaudeGolden.step()
  def stream_event(event), do: {:inbound, %{"type" => "stream_event", "event" => event}}

  @doc """
  A streamed `message_start` carrying the Anthropic API message id.

  This is the only streamed event that carries it, so it is what the adapter
  stamps on every `agent_message_chunk` / `agent_thought_chunk` of the message
  that follows.
  """
  @spec message_start(String.t(), map()) :: ClaudeGolden.step()
  def message_start(message_id, overrides \\ %{}) do
    stream_event(%{
      "type" => "message_start",
      "message" => Map.merge(%{"id" => message_id, "role" => "assistant"}, overrides)
    })
  end

  @doc "A streamed `text_delta`."
  @spec text_delta(String.t()) :: ClaudeGolden.step()
  def text_delta(text) do
    stream_event(%{
      "type" => "content_block_delta",
      "index" => 0,
      "delta" => %{"type" => "text_delta", "text" => text}
    })
  end

  @doc "A streamed `thinking_delta`."
  @spec thinking_delta(String.t()) :: ClaudeGolden.step()
  def thinking_delta(text) do
    stream_event(%{
      "type" => "content_block_delta",
      "index" => 0,
      "delta" => %{"type" => "thinking_delta", "thinking" => text}
    })
  end

  @doc "A streamed `content_block_start`."
  @spec block_start(map()) :: ClaudeGolden.step()
  def block_start(block) do
    stream_event(%{"type" => "content_block_start", "index" => 0, "content_block" => block})
  end

  @doc "A streamed `content_block_stop`."
  @spec block_stop() :: ClaudeGolden.step()
  def block_stop, do: stream_event(%{"type" => "content_block_stop", "index" => 0})

  @doc "A complete Claude SDK `assistant` message."
  @spec assistant([map()] | map(), map()) :: ClaudeGolden.step()
  def assistant(content, overrides \\ %{}) do
    message =
      Map.merge(
        %{"role" => "assistant", "content" => content, "id" => "msg_assistant_1"},
        Map.get(overrides, "message", %{})
      )

    {:inbound,
     overrides
     |> Map.delete("message")
     |> then(
       &Map.merge(
         %{"type" => "assistant", "session_id" => session_uuid(1), "message" => message},
         &1
       )
     )}
  end

  @doc "A Claude SDK `assistant` message carrying one text block."
  @spec assistant_text(String.t()) :: ClaudeGolden.step()
  def assistant_text(text), do: assistant([%{"type" => "text", "text" => text}])

  @doc "A Claude SDK `user` message carrying one `tool_result` block."
  @spec tool_result(String.t(), term(), keyword()) :: ClaudeGolden.step()
  def tool_result(tool_use_id, content, opts \\ []) do
    block =
      %{"type" => "tool_result", "tool_use_id" => tool_use_id, "content" => content}
      |> then(fn block ->
        case Keyword.get(opts, :is_error) do
          nil -> block
          value -> Map.put(block, "is_error", value)
        end
      end)

    {:inbound,
     %{
       "type" => "user",
       "session_id" => session_uuid(1),
       "message" => %{"role" => "user", "content" => [block]}
     }}
  end

  @doc "A Claude SDK `tool_use` content block."
  @spec tool_use(String.t(), String.t(), map()) :: map()
  def tool_use(id, name, input),
    do: %{"type" => "tool_use", "id" => id, "name" => name, "input" => input}

  @doc "A Claude SDK `result` event."
  @spec result(map()) :: ClaudeGolden.step()
  def result(overrides \\ %{}) do
    {:inbound,
     Map.merge(
       %{
         "type" => "result",
         "subtype" => "success",
         "session_id" => session_uuid(1),
         "is_error" => false,
         "result" => "",
         "usage" => %{
           "input_tokens" => 12,
           "output_tokens" => 7,
           "cache_read_input_tokens" => 3,
           "cache_creation_input_tokens" => 1
         }
       },
       overrides
     )}
  end

  @doc "A Claude SDK `can_use_tool` control request."
  @spec can_use_tool(String.t(), map()) :: ClaudeGolden.step()
  def can_use_tool(request_id \\ "req-1", overrides \\ %{}) do
    request =
      Map.merge(
        %{
          "subtype" => "can_use_tool",
          "tool_name" => "Bash",
          "tool_use_id" => "toolu_1",
          "input" => %{"command" => "ls -la"}
        },
        overrides
      )

    {:inbound, %{"type" => "control_request", "request_id" => request_id, "request" => request}}
  end

  @doc "A Claude SDK `read_file` control request."
  @spec read_file_request(String.t(), map()) :: ClaudeGolden.step()
  def read_file_request(request_id \\ "req-read", overrides \\ %{}) do
    request =
      Map.merge(%{"subtype" => "read_file", "path" => "#{@cwd}/notes.md"}, overrides)

    {:inbound, %{"type" => "control_request", "request_id" => request_id, "request" => request}}
  end

  @doc "A Claude SDK `control_cancel_request`."
  @spec control_cancel(String.t()) :: ClaudeGolden.step()
  def control_cancel(request_id) do
    {:inbound, %{"type" => "control_cancel_request", "request_id" => request_id}}
  end

  @doc """
  A session store transcript for `session_id` under the sandbox cwd.

  `entries` are written verbatim as JSONL; `summary_entries/1` builds the
  minimal shape the store accepts.
  """
  @spec session_jsonl(String.t(), [map()], keyword()) :: ClaudeGolden.step()
  def session_jsonl(session_id, entries, opts \\ []) do
    case Keyword.fetch(opts, :mtime) do
      {:ok, mtime} -> {:write_file, session_path(session_id), entries, [mtime: mtime]}
      :error -> {:write_file, session_path(session_id), entries}
    end
  end

  @doc "The minimal store entry list for a session whose first prompt is `prompt`."
  @spec summary_entries(String.t(), keyword()) :: [map()]
  def summary_entries(prompt, opts \\ []) do
    [
      %{
        "type" => "user",
        "uuid" => Keyword.get(opts, :uuid, "user-1"),
        "cwd" => Keyword.get(opts, :cwd, @cwd),
        "timestamp" => Keyword.get(opts, :timestamp, "2026-01-01T00:00:00Z"),
        "gitBranch" => Keyword.get(opts, :git_branch, "main"),
        "message" => %{"role" => "user", "content" => prompt}
      }
    ] ++ Keyword.get(opts, :extra, [])
  end
end
