defmodule ExMCP.ACP.Adapters.Codex do
  @moduledoc """
  Adapter for Codex CLI (OpenAI).

  Codex is a one-shot agent — each prompt launches a new subprocess.
  Translates between ACP JSON-RPC and Codex's NDJSON event stream.
  Ported from Arbor's `CodexCli`.

  ## Codex CLI Protocol

  - **Command:** `codex exec -m MODEL --json --skip-git-repo-check PROMPT`
  - **Output:** NDJSON events: `thread.started`, `item.completed`, `turn.completed`
  - **Lifecycle:** One subprocess per prompt, exits when done

  ## ACP Mapping

  | Codex Event | ACP Message |
  |---|---|
  | `thread.started` | session_id capture |
  | `item.completed` (agent_message) | `session/update` notification (kind: text) |
  | `turn.completed` | prompt response result (with usage) |
  """

  @behaviour ExMCP.ACP.Adapter

  require Logger

  @default_timeout 600_000

  @session_vars_to_clear ~w(
    CLAUDE_CODE_ENTRYPOINT CLAUDE_SESSION_ID CLAUDE_CONFIG_DIR
    CLAUDECODE
  )

  defstruct [
    :session_id,
    :model,
    pending_prompt_id: nil,
    opts: []
  ]

  # Adapter callbacks

  @impl true
  def init(opts) do
    {:ok, %__MODULE__{opts: opts, model: Keyword.get(opts, :model)}}
  end

  @impl true
  def command(_opts) do
    # Codex is one-shot — we manage subprocess per prompt
    :one_shot
  end

  @impl true
  def capabilities do
    %{
      "streaming" => false,
      "supportedModes" => []
    }
  end

  @impl true
  def translate_outbound(%{"method" => "initialize"}, state) do
    {:ok, :skip, state}
  end

  def translate_outbound(%{"method" => "session/new"}, state) do
    {:ok, :skip, state}
  end

  def translate_outbound(%{"method" => "session/prompt", "id" => id, "params" => params}, state) do
    content = extract_prompt_text(params["content"])
    state = %{state | pending_prompt_id: id}

    cmd_fn = fn ->
      execute_codex(content, state)
    end

    {:one_shot, cmd_fn, state}
  end

  def translate_outbound(_msg, state) do
    {:ok, :skip, state}
  end

  @impl true
  def translate_inbound(_line, state) do
    # One-shot adapter — inbound is handled via execute_codex, not Port lines
    {:skip, state}
  end

  # One-shot execution

  defp execute_codex(prompt, state) do
    case System.find_executable("codex") do
      nil ->
        {:error, :codex_not_found}

      codex_path ->
        args = build_args(prompt, state)
        shell_cmd = build_shell_command(codex_path, args)
        timeout = Keyword.get(state.opts, :timeout, @default_timeout)

        cwd = Keyword.get(state.opts, :cwd, File.cwd!())

        port_opts = [
          :binary,
          :exit_status,
          :stderr_to_stdout,
          {:env, safe_env()},
          {:cd, to_charlist(cwd)}
        ]

        port = Port.open({:spawn, shell_cmd}, port_opts)
        output = collect_output(port, <<>>, timeout)

        case output do
          {:ok, data} ->
            messages = parse_output(data, state)
            {:ok, messages}

          {:error, {:exit_code, _code, data}} ->
            messages = parse_output(data, state)
            {:ok, messages}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp build_args(prompt, state) do
    args = ["exec"]

    args =
      case state.model do
        nil -> args
        "default" -> args
        model -> args ++ ["-m", model]
      end

    args ++ ["--json", "--skip-git-repo-check", prompt]
  end

  defp build_shell_command(cmd, args) do
    escaped = Enum.map(args, &shell_escape/1)
    Enum.join([cmd | escaped], " ") <> " </dev/null"
  end

  defp shell_escape(arg) do
    "'" <> String.replace(arg, "'", "'\\''") <> "'"
  end

  defp collect_output(port, acc, timeout) do
    receive do
      {^port, {:data, data}} ->
        collect_output(port, acc <> data, timeout)

      {^port, {:exit_status, 0}} ->
        {:ok, acc}

      {^port, {:exit_status, code}} ->
        {:error, {:exit_code, code, acc}}
    after
      timeout ->
        Port.close(port)
        {:error, :timeout}
    end
  end

  defp parse_output(output, state) do
    events =
      output
      |> String.split("\n", trim: true)
      |> Enum.map(&String.trim/1)
      |> Enum.filter(&String.starts_with?(&1, "{"))
      |> Enum.flat_map(fn line ->
        case Jason.decode(line) do
          {:ok, event} -> [event]
          _ -> []
        end
      end)

    text = extract_agent_text(events)
    usage = extract_usage(events)
    session_id = extract_session_id(events) || state.session_id

    messages = []

    # Text update notification
    messages =
      if text != "" do
        notification = %{
          "jsonrpc" => "2.0",
          "method" => "session/update",
          "params" => %{
            "sessionId" => session_id || "default",
            "kind" => "text",
            "content" => text
          }
        }

        [Jason.encode!(notification) | messages]
      else
        messages
      end

    # Prompt response result
    messages =
      if state.pending_prompt_id do
        response = %{
          "jsonrpc" => "2.0",
          "result" => %{
            "stopReason" => if(text == "", do: "error", else: "end_turn"),
            "text" => text,
            "usage" => format_usage(usage),
            "sessionId" => session_id
          },
          "id" => state.pending_prompt_id
        }

        [Jason.encode!(response) | messages]
      else
        messages
      end

    Enum.reverse(messages)
  end

  defp extract_agent_text(events) do
    events
    |> Enum.filter(fn event ->
      event["type"] == "item.completed" &&
        get_in(event, ["item", "type"]) == "agent_message"
    end)
    |> Enum.map_join("\n", fn event -> get_in(event, ["item", "text"]) || "" end)
  end

  defp extract_usage(events) do
    case Enum.find(events, &(&1["type"] == "turn.completed")) do
      %{"usage" => usage} ->
        %{
          input_tokens: usage["input_tokens"] || 0,
          output_tokens: usage["output_tokens"] || 0
        }

      _ ->
        %{input_tokens: 0, output_tokens: 0}
    end
  end

  defp extract_session_id(events) do
    case Enum.find(events, &(&1["type"] == "thread.started")) do
      %{"thread_id" => id} -> id
      _ -> nil
    end
  end

  defp format_usage(usage) do
    %{
      "inputTokens" => usage.input_tokens,
      "outputTokens" => usage.output_tokens
    }
  end

  defp extract_prompt_text(nil), do: ""

  defp extract_prompt_text(blocks) when is_list(blocks) do
    blocks
    |> Enum.filter(&(&1["type"] == "text"))
    |> Enum.map_join("\n", &(&1["text"] || ""))
  end

  defp extract_prompt_text(text) when is_binary(text), do: text

  defp safe_env do
    cleared = Enum.map(@session_vars_to_clear, &{to_charlist(&1), false})
    [{~c"TERM", ~c"dumb"} | cleared]
  end
end
