defmodule ExMCP.ACP.Adapters.ClaudeSDK.Protocol do
  @moduledoc """
  Pure helpers for Claude Code's SDK-flavoured stream-json protocol.

  The official `@anthropic-ai/claude-agent-sdk` still launches Claude Code, but
  it uses a richer stdin/stdout protocol than the basic CLI stream. This module
  keeps that wire-shape construction side-effect free.
  """

  alias ExMCP.ACP.Maps

  @sdk_version "0.3.165"

  @permission_modes %{
    default: "default",
    accept_edits: "acceptEdits",
    plan: "plan",
    auto: "auto",
    dont_ask: "dontAsk",
    bypass_permissions: "bypassPermissions",
    bypass: "bypassPermissions"
  }

  @doc "Builds the Claude Code command line used by SDK-style sessions."
  @spec command(keyword()) :: {String.t(), [String.t()]}
  def command(opts) do
    args =
      [
        "--output-format",
        "stream-json",
        "--verbose",
        "--input-format",
        "stream-json"
      ]
      |> append_thinking(opts)
      |> append_optional(opts, :effort, "--effort")
      |> append_optional(opts, :max_turns, "--max-turns")
      |> append_optional(opts, :max_budget_usd, "--max-budget-usd")
      |> append_optional(opts, :model, "--model")
      |> append_optional(opts, :fallback_model, "--fallback-model")
      |> append_optional(opts, :agent, "--agent")
      |> append_permission_prompt(opts)
      |> append_tools(opts)
      |> append_mcp_config(opts)
      |> append_permission_mode(opts)
      |> append_flag(
        opts,
        :allow_dangerously_skip_permissions,
        "--allow-dangerously-skip-permissions"
      )
      |> append_flag(opts, :include_partial_messages, "--include-partial-messages", true)
      |> append_flag(opts, :strict_mcp_config, "--strict-mcp-config")
      |> append_repeated(opts, :additional_directories, "--add-dir")
      |> append_optional(opts, :resume, "--resume")
      |> append_optional(opts, :resume_session_at, "--resume-session-at")
      |> append_optional(opts, :session_id, "--session-id")
      |> append_extra_args(opts)

    {Keyword.get(opts, :cli_path, "claude"), args}
  end

  @doc "Environment required for Claude Code's SDK entrypoint."
  @spec env(keyword()) :: map()
  def env(_opts) do
    %{
      "CLAUDE_CODE_ENTRYPOINT" => "sdk-ts",
      "CLAUDE_AGENT_SDK_VERSION" => @sdk_version
    }
  end

  @doc "Encodes one SDK message as an NDJSON line."
  @spec line(map()) :: String.t()
  def line(message), do: Jason.encode!(message) <> "\n"

  @doc "Builds a host-to-Claude SDK control request."
  @spec control_request(String.t(), map()) :: map()
  def control_request(request_id, request) do
    %{"type" => "control_request", "request_id" => request_id, "request" => request}
  end

  @doc "Builds a success response for a Claude-originated control request."
  @spec control_success(String.t(), map() | nil) :: map()
  def control_success(request_id, response \\ %{}) do
    %{
      "type" => "control_response",
      "response" => %{
        "subtype" => "success",
        "request_id" => request_id,
        "response" => response || %{}
      }
    }
  end

  @doc "Builds an error response for a Claude-originated control request."
  @spec control_error(String.t(), String.t()) :: map()
  def control_error(request_id, error) do
    %{
      "type" => "control_response",
      "response" => %{
        "subtype" => "error",
        "request_id" => request_id,
        "error" => error
      }
    }
  end

  @doc "Builds the initial SDK control request sent after process startup."
  @spec initialize_request(keyword(), String.t()) :: map()
  def initialize_request(opts, request_id) do
    request =
      %{"subtype" => "initialize"}
      |> maybe_put("appendSystemPrompt", Keyword.get(opts, :append_system_prompt))
      |> maybe_put("systemPrompt", system_prompt(Keyword.get(opts, :system_prompt)))
      |> maybe_put("planModeInstructions", Keyword.get(opts, :plan_mode_instructions))
      |> maybe_put("title", Keyword.get(opts, :title))
      |> maybe_put("skills", Keyword.get(opts, :skills))
      |> maybe_put("agents", Keyword.get(opts, :agents))
      |> maybe_put("promptSuggestions", Keyword.get(opts, :prompt_suggestions))
      |> maybe_put("agentProgressSummaries", Keyword.get(opts, :agent_progress_summaries))
      |> maybe_put("forwardSubagentText", Keyword.get(opts, :forward_subagent_text))

    control_request(request_id, request)
  end

  @doc "Builds a user message for a prompt turn."
  @spec user_message(String.t() | nil, list() | String.t()) :: {:ok, map()} | {:error, String.t()}
  def user_message(session_id, prompt) do
    with {:ok, content} <- prompt_content(prompt) do
      {:ok,
       %{
         "type" => "user",
         "session_id" => session_id || "",
         "message" => %{"role" => "user", "content" => content},
         "parent_tool_use_id" => nil
       }}
    end
  end

  @doc "Converts ACP prompt blocks into Claude message content blocks."
  @spec prompt_content(list() | String.t() | nil) :: {:ok, list()} | {:error, String.t()}
  def prompt_content(nil), do: {:ok, [%{"type" => "text", "text" => ""}]}
  def prompt_content(text) when is_binary(text), do: {:ok, [%{"type" => "text", "text" => text}]}

  def prompt_content(blocks) when is_list(blocks) do
    blocks
    |> Enum.reduce_while({:ok, []}, fn block, {:ok, acc} ->
      case content_block(block) do
        {:ok, converted} -> {:cont, {:ok, [converted | acc]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:ok, converted} -> {:ok, Enum.reverse(converted)}
      error -> error
    end
  end

  def prompt_content(_), do: {:error, "Claude prompt must be text or a list of content blocks"}

  @doc "Builds ACP permission options for a Claude SDK permission request."
  @spec permission_options(map()) :: [map()]
  def permission_options(%{"permission_suggestions" => suggestions}) when is_list(suggestions) do
    [
      %{"optionId" => "allow_once", "name" => "Allow once", "kind" => "allow_once"},
      %{
        "optionId" => "allow_always",
        "name" => "Always allow",
        "kind" => "allow_always",
        "_meta" => %{"ex_mcp.claude_sdk" => %{"updatedPermissions" => suggestions}}
      },
      %{"optionId" => "reject_once", "name" => "Reject", "kind" => "reject_once"},
      %{"optionId" => "reject_always", "name" => "Always reject", "kind" => "reject_always"}
    ]
  end

  def permission_options(_request) do
    [
      %{"optionId" => "allow_once", "name" => "Allow", "kind" => "allow_once"},
      %{"optionId" => "reject_once", "name" => "Reject", "kind" => "reject_once"}
    ]
  end

  @doc "Converts an ACP permission outcome into a Claude SDK PermissionResult."
  @spec permission_result(map(), map()) :: map()
  def permission_result(%{"outcome" => %{"outcome" => _} = nested}, request),
    do: permission_result(nested, request)

  def permission_result(%{"outcome" => "selected", "optionId" => "allow_once"}, request) do
    %{"behavior" => "allow", "toolUseID" => request["tool_use_id"]}
    |> maybe_put("decisionClassification", "user_temporary")
  end

  def permission_result(%{"outcome" => "selected", "optionId" => "allow_always"}, request) do
    %{"behavior" => "allow", "toolUseID" => request["tool_use_id"]}
    |> maybe_put("updatedPermissions", request["permission_suggestions"])
    |> maybe_put("decisionClassification", "user_permanent")
  end

  def permission_result(%{"outcome" => "selected", "optionId" => option_id}, request)
      when option_id in ["reject_once", "reject_always"] do
    %{
      "behavior" => "deny",
      "message" => request["decision_reason"] || "Permission denied",
      "toolUseID" => request["tool_use_id"],
      "decisionClassification" => "user_reject"
    }
  end

  def permission_result(_outcome, request) do
    %{
      "behavior" => "deny",
      "message" => "Permission request cancelled",
      "interrupt" => true,
      "toolUseID" => request["tool_use_id"],
      "decisionClassification" => "user_reject"
    }
  end

  @doc "Builds a compact ACP tool call map for permission prompts."
  @spec permission_tool_call(map(), String.t() | nil) :: map()
  def permission_tool_call(request, cwd) do
    tool_name = request["tool_name"] || "tool"
    input = request["input"] || %{}

    tool_use_id =
      request["tool_use_id"] || request["toolUseID"] ||
        "tool_#{System.unique_integer([:positive])}"

    ExMCP.ACP.Adapters.ClaudeSDK.ToolInfo.from_use(tool_name, input, tool_use_id, cwd)
    |> Map.take(["title", "kind", "content", "locations", "rawInput"])
    |> Map.put("toolCallId", tool_use_id)
    |> Map.put_new("title", request["title"] || tool_name)
  end

  defp content_block(%{"type" => "text"} = block) do
    {:ok, %{"type" => "text", "text" => block["text"] || ""}}
  end

  defp content_block(%{"type" => "image"} = block) do
    {:ok,
     %{
       "type" => "image",
       "source" => %{
         "type" => "base64",
         "media_type" => block["mimeType"] || block["media_type"] || "image/png",
         "data" => block["data"] || ""
       }
     }}
  end

  defp content_block(%{"type" => "resource_link"} = block) do
    uri = block["uri"] || get_in(block, ["resource", "uri"])
    name = block["name"] || block["title"] || uri
    {:ok, %{"type" => "text", "text" => "@#{uri || name}"}}
  end

  defp content_block(%{"type" => "resource"} = block) do
    resource = block["resource"] || %{}
    uri = resource["uri"] || block["uri"] || "resource"
    text = resource["text"] || block["text"] || resource["blob"] || ""
    {:ok, %{"type" => "text", "text" => "Context from #{uri}:\n#{text}"}}
  end

  defp content_block(%{"type" => type}),
    do: {:error, "Claude does not support content block type=#{inspect(type)}"}

  defp content_block(block) when is_binary(block), do: {:ok, %{"type" => "text", "text" => block}}
  defp content_block(block), do: {:error, "Unsupported Claude content block: #{inspect(block)}"}

  defp append_thinking(args, opts) do
    cond do
      Keyword.has_key?(opts, :thinking) ->
        case Keyword.fetch!(opts, :thinking) do
          :disabled -> args ++ ["--thinking", "disabled"]
          :adaptive -> args ++ ["--thinking", "adaptive"]
          "disabled" -> args ++ ["--thinking", "disabled"]
          "adaptive" -> args ++ ["--thinking", "adaptive"]
          other -> args ++ ["--thinking", to_string(other)]
        end

      Keyword.get(opts, :max_thinking_tokens) == 0 ->
        args ++ ["--thinking", "disabled"]

      Keyword.get(opts, :max_thinking_tokens) ->
        args ++ ["--max-thinking-tokens", to_string(Keyword.fetch!(opts, :max_thinking_tokens))]

      true ->
        args
    end
  end

  defp append_permission_prompt(args, opts) do
    if Keyword.get(opts, :permission_prompt, true) do
      args ++ ["--permission-prompt-tool", "stdio"]
    else
      args
    end
  end

  defp append_tools(args, opts) do
    args
    |> append_csv(opts, :allowed_tools, "--allowedTools")
    |> append_csv(opts, :disallowed_tools, "--disallowedTools")
    |> append_csv(opts, :tools, "--tools")
  end

  defp append_mcp_config(args, opts) do
    case Keyword.get(opts, :mcp_servers) do
      nil ->
        args

      servers when servers == %{} ->
        args

      servers ->
        args ++ ["--mcp-config", Jason.encode!(%{"mcpServers" => Maps.stringify_keys(servers)})]
    end
  end

  defp append_permission_mode(args, opts) do
    mode = Keyword.get(opts, :permission_mode, :default)

    case encode_permission_mode(mode) do
      nil ->
        args

      "bypassPermissions" ->
        args ++ ["--permission-mode", "bypassPermissions", "--allow-dangerously-skip-permissions"]

      encoded ->
        args ++ ["--permission-mode", encoded]
    end
  end

  defp append_extra_args(args, opts) do
    opts
    |> Keyword.get(:extra_args, [])
    |> case do
      extra when is_list(extra) -> args ++ Enum.map(extra, &to_string/1)
      _ -> args
    end
  end

  defp append_optional(args, opts, key, flag) do
    case Keyword.get(opts, key) do
      nil -> args
      value -> args ++ [flag, to_string(value)]
    end
  end

  defp append_repeated(args, opts, key, flag) do
    opts
    |> Keyword.get(key, [])
    |> List.wrap()
    |> Enum.reject(&is_nil/1)
    |> Enum.reduce(args, fn value, acc -> acc ++ [flag, to_string(value)] end)
  end

  defp append_csv(args, opts, key, flag) do
    case Keyword.get(opts, key) do
      nil -> args
      [] -> args
      list when is_list(list) -> args ++ [flag, Enum.map_join(list, ",", &to_string/1)]
      "default" when key == :tools -> args ++ [flag, "default"]
      value -> args ++ [flag, to_string(value)]
    end
  end

  defp append_flag(args, opts, key, flag, default \\ false) do
    if Keyword.get(opts, key, default), do: args ++ [flag], else: args
  end

  defp encode_permission_mode(nil), do: nil
  defp encode_permission_mode(mode) when is_atom(mode), do: Map.fetch!(@permission_modes, mode)
  defp encode_permission_mode(mode) when is_binary(mode), do: mode

  defp system_prompt(nil), do: nil
  defp system_prompt(prompt) when is_list(prompt), do: prompt
  defp system_prompt(prompt) when is_binary(prompt), do: [prompt]

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, _key, []), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)
end
