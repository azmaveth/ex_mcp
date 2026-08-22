defmodule ExMCP.ACP.Adapters.ZCode.Protocol do
  @moduledoc """
  Pure helpers for ZCode's `app-server` stdio protocol.

  ZCode Protocol v1 is NDJSON over stdio. Messages are JSON-RPC-shaped but
  omit the `jsonrpc` field — the envelope is `{id?, method?, params?, result?,
  error?}`. IDs may be strings or integers.

  This module owns only wire-shape construction and parsing. Translation
  between ACP and ZCode message semantics lives in `ZCode.Mapper`.
  """

  alias ExMCP.Internal.Maps

  # ZCode turn.completed resultType → ACP stopReason
  @error_stop_reasons %{
    "error_max_turns" => "max_turn_requests",
    "error_max_budget" => "max_turn_requests",
    "error_max_tool_calls" => "max_turn_requests"
  }

  @doc "Returns the ZCode command line used by app-server sessions."
  @spec command(keyword()) :: {String.t(), [String.t()]}
  def command(opts) do
    {cli_path(opts), ["app-server"]}
  end

  @doc "Returns the ZCode executable path."
  @spec cli_path(keyword()) :: String.t()
  def cli_path(opts) do
    Keyword.get(opts, :cli_path) || System.get_env("ZCODE_EXECUTABLE") || "zcode"
  end

  @doc "Environment required for ZCode app-server."
  @spec env(keyword()) :: map()
  def env(opts) do
    base = %{}

    case Keyword.get(opts, :env) do
      env when is_list(env) -> Map.merge(base, Map.new(env))
      env when is_map(env) -> Map.merge(base, env)
      _ -> base
    end
  end

  @doc "Builds the workspace descriptor required by the ZCode Protocol."
  @spec workspace_ref(String.t()) :: map()
  def workspace_ref(path) when is_binary(path) and path != "" do
    %{
      "workspacePath" => path,
      "workspaceKey" => path
    }
  end

  @doc "Encodes one ZCode message as an NDJSON line."
  @spec line(map()) :: String.t()
  def line(message), do: Jason.encode!(message) <> "\n"

  @doc "Builds a ZCode request message."
  @spec encode_request(String.t() | integer(), String.t(), map() | nil) :: map()
  def encode_request(id, method, params \\ %{}) do
    msg =
      %{"id" => id, "method" => method}
      |> maybe_put("params", params)

    msg
  end

  @doc "Builds a ZCode success response."
  @spec encode_response(String.t() | integer(), map() | nil) :: map()
  def encode_response(id, result \\ %{}) do
    %{"id" => id, "result" => result}
  end

  @doc "Builds a ZCode error response."
  @spec encode_error(String.t() | integer(), integer(), String.t()) :: map()
  def encode_error(id, code, message) do
    %{"id" => id, "error" => %{"code" => code, "message" => message}}
  end

  @doc "Builds a ZCode notification (no id)."
  @spec encode_notification(String.t(), map() | nil) :: map()
  def encode_notification(method, params \\ nil) do
    %{"method" => method} |> maybe_put("params", params)
  end

  @doc """
  Maps a ZCode turn.completed resultType to an ACP stopReason.

  ZCode result types: success, cancelled, error_max_turns,
  error_max_budget, error_during_execution, error_max_tool_calls.
  """
  @spec stop_reason(String.t() | nil) :: String.t()
  def stop_reason(nil), do: "end_turn"
  def stop_reason("success"), do: "end_turn"
  def stop_reason("cancelled"), do: "cancelled"

  def stop_reason(reason) when is_map_key(@error_stop_reasons, reason),
    do: Map.fetch!(@error_stop_reasons, reason)

  def stop_reason("error_during_execution"), do: "refusal"
  def stop_reason(_), do: "end_turn"

  @doc """
  Maps a ZCode `interaction/requestPermission` request into an ACP tool call
  suitable for `session/request_permission`.
  """
  @spec permission_tool_call(map(), String.t() | nil) :: map()
  def permission_tool_call(request, cwd) do
    tool_name = request["toolName"] || "tool"
    input = request["input"] || %{}
    tool_call_id = request["toolCallId"] || "tool_#{System.unique_integer([:positive])}"

    info = tool_info(tool_name, input, tool_call_id, cwd)

    info
    |> Map.take(["title", "kind", "content", "locations", "rawInput"])
    |> Map.put("toolCallId", tool_call_id)
    |> Map.put_new("title", tool_name)
    |> compact()
  end

  @doc """
  Converts ZCode permission options into ACP permission options.

  ZCode options carry a pre-resolved `response` field (the decision that
  selecting each option produces). We map the ZCode decision vocabulary
  (`allow`/`deny`/`escalate`/`modify`) to ACP permission option kinds.
  """
  @spec permission_options([map()]) :: [map()]
  def permission_options(options) when is_list(options) do
    Enum.map(options, &permission_option/1)
  end

  defp permission_option(%{"optionId" => option_id, "kind" => _kind, "name" => name} = opt) do
    response = opt["response"] || %{}
    decision = response["decision"] || "deny"

    %{
      "optionId" => option_id,
      "name" => name,
      "kind" => acp_permission_kind(decision, option_id)
    }
    |> Maps.put_present("description", opt["description"])
  end

  defp permission_option(%{"optionId" => option_id, "name" => name} = opt) do
    response = opt["response"] || %{}
    decision = response["decision"] || "deny"

    %{
      "optionId" => option_id,
      "name" => name,
      "kind" => acp_permission_kind(decision, option_id)
    }
    |> Maps.put_present("description", opt["description"])
  end

  defp acp_permission_kind("allow", option_id) do
    if String.ends_with?(option_id, "_always"), do: "allow_always", else: "allow_once"
  end

  defp acp_permission_kind("deny", option_id) do
    if String.ends_with?(option_id, "_always"), do: "reject_always", else: "reject_once"
  end

  defp acp_permission_kind("escalate", _), do: "allow_once"
  defp acp_permission_kind("modify", _), do: "allow_once"
  defp acp_permission_kind(_, _), do: "allow_once"

  @doc """
  Converts an ACP permission outcome back into a ZCode permission response
  (`Nv` shape: decision/reason/modifiedInput/permissionUpdates).

  The ACP client responds with `{outcome: "selected", optionId: ...}` or
  `{outcome: "cancelled"}`. We find the matching ZCode option's pre-resolved
  `response` and return it, falling back to a deny.
  """
  @spec permission_result(map(), [map()]) :: map()
  def permission_result(%{"outcome" => %{"outcome" => nested}} = response, options) do
    permission_result(Map.put(response, "outcome", nested), options)
  end

  def permission_result(%{"outcome" => "selected", "optionId" => option_id}, options) do
    case Enum.find(options, &(&1["optionId"] == option_id)) do
      %{"response" => response} -> response
      _ -> deny_response("Permission option not found")
    end
  end

  def permission_result(%{"outcome" => "selected", "option" => %{"kind" => kind}}, _options) do
    decision_from_acp_kind(kind)
  end

  def permission_result(%{"outcome" => "cancelled"}, _options) do
    deny_response("Permission request cancelled")
  end

  def permission_result(_outcome, _options) do
    deny_response("Permission request not resolved")
  end

  defp decision_from_acp_kind(kind) when kind in ["allow_once", "allow_always"] do
    %{"decision" => "allow"}
  end

  defp decision_from_acp_kind(kind) when kind in ["reject_once", "reject_always"] do
    %{"decision" => "deny", "reason" => "Permission denied"}
  end

  defp decision_from_acp_kind(_), do: deny_response("Unrecognized permission kind")

  defp deny_response(reason) do
    %{"decision" => "deny", "reason" => reason}
  end

  @doc """
  Converts ACP prompt blocks into ZCode session/send content.

  ZCode's `session/send` accepts a `content` string. In v1 we support
  text-only prompts. Image/embedded-context support is a follow-up.
  """
  @spec prompt_content(list() | String.t() | nil) :: {:ok, String.t()} | {:error, String.t()}
  def prompt_content(nil), do: {:ok, ""}

  def prompt_content(text) when is_binary(text), do: {:ok, text}

  def prompt_content(blocks) when is_list(blocks) do
    text =
      blocks
      |> Enum.map_join("", fn
        %{"type" => "text", "text" => text} when is_binary(text) -> text
        %{"text" => text} when is_binary(text) -> text
        _ -> ""
      end)

    {:ok, text}
  end

  def prompt_content(_), do: {:error, "ZCode prompt must be text or a list of content blocks"}

  @doc "Extracts the session ID from a ZCode session snapshot."
  @spec session_id(map()) :: String.t() | nil
  def session_id(%{"session" => %{"sessionId" => id}}), do: id
  def session_id(%{"sessionId" => id}), do: id
  def session_id(%{"projection" => %{"sessionId" => id}}), do: id
  def session_id(_), do: nil

  @doc "Extracts the model reference from a ZCode session/snapshot."
  @spec model_ref(map()) :: map() | nil
  def model_ref(%{"session" => %{"model" => %{"providerId" => _, "modelId" => _} = ref}}), do: ref

  def model_ref(%{"model" => %{"providerId" => _, "modelId" => _} = ref}), do: ref

  def model_ref(%{"projection" => %{"model" => ref}}) when is_map(ref), do: ref
  def model_ref(_), do: nil

  # Tool info helpers — simplified compared to ClaudeSDK.ToolInfo but same shape.
  # ZCode tool calls arrive via tool.updated events with toolName + input.

  @tool_kinds %{
    "Read" => "read",
    "Write" => "edit",
    "Edit" => "edit",
    "MultiEdit" => "edit",
    "Bash" => "execute",
    "Grep" => "search",
    "Glob" => "search",
    "WebFetch" => "fetch",
    "WebSearch" => "search",
    "Task" => "think",
    "Agent" => "think",
    "TodoWrite" => "think",
    "TodoRead" => "read"
  }

  defp tool_info(name, input, id, cwd) do
    name = name || "tool"
    input = input || %{}

    do_tool_info(name, input, id, cwd)
    |> Map.put_new("kind", Map.get(@tool_kinds, name, "other"))
    |> Map.put_new("rawInput", input)
    |> compact()
  end

  defp do_tool_info("Bash", input, id, _cwd) do
    command = input["command"] || ""

    %{
      "title" => if(command != "", do: truncate(command, 80), else: "Terminal"),
      "kind" => "execute",
      "content" => [%{"type" => "terminal", "terminalId" => id}],
      "_meta" => %{"terminal_info" => %{"terminal_id" => id}}
    }
  end

  defp do_tool_info("Read", input, _id, cwd) do
    path = input["file_path"] || input["path"]

    %{
      "title" => "Read #{display_path(path, cwd)}",
      "kind" => "read",
      "locations" => if(path, do: [%{"path" => path}], else: [])
    }
  end

  defp do_tool_info("Write", input, _id, cwd) do
    path = input["file_path"] || input["path"]

    %{
      "title" => "Write #{display_path(path, cwd)}",
      "kind" => "edit",
      "locations" => if(path, do: [%{"path" => path}], else: [])
    }
  end

  defp do_tool_info("Edit", input, _id, cwd) do
    path = input["file_path"] || input["path"]

    %{
      "title" => "Edit #{display_path(path, cwd)}",
      "kind" => "edit",
      "locations" => if(path, do: [%{"path" => path}], else: [])
    }
  end

  defp do_tool_info("Grep", input, _id, _cwd) do
    %{"title" => "Search: #{truncate(input["pattern"] || "", 60)}", "kind" => "search"}
  end

  defp do_tool_info("Glob", input, _id, _cwd) do
    %{"title" => "Find: #{truncate(input["pattern"] || "", 60)}", "kind" => "search"}
  end

  defp do_tool_info("WebFetch", input, _id, _cwd) do
    %{"title" => "Fetch: #{truncate(input["url"] || "", 80)}", "kind" => "fetch"}
  end

  defp do_tool_info("WebSearch", input, _id, _cwd) do
    %{"title" => "Search: #{truncate(input["query"] || "", 60)}", "kind" => "search"}
  end

  defp do_tool_info(name, input, _id, _cwd) when name in ["Task", "Agent"] do
    desc = input["description"] || input["prompt"] || "Task"

    %{
      "title" => truncate(desc, 80),
      "kind" => "think",
      "content" => text_content(input["prompt"])
    }
  end

  defp do_tool_info(name, _input, _id, _cwd) do
    %{"title" => name, "kind" => Map.get(@tool_kinds, name, "other")}
  end

  defp text_content(nil), do: []
  defp text_content(""), do: []

  defp text_content(text),
    do: [%{"type" => "content", "content" => %{"type" => "text", "text" => text}}]

  defp display_path(nil, _cwd), do: "File"

  defp display_path(path, cwd) when is_binary(path) and is_binary(cwd) do
    resolved_cwd = Path.expand(cwd)

    if String.starts_with?(path, resolved_cwd <> "/") do
      Path.relative_to(path, resolved_cwd)
    else
      Path.basename(path)
    end
  end

  defp display_path(path, _cwd) when is_binary(path), do: Path.basename(path)

  defp truncate(str, max) when is_binary(str) and byte_size(str) > max do
    String.slice(str, 0, max) <> "..."
  end

  defp truncate(str, _max) when is_binary(str), do: str
  defp truncate(_, _max), do: ""

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, _key, value) when value in [%{}, []], do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp compact(map) do
    map
    |> Enum.reject(fn {_key, value} -> value in [nil, [], %{}] end)
    |> Map.new()
  end
end
