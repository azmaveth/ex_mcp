defmodule ExMCP.ACP.Adapters.Codex.Permissions do
  @moduledoc false
  # Pure permission mapping for the Codex ACP adapter: approval option
  # construction, structured-decision encoding/decoding, user-input form
  # schemas, and the fail-closed fallback responses used when a client
  # interaction is cancelled, arrives late, or cannot be interpreted. The root
  # adapter owns the pending-request state and the wire I/O.

  alias ExMCP.ACP.Adapters.Codex.Events
  alias ExMCP.Internal.Maps

  @type method :: String.t()
  @type params :: map()
  @type option :: map()
  @type entry :: map()

  # Approval option construction

  @spec permission_tool_call(method(), params()) :: map()
  def permission_tool_call(method, params) do
    %{
      "toolCallId" => params["itemId"] || params["callId"] || params["approvalId"] || method,
      "toolName" => permission_tool_name(method, params),
      "kind" => permission_tool_kind(method),
      "title" => permission_title(method, params),
      "status" => "pending",
      "rawInput" => params
    }
  end

  defp permission_tool_name("item/commandExecution/requestApproval", _params), do: "execute"
  defp permission_tool_name("execCommandApproval", _params), do: "execute"
  defp permission_tool_name("item/fileChange/requestApproval", _params), do: "edit"
  defp permission_tool_name("applyPatchApproval", _params), do: "edit"
  defp permission_tool_name("item/permissions/requestApproval", _params), do: "permissions"

  defp permission_tool_name("mcpServer/elicitation/request", params),
    do: "mcp:#{params["serverName"]}"

  defp permission_tool_name(_method, _params), do: "codex"

  defp permission_tool_kind(method)
       when method in ["item/commandExecution/requestApproval", "execCommandApproval"],
       do: "execute"

  defp permission_tool_kind("item/fileChange/requestApproval"), do: "edit"
  defp permission_tool_kind("applyPatchApproval"), do: "edit"

  defp permission_tool_kind("mcpServer/elicitation/request"), do: "other"
  defp permission_tool_kind(_method), do: "other"

  defp permission_title(method, params)
       when method in ["item/commandExecution/requestApproval", "execCommandApproval"] do
    Events.command_title(params["command"])
  end

  defp permission_title(method, _params)
       when method in ["item/fileChange/requestApproval", "applyPatchApproval"],
       do: "Approve File Changes"

  defp permission_title("item/permissions/requestApproval", _params), do: "Approve Permissions"

  defp permission_title("mcpServer/elicitation/request", params),
    do: params["message"] || "MCP Elicitation"

  defp permission_title(_method, _params), do: "Codex Permission"

  @spec permission_options(method(), params()) :: [option()]
  def permission_options("item/fileChange/requestApproval", _params),
    do: file_change_permission_options()

  def permission_options("item/permissions/requestApproval", _params),
    do: permission_profile_options()

  def permission_options("mcpServer/elicitation/request", params),
    do: mcp_permission_options(params)

  def permission_options(_method, _params) do
    [
      permission_option("allow_once", "Allow Once", "allow_once"),
      permission_option("allow_always", "Allow for Session", "allow_always"),
      permission_option("reject_once", "Reject", "reject_once")
    ]
  end

  defp permission_option(option_id, name, kind) do
    %{"optionId" => option_id, "name" => name, "kind" => kind}
  end

  defp file_change_permission_options do
    [
      permission_option("allow_once", "Yes, proceed", "allow_once"),
      permission_option(
        "allow_for_session",
        "Yes, and don't ask again for these files",
        "allow_always"
      ),
      permission_option("cancel", "No, and tell Codex what to do differently", "reject_once")
    ]
  end

  defp permission_profile_options do
    [
      permission_option(
        "allow_permissions_turn",
        "Yes, grant these permissions for this turn",
        "allow_once"
      ),
      permission_option(
        "allow_permissions_turn_strict_auto_review",
        "Yes, grant for this turn with strict auto review",
        "allow_once"
      ),
      permission_option(
        "allow_permissions_session",
        "Yes, grant these permissions for this session",
        "allow_always"
      ),
      permission_option("reject_permissions", "No, continue without permissions", "reject_once")
    ]
  end

  defp mcp_permission_options(params) do
    meta = params["_meta"] || %{}
    persist = persist_options(meta)
    tool_approval? = mcp_tool_approval?(meta)

    allow_once =
      if tool_approval? do
        mcp_option("allow_once", "Allow", "allow_once", "Run the tool and continue.")
      else
        mcp_option("accept", "Allow", "allow_once", "Allow this request and continue.")
      end

    options = [allow_once]

    options =
      if "session" in persist do
        options ++
          [
            mcp_option(
              "allow_session",
              "Allow for this session",
              "allow_always",
              if(tool_approval?,
                do: "Run the tool and remember this choice for this session.",
                else: "Allow this request and remember this choice for this session."
              )
            )
          ]
      else
        options
      end

    options =
      if "always" in persist do
        options ++
          [
            mcp_option(
              "allow_always",
              "Always allow",
              "allow_always",
              if(tool_approval?,
                do: "Run the tool and remember this choice for future tool calls.",
                else: "Allow this request and remember this choice for future requests."
              )
            )
          ]
      else
        options
      end

    if tool_approval? do
      options ++ [mcp_option("cancel", "Cancel", "reject_once", "Cancel this tool call")]
    else
      options ++
        [
          mcp_option("decline", "Deny", "reject_once", "Decline this request and continue."),
          mcp_option("cancel", "Cancel", "reject_once", "Cancel this request")
        ]
    end
  end

  defp mcp_option(option_id, name, kind, description) do
    %{
      "optionId" => option_id,
      "name" => name,
      "kind" => kind,
      "_meta" => %{"permission" => %{"version" => 1, "description" => description}}
    }
  end

  defp persist_options(%{"persist" => "session"}), do: ["session"]
  defp persist_options(%{"persist" => "always"}), do: ["always"]

  defp persist_options(%{"persist" => persist}) when is_list(persist) do
    Enum.filter(persist, &(&1 in ["session", "always"]))
  end

  defp persist_options(_meta), do: []

  defp mcp_tool_approval?(%{"codex_approval_kind" => "mcp_tool_call"}), do: true
  defp mcp_tool_approval?(_meta), do: false

  @spec command_decision_options(params()) :: {:ok, [{option(), term()}]} | :error
  def command_decision_options(params) do
    with {:ok, decisions} <- parse_available_command_decisions(params) do
      {pairs, _network_index} =
        Enum.reduce(decisions, {[], 0}, fn decision, {pairs, network_index} ->
          case command_decision_option(decision, params, network_index) do
            {:skip, network_index} ->
              {pairs, network_index}

            {option, mapped, network_index} ->
              {pairs ++ [{option, mapped}], network_index}
          end
        end)

      pairs = sort_command_decision_options(pairs)

      if valid_command_option_set?(pairs) do
        {:ok, pairs}
      else
        :error
      end
    end
  end

  defp parse_available_command_decisions(params) do
    case Map.fetch(params, "availableDecisions") do
      :error ->
        {:ok, default_command_decisions(params)}

      {:ok, nil} ->
        {:ok, default_command_decisions(params)}

      {:ok, decisions} when is_list(decisions) and decisions != [] ->
        Enum.reduce_while(decisions, {:ok, []}, fn candidate, {:ok, acc} ->
          case parse_command_decision(candidate, params) do
            {:ok, decision} -> {:cont, {:ok, acc ++ [decision]}}
            :error -> {:halt, :error}
          end
        end)

      {:ok, _other} ->
        :error
    end
  end

  defp default_command_decisions(%{"networkApprovalContext" => network} = params)
       when is_map(network) do
    amendments =
      params
      |> Map.get("proposedNetworkPolicyAmendments")
      |> List.wrap()
      |> Enum.filter(&is_map/1)
      |> Enum.map(fn amendment ->
        %{"applyNetworkPolicyAmendment" => %{"network_policy_amendment" => amendment}}
      end)

    ["accept", "acceptForSession"] ++ amendments ++ ["decline", "cancel"]
  end

  defp default_command_decisions(%{"additionalPermissions" => permissions})
       when is_map(permissions) do
    ["accept", "cancel"]
  end

  defp default_command_decisions(params) do
    decisions = ["accept", "acceptForSession"]

    decisions =
      case params["proposedExecpolicyAmendment"] do
        amendment when is_list(amendment) and amendment != [] ->
          decisions ++
            [
              %{
                "acceptWithExecpolicyAmendment" => %{"execpolicy_amendment" => amendment}
              }
            ]

        _ ->
          decisions
      end

    decisions ++ ["decline", "cancel"]
  end

  defp parse_command_decision(candidate, _params)
       when candidate in ["accept", "acceptForSession", "decline", "cancel"] do
    {:ok, candidate}
  end

  defp parse_command_decision(
         %{"acceptWithExecpolicyAmendment" => %{"execpolicy_amendment" => amendment}} = decision,
         params
       )
       when is_list(amendment) and amendment != [] do
    if same_string_list?(amendment, params["proposedExecpolicyAmendment"]) do
      {:ok, decision}
    else
      :error
    end
  end

  defp parse_command_decision(
         %{"applyNetworkPolicyAmendment" => %{"network_policy_amendment" => amendment}} =
           decision,
         params
       ) do
    host = amendment["host"]
    action = amendment["action"]
    network = params["networkApprovalContext"]

    valid? =
      is_binary(host) and action in ["allow", "deny"] and is_map(network) and
        network["host"] == host and
        Enum.any?(List.wrap(params["proposedNetworkPolicyAmendments"]), fn proposed ->
          is_map(proposed) and proposed["host"] == host and proposed["action"] == action
        end)

    if valid?, do: {:ok, decision}, else: :error
  end

  defp parse_command_decision(_candidate, _params), do: :error

  defp same_string_list?(left, right) when is_list(left) and is_list(right) do
    length(left) == length(right) and Enum.all?(left, &is_binary/1) and left == right
  end

  defp same_string_list?(_left, _right), do: false

  defp command_decision_option("accept", params, network_index) do
    name =
      if is_map(params["networkApprovalContext"]),
        do: "Yes, just this once",
        else: "Yes, proceed"

    {permission_option("allow_once", name, "allow_once"), "accept", network_index}
  end

  defp command_decision_option("acceptForSession", params, network_index) do
    name =
      cond do
        is_map(params["networkApprovalContext"]) ->
          "Yes, and allow this host for this conversation"

        is_map(params["additionalPermissions"]) ->
          "Yes, and allow these permissions for this session"

        true ->
          "Yes, and don't ask again for this command in this session"
      end

    {permission_option("allow_for_session", name, "allow_always"), "acceptForSession",
     network_index}
  end

  defp command_decision_option("decline", _params, network_index) do
    {permission_option("decline", "No, continue without running it", "reject_once"), "decline",
     network_index}
  end

  defp command_decision_option("cancel", _params, network_index) do
    {permission_option("cancel", "No, and tell Codex what to do differently", "reject_once"),
     "cancel", network_index}
  end

  defp command_decision_option(
         %{"acceptWithExecpolicyAmendment" => %{"execpolicy_amendment" => amendment}} = decision,
         _params,
         network_index
       ) do
    prefix = Enum.join(amendment, " ")

    if String.contains?(prefix, ["\n", "\r"]) do
      {:skip, network_index}
    else
      {permission_option(
         "accept_execpolicy_amendment",
         "Yes, and don't ask again for commands that start with `#{prefix}`",
         "allow_always"
       ), decision, network_index}
    end
  end

  defp command_decision_option(
         %{"applyNetworkPolicyAmendment" => %{"network_policy_amendment" => amendment}} =
           decision,
         _params,
         network_index
       ) do
    {name, kind} =
      if amendment["action"] == "allow" do
        {"Yes, and allow this host in the future", "allow_always"}
      else
        {"No, and block this host in the future", "reject_always"}
      end

    {permission_option("apply_network_policy_amendment:#{network_index}", name, kind), decision,
     network_index + 1}
  end

  defp command_decision_option(_decision, _params, network_index), do: {:skip, network_index}

  defp sort_command_decision_options(pairs) do
    Enum.sort_by(pairs, fn {option, _decision} ->
      case option["kind"] do
        "allow_once" -> 0
        "allow_always" -> 1
        _ -> 2
      end
    end)
  end

  defp valid_command_option_set?(pairs) do
    kinds = Enum.map(pairs, fn {option, _decision} -> option["kind"] end)
    ids = Enum.map(pairs, fn {option, _decision} -> option["optionId"] end)

    has_allow = Enum.any?(kinds, &(&1 in ["allow_once", "allow_always"]))
    has_reject = Enum.any?(kinds, &(&1 in ["reject_once", "reject_always"]))
    unique_ids? = length(Enum.uniq(ids)) == length(ids)

    pairs != [] and has_allow and has_reject and unique_ids?
  end

  @spec permission_request_meta(method(), params()) :: map()
  def permission_request_meta(method, params) do
    %{
      "ex_mcp" => %{"codex" => %{"method" => method, "params" => params}}
    }
    |> maybe_put_permission_meta(permission_prompt_meta(method, params))
  end

  defp maybe_put_permission_meta(meta, nil), do: meta
  defp maybe_put_permission_meta(meta, permission), do: Map.put(meta, "permission", permission)

  defp permission_prompt_meta(method, params)
       when method in ["item/commandExecution/requestApproval", "execCommandApproval"] do
    title =
      if is_map(params["networkApprovalContext"]),
        do: "Allow network access?",
        else: "Run command?"

    permission_title_meta(title, params["reason"])
  end

  defp permission_prompt_meta(method, params)
       when method in ["item/fileChange/requestApproval", "applyPatchApproval"] do
    permission_title_meta("Make edits?", params["reason"])
  end

  defp permission_prompt_meta("item/permissions/requestApproval", params) do
    permission_title_meta("Grant permissions?", params["reason"])
  end

  defp permission_prompt_meta(_method, _params), do: nil

  defp permission_title_meta(title, reason) do
    %{"version" => 1, "title" => title}
    |> Maps.put_non_empty("description", trimmed_permission_text(reason))
  end

  defp trimmed_permission_text(value) when is_binary(value) do
    case String.trim(value) do
      "" -> nil
      text -> text
    end
  end

  defp trimmed_permission_text(_value), do: nil

  # User-input form

  # Mirrors codex-acp's `request_user_input` form (agentclientprotocol/codex-acp#299):
  # the full question is the field title and the short header its description,
  # every primary question is required, an `isOther` question with options gets
  # a "None of the above" choice, and its free text lives in a separate note
  # field tagged `_meta.codex.role: "user_note"`. The note travels back to
  # Codex as `user_note: <text>` beside the selection rather than replacing it.
  @user_input_other_option "None of the above"
  @user_input_note_prefix "user_note: "

  @spec user_input_schema([map()]) :: {map(), [String.t()], map()}
  def user_input_schema(questions) do
    question_ids = MapSet.new(questions, & &1["id"])

    Enum.reduce(questions, {%{}, [], %{}}, fn question, {properties, required, note_fields} ->
      id = question["id"]

      if is_binary(id) and id != "" do
        has_other_answer = question["isOther"] == true and List.wrap(question["options"]) != []
        properties = Map.put(properties, id, user_input_property(question, has_other_answer))

        {properties, note_fields} =
          if has_other_answer do
            note_id = user_input_note_field_id(id, question_ids)

            note = %{
              "type" => "string",
              "title" => "Additional answer or note",
              "_meta" => %{
                "codex" => %{
                  "questionId" => id,
                  "role" => "user_note",
                  "isSecret" => question["isSecret"] == true
                }
              }
            }

            {Map.put(properties, note_id, note), Map.put(note_fields, id, note_id)}
          else
            {properties, note_fields}
          end

        {properties, required ++ [id], note_fields}
      else
        {properties, required, note_fields}
      end
    end)
  end

  defp user_input_note_field_id(question_id, question_ids, index \\ 0) do
    candidate = question_id <> "_note" <> if(index == 0, do: "", else: Integer.to_string(index))

    if MapSet.member?(question_ids, candidate),
      do: user_input_note_field_id(question_id, question_ids, index + 1),
      else: candidate
  end

  defp user_input_property(question, has_other_answer) do
    base =
      %{
        "type" => "string",
        "title" => question["question"] || question["header"] || question["id"],
        "_meta" => %{
          "codex" => %{
            "isOther" => question["isOther"] == true,
            "isSecret" => question["isSecret"] == true
          }
        }
      }
      |> Maps.put_non_empty("description", question["header"])

    case question["options"] do
      options when is_list(options) and options != [] ->
        choices = Enum.map(options, &user_input_option/1)

        choices =
          if has_other_answer and
               not Enum.any?(options, &(&1["label"] == @user_input_other_option)),
             do:
               choices ++
                 [
                   %{
                     "const" => @user_input_other_option,
                     "title" => @user_input_other_option,
                     "description" => "Provide a different answer in the note field."
                   }
                 ],
             else: choices

        Map.put(base, "oneOf", choices)

      _no_options ->
        base
    end
  end

  defp user_input_option(option) do
    %{"const" => option["label"], "title" => option["label"]}
    |> Maps.put_non_empty("description", option["description"])
  end

  # Structured-decision decoding and fail-closed fallbacks

  @spec permission_response(entry(), map()) :: map()
  def permission_response(
        %{method: "item/commandExecution/requestApproval", params: params},
        response
      ) do
    command_permission_response(params, response)
  end

  def permission_response(%{method: "item/fileChange/requestApproval"}, response) do
    file_change_permission_response(response)
  end

  def permission_response(
        %{method: "item/permissions/requestApproval", params: params},
        response
      ) do
    permissions_approval_response(params, response)
  end

  def permission_response(%{method: "mcpServer/elicitation/request", params: params}, response) do
    mcp_permission_response(params, response)
  end

  def permission_response(%{method: method}, %{
        "result" => %{"outcome" => %{"outcome" => "cancelled"}}
      }) do
    codex_cancel_response(method)
  end

  def permission_response(%{method: method}, %{
        "result" => %{"outcome" => %{"optionId" => option_id}}
      }) do
    codex_decision_response(method, option_id)
  end

  def permission_response(%{method: method}, %{"error" => _error}) do
    codex_cancel_response(method)
  end

  def permission_response(%{method: method}, _response), do: codex_cancel_response(method)

  defp command_permission_response(_params, %{
         "result" => %{"outcome" => %{"outcome" => "cancelled"}}
       }) do
    %{"decision" => "cancel"}
  end

  defp command_permission_response(params, %{
         "result" => %{"outcome" => %{"optionId" => option_id}}
       }) do
    case command_decision_options(params) do
      {:ok, pairs} ->
        case Enum.find(pairs, fn {option, _decision} -> option["optionId"] == option_id end) do
          {_option, decision} when is_binary(decision) -> %{"decision" => decision}
          {_option, decision} when is_map(decision) -> %{"decision" => decision}
          nil -> %{"decision" => "cancel"}
        end

      :error ->
        %{"decision" => "cancel"}
    end
  end

  defp command_permission_response(_params, _response), do: %{"decision" => "cancel"}

  defp file_change_permission_response(%{
         "result" => %{"outcome" => %{"outcome" => "cancelled"}}
       }) do
    %{"decision" => "cancel"}
  end

  defp file_change_permission_response(%{"result" => %{"outcome" => %{"optionId" => option_id}}}) do
    decision =
      case option_id do
        "allow_once" -> "accept"
        "allow_for_session" -> "acceptForSession"
        "cancel" -> "cancel"
        _ -> "cancel"
      end

    %{"decision" => decision}
  end

  defp file_change_permission_response(_response), do: %{"decision" => "cancel"}

  defp permissions_approval_response(_params, %{
         "result" => %{"outcome" => %{"outcome" => "cancelled"}}
       }) do
    reject_permissions_response()
  end

  defp permissions_approval_response(params, %{
         "result" => %{"outcome" => %{"optionId" => option_id}}
       }) do
    requested = params["permissions"] || %{}

    case option_id do
      "allow_permissions_turn" ->
        granted_permissions_response(requested, "turn", false)

      "allow_permissions_turn_strict_auto_review" ->
        granted_permissions_response(requested, "turn", true)

      "allow_permissions_session" ->
        granted_permissions_response(requested, "session", false)

      _ ->
        reject_permissions_response()
    end
  end

  defp permissions_approval_response(_params, _response), do: reject_permissions_response()

  defp granted_permissions_response(permissions, scope, strict_auto_review) do
    %{
      "permissions" => granted_permissions(permissions),
      "scope" => scope,
      "strictAutoReview" => strict_auto_review
    }
  end

  defp reject_permissions_response do
    %{"permissions" => %{}, "scope" => "turn", "strictAutoReview" => false}
  end

  defp granted_permissions(permissions) when is_map(permissions) do
    Map.take(permissions, ["network", "fileSystem"])
  end

  defp granted_permissions(_permissions), do: %{}

  defp mcp_permission_response(_params, %{
         "result" => %{"outcome" => %{"outcome" => "cancelled"}}
       }) do
    %{"action" => "cancel"}
  end

  defp mcp_permission_response(params, %{"result" => %{"outcome" => %{"optionId" => option_id}}}) do
    meta = params["_meta"] || %{}
    mcp_option_response(option_id, persist_options(meta), mcp_tool_approval?(meta))
  end

  defp mcp_permission_response(_params, _response), do: %{"action" => "cancel"}

  defp mcp_option_response("allow_session", persist, _tool_approval?) do
    mcp_persist_accept(persist, "session")
  end

  defp mcp_option_response("allow_always", persist, _tool_approval?) do
    mcp_persist_accept(persist, "always")
  end

  defp mcp_option_response("allow_once", _persist, true), do: %{"action" => "accept"}
  defp mcp_option_response("accept", _persist, false), do: %{"action" => "accept"}
  defp mcp_option_response("decline", _persist, false), do: %{"action" => "decline"}
  defp mcp_option_response(_option_id, _persist, _tool_approval?), do: %{"action" => "cancel"}

  defp mcp_persist_accept(persist, scope) do
    if scope in persist do
      %{"action" => "accept", "_meta" => %{"persist" => scope}}
    else
      %{"action" => "cancel"}
    end
  end

  defp codex_decision_response(method, option_id)
       when method in ["execCommandApproval", "applyPatchApproval"] do
    %{"decision" => legacy_review_decision(option_id)}
  end

  defp codex_decision_response(_method, option_id) do
    %{"decision" => app_server_decision(option_id)}
  end

  @spec codex_cancel_response(method()) :: map()
  def codex_cancel_response(method) when method in ["execCommandApproval", "applyPatchApproval"],
    do: %{"decision" => "abort"}

  def codex_cancel_response("mcpServer/elicitation/request"), do: %{"action" => "cancel"}

  def codex_cancel_response("item/permissions/requestApproval"),
    do: reject_permissions_response()

  def codex_cancel_response(_method), do: %{"decision" => "cancel"}

  defp app_server_decision(option_id) do
    cond do
      always_option?(option_id) -> "acceptForSession"
      allow_option?(option_id) -> "accept"
      String.contains?(to_string(option_id), "cancel") -> "cancel"
      true -> "decline"
    end
  end

  defp legacy_review_decision(option_id) do
    cond do
      always_option?(option_id) -> "approved_for_session"
      allow_option?(option_id) -> "approved"
      String.contains?(to_string(option_id), "cancel") -> "abort"
      true -> "denied"
    end
  end

  defp allow_option?(option_id) do
    option_id = to_string(option_id)

    String.contains?(option_id, "allow") || String.contains?(option_id, "accept") ||
      String.contains?(option_id, "approved")
  end

  defp always_option?(option_id) do
    option_id = to_string(option_id)
    String.contains?(option_id, "always") || String.contains?(option_id, "session")
  end

  @spec elicitation_response(map()) :: {map(), boolean()}
  def elicitation_response(%{"result" => %{"action" => "accept"} = result}) do
    content = result["content"]

    if is_nil(content) or is_map(content) do
      {result, true}
    else
      {%{"action" => "cancel"}, false}
    end
  end

  def elicitation_response(%{"result" => %{"action" => action} = result})
      when action in ["decline", "cancel"],
      do: {result, false}

  def elicitation_response(_response), do: {%{"action" => "cancel"}, false}

  @spec user_input_response(entry(), map()) :: map()
  def user_input_response(entry, %{"result" => %{"action" => "accept", "content" => content}})
      when is_map(content) do
    answers =
      Enum.reduce(entry.questions, %{}, fn question, answers ->
        id = question["id"]
        values = user_input_values(content[id])

        notes =
          case Map.get(entry, :note_fields, %{})[id] do
            nil ->
              []

            note_id ->
              Enum.map(
                user_input_values(content[note_id]),
                &(@user_input_note_prefix <> String.trim(&1))
              )
          end

        case values ++ notes do
          [] -> answers
          all -> Map.put(answers, id, %{"answers" => all})
        end
      end)

    %{"answers" => answers}
  end

  def user_input_response(_entry, _response), do: %{"answers" => %{}}

  defp user_input_values(value) when is_binary(value),
    do: if(String.trim(value) == "", do: [], else: [value])

  defp user_input_values(values) when is_list(values),
    do: Enum.filter(values, &(is_binary(&1) and String.trim(&1) != ""))

  defp user_input_values(_value), do: []

  @spec late_server_request_result(method()) :: map()
  def late_server_request_result("item/tool/requestUserInput"), do: %{"answers" => %{}}
  def late_server_request_result("mcpServer/elicitation/request"), do: %{"action" => "cancel"}
  def late_server_request_result(method), do: codex_cancel_response(method)

  @spec cancelled_client_request_result(entry()) :: map()
  def cancelled_client_request_result(%{kind: :user_input}), do: %{"answers" => %{}}
  def cancelled_client_request_result(%{kind: :elicitation}), do: %{"action" => "cancel"}
  def cancelled_client_request_result(%{method: method}), do: codex_cancel_response(method)
end
