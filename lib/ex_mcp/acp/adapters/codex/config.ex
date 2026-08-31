defmodule ExMCP.ACP.Adapters.Codex.Config do
  @moduledoc """
  Pure config and mode helpers for the Codex ACP adapter.
  """

  @default_mode "agent"
  @default_reasoning_effort "medium"

  @workspace_write_policy %{
    "type" => "workspaceWrite",
    "writableRoots" => [],
    "networkAccess" => false,
    "excludeTmpdirEnvVar" => false,
    "excludeSlashTmp" => false
  }

  @mode_profiles %{
    "read-only" => %{
      approval: "on-request",
      approvals_reviewer: "user",
      sandbox: "workspace-write",
      sandbox_policy: @workspace_write_policy
    },
    "agent" => %{
      approval: "on-request",
      approvals_reviewer: "auto_review",
      sandbox: "workspace-write",
      sandbox_policy: @workspace_write_policy
    },
    "agent-full-access" => %{
      approval: "never",
      approvals_reviewer: "user",
      sandbox: "danger-full-access",
      sandbox_policy: %{"type" => "dangerFullAccess"}
    }
  }

  @reasoning_efforts [
    {"minimal", "Minimal"},
    {"low", "Low"},
    {"medium", "Medium"},
    {"high", "High"}
  ]

  @spec default_mode() :: String.t()
  def default_mode, do: @default_mode

  @spec default_reasoning_effort() :: String.t()
  def default_reasoning_effort, do: @default_reasoning_effort

  @spec reasoning_efforts() :: [{String.t(), String.t()}]
  def reasoning_efforts, do: @reasoning_efforts

  @spec modes() :: [map()]
  def modes do
    [
      %{
        "id" => "read-only",
        "name" => "Ask for approval",
        "description" => "Always ask to edit external files and use the internet",
        "_meta" => %{"kind" => "standard"}
      },
      %{
        "id" => "agent",
        "name" => "Approve for me",
        "description" => "Only ask for actions detected as potentially unsafe",
        "_meta" => %{"kind" => "auto_review"}
      },
      %{
        "id" => "agent-full-access",
        "name" => "Full access",
        "description" => "Unrestricted access to the internet and any file on your computer",
        "_meta" => %{"kind" => "full_access"}
      }
    ]
  end

  @spec normalize_requested_mode(any()) :: {:ok, String.t()} | {:error, String.t()}
  def normalize_requested_mode(mode_id) do
    mode_id = normalize_mode_id(mode_id)

    if Map.has_key?(@mode_profiles, mode_id) do
      {:ok, mode_id}
    else
      {:error, "Unsupported Codex mode: #{inspect(mode_id)}"}
    end
  end

  @spec normalize_mode_id(any()) :: String.t()
  def normalize_mode_id(nil), do: @default_mode

  def normalize_mode_id(mode_id) do
    to_string(mode_id)
  end

  @spec merge_thread_mode_wire_params(map(), String.t() | nil) :: map()
  def merge_thread_mode_wire_params(map, nil), do: map

  def merge_thread_mode_wire_params(map, mode_id) do
    case Map.get(@mode_profiles, mode_id) do
      nil ->
        map

      %{sandbox: sandbox, approval: approval} ->
        map
        |> Map.put("sandbox", sandbox)
        |> Map.put("approvalPolicy", approval)
    end
  end

  @spec merge_turn_mode_wire_params(map(), String.t() | nil, [String.t()]) :: map()
  def merge_turn_mode_wire_params(map, mode_id, additional_directories \\ [])

  def merge_turn_mode_wire_params(map, nil, _additional_directories), do: map

  def merge_turn_mode_wire_params(map, mode_id, additional_directories) do
    case Map.get(@mode_profiles, mode_id) do
      nil ->
        map

      %{sandbox_policy: sandbox_policy, approval: approval, approvals_reviewer: reviewer} ->
        map
        |> Map.put("sandboxPolicy", add_writable_roots(sandbox_policy, additional_directories))
        |> Map.put("approvalPolicy", approval)
        |> Map.put("approvalsReviewer", reviewer)
    end
  end

  @doc false
  @spec merge_mode_wire_params(map(), String.t() | nil) :: map()
  def merge_mode_wire_params(map, mode_id), do: merge_thread_mode_wire_params(map, mode_id)

  @spec mode_id_from_result(map()) :: String.t() | nil
  def mode_id_from_result(result) do
    active_profile =
      get_in(result, ["activePermissionProfile", "id"]) ||
        get_in(result, ["settings", "activePermissionProfile", "id"])

    case active_profile do
      ":read-only" -> "read-only"
      ":workspace" -> "agent"
      ":danger-no-sandbox" -> "agent-full-access"
      _ -> mode_id_from_settings(result["threadSettings"] || result["settings"] || result)
    end
  end

  defp mode_id_from_settings(%{"sandboxPolicy" => %{"type" => "readOnly"}}), do: "read-only"

  defp mode_id_from_settings(%{"sandboxPolicy" => %{"type" => "workspaceWrite"}} = settings) do
    workspace_write_mode(settings["approvalsReviewer"])
  end

  defp mode_id_from_settings(%{"sandboxPolicy" => %{"type" => "dangerFullAccess"}}),
    do: "agent-full-access"

  defp mode_id_from_settings(%{"sandbox" => "read-only"}), do: "read-only"

  defp mode_id_from_settings(%{"sandbox" => "workspace-write"} = settings) do
    workspace_write_mode(settings["approvalsReviewer"])
  end

  defp mode_id_from_settings(%{"sandbox" => "danger-full-access"}), do: "agent-full-access"
  defp mode_id_from_settings(_settings), do: nil

  defp workspace_write_mode("user"), do: "read-only"
  defp workspace_write_mode("auto_review"), do: "agent"
  defp workspace_write_mode(_reviewer), do: "agent"

  defp add_writable_roots(%{"type" => "workspaceWrite"} = sandbox_policy, additional_directories) do
    roots =
      sandbox_policy
      |> Map.get("writableRoots", [])
      |> List.wrap()
      |> Enum.concat(additional_directories)
      |> Enum.filter(&is_binary/1)
      |> Enum.uniq()

    Map.put(sandbox_policy, "writableRoots", roots)
  end

  defp add_writable_roots(sandbox_policy, _additional_directories), do: sandbox_policy
end
