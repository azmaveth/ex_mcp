defmodule ExMCP.ACP.Adapters.Codex.Config do
  @moduledoc """
  Pure config and mode helpers for the Codex ACP adapter.
  """

  @default_mode "auto"
  @default_reasoning_effort "medium"

  @mode_profiles %{
    "read-only" => %{permissions: ":read-only", approval: "on-request"},
    "auto" => %{permissions: ":workspace", approval: "on-request"},
    "full-access" => %{permissions: ":danger-no-sandbox", approval: "never"}
  }

  @legacy_mode_aliases %{
    "suggest" => "read-only",
    "auto-edit" => "auto",
    "full-auto" => "full-access"
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
        "name" => "Read Only",
        "description" => "Inspect files and propose changes without writing to the workspace"
      },
      %{
        "id" => "auto",
        "name" => "Auto",
        "description" => "Edit within the workspace and request approval for sensitive actions"
      },
      %{
        "id" => "full-access",
        "name" => "Full Access",
        "description" => "Run without sandbox restrictions"
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
    mode_id = to_string(mode_id)
    Map.get(@legacy_mode_aliases, mode_id, mode_id)
  end

  @spec merge_mode_wire_params(map(), String.t() | nil) :: map()
  def merge_mode_wire_params(map, nil), do: map

  def merge_mode_wire_params(map, mode_id) do
    case Map.get(@mode_profiles, mode_id) do
      nil ->
        map

      %{permissions: permissions, approval: approval} ->
        map
        |> Map.put("permissions", permissions)
        |> Map.put("approvalPolicy", approval)
    end
  end

  @spec mode_id_from_result(map()) :: String.t() | nil
  def mode_id_from_result(result) do
    active_profile =
      get_in(result, ["activePermissionProfile", "id"]) ||
        get_in(result, ["settings", "activePermissionProfile", "id"])

    case active_profile do
      ":read-only" -> "read-only"
      ":workspace" -> "auto"
      ":danger-no-sandbox" -> "full-access"
      _ -> nil
    end
  end
end
