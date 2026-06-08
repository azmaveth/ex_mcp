defmodule ExMCP.ACP.Adapters.Pi.Settings do
  @moduledoc false

  @spec load(String.t() | nil, keyword()) :: map()
  def load(cwd, opts \\ []) do
    global = read_json(Path.join(agent_dir(), "settings.json"))
    project = read_json(project_settings_path(cwd))

    global
    |> deep_merge(project)
    |> Map.put("_agentDir", agent_dir())
    |> Map.put("_cwd", cwd)
    |> Map.put("_opts", opts)
  end

  @spec agent_dir() :: String.t()
  def agent_dir do
    case System.get_env("PI_CODING_AGENT_DIR") do
      nil -> Path.join(System.user_home!(), ".pi/agent")
      "" -> Path.join(System.user_home!(), ".pi/agent")
      dir -> Path.expand(dir)
    end
  end

  @spec enable_skill_commands?(map()) :: boolean()
  def enable_skill_commands?(settings) do
    cond do
      is_boolean(settings["enableSkillCommands"]) ->
        settings["enableSkillCommands"]

      is_boolean(get_in(settings, ["skills", "enableSkillCommands"])) ->
        get_in(settings, ["skills", "enableSkillCommands"])

      true ->
        true
    end
  end

  @spec quiet_startup?(map()) :: boolean()
  def quiet_startup?(settings) do
    cond do
      is_boolean(settings["quietStartup"]) -> settings["quietStartup"]
      is_boolean(settings["quietStart"]) -> settings["quietStart"]
      true -> false
    end
  end

  @spec update_notice?(keyword()) :: boolean()
  def update_notice?(opts) do
    Keyword.get_lazy(opts, :update_notice, fn ->
      System.get_env("PI_ACP_UPDATE_NOTICE") in ["1", "true", "TRUE", "yes", "on"]
    end)
  end

  @spec pi_command(keyword(), map()) :: String.t()
  def pi_command(opts, settings) do
    Keyword.get(opts, :cli_path) ||
      Keyword.get(opts, :pi_command) ||
      System.get_env("PI_ACP_PI_COMMAND") ||
      settings["piCommand"] ||
      "pi"
  end

  defp project_settings_path(cwd) when is_binary(cwd) and cwd != "",
    do: Path.join([cwd, ".pi", "settings.json"])

  defp project_settings_path(_cwd), do: nil

  defp read_json(nil), do: %{}

  defp read_json(path) do
    with {:ok, raw} <- File.read(path),
         {:ok, data} when is_map(data) <- Jason.decode(raw) do
      data
    else
      _ -> %{}
    end
  end

  defp deep_merge(left, right) when is_map(left) and is_map(right) do
    Map.merge(left, right, fn _key, left_value, right_value ->
      if is_map(left_value) and is_map(right_value) do
        deep_merge(left_value, right_value)
      else
        right_value
      end
    end)
  end
end
