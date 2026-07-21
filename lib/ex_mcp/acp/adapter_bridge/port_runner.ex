defmodule ExMCP.ACP.AdapterBridge.PortRunner do
  @moduledoc false

  @session_vars_to_clear ~w(
    CLAUDE_CODE_ENTRYPOINT CLAUDE_SESSION_ID CLAUDE_CONFIG_DIR
    CLAUDECODE CODEX_API_KEY OPENAI_API_KEY ANTHROPIC_API_KEY GEMINI_API_KEY
    GOOGLE_API_KEY PI_API_KEY MIX_ENV MIX_TARGET
  )

  @spec open(String.t(), [String.t()], keyword(), module()) ::
          {:ok, port()} | {:error, term()}
  def open(cmd, args, opts, adapter_mod) do
    executable = System.find_executable(cmd)

    if executable do
      cwd = Keyword.get(opts, :cwd, File.cwd!())

      port_opts = [
        :binary,
        :exit_status,
        :use_stdio,
        :stderr_to_stdout,
        args: Enum.map(args, &to_charlist/1),
        cd: to_charlist(cwd),
        env: safe_env(opts, adapter_mod)
      ]

      try do
        port = Port.open({:spawn_executable, to_charlist(executable)}, port_opts)
        {:ok, port}
      catch
        :error, reason -> {:error, {:port_open_failed, reason}}
      end
    else
      {:error, {:executable_not_found, cmd}}
    end
  end

  @spec command(port(), iodata()) :: :ok | {:error, term()}
  def command(port, data) do
    Port.command(port, data)
    :ok
  catch
    :error, reason -> {:error, reason}
  end

  @spec close(port() | nil) :: :ok
  def close(nil), do: :ok

  def close(port) do
    Port.close(port)
    :ok
  catch
    :error, _ -> :ok
  end

  @spec safe_env(keyword(), module()) :: [{charlist(), charlist() | false}]
  def safe_env(opts, adapter_mod) do
    @session_vars_to_clear
    |> Map.new(&{&1, false})
    |> Map.put("TERM", "dumb")
    |> Map.merge(adapter_env(opts, adapter_mod))
    |> port_env()
  end

  defp adapter_env(opts, adapter_mod) do
    adapter_default_env =
      if function_exported?(adapter_mod, :env, 1) do
        adapter_mod.env(opts)
      else
        []
      end

    adapter_default_env
    |> normalize_env()
    |> Map.merge(opts |> Keyword.get(:env, []) |> normalize_env())
    |> maybe_put_api_key(Keyword.get(opts, :api_key))
  end

  defp normalize_env(env) when is_map(env) do
    Map.new(env, fn {name, value} -> {to_string(name), normalize_env_value(value)} end)
  end

  defp normalize_env(env) when is_list(env) do
    Map.new(env, fn
      %{"name" => name, "value" => value} ->
        {to_string(name), normalize_env_value(value)}

      %{name: name, value: value} ->
        {to_string(name), normalize_env_value(value)}

      {name, value} ->
        {to_string(name), normalize_env_value(value)}
    end)
  end

  defp normalize_env(_env), do: %{}

  defp normalize_env_value(false), do: false
  defp normalize_env_value(value), do: to_string(value)

  defp port_env(env) do
    Enum.map(env, fn
      {name, false} -> {to_charlist(name), false}
      {name, value} -> {to_charlist(name), to_charlist(value)}
    end)
  end

  defp maybe_put_api_key(env, nil), do: env
  defp maybe_put_api_key(env, api_key), do: Map.put(env, "PI_API_KEY", to_string(api_key))
end
