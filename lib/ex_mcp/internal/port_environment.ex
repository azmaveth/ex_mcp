defmodule ExMCP.Internal.PortEnvironment do
  @moduledoc false

  @isolated_allowlist ~w(
    HOME LANG LOGNAME NIX_SSL_CERT_FILE PATH SHELL SSL_CERT_DIR SSL_CERT_FILE
    TEMP TMP TMPDIR TZ USER
  )

  @type value :: String.t() | false
  @type normalized :: %{optional(String.t()) => value()}

  @spec validate_policy(keyword()) :: :ok | {:error, {:invalid_environment_policy, term()}}
  def validate_policy(opts) do
    case Keyword.get(opts, :environment_policy, :isolated) do
      policy when policy in [:isolated, :inherit] -> :ok
      policy -> {:error, {:invalid_environment_policy, policy}}
    end
  end

  @spec base(keyword()) :: normalized()
  def base(opts) do
    case Keyword.get(opts, :environment_policy, :isolated) do
      :inherit -> %{}
      :isolated -> isolated_base()
    end
  end

  @spec normalize(map() | list() | term()) :: normalized()
  def normalize(env) when is_map(env) do
    Map.new(env, fn {name, value} -> {to_string(name), normalize_value(value)} end)
  end

  def normalize(env) when is_list(env) do
    Map.new(env, fn
      %{"name" => name, "value" => value} ->
        {to_string(name), normalize_value(value)}

      %{name: name, value: value} ->
        {to_string(name), normalize_value(value)}

      {name, value} ->
        {to_string(name), normalize_value(value)}
    end)
  end

  def normalize(_env), do: %{}

  @spec to_port(normalized()) :: [{charlist(), charlist() | false}]
  def to_port(env) when is_map(env) do
    Enum.map(env, fn
      {name, false} -> {to_charlist(name), false}
      {name, value} -> {to_charlist(name), to_charlist(value)}
    end)
  end

  defp isolated_base do
    parent_env = System.get_env()

    retained =
      Map.filter(parent_env, fn {name, _value} ->
        name in @isolated_allowlist or String.starts_with?(name, "LC_")
      end)

    parent_env
    |> Map.new(fn {name, _value} -> {name, false} end)
    |> Map.merge(retained)
  end

  defp normalize_value(false), do: false
  defp normalize_value(value), do: to_string(value)
end
