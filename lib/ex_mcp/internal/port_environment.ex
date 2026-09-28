defmodule ExMCP.Internal.PortEnvironment do
  @moduledoc false

  @isolated_allowlist ~w(
    HOME LANG LOGNAME NIX_SSL_CERT_FILE PATH SHELL SSL_CERT_DIR SSL_CERT_FILE
    TEMP TMP TMPDIR TZ USER
  )

  @type value :: String.t() | false
  @type os_type :: {:unix | :win32, atom()}
  @type normalized :: %{optional(String.t()) => value()}

  @spec validate_policy(keyword()) :: :ok | {:error, {:invalid_environment_policy, term()}}
  def validate_policy(opts) do
    case Keyword.get(opts, :environment_policy, :isolated) do
      policy when policy in [:isolated, :inherit] -> :ok
      policy -> {:error, {:invalid_environment_policy, policy}}
    end
  end

  @doc """
  The environment a child starts from, before its explicit `:env`.

  Under either policy the inherited `PATH` is cleaned for a child: an OTP
  release puts its own `erts-*/bin` and `bin` directories at the front of
  `PATH` and names its root in `RELEASE_ROOT`, so a child that is itself an
  Erlang or Elixir program would find the release's `erl`, which looks for
  the release's boot files and fails to start. Entries under `RELEASE_ROOT`
  are dropped. An explicit `PATH` in `:env` is used as given.
  """
  @spec base(keyword(), %{optional(String.t()) => String.t()}, os_type()) :: normalized()
  def base(opts, host_env \\ System.get_env(), os_type \\ :os.type()) do
    case Keyword.get(opts, :environment_policy, :isolated) do
      :inherit -> inherited_path_override(host_env, os_type)
      :isolated -> isolated_base(host_env, os_type)
    end
  end

  @doc """
  The `PATH` the child will see (explicit `:env` first, then the cleaned
  inherited one), or nil when it has none. Resolve the command against this,
  not the VM's own `PATH`.
  """
  @spec child_path(keyword(), %{optional(String.t()) => String.t()}, os_type()) ::
          String.t() | nil
  def child_path(opts, host_env \\ System.get_env(), os_type \\ :os.type()) do
    explicit = opts |> Keyword.get(:env, []) |> normalize()

    case Map.fetch(explicit, "PATH") do
      {:ok, false} -> nil
      {:ok, path} -> path
      :error -> clean_path(host_env, os_type)
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

  defp isolated_base(parent_env, os_type) do
    retained =
      Map.filter(parent_env, fn {name, _value} ->
        name in @isolated_allowlist or String.starts_with?(name, "LC_")
      end)

    parent_env
    |> Map.new(fn {name, _value} -> {name, false} end)
    |> Map.merge(retained)
    |> Map.merge(inherited_path_override(parent_env, os_type))
  end

  # The port inherits the VM's environment unless told otherwise, so PATH
  # only needs overriding when cleaning it changed something.
  defp inherited_path_override(host_env, os_type) do
    case {Map.get(host_env, "PATH"), clean_path(host_env, os_type)} do
      {same, same} -> %{}
      {_original, cleaned} -> %{"PATH" => cleaned}
    end
  end

  defp clean_path(host_env, os_type) do
    case {Map.get(host_env, "PATH"), Map.get(host_env, "RELEASE_ROOT")} do
      {nil, _root} -> nil
      {path, root} when is_binary(root) and root != "" -> without_release(path, root, os_type)
      {path, _no_release} -> path
    end
  end

  defp without_release(path, root, os_type) do
    separator = path_separator(os_type)
    root = directory_key(root, os_type)

    path
    |> String.split(separator)
    |> Enum.reject(fn entry ->
      key = directory_key(entry, os_type)
      key != nil and root != nil and (key == root or String.starts_with?(key, root <> "/"))
    end)
    |> Enum.join(separator)
  end

  defp path_separator({:win32, _name}), do: ";"
  defp path_separator(_unix), do: ":"

  # A comparable form of an absolute directory, or nil for anything else.
  # Windows paths compare case-insensitively with either slash.
  defp directory_key(dir, {:win32, _name}) do
    normalized = dir |> String.replace("\\", "/") |> String.trim_trailing("/")

    if Regex.match?(~r{^([A-Za-z]:/|//)}, normalized <> "/"),
      do: String.downcase(normalized),
      else: nil
  end

  defp directory_key(dir, _unix) do
    if dir != "" and Path.type(dir) == :absolute, do: Path.expand(dir), else: nil
  end

  defp normalize_value(false), do: false
  defp normalize_value(value), do: to_string(value)
end
