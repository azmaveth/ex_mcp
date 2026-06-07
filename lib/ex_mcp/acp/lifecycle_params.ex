defmodule ExMCP.ACP.LifecycleParams do
  @moduledoc """
  Pure normalization and validation for ACP session lifecycle parameters.
  """

  alias ExMCP.ACP.{Capabilities, Maps}

  @spec client_opts(keyword()) :: keyword()
  def client_opts(opts) when is_list(opts) do
    Keyword.take(opts, [:mcp_servers, :additional_directories])
  end

  @spec validate_cwd(any()) :: :ok | {:error, {:invalid_params, :cwd_must_be_absolute}}
  def validate_cwd(cwd) when is_binary(cwd) do
    if absolute_path?(cwd), do: :ok, else: {:error, {:invalid_params, :cwd_must_be_absolute}}
  end

  def validate_cwd(_cwd), do: {:error, {:invalid_params, :cwd_must_be_absolute}}

  @spec validate(keyword(), map() | nil) :: :ok | {:error, term()}
  def validate(opts, capabilities) do
    additional_directories = Keyword.get(opts, :additional_directories)
    mcp_servers = mcp_servers(opts)

    cond do
      invalid_mcp_servers?(mcp_servers, capabilities || %{}) ->
        {:error, {:unsupported_capability, :mcp_native}}

      not Capabilities.supported?(capabilities || %{}, :additional_directories) ->
        if is_nil(additional_directories) do
          :ok
        else
          {:error, {:unsupported_capability, :additional_directories}}
        end

      is_nil(additional_directories) ->
        :ok

      not valid_additional_directories?(additional_directories) ->
        {:error, {:invalid_params, :additional_directories_must_be_absolute_paths}}

      true ->
        :ok
    end
  end

  @spec normalize(map(), keyword() | map() | nil) :: map()
  def normalize(params, opts) when is_map(params) do
    params
    |> Map.put("mcpServers", mcp_servers(opts))
    |> Maps.put_present("additionalDirectories", validate_additional_directories!(opts))
  end

  @spec mcp_servers(keyword() | map() | nil) :: list()
  def mcp_servers(opts) when is_list(opts) do
    if Keyword.keyword?(opts), do: Keyword.get(opts, :mcp_servers, []), else: opts
  end

  def mcp_servers(%{} = opts) do
    Maps.get(opts, "mcpServers") || Maps.get(opts, :mcp_servers) || Maps.get(opts, :mcpServers) ||
      []
  end

  def mcp_servers(nil), do: []

  @spec additional_directories(keyword() | map() | nil) :: any()
  def additional_directories(opts) when is_list(opts) do
    if Keyword.keyword?(opts), do: Keyword.get(opts, :additional_directories), else: nil
  end

  def additional_directories(%{} = opts) do
    Maps.get(opts, "additionalDirectories") || Maps.get(opts, :additional_directories) ||
      Maps.get(opts, :additionalDirectories)
  end

  def additional_directories(nil), do: nil

  @spec valid_additional_directories?(any()) :: boolean()
  def valid_additional_directories?(directories) when is_list(directories) do
    directories != [] and Enum.all?(directories, &absolute_path?/1)
  end

  def valid_additional_directories?(_directories), do: false

  @spec validate_additional_directories!(keyword() | map() | nil | list(String.t())) ::
          list(String.t()) | nil
  def validate_additional_directories!(opts) do
    opts
    |> additional_directories()
    |> do_validate_additional_directories!()
  end

  @spec absolute_path?(any()) :: boolean()
  def absolute_path?(path) when is_binary(path) and path != "",
    do: Path.type(path) == :absolute

  def absolute_path?(_path), do: false

  defp do_validate_additional_directories!(nil), do: nil

  defp do_validate_additional_directories!(directories) when is_list(directories) do
    if valid_additional_directories?(directories) do
      directories
    else
      raise ArgumentError, "additionalDirectories must be a non-empty list of absolute paths"
    end
  end

  defp do_validate_additional_directories!(_directories) do
    raise ArgumentError, "additionalDirectories must be a non-empty list of absolute paths"
  end

  defp invalid_mcp_servers?(servers, capabilities) do
    Enum.any?(servers, &native_mcp_server?/1) and
      not Capabilities.supported?(capabilities, :mcp_native)
  end

  defp native_mcp_server?(%{"type" => type}) when type in ["native", "beam"], do: true
  defp native_mcp_server?(%{type: type}) when type in [:native, :beam, "native", "beam"], do: true
  defp native_mcp_server?(_server), do: false
end
