defmodule ExMCP.ACP.Capabilities do
  @moduledoc """
  Pure helpers for ACP capability maps.

  ACP capabilities are JSON-shaped maps, but callers may pass atom-keyed maps
  in tests or local APIs. This module centralizes those lookups.
  """

  alias ExMCP.ACP.Maps

  @session_keys %{
    session_list: "list",
    session_resume: "resume",
    session_close: "close",
    session_delete: "delete",
    additional_directories: "additionalDirectories"
  }

  @handler_callbacks %{
    load_session: {:handle_load_session, 3},
    session_list: {:handle_list_sessions, 3},
    session_resume: {:handle_resume_session, 3},
    session_close: {:handle_close_session, 3},
    session_delete: {:handle_delete_session, 3},
    logout: {:handle_logout, 2}
  }

  @spec merge(map(), map() | nil) :: map()
  def merge(auto, nil), do: auto
  def merge(_auto, explicit), do: explicit

  @spec supported?(map() | nil, atom()) :: boolean()
  def supported?(caps, :load_session), do: caps |> Maps.get("loadSession") |> Maps.truthy?()

  def supported?(caps, :logout) do
    caps
    |> Maps.get("auth")
    |> Maps.get("logout")
    |> Maps.truthy?()
  end

  def supported?(caps, capability) when is_map_key(@session_keys, capability) do
    key = Map.fetch!(@session_keys, capability)

    caps
    |> Maps.get("sessionCapabilities")
    |> Maps.get(key)
    |> Maps.truthy?()
  end

  def supported?(_caps, _capability), do: false

  @spec ensure(map() | nil, atom()) :: :ok | {:error, {:unsupported_capability, atom()}}
  def ensure(caps, capability) do
    if supported?(caps || %{}, capability) do
      :ok
    else
      {:error, {:unsupported_capability, capability}}
    end
  end

  @spec put(map(), atom(), boolean() | map()) :: map()
  def put(caps, _capability, false), do: caps
  def put(caps, _capability, nil), do: caps

  def put(caps, :load_session, true), do: Map.put(caps, "loadSession", true)

  def put(caps, :logout, true) do
    caps
    |> auth_caps()
    |> Map.put("logout", %{})
    |> then(&Map.put(caps, "auth", &1))
  end

  def put(caps, capability, value) when is_map_key(@session_keys, capability) do
    session_value = if value == true, do: %{}, else: value
    key = Map.fetch!(@session_keys, capability)

    caps
    |> session_caps()
    |> Map.put(key, session_value)
    |> then(&Map.put(caps, "sessionCapabilities", &1))
  end

  @spec from_handler(module()) :: map()
  def from_handler(handler_mod) do
    Code.ensure_loaded(handler_mod)

    Enum.reduce(@handler_callbacks, %{}, fn {capability, {callback, arity}}, caps ->
      put(caps, capability, function_exported?(handler_mod, callback, arity))
    end)
  end

  @spec advertise_adapter_session_list(map(), module()) :: map()
  def advertise_adapter_session_list(caps, adapter_mod) do
    put(caps, :session_list, function_exported?(adapter_mod, :list_sessions, 1))
  end

  defp session_caps(caps) do
    case Maps.get(caps, "sessionCapabilities") do
      map when is_map(map) -> map
      _ -> %{}
    end
  end

  defp auth_caps(caps) do
    case Maps.get(caps, "auth") do
      map when is_map(map) -> map
      _ -> %{}
    end
  end
end
