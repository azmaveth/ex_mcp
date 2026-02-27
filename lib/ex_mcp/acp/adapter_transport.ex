defmodule ExMCP.ACP.AdapterTransport do
  @moduledoc """
  `ExMCP.Transport` implementation that delegates to an `AdapterBridge`.

  This lets `ACP.Client` use adapted (non-native) agents identically to
  native ACP agents:

      {:ok, client} = ACP.Client.start_link(
        transport_mod: AdapterTransport,
        adapter: Adapters.Claude,
        adapter_opts: [model: "sonnet"]
      )

  The transport starts an `AdapterBridge` on connect, which in turn launches
  the agent subprocess and handles protocol translation.
  """

  @behaviour ExMCP.Transport

  alias ExMCP.ACP.AdapterBridge

  defstruct [:bridge]

  @impl true
  def connect(opts) do
    adapter = Keyword.fetch!(opts, :adapter)
    adapter_opts = Keyword.get(opts, :adapter_opts, [])

    bridge_opts = [
      adapter: adapter,
      adapter_opts: adapter_opts
    ]

    case AdapterBridge.start_link(bridge_opts) do
      {:ok, bridge} ->
        {:ok, %__MODULE__{bridge: bridge}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @impl true
  def send_message(message, %__MODULE__{bridge: bridge} = state) do
    case AdapterBridge.send_message(bridge, message) do
      :ok -> {:ok, state}
      {:error, reason} -> {:error, reason}
    end
  end

  @impl true
  def receive_message(%__MODULE__{bridge: bridge} = state) do
    case AdapterBridge.receive_message(bridge) do
      {:ok, message} -> {:ok, message, state}
      {:error, reason} -> {:error, reason}
    end
  end

  @impl true
  def close(%__MODULE__{bridge: bridge}) do
    AdapterBridge.close(bridge)
  catch
    :exit, _ -> :ok
  end

  @impl true
  def connected?(%__MODULE__{bridge: bridge}) do
    Process.alive?(bridge)
  end
end
