defmodule ExMCP.ACP.AdapterTransport do
  @moduledoc """
  `ExMCP.Transport` implementation that delegates to an `AdapterBridge`.

  This lets `ACP.Client` use adapted (non-native) agents identically to
  native ACP agents:

      {:ok, client} = ACP.Client.start_link(
        transport_mod: AdapterTransport,
        adapter: Adapters.ClaudeSDK,
        adapter_opts: [model: "sonnet"]
      )

  The transport starts an `AdapterBridge` on connect, which in turn launches
  the agent subprocess and handles protocol translation.

  ## Native event metadata

  With the `:native_events` option, every ACP message the adapter derives from
  a native agent line carries `_meta.ex_mcp.native` with the adapter `name`
  and a per-connection `sequence` number:

    * `:off` (default) - no native block; the wire shape is unchanged
    * `:summary` - adapter name and sequence only
    * `:raw` - also embeds the decoded native event under `event` (or the
      unparsed line under `raw`); this can make updates much larger and the
      retained data counts toward the client's `:max_update_queue_bytes`

  For `session/update` the block sits on the update's `_meta`; for other
  agent-to-client requests it sits on `params._meta`; for responses on
  `result._meta`.
  """

  @behaviour ExMCP.Transport

  alias ExMCP.ACP.AdapterBridge

  defstruct [:bridge, receive_timeout: :infinity]

  @impl true
  def connect(opts) do
    adapter = Keyword.fetch!(opts, :adapter)
    adapter_opts = Keyword.get(opts, :adapter_opts, [])
    receive_timeout = Keyword.get(opts, :receive_timeout, :infinity)

    bridge_opts =
      [adapter: adapter, adapter_opts: adapter_opts] ++
        Keyword.take(opts, [
          :max_buffer_bytes,
          :max_outbox_messages,
          :max_outbox_bytes,
          :max_waiters,
          :max_one_shot_tasks,
          :native_events
        ])

    case AdapterBridge.start_link(bridge_opts) do
      {:ok, bridge} ->
        {:ok, %__MODULE__{bridge: bridge, receive_timeout: receive_timeout}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @impl true
  def send_message(message, %__MODULE__{bridge: bridge} = state) do
    :telemetry.execute(
      [:ex_mcp, :acp, :transport, :message_sent],
      %{size: byte_size(message)},
      %{}
    )

    case AdapterBridge.send_message(bridge, message) do
      :ok -> {:ok, state}
      {:error, reason} -> {:error, reason}
    end
  end

  @impl true
  def receive_message(%__MODULE__{bridge: bridge} = state) do
    # Use Map.get for backward compat: old structs without receive_timeout default to :infinity
    timeout = Map.get(state, :receive_timeout, :infinity)

    case AdapterBridge.receive_message(bridge, timeout) do
      {:ok, message} ->
        :telemetry.execute(
          [:ex_mcp, :acp, :transport, :message_received],
          %{size: byte_size(message)},
          %{}
        )

        {:ok, message, state}

      {:error, reason} ->
        {:error, reason}
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
