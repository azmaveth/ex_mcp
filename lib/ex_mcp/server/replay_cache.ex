defmodule ExMCP.Server.ReplayCache do
  @moduledoc """
  Behaviour for atomically consuming MRTR continuation identifiers.

  A replay cache is optional because consuming a token changes ambiguous
  network failure semantics. Enable one for resumed handlers that may cause
  side effects; otherwise `ExMCP.Server.RequestContext.delivery_semantics` is
  explicitly `:at_least_once`.
  """

  @callback consume(jti :: String.t(), expires_at :: integer(), opts :: keyword()) ::
              :ok | {:error, :replayed | term()}
end

defmodule ExMCP.Server.ReplayCache.ETS do
  @moduledoc """
  Node-local atomic replay cache for MRTR request state.

  Clustered servers should configure an adapter backed by their shared
  consistency store instead.
  """

  use GenServer

  @behaviour ExMCP.Server.ReplayCache

  @name __MODULE__

  def start_link(opts \\ []) do
    case Keyword.get(opts, :name, @name) do
      nil -> GenServer.start_link(__MODULE__, opts)
      name -> GenServer.start_link(__MODULE__, opts, name: name)
    end
  end

  @impl ExMCP.Server.ReplayCache
  def consume(jti, expires_at, opts \\ [])
      when is_binary(jti) and is_integer(expires_at) do
    server = Keyword.get(opts, :server, @name)
    GenServer.call(server, {:consume, jti, expires_at, System.system_time(:second)})
  catch
    :exit, reason -> {:error, {:replay_cache_unavailable, reason}}
  end

  @impl GenServer
  def init(_opts), do: {:ok, %{}}

  @impl GenServer
  def handle_call({:consume, jti, expires_at, now}, _from, entries) do
    entries = Map.reject(entries, fn {_seen_jti, expiry} -> expiry < now end)

    if Map.has_key?(entries, jti) do
      {:reply, {:error, :replayed}, entries}
    else
      {:reply, :ok, Map.put(entries, jti, expires_at)}
    end
  end
end
