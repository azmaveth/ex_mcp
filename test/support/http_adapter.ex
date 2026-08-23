defmodule ExMCP.Test.HTTPAdapter do
  @moduledoc false

  import ExUnit.Callbacks

  alias ExMCP.Server.Transport

  @adapters [:cowboy, :bandit]

  @doc """
  HTTP adapters exercised by server/HttpPlug tests.
  """
  def adapters, do: @adapters

  @doc """
  Starts `plug` under `adapter` and shuts it down when the test exits.

  Cowboy injects a unique `:ranch_ref` so two listeners can coexist.
  Bandit isolation is the supervisor pid plus a free port.
  """
  def start_plug(plug, plug_opts, opts) do
    adapter = Keyword.fetch!(opts, :adapter)
    port = Keyword.get_lazy(opts, :port, &free_port/0)
    ip = Keyword.get(opts, :ip, {127, 0, 0, 1})
    id = {:ex_mcp_test_http, adapter, System.unique_integer([:positive])}

    {spec, ref} = child_spec(adapter, plug, plug_opts, port, ip, id, opts)
    pid = start_supervised!(spec)
    handle = %{adapter: adapter, pid: pid, port: port, ref: ref}
    :ok = wait_until_listening(port)
    {:ok, handle}
  end

  @doc """
  Starts `ExMCP.HttpPlug` through `Transport.start_http_server/4`.
  """
  def start_mcp_http(handler, opts) do
    adapter = Keyword.fetch!(opts, :adapter)
    server_info = Keyword.get(opts, :server_info, %{name: "test", version: "1.0.0"})
    port = Keyword.get_lazy(opts, :port, &free_port/0)

    start_opts =
      opts
      |> Keyword.drop([:server_info])
      |> Keyword.put(:transport, :http)
      |> Keyword.put(:adapter, adapter)
      |> Keyword.put(:port, port)
      |> maybe_put_cowboy_ref(adapter)

    case Transport.start_http_server(handler, server_info, [], start_opts) do
      {:ok, pid} ->
        handle = %{
          adapter: adapter,
          pid: pid,
          port: port,
          ref: Keyword.get(start_opts, :ranch_ref)
        }

        :ok = wait_until_listening(port)
        on_exit(fn -> shutdown(handle) end)
        {:ok, handle}

      error ->
        error
    end
  end

  @doc """
  Stops a listener started by `start_plug/3` or `start_mcp_http/2`.
  """
  def shutdown(%{adapter: :cowboy, ref: ref}) when not is_nil(ref) do
    Plug.Cowboy.shutdown(ref)
    :ok
  catch
    :exit, _ -> :ok
  end

  def shutdown(%{pid: pid}) when is_pid(pid) do
    Transport.stop_http_server(pid)
  end

  def shutdown(_handle), do: :ok

  @doc """
  Binds port 0 and returns a free TCP port.
  """
  def free_port do
    {:ok, socket} = :gen_tcp.listen(0, [:binary, ip: {127, 0, 0, 1}, reuseaddr: true])
    {:ok, port} = :inet.port(socket)
    :ok = :gen_tcp.close(socket)
    port
  end

  defp child_spec(:cowboy, plug, plug_opts, port, ip, id, opts) do
    _ = Application.ensure_all_started(:plug_cowboy)
    ref = Keyword.get(opts, :ref) || {:ex_mcp_test_cowboy, System.unique_integer([:positive])}

    spec =
      Supervisor.child_spec(
        {Plug.Cowboy,
         scheme: :http, plug: {plug, plug_opts}, options: [port: port, ip: ip, ref: ref]},
        id: id
      )

    {spec, ref}
  end

  defp child_spec(:bandit, plug, plug_opts, port, ip, id, _opts) do
    _ = Application.ensure_all_started(:bandit)

    spec =
      Supervisor.child_spec(
        {Bandit, plug: {plug, plug_opts}, scheme: :http, port: port, ip: ip},
        id: id
      )

    {spec, nil}
  end

  defp maybe_put_cowboy_ref(opts, :cowboy) do
    Keyword.put_new(opts, :ranch_ref, {:ex_mcp_test_cowboy, System.unique_integer([:positive])})
  end

  defp maybe_put_cowboy_ref(opts, _adapter), do: opts

  defp wait_until_listening(port, attempts \\ 50)

  defp wait_until_listening(_port, 0), do: {:error, :timeout}

  defp wait_until_listening(port, attempts) do
    case :gen_tcp.connect({127, 0, 0, 1}, port, [:binary, active: false], 50) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        :ok

      {:error, _} ->
        Process.sleep(10)
        wait_until_listening(port, attempts - 1)
    end
  end
end
