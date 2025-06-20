defmodule ExMCP.TestHelpers do
  @moduledoc """
  Helpers for setting up test servers and managing test infrastructure.
  """

  alias ExMCP.TestServer

  @doc """
  Starts a test HTTP server on an available port.

  Returns `{:ok, server_pid, port}` or `{:error, reason}`.
  """
  def start_http_server(opts \\ []) do
    # Start ranch if it's not already running
    ensure_ranch_started()

    port = find_available_port(Keyword.get(opts, :preferred_port, 8080))

    server_opts =
      [
        transport: :http,
        port: port,
        host: "localhost",
        # Disable SSE for simpler testing
        sse_enabled: false
      ] ++ opts

    case TestServer.start_link(server_opts) do
      {:ok, pid} ->
        # Give the server a moment to fully start
        Process.sleep(200)
        :ok = wait_for_server_ready(port)
        {:ok, pid, port}

      {:error, {:transport_error, {:start_server_failed, _reason}}} ->
        # Ranch might be down, try to restart it
        :ok = restart_ranch()

        case TestServer.start_link(server_opts) do
          {:ok, pid} ->
            Process.sleep(200)
            :ok = wait_for_server_ready(port)
            {:ok, pid, port}

          error ->
            error
        end

      error ->
        error
    end
  end

  @doc """
  Starts a test stdio server.

  Returns `{:ok, server_pid}` or `{:error, reason}`.
  """
  def start_stdio_server(opts \\ []) do
    server_opts = [transport: :stdio] ++ opts
    TestServer.start_link(server_opts)
  end

  @doc """
  Starts a test native/BEAM server.

  Returns `{:ok, server_pid}` or `{:error, reason}`.
  """
  def start_native_server(opts \\ []) do
    server_opts = [transport: :native] ++ opts
    TestServer.start_link(server_opts)
  end

  @doc """
  Stops a test server gracefully.
  """
  def stop_server(server_pid) do
    if Process.alive?(server_pid) do
      GenServer.stop(server_pid, :normal)
    end

    :ok
  end

  @doc """
  Waits for a server to be ready to accept connections.
  """
  def wait_for_server_ready(port, max_attempts \\ 50) do
    wait_for_server_ready(port, max_attempts, 0)
  end

  defp wait_for_server_ready(_port, max_attempts, attempts) when attempts >= max_attempts do
    {:error, :timeout}
  end

  defp wait_for_server_ready(port, max_attempts, attempts) do
    case :gen_tcp.connect(~c"localhost", port, [:binary, active: false], 100) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        :ok

      {:error, _reason} ->
        Process.sleep(10)
        wait_for_server_ready(port, max_attempts, attempts + 1)
    end
  end

  @doc """
  Finds an available port starting from the given port number.
  """
  def find_available_port(starting_port \\ 8080) do
    find_available_port(starting_port, starting_port + 100)
  end

  defp find_available_port(current_port, max_port) when current_port <= max_port do
    case :gen_tcp.listen(current_port, [:binary, active: false, reuseaddr: true]) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        current_port

      {:error, :eaddrinuse} ->
        find_available_port(current_port + 1, max_port)

      {:error, _other} ->
        find_available_port(current_port + 1, max_port)
    end
  end

  defp find_available_port(_current_port, _max_port) do
    # Fallback to a random high port if we can't find one in our range
    :rand.uniform(10000) + 50000
  end

  @doc """
  Creates a test setup that starts and stops servers automatically.

  ## Usage

      setup do
        ExMCP.TestHelpers.setup_test_servers()
      end
  """
  def setup_test_servers(opts \\ []) do
    # Always restart ranch to ensure clean state
    restart_ranch()

    # Start HTTP server
    {:ok, http_server, http_port} = start_http_server(opts)
    :ok = wait_for_server_ready(http_port)

    # Start native server for fallback testing
    {:ok, native_server} = start_native_server(opts)

    on_exit(fn ->
      stop_server(http_server)
      stop_server(native_server)
      # Keep ranch running for subsequent tests
    end)

    %{
      http_server: http_server,
      http_port: http_port,
      http_url: "http://localhost:#{http_port}",
      native_server: native_server
    }
  end

  @doc """
  Sets up multiple test servers for fallback testing.
  """
  def setup_multiple_test_servers(count \\ 2) do
    servers =
      for _i <- 1..count do
        {:ok, server, port} = start_http_server()
        :ok = wait_for_server_ready(port)
        %{server: server, port: port, url: "http://localhost:#{port}"}
      end

    on_exit(fn ->
      for %{server: server} <- servers do
        stop_server(server)
      end
    end)

    %{servers: servers}
  end

  defp on_exit(cleanup_fn) do
    ExUnit.Callbacks.on_exit(cleanup_fn)
  end

  @doc """
  Ensures ranch is started for HTTP server tests.
  """
  def ensure_ranch_started do
    # First check if ranch is loaded
    case Application.load(:ranch) do
      :ok -> :ok
      {:error, {:already_loaded, :ranch}} -> :ok
      error -> error
    end

    # Then ensure it's started
    case Application.ensure_all_started(:ranch) do
      {:ok, _} -> :ok
      {:error, {:ranch, {:already_started, :ranch}}} -> :ok
      {:error, {:already_started, :ranch}} -> :ok
      error -> error
    end
  end

  @doc """
  Restarts ranch if it has stopped.
  """
  def restart_ranch do
    # Stop ranch if it's running
    case Application.stop(:ranch) do
      :ok -> :ok
      {:error, {:not_started, :ranch}} -> :ok
      _ -> :ok
    end

    # Give it a moment to fully stop
    Process.sleep(100)

    # Restart ranch
    case ensure_ranch_started() do
      :ok -> :ok
      # Continue even if restart fails - test might work anyway
      _ -> :ok
    end
  end
end
