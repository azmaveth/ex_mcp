defmodule ExMCP.TestHelpers do
  @moduledoc """
  Helpers for setting up test servers and managing test infrastructure.
  """
  import ExUnit.Callbacks

  @doc """
  Ensures the `ExMCP.TestServer` module is compiled and loaded.

  This function uses `Code.ensure_compiled!/1` to robustly handle the
  compilation and loading of the test server. This is necessary because
  the test server is defined in the `test/support` directory and may not
  be automatically compiled with the rest of the application, especially
  when running individual tests.
  """
  def ensure_test_server_loaded do
    Code.ensure_compiled!(ExMCP.TestServer)
  end

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

    ensure_test_server_loaded()

    case ExMCP.TestServer.start_link(server_opts) do
      {:ok, pid} ->
        # Give the server a moment to fully start
        Process.sleep(200)
        :ok = wait_for_server_ready(port)
        {:ok, pid, port}

      {:error, {:transport_error, {:start_server_failed, _reason}}} ->
        # Server failed to start, likely port conflict
        # Try to find another port
        retry_port = find_available_port(port + 1)

        server_opts = Keyword.put(server_opts, :port, retry_port)
        ensure_test_server_loaded()

        case ExMCP.TestServer.start_link(server_opts) do
          {:ok, pid} ->
            Process.sleep(200)
            :ok = wait_for_server_ready(retry_port)
            {:ok, pid, retry_port}

          error ->
            error
        end

      error ->
        error
    end
  end

  @doc """
  Starts a test server with stdio transport.
  """
  def start_stdio_server(opts \\ []) do
    server_opts = [transport: :stdio] ++ opts
    ensure_test_server_loaded()
    ExMCP.TestServer.start_link(server_opts)
  end

  @doc """
  Starts a test server with native/beam transport.
  """
  def start_native_server(opts \\ []) do
    server_opts = [transport: :native] ++ opts
    ensure_test_server_loaded()
    ExMCP.TestServer.start_link(server_opts)
  end

  @doc """
  Waits for a service to be registered with the native service registry.

  This helper is useful in tests to avoid race conditions where a test
  tries to use a service before its registration is complete. It uses
  an exponential backoff strategy to poll for service availability.

  ## Parameters
  - `service_name`: The atom name of the service to wait for.
  - `max_wait_ms`: The maximum time to wait in milliseconds. Defaults to 5000.

  ## Return Value
  - `{:ok, service_name}` if the service becomes available within the timeout.
  - `{:error, :timeout}` if the service does not become available.
  """
  @spec wait_for_service_registration(atom(), non_neg_integer()) ::
          {:ok, atom()} | {:error, :timeout}
  def wait_for_service_registration(service_name, max_wait_ms \\ 5000) do
    start_time = System.monotonic_time(:millisecond)
    initial_delay = 10
    do_wait_for_service_registration(service_name, start_time, max_wait_ms, initial_delay)
  end

  defmodule ApiTestServer do
    @moduledoc """
    Test server for API integration testing.

    Provides basic tools for testing MCP server functionality
    including echo and arithmetic operations.
    """
    use ExMCP.Server

    deftool "echo" do
      meta do
        description("Echoes the input message")

        input_schema(%{
          "type" => "object",
          "properties" => %{"message" => %{"type" => "string"}},
          "required" => ["message"]
        })
      end
    end

    deftool "add" do
      meta do
        description("Adds two numbers")

        input_schema(%{
          "type" => "object",
          "properties" => %{
            "a" => %{"type" => "number"},
            "b" => %{"type" => "number"}
          },
          "required" => ["a", "b"]
        })
      end
    end

    deftool "greet" do
      meta do
        description("Greets someone")

        input_schema(%{
          "type" => "object",
          "properties" => %{"name" => %{"type" => "string"}},
          "required" => ["name"]
        })
      end
    end

    @impl true
    def handle_tool_call("echo", %{"message" => message}, state) do
      result = %{content: [%{type: "text", text: "Echo: #{message}"}]}
      {:ok, result, state}
    end

    @impl true
    def handle_tool_call("add", %{"a" => a, "b" => b}, state) do
      result = %{content: [%{type: "text", text: "#{a} + #{b} = #{a + b}"}]}
      {:ok, result, state}
    end

    @impl true
    def handle_tool_call("greet", %{"name" => name}, state) do
      result = %{content: [%{type: "text", text: "Hello, #{name}!"}]}
      {:ok, result, state}
    end
  end

  @doc """
  Sets up a simple test server with HTTP transport for API integration tests.

  This function is designed to be called from a `setup` block. It handles:
  - Starting the `ExMCP.TestHelpers.ApiTestServer` with the HTTP transport.
  - Tearing down the server process on test exit.
  - Returning a context map with `:http_url` for the test to use.

  This is a simplified helper that does **not** use Horde, making it suitable
  for tests that only need to verify basic HTTP API interactions without
  distributed features.

  **NOTE:** Tests using this helper should be marked with `async: false`.

  ## Example

      defmodule MyApiTest do
        use ExUnit.Case, async: false
        import ExMCP.TestHelpers

        setup context do
          # The context from setup is passed to the helper
          start_test_servers_for_api(context)
        end

        test "communicates with the server", %{http_url: http_url} do
          # use http_url to connect to the server
        end
      end
  """
  def start_test_servers_for_api(_context) do
    # This is a simplified, non-Horde version for basic API tests.
    ensure_ranch_started()
    ensure_session_manager_started()
    port = find_available_port(8080)

    server_opts = [transport: :http, port: port, sse_enabled: false]

    # 1. Start the HTTP server using a real ExMCP.Server.
    case ApiTestServer.start_link(server_opts) do
      {:ok, pid} ->
        # Wait for the server to be ready to accept connections
        :ok = wait_for_server_ready(port)

        # 2. Register on_exit for the server process itself.
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid, :shutdown, 500) end)

        # 3. Return the context map required by the tests.
        %{http_url: "http://localhost:#{port}"}

      {:error, {:transport_error, {:start_server_failed, _reason}}} ->
        # Retry on a different port
        retry_port = find_available_port(port + 1)
        server_opts = [transport: :http, port: retry_port, sse_enabled: false]

        case ApiTestServer.start_link(server_opts) do
          {:ok, pid} ->
            :ok = wait_for_server_ready(retry_port)
            on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid, :shutdown, 500) end)
            %{http_url: "http://localhost:#{retry_port}"}

          {:error, reason} ->
            raise "Failed to start test HTTP server on retry: #{inspect(reason)}"
        end

      {:error, reason} ->
        # If server setup fails, we should fail the test setup.
        # Raising is a clear way to do this.
        raise "Failed to start test HTTP server: #{inspect(reason)}"
    end
  end

  @doc """
  Sets up isolated test servers for integration testing.

  This macro simplifies the setup of test servers with different transports
  while ensuring process isolation using HordeTestHelpers. It should be
  called within a `setup` block in tests that require a running MCP server.

  It handles:
  - Setting up an isolated Horde registry and supervisor.
  - Starting the `ExMCP.TestServer` with the specified transport.
  - Configuring the application environment to use the isolated Horde instance.
  - Tearing down all started processes on test exit.

  **NOTE:** Tests using this helper should be marked with `async: false`.

  ## Options

  - `:transport` - The transport to use. Can be `:http`, `:stdio`, or `:native`.
    Defaults to `:native`.

  ## Return Value

  On success, returns `{:ok, config}` where `config` is a map containing
  details about the started server and Horde processes, for example:
  - `:pid` - The PID of the server process.
  - `:transport` - The transport used.
  - `:port` - The port number (for `:http` transport).
  - `:supervisor_name` - The name of the isolated Horde supervisor.
  - `:registry_name` - The name of the isolated Horde registry.

  ## Example

      defmodule MyServerTest do
        use ExUnit.Case, async: false
        import ExMCP.TestHelpers

        setup do
          {:ok, server_config} = setup_test_servers(transport: :http)
          %{server_config: server_config}
        end

        test "communicates with the server", %{server_config: config} do
          # use config.port or config.pid to connect to the server
        end
      end
  """
  defmacro setup_test_servers(opts \\ []) do
    quote do
      # Ensure Horde helpers are available for the test module
      import ExMCP.HordeTestHelpers

      # 1. Setup isolated Horde instance using the helper macro.
      # This starts unique Horde processes and registers on_exit cleanup.
      {:ok, horde_config} = setup_test_horde()

      # 2. Configure the application to use the isolated Horde processes.
      # This assumes the server reads these names from the application environment.
      Application.put_env(:ex_mcp, :horde_registry, horde_config.registry_name)
      Application.put_env(:ex_mcp, :horde_supervisor, horde_config.supervisor_name)

      # 3. Start the server with the correct transport.
      transport = Keyword.get(unquote(opts), :transport, :native)
      server_opts = unquote(opts)

      # Delegate to a private helper to start the server and set up its cleanup.
      case ExMCP.TestHelpers.__setup_server_by_transport__(transport, server_opts) do
        {:ok, server_config} ->
          # Register on_exit for the server process
          pid = server_config.pid
          on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid, :shutdown, 500) end)

          # 4. Combine configs and return for use in the test.
          config = Map.merge(horde_config, server_config)
          {:ok, config}

        error ->
          error
      end
    end
  end

  @doc false
  # Private helper to start a server based on transport and set up teardown.
  # This function is intended to be called from the `setup_test_servers` macro
  # and runs within the context of a test's `setup` block.
  def __setup_server_by_transport__(transport, opts) do
    case transport do
      :http ->
        case start_http_server(opts) do
          {:ok, pid, port} ->
            {:ok, %{pid: pid, port: port, transport: :http}}

          error ->
            error
        end

      :stdio ->
        case start_stdio_server(opts) do
          {:ok, pid} ->
            {:ok, %{pid: pid, transport: :stdio}}

          error ->
            error
        end

      :native ->
        case start_native_server(opts) do
          {:ok, pid} ->
            {:ok, %{pid: pid, transport: :native}}

          error ->
            error
        end

      _ ->
        {:error, {:invalid_transport, transport}}
    end
  end

  # Private helpers

  defp do_wait_for_service_registration(service_name, start_time, max_wait_ms, delay) do
    if ExMCP.Native.service_available?(service_name) do
      {:ok, service_name}
    else
      elapsed_ms = System.monotonic_time(:millisecond) - start_time

      if elapsed_ms + delay > max_wait_ms do
        # Check one last time before timing out
        if ExMCP.Native.service_available?(service_name) do
          {:ok, service_name}
        else
          {:error, :timeout}
        end
      else
        Process.sleep(delay)
        # Exponential backoff, capped to prevent excessively long sleeps
        next_delay = min(delay * 2, 500)
        do_wait_for_service_registration(service_name, start_time, max_wait_ms, next_delay)
      end
    end
  end

  defp ensure_ranch_started do
    case Application.ensure_all_started(:ranch) do
      {:ok, _} -> :ok
      {:error, _} -> :ok
    end
  end

  defp ensure_session_manager_started do
    # Check if SessionManager is already running
    case GenServer.whereis(ExMCP.SessionManager) do
      nil ->
        # Start SessionManager if not running
        case ExMCP.SessionManager.start_link([]) do
          {:ok, _pid} -> :ok
          {:error, {:already_started, _pid}} -> :ok
          {:error, reason} -> raise "Failed to start SessionManager: #{inspect(reason)}"
        end

      _pid ->
        :ok
    end
  end

  defp find_available_port(preferred_port) do
    case :gen_tcp.listen(preferred_port, []) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        preferred_port

      {:error, :eaddrinuse} ->
        find_available_port(preferred_port + 1)

      {:error, _} ->
        # Fallback to default if there's any other error
        8080
    end
  end

  defp wait_for_server_ready(port, attempts \\ 10) do
    case :gen_tcp.connect(~c"localhost", port, []) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        :ok

      {:error, :econnrefused} when attempts > 0 ->
        Process.sleep(50)
        wait_for_server_ready(port, attempts - 1)

      {:error, reason} ->
        {:error, {:server_not_ready, reason}}
    end
  end
end
