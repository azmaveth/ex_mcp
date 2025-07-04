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
  Starts a test server with the given module.
  """
  def start_test_server(server_module, opts \\ []) do
    GenServer.start_link(server_module, opts)
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

    defresource "test://config" do
      meta do
        name("Test Config")
        description("Test configuration resource")
      end

      mime_type("application/json")
    end

    defprompt "test_prompt" do
      meta do
        name("Test Prompt")
        description("A test prompt for API testing")
      end

      arguments do
        arg(:message, required: true, description: "The message to process")
      end
    end

    @impl true
    def handle_prompt_get("test_prompt", arguments, state) do
      message = Map.get(arguments, "message", "default")

      result = %{
        messages: [
          %{
            role: "user",
            content: %{type: "text", text: "Test prompt with message: #{message}"}
          }
        ]
      }

      {:ok, result, state}
    end

    @impl true
    def handle_prompt_get(_prompt_name, _arguments, state) do
      {:error, "Prompt not found", state}
    end

    @impl true
    def handle_resource_read("test://config", _full_uri, state) do
      content = %{
        type: "text",
        text: Jason.encode!(%{test: true, port: 8080})
      }

      {:ok, content, state}
    end

    @impl true
    def handle_resource_read(_uri, _full_uri, state) do
      {:error, "Resource not found", state}
    end

    @impl true
    def handle_tool_call("echo", %{"message" => message}, state) when is_binary(message) do
      result = %{content: [%{type: "text", text: "Echo: #{message}"}]}
      {:ok, result, state}
    end

    @impl true
    def handle_tool_call("add", %{"a" => a, "b" => b}, state)
        when is_number(a) and is_number(b) do
      result = %{content: [%{type: "text", text: "#{a} + #{b} = #{a + b}"}]}
      {:ok, result, state}
    end

    @impl true
    def handle_tool_call("greet", %{"name" => name}, state) when is_binary(name) do
      result = %{content: [%{type: "text", text: "Hello, #{name}!"}]}
      {:ok, result, state}
    end

    @impl true
    def handle_tool_call(_tool_name, _arguments, state) do
      {:error, "Tool not implemented or invalid arguments", state}
    end

    @impl true
    def handle_initialize(params, state) do
      # Add some basic validation to make error pattern reachable
      case Map.get(params, "protocolVersion") do
        nil ->
          {:error, "Protocol version required", state}

        version when version in ["2025-03-26", "2024-11-05"] ->
          {:ok,
           %{
             protocolVersion: version,
             serverInfo: %{name: "api-test-server", version: "1.0.0"},
             capabilities: %{tools: %{}, resources: %{}, prompts: %{}}
           }, state}

        _unsupported ->
          {:error, "Unsupported protocol version", state}
      end
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
  def start_test_servers_for_api(context) do
    # This is a simplified, non-Horde version for basic API tests.
    ensure_ranch_started()
    ensure_session_manager_started()

    {server_name, ranch_ref, port} = generate_server_config(context)
    server_opts = build_server_opts(server_name, ranch_ref, port)

    start_server_with_retries(server_opts, server_name, ranch_ref, port)
  end

  # Extract server configuration generation
  defp generate_server_config(context) do
    test_name = Map.get(context, :test, :default_test)
    unique_id = System.unique_integer([:positive])
    server_name = :"ApiTestServer_#{test_name}_#{unique_id}"
    ranch_ref = :"ranch_listener_#{test_name}_#{unique_id}"

    base_port = 8080 + :rand.uniform(5000)
    port = find_available_port(base_port)

    {server_name, ranch_ref, port}
  end

  # Extract server options building
  defp build_server_opts(server_name, ranch_ref, port) do
    [
      transport: :http,
      port: port,
      sse_enabled: false,
      name: server_name,
      ranch_ref: ranch_ref
    ]
  end

  # Main server starting logic with retries
  defp start_server_with_retries(server_opts, server_name, ranch_ref, port) do
    case ApiTestServer.start_link(server_opts) do
      {:ok, _pid} ->
        handle_successful_start(server_name, ranch_ref, port)

      {:error, {:already_started, _}} ->
        handle_already_started_error(server_opts, server_name, port)

      {:error, reason} when reason in [:eaddrinuse, :eacces, :enotfound] ->
        handle_port_binding_error(server_name, port)

      {:error, {:transport_error, {:start_server_failed, _reason}}} ->
        # Pass through transport errors
        raise "Failed to start test HTTP server due to transport error"

      {:error, reason} ->
        raise "Failed to start test HTTP server: #{inspect(reason)}"
    end
  end

  # Handle successful server start
  defp handle_successful_start(server_name, ranch_ref, port) do
    Process.sleep(300)
    ensure_server_ready(port)
    register_cleanup(server_name, ranch_ref)
    %{http_url: "http://localhost:#{port}"}
  end

  # Handle already started error by retrying with different ranch ref
  defp handle_already_started_error(server_opts, server_name, port) do
    test_name = server_opts[:name]
    unique_id = System.unique_integer([:positive])
    retry_ranch_ref = :"ranch_listener_retry_#{test_name}_#{unique_id}"
    retry_opts = Keyword.put(server_opts, :ranch_ref, retry_ranch_ref)

    case ApiTestServer.start_link(retry_opts) do
      {:ok, _pid} ->
        Process.sleep(300)
        ensure_server_ready(port)
        register_cleanup(server_name, retry_ranch_ref)
        %{http_url: "http://localhost:#{port}"}

      {:error, reason} ->
        raise "Failed to restart test HTTP server: #{inspect(reason)}"
    end
  end

  # Handle port binding errors by trying a different port
  defp handle_port_binding_error(_server_name, original_port) do
    retry_port = find_available_port(original_port + 1)
    server_opts = [transport: :http, port: retry_port, sse_enabled: false]

    case ApiTestServer.start_link(server_opts) do
      {:ok, pid} ->
        Process.sleep(300)
        ensure_server_ready(retry_port)
        register_simple_cleanup(pid)
        %{http_url: "http://localhost:#{retry_port}"}

      {:error, reason} ->
        raise "Failed to start test HTTP server on retry: #{inspect(reason)}"
    end
  end

  # Ensure server is ready or raise error
  defp ensure_server_ready(port) do
    case wait_for_server_ready(port, 100) do
      :ok -> :ok
      {:error, reason} -> raise "Server not ready: #{inspect(reason)}"
    end
  end

  # Register cleanup for server and ranch listener
  defp register_cleanup(server_name, ranch_ref) do
    on_exit(fn ->
      cleanup_ranch_listener(ranch_ref)
      safe_stop_process(server_name)
      Process.sleep(100)
    end)
  end

  # Register simple cleanup for just the process
  defp register_simple_cleanup(pid) do
    on_exit(fn ->
      if Process.alive?(pid) do
        GenServer.stop(pid, :shutdown, 500)
        Process.sleep(100)
      end
    end)
  end

  # Extract ranch cleanup logic
  defp cleanup_ranch_listener(ranch_ref) do
    :ranch.stop_listener(ranch_ref)
  catch
    :exit, _ -> :ok
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

  defp find_available_port(preferred_port, max_attempts \\ 100) do
    case :gen_tcp.listen(preferred_port, []) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        preferred_port

      {:error, :eaddrinuse} when max_attempts > 0 ->
        find_available_port(preferred_port + 1, max_attempts - 1)

      {:error, _} ->
        # Fallback or raise error if we can't find any port
        if max_attempts > 0 do
          find_available_port(preferred_port + 1, max_attempts - 1)
        else
          raise "Could not find available port after 100 attempts starting from #{preferred_port - 100}"
        end
    end
  end

  defp wait_for_server_ready(port, attempts \\ 50) do
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

  @doc """
  Safely stops a GenServer process, ignoring if it's already dead.

  This is a robust alternative to `GenServer.stop/1` for test teardown
  that handles race conditions where processes might die before cleanup.
  """
  def safe_stop_process(pid_or_name, reason \\ :normal, timeout \\ 5000)
      when is_pid(pid_or_name) or is_atom(pid_or_name) do
    # For atoms, we need to check if the process exists first
    should_stop =
      case pid_or_name do
        pid when is_pid(pid) -> Process.alive?(pid)
        name when is_atom(name) -> Process.whereis(name) != nil
      end

    if should_stop do
      GenServer.stop(pid_or_name, reason, timeout)
    end
  catch
    :exit, {:noproc, _} -> :ok
    :exit, {:normal, _} -> :ok
    :exit, {:shutdown, _} -> :ok
    :exit, _ -> :ok
  end
end
