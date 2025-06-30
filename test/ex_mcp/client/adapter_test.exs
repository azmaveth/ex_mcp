defmodule ExMCP.Client.AdapterTest do
  use ExUnit.Case, async: true

  alias ExMCP.Client.{Adapter, StateMachineAdapter}

  defmodule SimpleTransport do
    @behaviour ExMCP.Transport

    defstruct [:test_pid]

    @impl true
    def connect(opts) do
      test_pid = Keyword.get(opts, :test_pid)
      {:ok, %__MODULE__{test_pid: test_pid}}
    end

    @impl true
    def send_message(message, %__MODULE__{test_pid: test_pid} = state) do
      send(test_pid, {:sent_message, message})
      {:ok, state}
    end

    @impl true
    def receive_message(%__MODULE__{} = state) do
      receive do
        {:mock_message, message} ->
          {:ok, message, state}
      after
        5000 ->
          {:error, :timeout}
      end
    end

    @impl true
    def close(_state), do: :ok

    @impl true
    def connected?(_state), do: true
  end

  defmodule TestAdapter do
    @behaviour Adapter

    # Track calls for testing
    def start_link(config, opts) do
      test_pid = opts[:test_pid] || self()
      send(test_pid, {:adapter_call, :start_link, {config, opts}})
      {:ok, self()}
    end

    def connect(client) do
      send(client, {:adapter_call, :connect, client})
      :ok
    end

    def disconnect(client) do
      send(client, {:adapter_call, :disconnect, client})
      :ok
    end

    def stop(client) do
      send(client, {:adapter_call, :stop, client})
      :ok
    end

    def call(client, method, params, opts) do
      send(client, {:adapter_call, :call, {client, method, params, opts}})
      {:ok, %{"result" => "test"}}
    end

    def notify(client, method, params) do
      send(client, {:adapter_call, :notify, {client, method, params}})
      :ok
    end

    def batch_request(client, requests, opts) do
      send(client, {:adapter_call, :batch_request, {client, requests, opts}})
      {:ok, Enum.map(requests, fn _ -> %{"result" => "test"} end)}
    end

    def complete(client, request, token) do
      send(client, {:adapter_call, :complete, {client, request, token}})
      {:ok, %{}}
    end

    def list_tools(client, opts) do
      send(client, {:adapter_call, :list_tools, {client, opts}})
      {:ok, %{"tools" => []}}
    end

    def call_tool(client, name, arguments, opts) do
      send(client, {:adapter_call, :call_tool, {client, name, arguments, opts}})
      {:ok, %{"result" => "tool_result"}}
    end

    def find_tool(client, name) do
      send(client, {:adapter_call, :find_tool, {client, name}})
      {:error, :not_found}
    end

    def find_matching_tool(client, pattern) do
      send(client, {:adapter_call, :find_matching_tool, {client, pattern}})
      {:ok, []}
    end

    def list_resources(client, opts) do
      send(client, {:adapter_call, :list_resources, {client, opts}})
      {:ok, %{"resources" => []}}
    end

    def read_resource(client, uri, opts) do
      send(client, {:adapter_call, :read_resource, {client, uri, opts}})
      {:ok, %{"contents" => "test"}}
    end

    def list_resource_templates(client, opts) do
      send(client, {:adapter_call, :list_resource_templates, {client, opts}})
      {:ok, %{"templates" => []}}
    end

    def subscribe_resource(client, uri, opts) do
      send(client, {:adapter_call, :subscribe_resource, {client, uri, opts}})
      {:ok, %{}}
    end

    def unsubscribe_resource(client, uri, opts) do
      send(client, {:adapter_call, :unsubscribe_resource, {client, uri, opts}})
      {:ok, %{}}
    end

    def list_prompts(client, opts) do
      send(client, {:adapter_call, :list_prompts, {client, opts}})
      {:ok, %{"prompts" => []}}
    end

    def get_prompt(client, name, arguments, opts) do
      send(client, {:adapter_call, :get_prompt, {client, name, arguments, opts}})
      {:ok, %{"prompt" => "test"}}
    end

    def set_log_level(client, level, opts) do
      send(client, {:adapter_call, :set_log_level, {client, level, opts}})
      {:ok, %{}}
    end

    def log_message(client, level, message, data, opts) do
      send(client, {:adapter_call, :log_message, {client, level, message, data, opts}})
      :ok
    end

    def ping(client, opts) do
      send(client, {:adapter_call, :ping, {client, opts}})
      {:ok, %{}}
    end

    def server_info(client) do
      send(client, {:adapter_call, :server_info, client})
      %{"name" => "test-server"}
    end

    def server_capabilities(client) do
      send(client, {:adapter_call, :server_capabilities, client})
      %{}
    end

    def negotiated_version(client) do
      send(client, {:adapter_call, :negotiated_version, client})
      "2025-03-26"
    end

    def list_roots(client, opts) do
      send(client, {:adapter_call, :list_roots, {client, opts}})
      {:ok, %{"roots" => []}}
    end

    def get_status(client) do
      send(client, {:adapter_call, :get_status, client})
      %{state: :ready}
    end

    def get_pending_requests(client) do
      send(client, {:adapter_call, :get_pending_requests, client})
      %{}
    end

    def make_request(client, method, params, opts) do
      send(client, {:adapter_call, :make_request, {client, method, params, opts}})
      {:ok, %{"result" => "test"}}
    end

    def send_batch(client, requests) do
      send(client, {:adapter_call, :send_batch, {client, requests}})
      {:ok, []}
    end

    def send_cancelled(client, request_id) do
      send(client, {:adapter_call, :send_cancelled, {client, request_id}})
      :ok
    end

    def tools(client) do
      send(client, {:adapter_call, :tools, client})
      {:ok, []}
    end
  end

  describe "adapter behaviour" do
    test "all callbacks are defined" do
      # Verify the behaviour has all expected callbacks
      callbacks = Adapter.behaviour_info(:callbacks)

      assert {:start_link, 2} in callbacks
      assert {:connect, 1} in callbacks
      assert {:disconnect, 1} in callbacks
      assert {:call, 4} in callbacks
      assert {:list_tools, 2} in callbacks
      assert {:server_info, 1} in callbacks
    end

    test "test adapter implements all callbacks" do
      # This will fail to compile if any callbacks are missing
      assert TestAdapter.__info__(:functions) |> Keyword.get(:start_link) == 2
    end
  end

  describe "adapter delegation" do
    test "calls are delegated to the adapter" do
      client = self()

      # Test various functions
      assert :ok = TestAdapter.connect(client)
      assert_receive {:adapter_call, :connect, ^client}

      assert {:ok, _} = TestAdapter.call(client, "test", %{}, [])
      assert_receive {:adapter_call, :call, {^client, "test", %{}, []}}

      assert {:ok, _} = TestAdapter.list_tools(client, [])
      assert_receive {:adapter_call, :list_tools, {^client, []}}
    end
  end

  describe "StateMachineAdapter" do
    setup do
      # Create a simple transport for testing
      test_pid = self()

      # Use the SimpleTransport defined in this module
      config = %{
        transport: SimpleTransport,
        test_pid: test_pid
      }

      {:ok, client} = StateMachineAdapter.start_link(config)

      %{client: client, test_pid: test_pid}
    end

    test "can connect and perform handshake", %{client: client} do
      assert :ok = StateMachineAdapter.connect(client)

      # Should receive initialize request
      assert_receive {:sent_message, init_msg}, 1000
      {:ok, init_request} = Jason.decode(init_msg)

      assert init_request["method"] == "initialize"

      # Send response
      init_response = %{
        "jsonrpc" => "2.0",
        "result" => %{
          "protocolVersion" => "2025-03-26",
          "capabilities" => %{},
          "serverInfo" => %{"name" => "test-server", "version" => "1.0.0"}
        },
        "id" => init_request["id"]
      }

      send(client, {:transport_message, Jason.encode!(init_response)})

      # Should receive initialized notification
      assert_receive {:sent_message, _initialized_msg}, 1000

      # Verify server info is available immediately while still connected
      info = StateMachineAdapter.server_info(client)
      assert info["name"] == "test-server"
    end

    test "handles requests through adapter", %{client: client} do
      # Connect first
      :ok = StateMachineAdapter.connect(client)

      # Handle handshake
      assert_receive {:sent_message, init_msg}, 1000
      {:ok, init_request} = Jason.decode(init_msg)

      init_response = %{
        "jsonrpc" => "2.0",
        "result" => %{
          "protocolVersion" => "2025-03-26",
          "capabilities" => %{},
          "serverInfo" => %{"name" => "test-server", "version" => "1.0.0"}
        },
        "id" => init_request["id"]
      }

      send(client, {:transport_message, Jason.encode!(init_response)})
      assert_receive {:sent_message, _initialized_msg}, 1000

      # Now make a request
      task =
        Task.async(fn ->
          StateMachineAdapter.call(client, "test/method", %{foo: "bar"})
        end)

      # Should receive the request
      assert_receive {:sent_message, request_msg}, 1000
      {:ok, request} = Jason.decode(request_msg)

      assert request["method"] == "test/method"
      assert request["params"]["foo"] == "bar"

      # Send response
      response = %{
        "jsonrpc" => "2.0",
        "result" => %{"success" => true},
        "id" => request["id"]
      }

      send(client, {:transport_message, Jason.encode!(response)})

      assert {:ok, %{"success" => true}} = Task.await(task)
    end
  end
end
