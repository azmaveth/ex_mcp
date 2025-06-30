defmodule ExMCP.Client.ConfigurationTest do
  use ExUnit.Case, async: true

  alias ExMCP.Client.{Configuration, LegacyAdapter, StateMachineAdapter, Wrapper}

  describe "configuration validation" do
    test "returns default adapter" do
      assert Configuration.default_adapter() == LegacyAdapter
    end

    test "lists available adapters" do
      adapters = Configuration.available_adapters()
      assert LegacyAdapter in adapters
      assert StateMachineAdapter in adapters
      assert length(adapters) == 2
    end

    test "validates adapter modules" do
      assert Configuration.valid_adapter?(LegacyAdapter)
      assert Configuration.valid_adapter?(StateMachineAdapter)
      assert not Configuration.valid_adapter?(SomeRandomModule)
      assert not Configuration.valid_adapter?("not_an_atom")
      assert not Configuration.valid_adapter?(nil)
    end

    test "provides telemetry events list" do
      events = Configuration.telemetry_events()
      assert is_list(events)
      assert length(events) > 0

      # Verify format - all should be lists with atoms
      Enum.each(events, fn event ->
        assert is_list(event)
        assert Enum.all?(event, &is_atom/1)
      end)
    end
  end

  describe "adapter selection priority" do
    test "uses adapter from opts first" do
      config = %{adapter: LegacyAdapter}
      opts = [adapter: StateMachineAdapter]

      adapter = Wrapper.get_adapter(config, opts)
      assert adapter == StateMachineAdapter
    end

    test "uses adapter from config map second" do
      config = %{adapter: StateMachineAdapter, transport: :stdio}
      opts = []

      adapter = Wrapper.get_adapter(config, opts)
      assert adapter == StateMachineAdapter
    end

    test "uses application environment third" do
      # Save original value
      original = Application.get_env(:ex_mcp, :client_adapter)

      try do
        Application.put_env(:ex_mcp, :client_adapter, StateMachineAdapter)

        config = %{transport: :stdio}
        opts = []

        adapter = Wrapper.get_adapter(config, opts)
        assert adapter == StateMachineAdapter
      after
        # Restore original value
        if original do
          Application.put_env(:ex_mcp, :client_adapter, original)
        else
          Application.delete_env(:ex_mcp, :client_adapter)
        end
      end
    end

    test "uses default adapter last" do
      # Ensure no app env is set
      original = Application.get_env(:ex_mcp, :client_adapter)
      Application.delete_env(:ex_mcp, :client_adapter)

      try do
        config = %{transport: :stdio}
        opts = []

        adapter = Wrapper.get_adapter(config, opts)
        assert adapter == LegacyAdapter
      after
        # Restore original value
        if original do
          Application.put_env(:ex_mcp, :client_adapter, original)
        end
      end
    end
  end

  describe "wrapper functionality" do
    # Define a minimal test adapter for testing
    defmodule TestMinimalAdapter do
      @behaviour ExMCP.Client.Adapter

      def start_link(_config, _opts), do: {:ok, self()}
      def connect(_client), do: :ok
      def disconnect(_client), do: :ok
      def stop(_client), do: :ok
      def call(_client, _method, _params, _opts), do: {:ok, %{"result" => "test"}}
      def notify(_client, _method, _params), do: :ok
      def batch_request(_client, _requests, _opts), do: {:ok, []}
      def complete(_client, _request, _token), do: {:ok, %{}}
      def list_tools(_client, _opts), do: {:ok, %{"tools" => []}}
      def call_tool(_client, _name, _arguments, _opts), do: {:ok, %{"result" => "test"}}
      def find_tool(_client, _name), do: {:error, :not_found}
      def find_matching_tool(_client, _pattern), do: {:ok, []}
      def list_resources(_client, _opts), do: {:ok, %{"resources" => []}}
      def read_resource(_client, _uri, _opts), do: {:ok, %{"contents" => "test"}}
      def list_resource_templates(_client, _opts), do: {:ok, %{"templates" => []}}
      def subscribe_resource(_client, _uri, _opts), do: {:ok, %{}}
      def unsubscribe_resource(_client, _uri, _opts), do: {:ok, %{}}
      def list_prompts(_client, _opts), do: {:ok, %{"prompts" => []}}
      def get_prompt(_client, _name, _arguments, _opts), do: {:ok, %{"prompt" => "test"}}
      def set_log_level(_client, _level, _opts), do: {:ok, %{}}
      def log_message(_client, _level, _message, _data, _opts), do: :ok
      def ping(_client, _opts), do: {:ok, %{}}
      def server_info(_client), do: %{"name" => "test"}
      def server_capabilities(_client), do: %{}
      def negotiated_version(_client), do: "2025-03-26"
      def list_roots(_client, _opts), do: {:ok, %{"roots" => []}}
      def get_status(_client), do: %{state: :ready}
      def get_pending_requests(_client), do: %{}
      def make_request(_client, _method, _params, _opts), do: {:ok, %{"result" => "test"}}
      def send_batch(_client, _requests), do: {:ok, []}
      def send_cancelled(_client, _request_id), do: :ok
      def tools(_client), do: {:ok, []}
    end

    test "wrapper starts with specified adapter" do
      config = %{transport: :test}
      opts = [adapter: TestMinimalAdapter]

      {:ok, wrapped_client} = Wrapper.start_link(config, opts)
      assert %Wrapper.State{adapter: TestMinimalAdapter} = wrapped_client
    end

    test "wrapper delegates calls to adapter" do
      config = %{transport: :test}
      opts = [adapter: TestMinimalAdapter]

      {:ok, wrapped_client} = Wrapper.start_link(config, opts)

      # Test that calls are delegated
      assert :ok = Wrapper.connect(wrapped_client)
      assert {:ok, %{"result" => "test"}} = Wrapper.call(wrapped_client, "test", %{})
      assert %{"name" => "test"} = Wrapper.server_info(wrapped_client)
    end

    test "wrapper handles raw client PIDs for legacy compatibility" do
      # Test legacy behavior for raw PIDs
      client_pid = self()

      # These should not crash
      assert :ok = Wrapper.connect(client_pid)
      assert :ok = Wrapper.disconnect(client_pid)
    end
  end

  describe "migration scenarios" do
    test "can start client with legacy adapter explicitly" do
      config = %{transport: :test}
      opts = [adapter: LegacyAdapter]

      adapter = Wrapper.get_adapter(config, opts)
      assert adapter == LegacyAdapter
    end

    test "can start client with state machine adapter explicitly" do
      config = %{transport: :test}
      opts = [adapter: StateMachineAdapter]

      adapter = Wrapper.get_adapter(config, opts)
      assert adapter == StateMachineAdapter
    end

    test "application config affects all clients by default" do
      original = Application.get_env(:ex_mcp, :client_adapter)

      try do
        # Set application default to state machine
        Application.put_env(:ex_mcp, :client_adapter, StateMachineAdapter)

        config = %{transport: :test}
        opts = []

        adapter = Wrapper.get_adapter(config, opts)
        assert adapter == StateMachineAdapter

        # But explicit opts still override
        opts_with_legacy = [adapter: LegacyAdapter]
        adapter_override = Wrapper.get_adapter(config, opts_with_legacy)
        assert adapter_override == LegacyAdapter
      after
        if original do
          Application.put_env(:ex_mcp, :client_adapter, original)
        else
          Application.delete_env(:ex_mcp, :client_adapter)
        end
      end
    end
  end

  describe "configuration edge cases" do
    test "handles nil values gracefully" do
      config = %{transport: :test, adapter: nil}
      opts = [adapter: nil]

      # Should fall back to default
      adapter = Wrapper.get_adapter(config, opts)
      assert adapter == LegacyAdapter
    end

    test "handles non-map config" do
      config = [transport: :test, adapter: StateMachineAdapter]
      opts = []

      # Should fall back to checking opts and app env
      adapter = Wrapper.get_adapter(config, opts)
      # Default since not in map
      assert adapter == LegacyAdapter
    end

    test "keyword config doesn't override map config" do
      config = %{adapter: StateMachineAdapter}
      opts = []

      adapter = Wrapper.get_adapter(config, opts)
      assert adapter == StateMachineAdapter
    end
  end

  describe "documentation examples work" do
    test "basic configuration example" do
      # Example from docs should work
      original = Application.get_env(:ex_mcp, :client_adapter)

      try do
        Application.put_env(:ex_mcp, :client_adapter, StateMachineAdapter)

        config = %{transport: :stdio, command: "mcp-server"}
        opts = []

        adapter = Wrapper.get_adapter(config, opts)
        assert adapter == StateMachineAdapter
      after
        if original do
          Application.put_env(:ex_mcp, :client_adapter, original)
        else
          Application.delete_env(:ex_mcp, :client_adapter)
        end
      end
    end

    test "per-client override example" do
      config = %{transport: :stdio, command: "mcp-server"}
      opts = [adapter: StateMachineAdapter]

      adapter = Wrapper.get_adapter(config, opts)
      assert adapter == StateMachineAdapter
    end

    test "hybrid environment example" do
      original = Application.get_env(:ex_mcp, :client_adapter)

      try do
        # Most clients use state machine
        Application.put_env(:ex_mcp, :client_adapter, StateMachineAdapter)

        # Default adapter
        config1 = %{transport: :stdio}
        adapter1 = Wrapper.get_adapter(config1, [])
        assert adapter1 == StateMachineAdapter

        # Specific legacy client
        config2 = %{transport: :stdio}
        opts2 = [adapter: LegacyAdapter]
        adapter2 = Wrapper.get_adapter(config2, opts2)
        assert adapter2 == LegacyAdapter
      after
        if original do
          Application.put_env(:ex_mcp, :client_adapter, original)
        else
          Application.delete_env(:ex_mcp, :client_adapter)
        end
      end
    end
  end
end
