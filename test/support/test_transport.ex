defmodule ExMCP.TestHelpers.TestTransport do
  @moduledoc """
  Test transport for integration testing the state machine.

  Provides various test modes to simulate different scenarios:
  - :controlled - Manual control over messages
  - :echo - Echoes requests back
  - :fail_connect - Fails on connect
  - :no_response - Connects but doesn't respond
  - :flaky - Simulates disconnections
  - :always_fail - Always fails
  - :interactive - Full control
  """

  @behaviour ExMCP.Transport

  use GenServer

  defstruct [:mode, :client_pid, :test_pid, :connected, :messages, :connection_count]

  # Transport behaviour implementation

  @impl true
  def connect(opts) do
    mode = Keyword.get(opts, :test_mode, :controlled)
    test_pid = Keyword.get(opts, :test_pid, self())
    client_pid = Keyword.get(opts, :client_pid, self())

    case mode do
      :fail_connect ->
        {:error, :connection_refused}

      :always_fail ->
        {:error, :transport_error}

      :flaky ->
        # Check if this is a reconnection attempt (transport already exists)
        case :global.whereis_name({:test_transport, client_pid}) do
          :undefined ->
            # First connection - succeed
            {:ok, state} = GenServer.start_link(__MODULE__, {mode, test_pid, client_pid})
            :global.register_name({:test_transport, client_pid}, state)
            {:ok, state}

          _existing_pid ->
            # Reconnection attempt - fail
            {:error, :reconnection_failed}
        end

      :reconnect_timeout ->
        # Similar to flaky but designed for timeout scenarios
        case :global.whereis_name({:test_transport, client_pid}) do
          :undefined ->
            {:ok, state} = GenServer.start_link(__MODULE__, {mode, test_pid, client_pid})
            :global.register_name({:test_transport, client_pid}, state)
            {:ok, state}

          _existing_pid ->
            {:error, :connection_timeout}
        end

      _ ->
        {:ok, state} = GenServer.start_link(__MODULE__, {mode, test_pid, client_pid})
        # Register for tracking
        :global.register_name({:test_transport, client_pid}, state)
        {:ok, state}
    end
  end

  @impl true
  def send_message(message, transport_pid) when is_pid(transport_pid) do
    GenServer.call(transport_pid, {:send, message})
  end

  @impl true
  def receive_message(transport_pid) when is_pid(transport_pid) do
    # For test transport, we don't use this - we send messages directly
    # This will timeout quickly since we send via send_to_client
    GenServer.call(transport_pid, :receive, 100)
  end

  @impl true
  def close(transport_pid) when is_pid(transport_pid) do
    GenServer.stop(transport_pid)
  end

  @impl true
  def connected?(transport_pid) when is_pid(transport_pid) do
    GenServer.call(transport_pid, :connected?)
  end

  # Test API

  def send_to_client(client_pid, message) do
    # Send message directly to the state machine process
    # The receiver loop expects this format
    send(client_pid, {:transport_message, Jason.encode!(message)})
    :ok
  end

  def send_raw_to_client(client_pid, raw_message) do
    # Send raw message directly to the state machine process
    send(client_pid, {:transport_message, raw_message})
    :ok
  end

  def simulate_disconnect(client_pid) do
    # Send transport closed message to trigger reconnection behavior
    send(client_pid, {:transport_closed, :connection_lost})

    # Clean up global registration to allow reconnection behavior
    case :global.whereis_name({:test_transport, client_pid}) do
      :undefined ->
        :ok

      transport_pid ->
        :global.unregister_name({:test_transport, client_pid})
        GenServer.stop(transport_pid, :normal)
    end

    :ok
  end

  def get_last_message(client_pid) do
    # For testing, we'll use a registry to track transport messages
    # This is simplified - in real usage the transport would handle this
    case :global.whereis_name({:test_transport, client_pid}) do
      :undefined ->
        {:error, :no_transport}

      transport_pid ->
        GenServer.call(transport_pid, :get_last_message)
    end
  end

  def get_last_sent_id(client_pid) do
    case get_last_message(client_pid) do
      {:ok, %{"id" => id}} -> {:ok, id}
      {:error, reason} -> {:error, reason}
      _ -> {:error, :no_id}
    end
  end

  def respond_to_handshake(client_pid) do
    # Get the last sent message to find the ID to respond to
    case get_last_sent_id(client_pid) do
      {:ok, id} ->
        response = %{
          "id" => id,
          "result" => %{
            "protocolVersion" => "2025-03-26",
            "capabilities" => %{},
            "serverInfo" => %{"name" => "test", "version" => "1.0"}
          }
        }

        send_to_client(client_pid, response)

      {:error, reason} ->
        {:error, reason}
    end
  end

  # GenServer implementation

  @impl true
  def init({mode, test_pid, client_pid}) do
    state = %__MODULE__{
      mode: mode,
      test_pid: test_pid,
      client_pid: client_pid,
      connected: true,
      messages: [],
      connection_count: 1
    }

    {:ok, state}
  end

  # Legacy init for existing code
  def init({mode, test_pid}) do
    init({mode, test_pid, test_pid})
  end

  # Helper functions for handling different transport modes
  defp handle_mode_response(:controlled, message, state) do
    handle_controlled_mode(message, state)
  end

  defp handle_mode_response(:echo, message, state) do
    handle_echo_mode(message, state)
  end

  defp handle_mode_response(:no_response, _message, _state) do
    IO.puts("DEBUG: TestTransport in no_response mode, not sending response")
    :ok
  end

  defp handle_mode_response(mode, _message, _state)
       when mode in [:always_fail, :flaky, :interactive] do
    :ok
  end

  defp handle_mode_response(_mode, _message, _state) do
    :ok
  end

  defp handle_controlled_mode(message, state) do
    case Jason.decode(message) do
      {:ok, %{"id" => id, "method" => "initialize"}} ->
        response = %{
          "id" => id,
          "result" => %{
            "protocolVersion" => "2025-03-26",
            "capabilities" => %{
              "tools" => true,
              "resources" => true
            },
            "serverInfo" => %{
              "name" => "test-server",
              "version" => "1.0.0"
            }
          }
        }

        send_response_to_client(response, state)

      _ ->
        :ok
    end
  end

  defp handle_echo_mode(message, state) do
    case Jason.decode(message) do
      {:ok, %{"id" => id, "method" => "initialize", "params" => _params}} ->
        response = %{
          "id" => id,
          "result" => %{
            "protocolVersion" => "2025-03-26",
            "capabilities" => %{},
            "serverInfo" => %{"name" => "test", "version" => "1.0"}
          }
        }

        send_response_to_client(response, state)

      {:ok, %{"id" => id, "method" => method, "params" => params}} ->
        response = %{
          "id" => id,
          "result" => %{"echo" => %{"method" => method, "params" => params}}
        }

        send_response_to_client(response, state)

      _ ->
        :ok
    end
  end

  defp send_response_to_client(response, state) do
    if state.client_pid do
      send(state.client_pid, {:transport_message, Jason.encode!(response)})
    end
  end

  @impl true
  def handle_call({:send, message}, _from, state) do
    # Store message
    new_state = %{state | messages: [message | state.messages]}

    # Handle based on mode
    handle_mode_response(state.mode, message, state)

    {:reply, {:ok, self()}, new_state}
  end

  def handle_call(:receive, _from, %{mode: :always_fail} = state) do
    {:reply, {:error, :transport_error}, state}
  end

  def handle_call(:receive, _from, %{connected: false} = state) do
    {:reply, {:error, :closed}, state}
  end

  def handle_call(:receive, from, state) do
    # Wait for messages
    receive do
      {:test_message, message} ->
        {:reply, {:ok, message, self()}, state}
    after
      100 ->
        # Check if we're still waiting
        if state.mode == :no_response do
          # Keep waiting indefinitely
          handle_call(:receive, from, state)
        else
          # Return timeout
          {:reply, {:error, :timeout}, state}
        end
    end
  end

  def handle_call(:connected?, _from, state) do
    {:reply, state.connected, state}
  end

  def handle_call(:get_last_message, _from, state) do
    case state.messages do
      [last | _] ->
        {:reply, {:ok, Jason.decode!(last)}, state}

      [] ->
        {:reply, {:error, :no_messages}, state}
    end
  end

  @impl true
  def handle_cast(:disconnect, state) do
    new_state = %{state | connected: false}
    {:noreply, new_state}
  end
end
