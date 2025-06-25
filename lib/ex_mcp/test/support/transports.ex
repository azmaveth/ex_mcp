defmodule ExMcp.Test.Support.Transports do
  @moduledoc """
  Test support module for transport-related testing utilities.

  This module provides helpers for testing different transport mechanisms
  used in ExMCP, including HTTP, stdio, and BEAM transports. It includes
  mock transport implementations and utilities for testing transport behavior.
  """

  @doc """
  Creates a mock transport configuration for testing.

  ## Parameters
  - `transport_type` - The type of transport (:http, :stdio, :beam, :test)
  - `opts` - Additional options for the transport

  ## Returns
  A transport configuration map suitable for testing.
  """
  @spec mock_transport_config(atom(), keyword()) :: map()
  def mock_transport_config(transport_type, opts \\ []) do
    base_config = %{
      type: transport_type,
      test_mode: true,
      timeout: Keyword.get(opts, :timeout, 5000)
    }

    case transport_type do
      :http ->
        Map.merge(base_config, %{
          host: Keyword.get(opts, :host, "localhost"),
          # Use random port for testing
          port: Keyword.get(opts, :port, 0),
          path: Keyword.get(opts, :path, "/mcp"),
          ssl: Keyword.get(opts, :ssl, false)
        })

      :stdio ->
        Map.merge(base_config, %{
          command: Keyword.get(opts, :command, "echo"),
          args: Keyword.get(opts, :args, []),
          cwd: Keyword.get(opts, :cwd, System.tmp_dir())
        })

      :beam ->
        Map.merge(base_config, %{
          target_node: Keyword.get(opts, :target_node, node()),
          module: Keyword.get(opts, :module, ExMCP.TestServer),
          function: Keyword.get(opts, :function, :handle_message)
        })

      :test ->
        Map.merge(base_config, %{
          mock_responses: Keyword.get(opts, :mock_responses, []),
          fail_after: Keyword.get(opts, :fail_after, nil),
          delay: Keyword.get(opts, :delay, 0)
        })

      _ ->
        base_config
    end
  end

  @doc """
  Creates a mock transport process for testing.

  This creates a GenServer process that simulates transport behavior
  for testing purposes without requiring actual network connections.
  """
  @spec start_mock_transport(atom(), keyword()) :: {:ok, pid()} | {:error, term()}
  def start_mock_transport(transport_type, opts \\ []) do
    config = mock_transport_config(transport_type, opts)

    GenServer.start_link(__MODULE__.MockTransport, config, [])
  end

  @doc """
  Simulates a transport connection for testing.

  This function creates a mock connection that can be used in tests
  to verify transport behavior without external dependencies.
  """
  @spec mock_connection(atom(), keyword()) :: %{
          transport: atom(),
          config: map(),
          pid: pid() | nil
        }
  def mock_connection(transport_type, opts \\ []) do
    config = mock_transport_config(transport_type, opts)

    case start_mock_transport(transport_type, opts) do
      {:ok, pid} ->
        %{transport: transport_type, config: config, pid: pid}

      {:error, _reason} ->
        %{transport: transport_type, config: config, pid: nil}
    end
  end

  @doc """
  Validates transport configuration for testing.

  Ensures that transport configurations are valid and contain
  required fields for testing scenarios.
  """
  @spec validate_transport_config(map()) :: :ok | {:error, String.t()}
  def validate_transport_config(%{type: type} = config)
      when type in [:http, :stdio, :beam, :test] do
    case type do
      :http ->
        required_fields = [:host, :port]
        validate_required_fields(config, required_fields)

      :stdio ->
        required_fields = [:command]
        validate_required_fields(config, required_fields)

      :beam ->
        required_fields = [:target_node, :module]
        validate_required_fields(config, required_fields)

      :test ->
        # Test transport is always valid
        :ok

      _ ->
        {:error, "Unknown transport type: #{type}"}
    end
  end

  def validate_transport_config(_config) do
    {:error, "Transport configuration must include :type field"}
  end

  @doc """
  Creates transport headers for HTTP testing.

  Generates appropriate headers for different HTTP test scenarios,
  including authentication headers and content type headers.
  """
  @spec mock_http_headers(keyword()) :: map()
  def mock_http_headers(opts \\ []) do
    base_headers = %{
      "content-type" => "application/json",
      "user-agent" => "ExMCP-Test/1.0"
    }

    auth_headers =
      case Keyword.get(opts, :auth) do
        {:bearer, token} ->
          %{"authorization" => "Bearer #{token}"}

        {:api_key, key} ->
          %{"x-api-key" => key}

        {:basic, {user, pass}} ->
          encoded = Base.encode64("#{user}:#{pass}")
          %{"authorization" => "Basic #{encoded}"}

        _ ->
          %{}
      end

    custom_headers = Keyword.get(opts, :headers, %{})

    Map.merge(base_headers, auth_headers)
    |> Map.merge(custom_headers)
  end

  @doc """
  Simulates transport errors for testing error handling.

  Creates various error scenarios that can occur during transport
  operations to test error handling and recovery mechanisms.
  """
  @spec simulate_transport_error(atom()) :: {:error, term()}
  def simulate_transport_error(error_type) do
    case error_type do
      :connection_refused -> {:error, :econnrefused}
      :timeout -> {:error, :timeout}
      :network_unreachable -> {:error, :enetunreach}
      :host_unreachable -> {:error, :ehostunreach}
      :ssl_error -> {:error, {:tls_alert, {:handshake_failure, "SSL handshake failed"}}}
      :authentication_failed -> {:error, :authentication_failed}
      :protocol_error -> {:error, :protocol_error}
      :invalid_response -> {:error, :invalid_response}
      _ -> {:error, :unknown_transport_error}
    end
  end

  @doc """
  Creates a transport message in the expected format.

  Formats messages according to the transport protocol specifications
  for use in testing scenarios.
  """
  @spec format_transport_message(String.t(), map()) :: map()
  def format_transport_message(method, params) do
    %{
      "jsonrpc" => "2.0",
      "method" => method,
      "params" => params,
      "id" => ExMCP.Internal.Protocol.generate_id()
    }
  end

  # Private helper functions

  defp validate_required_fields(config, required_fields) do
    missing_fields = Enum.reject(required_fields, &Map.has_key?(config, &1))

    case missing_fields do
      [] -> :ok
      fields -> {:error, "Missing required fields: #{Enum.join(fields, ", ")}"}
    end
  end
end

defmodule ExMcp.Test.Support.Transports.MockTransport do
  @moduledoc false
  # Mock transport GenServer for testing

  use GenServer

  def init(config) do
    {:ok, %{config: config, messages: [], connected: false}}
  end

  def handle_call(:connect, _from, state) do
    {:reply, :ok, %{state | connected: true}}
  end

  def handle_call(:disconnect, _from, state) do
    {:reply, :ok, %{state | connected: false}}
  end

  def handle_call({:send_message, message}, _from, state) do
    new_messages = [message | state.messages]
    response = mock_response(message, state.config)
    {:reply, {:ok, response}, %{state | messages: new_messages}}
  end

  def handle_call(:get_messages, _from, state) do
    {:reply, Enum.reverse(state.messages), state}
  end

  def handle_call(:clear_messages, _from, state) do
    {:reply, :ok, %{state | messages: []}}
  end

  def handle_call(_request, _from, state) do
    {:reply, {:error, :unknown_request}, state}
  end

  def handle_cast(_request, state) do
    {:noreply, state}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end

  # Generate mock responses based on message type
  defp mock_response(%{"method" => "initialize"}, _config) do
    %{
      "jsonrpc" => "2.0",
      "result" => %{
        "protocolVersion" => "2025-06-18",
        "capabilities" => %{},
        "serverInfo" => %{"name" => "MockServer", "version" => "1.0.0"}
      },
      "id" => 1
    }
  end

  defp mock_response(%{"method" => "tools/list"}, _config) do
    %{
      "jsonrpc" => "2.0",
      "result" => %{"tools" => []},
      "id" => 2
    }
  end

  defp mock_response(%{"method" => "ping"}, _config) do
    %{
      "jsonrpc" => "2.0",
      "result" => %{},
      "id" => 3
    }
  end

  defp mock_response(_message, _config) do
    %{
      "jsonrpc" => "2.0",
      "error" => %{"code" => -32601, "message" => "Method not found"},
      "id" => nil
    }
  end
end
