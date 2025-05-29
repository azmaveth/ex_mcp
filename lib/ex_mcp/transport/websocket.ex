defmodule ExMCP.Transport.WebSocket do
  @moduledoc """
  This module provides ExMCP extensions beyond the standard MCP specification.

  WebSocket transport implementation for ExMCP.

  This transport enables MCP communication over WebSocket connections,
  providing real-time bidirectional communication suitable for web applications
  and scenarios requiring persistent connections through firewalls and proxies.

  > #### Extension Module {: .info}
  > This transport is an ExMCP extension and is not part of the official MCP specification.
  > For specification compliance, use stdio or SSE transports.

  ## Features

  - Full-duplex communication over a single TCP connection
  - Built-in message framing
  - Automatic ping/pong for connection health
  - Support for both text and binary frames (uses text for JSON-RPC)
  - Works with standard WebSocket libraries and browsers

  ## Client Usage

  ```elixir
  {:ok, client} = ExMCP.Client.start_link(
    transport: :websocket,
    url: "ws://localhost:8080/mcp",
    headers: [{"Authorization", "Bearer token"}],
    protocols: ["mcp"]  # Optional subprotocols
  )
  ```

  ## Server Usage

  WebSocket server mode requires an HTTP server with WebSocket upgrade support.
  Currently, this transport only implements client mode. For server mode, use
  a WebSocket server library like Cowboy or Bandit with an MCP handler.

  ## Connection Health

  The transport automatically handles ping/pong frames to maintain connection
  health and detect disconnections promptly.
  """

  @behaviour ExMCP.Transport

  require Logger
  alias ExMCP.Security

  defmodule State do
    @moduledoc false
    defstruct [
      :conn,
      :websocket,
      :request_ref,
      :caller_pid,
      :url,
      :uri,
      :headers,
      :protocols,
      :mode,
      :security,
      :origin,
      buffer: "",
      status: :connecting
    ]
  end

  @impl true
  def connect(opts) do
    url = Keyword.fetch!(opts, :url)
    headers = Keyword.get(opts, :headers, [])
    protocols = Keyword.get(opts, :protocols, [])
    security = Keyword.get(opts, :security)

    uri = URI.parse(url)

    # Extract origin if provided
    origin =
      case security do
        %{origin: origin} -> origin
        _ -> extract_origin_from_url(url)
      end

    # Build security headers
    security_headers =
      if security do
        Security.build_security_headers(security)
      else
        []
      end

    # Merge all headers
    all_headers = Enum.uniq_by(headers ++ security_headers, fn {name, _} -> name end)

    state = %State{
      url: url,
      uri: uri,
      headers: all_headers,
      protocols: protocols,
      mode: :client,
      caller_pid: self(),
      security: security,
      origin: origin
    }

    # Validate security configuration
    with :ok <- validate_security(state) do
      do_connect(state)
    end
  end

  # Note: accept/1 is not part of the Transport behaviour for WebSocket
  # Server mode requires an HTTP server with upgrade support
  def accept(_opts) do
    {:error, :server_mode_not_implemented}
  end

  @impl true
  def send_message(_message, %State{status: :closed}) do
    {:error, :connection_closed}
  end

  def send_message(
        message,
        %State{status: :connected, conn: conn, websocket: websocket, request_ref: ref} = state
      ) do
    # Encode the message as a text frame
    case Mint.WebSocket.encode(websocket, {:text, message}) do
      {:ok, websocket, data} ->
        case Mint.HTTP.stream_request_body(conn, ref, data) do
          {:ok, conn} ->
            {:ok, %{state | conn: conn, websocket: websocket}}

          {:error, _conn, reason} ->
            Logger.error("Failed to send WebSocket message: #{inspect(reason)}")
            {:error, reason}
        end

      {:error, _websocket, reason} ->
        Logger.error("Failed to encode WebSocket message: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def send_message(_message, %State{}) do
    {:error, :not_connected}
  end

  @impl true
  def receive_message(%State{status: :closed}) do
    {:error, :connection_closed}
  end

  def receive_message(%State{status: :connected, conn: conn} = state) do
    # Read from the connection
    case Mint.HTTP.recv(conn, 0, 5_000) do
      {:ok, conn, responses} ->
        process_responses(responses, %{state | conn: conn})

      {:error, conn, reason, _responses} ->
        case reason do
          %Mint.TransportError{reason: :timeout} ->
            # Timeout is okay, just return to wait for more data
            {:cont, %{state | conn: conn}}

          _ ->
            Logger.error("WebSocket receive error: #{inspect(reason)}")
            {:error, reason}
        end
    end
  end

  def receive_message(%State{}) do
    {:error, :not_connected}
  end

  @impl true
  def close(%State{conn: nil}), do: :ok

  def close(%State{status: :connected, conn: conn, websocket: websocket, request_ref: ref}) do
    # Send close frame
    case Mint.WebSocket.encode(websocket, :close) do
      {:ok, _websocket, data} ->
        Mint.HTTP.stream_request_body(conn, ref, data)

      _ ->
        :ok
    end

    Mint.HTTP.close(conn)
    :ok
  end

  def close(%State{conn: conn}) when not is_nil(conn) do
    Mint.HTTP.close(conn)
    :ok
  end

  def close(_state), do: :ok

  # Private functions

  defp do_connect(state) do
    with {:ok, scheme} <- validate_scheme(state.uri),
         {:ok, conn} <- create_connection(scheme, state.uri, state),
         {:ok, conn, ref} <- upgrade_to_websocket(conn, state),
         {:ok, conn, websocket, status} <- await_upgrade(conn, ref) do
      new_state = %{state | conn: conn, websocket: websocket, request_ref: ref, status: status}

      {:ok, new_state}
    else
      {:error, reason} ->
        Logger.error("WebSocket connection failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp validate_scheme(%URI{scheme: "ws"}), do: {:ok, :http}
  defp validate_scheme(%URI{scheme: "wss"}), do: {:ok, :https}
  defp validate_scheme(%URI{scheme: scheme}), do: {:error, {:invalid_scheme, scheme}}

  defp create_connection(scheme, uri, state) do
    host = uri.host || "localhost"
    port = uri.port || default_port(scheme)

    # Build connection options with TLS if needed
    connect_opts =
      case scheme do
        :https -> build_tls_options(state)
        _ -> []
      end

    Mint.HTTP.connect(scheme, host, port, connect_opts)
  end

  defp default_port(:http), do: 80
  defp default_port(:https), do: 443

  defp upgrade_to_websocket(conn, state) do
    path = state.uri.path || "/"

    headers = build_headers(state)

    case Mint.WebSocket.upgrade(:ws, conn, path, headers) do
      {:ok, conn, ref} ->
        {:ok, conn, ref}

      {:error, _conn, reason} ->
        {:error, reason}
    end
  end

  defp build_headers(state) do
    base_headers = state.headers

    # Add subprotocols if specified
    case state.protocols do
      [] ->
        base_headers

      protocols ->
        [{"sec-websocket-protocol", Enum.join(protocols, ", ")} | base_headers]
    end
  end

  defp await_upgrade(conn, ref) do
    # Set connection to passive mode to use recv/3
    case Mint.HTTP.set_mode(conn, :passive) do
      {:ok, conn} ->
        case Mint.HTTP.recv(conn, 0, 5_000) do
          {:ok, conn, responses} ->
            handle_upgrade_response(conn, ref, responses)

          {:error, _conn, reason, _responses} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_upgrade_response(conn, ref, responses) do
    case check_upgrade_status(responses, ref) do
      {:ok, status, headers} ->
        # Use the Mint.WebSocket.new/4 function with the actual status code
        case Mint.WebSocket.new(conn, ref, status, headers) do
          {:ok, conn, websocket} ->
            {:ok, conn, websocket, :connected}

          {:error, _conn, reason} ->
            {:error, reason}
        end

      {:error, status} ->
        {:error, {:upgrade_failed, status}}
    end
  end

  defp check_upgrade_status(responses, ref) do
    status = get_status(responses, ref)
    headers = get_headers(responses, ref)

    case status do
      101 -> {:ok, 101, headers}
      nil -> {:error, :no_status}
      other -> {:error, other}
    end
  end

  defp get_status(responses, ref) do
    Enum.find_value(responses, fn
      {:status, ^ref, status} -> status
      _ -> nil
    end)
  end

  defp get_headers(responses, ref) do
    Enum.reduce(responses, [], fn
      {:headers, ^ref, headers}, acc -> acc ++ headers
      _, acc -> acc
    end)
  end

  defp process_responses([], state), do: {:cont, state}

  defp process_responses([response | rest], state) do
    case process_response(response, state) do
      {:ok, new_state} ->
        process_responses(rest, new_state)

      {:ok, message, new_state} ->
        # Found a complete message
        {:ok, message, new_state}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp process_response(
         {:data, ref, data},
         %State{request_ref: ref, websocket: websocket} = state
       ) do
    case Mint.WebSocket.decode(websocket, data) do
      {:ok, websocket, frames} ->
        handle_frames(frames, %{state | websocket: websocket})

      {:error, _websocket, reason} ->
        Logger.error("WebSocket decode error: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp process_response({:done, ref}, %State{request_ref: ref}) do
    # Connection closed
    {:error, :connection_closed}
  end

  defp process_response(_, state), do: {:ok, state}

  defp handle_frames([], state), do: {:ok, state}

  defp handle_frames([frame | rest], state) do
    case handle_frame(frame, state) do
      {:ok, new_state} ->
        handle_frames(rest, new_state)

      {:ok, message, new_state} ->
        # Return first complete message
        {:ok, message, new_state}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_frame({:text, message}, state) do
    # Return the text message for JSON-RPC processing
    {:ok, message, state}
  end

  defp handle_frame({:binary, _data}, state) do
    # MCP uses text frames
    Logger.warning("Ignoring binary WebSocket frame")
    {:ok, state}
  end

  defp handle_frame(
         {:ping, data},
         %State{conn: conn, websocket: websocket, request_ref: ref} = state
       ) do
    # Respond with pong
    case Mint.WebSocket.encode(websocket, {:pong, data}) do
      {:ok, websocket, encoded} ->
        case Mint.HTTP.stream_request_body(conn, ref, encoded) do
          {:ok, conn} ->
            {:ok, %{state | conn: conn, websocket: websocket}}

          {:error, _conn, reason} ->
            Logger.error("Failed to send pong: #{inspect(reason)}")
            {:ok, %{state | conn: conn, websocket: websocket}}
        end

      {:error, websocket, reason} ->
        Logger.error("Failed to encode pong: #{inspect(reason)}")
        {:ok, %{state | websocket: websocket}}
    end
  end

  defp handle_frame({:pong, _data}, state) do
    # Pong received, connection is healthy
    {:ok, state}
  end

  defp handle_frame({:close, code, reason}, _state) do
    Logger.info("WebSocket closed by remote: #{code} - #{reason}")
    {:error, {:closed, code, reason}}
  end

  defp handle_frame(frame, state) do
    Logger.debug("Unhandled WebSocket frame: #{inspect(frame)}")
    {:ok, state}
  end

  # Security helper functions

  defp validate_security(%{security: nil}), do: :ok

  defp validate_security(%{security: security}) do
    Security.validate_config(security)
  end

  defp extract_origin_from_url(url) do
    uri = URI.parse(url)

    if uri.scheme && uri.host do
      scheme =
        case uri.scheme do
          "ws" -> "http"
          "wss" -> "https"
          other -> other
        end

      "#{scheme}://#{uri.host}#{if uri.port && uri.port != default_port(scheme_to_atom(scheme)), do: ":#{uri.port}", else: ""}"
    else
      nil
    end
  end

  defp scheme_to_atom("http"), do: :http
  defp scheme_to_atom("https"), do: :https
  defp scheme_to_atom(_), do: nil

  defp build_tls_options(%{security: %{tls: tls_config}}) when is_map(tls_config) do
    [
      transport_opts:
        [
          verify: Map.get(tls_config, :verify, :verify_peer),
          cacerts: Map.get(tls_config, :cacerts, :public_key.cacerts_get()),
          versions: Map.get(tls_config, :versions, [:"tlsv1.2", :"tlsv1.3"]),
          cert: Map.get(tls_config, :cert),
          key: Map.get(tls_config, :key)
        ]
        |> Enum.reject(fn {_k, v} -> is_nil(v) end)
    ]
  end

  defp build_tls_options(_state) do
    # Default TLS options for WSS
    [
      transport_opts: [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        versions: [:"tlsv1.2", :"tlsv1.3"]
      ]
    ]
  end
end
