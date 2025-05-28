defmodule ExMCP.Transport.HTTP do
  @moduledoc """
  @mcp_spec

  Streamable HTTP transport for MCP.

  This transport uses HTTP POST and GET requests with Server-Sent Events (SSE)
  for streaming server-to-client messages. This is one of the two
  official MCP transports defined in the specification.

  ## Security Features

  The Streamable HTTP transport supports comprehensive security features:

  - **Authentication**: Bearer tokens, API keys, basic auth
  - **Origin Validation**: Prevent DNS rebinding attacks
  - **CORS Headers**: Cross-origin resource sharing
  - **Security Headers**: XSS protection, frame options, etc.
  - **TLS/SSL**: Secure connections with certificate validation

  ## Example with Security

      {:ok, client} = ExMCP.Client.start_link(
        transport: :http,
        url: "https://api.example.com",
        security: %{
          auth: {:bearer, "your-token"},
          validate_origin: true,
          allowed_origins: ["https://app.example.com"],
          cors: %{
            allowed_methods: ["GET", "POST"],
            allow_credentials: true
          }
        }
      )

  > #### Security Warning {: .warning}
  > When implementing Streamable HTTP servers, always validate Origin headers,
  > bind to localhost only, and implement proper authentication.
  """

  @behaviour ExMCP.Transport

  alias ExMCP.Security

  defstruct [
    :base_url,
    :headers,
    :http_client,
    :sse_pid,
    :endpoint,
    :security,
    :origin
  ]

  @type t :: %__MODULE__{
          base_url: String.t(),
          headers: [{String.t(), String.t()}],
          http_client: module(),
          sse_pid: pid() | nil,
          endpoint: String.t(),
          security: Security.security_config() | nil,
          origin: String.t() | nil
        }

  @default_endpoint "/mcp/v1"

  @impl true
  def connect(config) do
    base_url = Keyword.fetch!(config, :url)
    headers = Keyword.get(config, :headers, [])
    http_client = Keyword.get(config, :http_client, :httpc)
    endpoint = Keyword.get(config, :endpoint, @default_endpoint)
    security = Keyword.get(config, :security)

    # Extract origin if provided
    origin =
      case security do
        %{origin: origin} -> origin
        _ -> extract_origin_from_url(base_url)
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

    state = %__MODULE__{
      base_url: base_url,
      headers: all_headers,
      http_client: http_client,
      endpoint: endpoint,
      security: security,
      origin: origin
    }

    # Validate security configuration
    with :ok <- validate_security(state),
         {:ok, sse_pid} <- start_sse(state) do
      {:ok, %{state | sse_pid: sse_pid}}
    end
  end

  @impl true
  def send_message(message, %__MODULE__{} = state) do
    url = build_url(state, "/messages")

    # Build headers with security
    headers = build_request_headers(state)

    case Jason.encode(message) do
      {:ok, body} ->
        request = {
          String.to_charlist(url),
          Enum.map(headers, fn {k, v} -> {String.to_charlist(k), String.to_charlist(v)} end),
          String.to_charlist("application/json"),
          body
        }

        # Add SSL options if using HTTPS
        http_opts =
          case URI.parse(url).scheme do
            "https" -> build_ssl_options(state)
            _ -> []
          end

        case :httpc.request(:post, request, http_opts, []) do
          {:ok, {{_, 200, _}, response_headers, _response_body}} ->
            # Validate CORS if configured
            validate_cors_response(response_headers, state)

          {:ok, {{_, status, _}, _, body}} ->
            {:error, {:http_error, status, body}}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, {:encoding_error, reason}}
    end
  end

  @impl true
  def receive_message(%__MODULE__{sse_pid: sse_pid} = state) do
    receive do
      {:sse_event, ^sse_pid, event} ->
        case parse_sse_event(event) do
          {:ok, message} ->
            {:ok, message, state}

          error ->
            error
        end

      {:sse_error, ^sse_pid, reason} ->
        {:error, {:sse_error, reason}}

      {:sse_closed, ^sse_pid} ->
        {:error, :connection_closed}
    end
  end

  @impl true
  def close(%__MODULE__{sse_pid: sse_pid}) when is_pid(sse_pid) do
    Process.exit(sse_pid, :normal)
    :ok
  end

  def close(%__MODULE__{}), do: :ok

  # Private functions

  defp start_sse(state) do
    parent = self()
    url = build_url(state, "/sse")

    sse_pid =
      spawn_link(fn ->
        sse_loop(parent, url, state)
      end)

    # Wait for connection confirmation
    receive do
      {:sse_connected, ^sse_pid} ->
        {:ok, sse_pid}

      {:sse_error, ^sse_pid, reason} ->
        {:error, reason}
    after
      5000 ->
        Process.exit(sse_pid, :kill)
        {:error, :connection_timeout}
    end
  end

  defp sse_loop(parent, url, state) do
    # Build SSL options if needed
    ssl_opts = build_ssl_options(state)

    # This is a simplified SSE client - in production you'd want
    # to use a proper SSE client library
    case connect_sse(url, state.headers, ssl_opts) do
      {:ok, ref} ->
        send(parent, {:sse_connected, self()})
        receive_sse_loop(parent, ref, "")

      {:error, reason} ->
        send(parent, {:sse_error, self(), reason})
    end
  end

  defp connect_sse(url, headers, ssl_opts) do
    headers = [
      {"accept", "text/event-stream"},
      {"cache-control", "no-cache"} | headers
    ]

    request = {
      String.to_charlist(url),
      Enum.map(headers, fn {k, v} -> {String.to_charlist(k), String.to_charlist(v)} end)
    }

    # Determine if we need SSL options
    http_opts =
      case URI.parse(url).scheme do
        "https" -> ssl_opts
        _ -> []
      end

    case :httpc.request(:get, request, http_opts, [{:sync, false}, {:stream, :self}]) do
      {:ok, ref} ->
        {:ok, ref}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp receive_sse_loop(parent, ref, buffer) do
    receive do
      {:http, {^ref, :stream_start, _headers}} ->
        receive_sse_loop(parent, ref, buffer)

      {:http, {^ref, :stream, chunk}} ->
        new_buffer = buffer <> chunk
        {events, remaining} = parse_sse_chunk(new_buffer)

        Enum.each(events, fn event ->
          send(parent, {:sse_event, self(), event})
        end)

        receive_sse_loop(parent, ref, remaining)

      {:http, {^ref, :stream_end, _headers}} ->
        send(parent, {:sse_closed, self()})

      {:http, {^ref, {:error, reason}}} ->
        send(parent, {:sse_error, self(), reason})
    end
  end

  defp parse_sse_chunk(buffer) do
    # Split but keep the last incomplete line
    case String.split(buffer, "\n") do
      lines when length(lines) > 1 ->
        {complete_lines, [last_line]} = Enum.split(lines, -1)

        # Check if the buffer ended with a newline
        if String.ends_with?(buffer, "\n") do
          # Last line is complete
          {events, _} = parse_sse_lines(lines, %{}, [])
          {Enum.reverse(events), ""}
        else
          # Last line is incomplete
          {events, _} = parse_sse_lines(complete_lines, %{}, [])
          {Enum.reverse(events), last_line}
        end

      [single_line] ->
        # No complete lines yet
        {[], single_line}
    end
  end

  defp parse_sse_lines([line | rest], current_event, events) do
    case line do
      "" ->
        # Empty line signals end of event
        if map_size(current_event) > 0 do
          parse_sse_lines(rest, %{}, [current_event | events])
        else
          parse_sse_lines(rest, current_event, events)
        end

      ":" <> _comment ->
        # Comment, ignore
        parse_sse_lines(rest, current_event, events)

      _ ->
        case String.split(line, ":", parts: 2) do
          [field, value] ->
            value = String.trim_leading(value)
            updated_event = Map.put(current_event, field, value)
            parse_sse_lines(rest, updated_event, events)

          _ ->
            # Invalid line, skip
            parse_sse_lines(rest, current_event, events)
        end
    end
  end

  defp parse_sse_lines([], _current_event, events) do
    # Return events and empty remaining buffer
    {events, ""}
  end

  defp parse_sse_event(%{"data" => data}) do
    case Jason.decode(data) do
      {:ok, message} ->
        {:ok, Jason.encode!(message)}

      {:error, reason} ->
        {:error, {:parse_error, reason}}
    end
  end

  defp parse_sse_event(_), do: {:error, :invalid_sse_event}

  defp build_url(%__MODULE__{base_url: base_url, endpoint: endpoint}, path) do
    base_url
    |> URI.parse()
    |> Map.put(:path, endpoint <> path)
    |> URI.to_string()
  end

  # Security-related helper functions

  defp validate_security(%{security: nil}), do: :ok

  defp validate_security(%{security: security}) do
    Security.validate_config(security)
  end

  defp extract_origin_from_url(url) do
    uri = URI.parse(url)

    if uri.scheme && uri.host do
      "#{uri.scheme}://#{uri.host}#{if uri.port && uri.port != default_port(uri.scheme), do: ":#{uri.port}", else: ""}"
    else
      nil
    end
  end

  defp default_port("http"), do: 80
  defp default_port("https"), do: 443
  defp default_port(_), do: nil

  defp build_request_headers(%{headers: headers, security: security, origin: origin}) do
    base_headers = [{"content-type", "application/json"} | headers]

    # Add Origin header if we have one
    headers_with_origin =
      if origin do
        [{"Origin", origin} | base_headers]
      else
        base_headers
      end

    # Add security headers if configured
    if security && Map.get(security, :include_security_headers, false) do
      headers_with_origin ++ Security.build_standard_security_headers()
    else
      headers_with_origin
    end
  end

  defp build_ssl_options(%{security: %{tls: tls_config}}) when is_map(tls_config) do
    ssl_opts = [
      ssl: [
        verify: Map.get(tls_config, :verify, :verify_peer),
        cacerts: Map.get(tls_config, :cacerts, :public_key.cacerts_get()),
        versions: Map.get(tls_config, :versions, [:"tlsv1.2", :"tlsv1.3"])
      ]
    ]

    # Add client cert if provided
    ssl_opts =
      case Map.get(tls_config, :cert) do
        nil -> ssl_opts
        cert -> put_in(ssl_opts, [:ssl, :cert], cert)
      end

    case Map.get(tls_config, :key) do
      nil -> ssl_opts
      key -> put_in(ssl_opts, [:ssl, :key], key)
    end
  end

  defp build_ssl_options(_state) do
    # Default SSL options
    [
      ssl: [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        versions: [:"tlsv1.2", :"tlsv1.3"]
      ]
    ]
  end

  defp validate_cors_response(
         response_headers,
         %{security: %{validate_origin: true, allowed_origins: allowed}} = state
       ) do
    origin_header = find_header(response_headers, "access-control-allow-origin")

    case origin_header do
      nil ->
        {:error, :missing_cors_header}

      "*" when allowed != :any ->
        {:error, :wildcard_origin_not_allowed}

      origin ->
        if origin == state.origin || origin in (allowed || []) do
          :ok
        else
          {:error, {:origin_not_allowed, origin}}
        end
    end
  end

  defp validate_cors_response(_headers, _state), do: :ok

  defp find_header(headers, name) do
    name_lower = String.downcase(name)

    Enum.find_value(headers, fn
      {key, value} when is_list(key) ->
        if String.downcase(to_string(key)) == name_lower do
          to_string(value)
        end

      {key, value} when is_binary(key) ->
        if String.downcase(key) == name_lower do
          value
        end

      _ ->
        nil
    end)
  end
end
