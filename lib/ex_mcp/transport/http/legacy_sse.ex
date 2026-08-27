defmodule ExMCP.Transport.HTTP.LegacySSE do
  @moduledoc """
  Client transport for the deprecated MCP 2024-11-05 HTTP+SSE protocol.

  This is **not** Streamable HTTP. `transport: :http, use_sse: true` GETs the
  same MCP endpoint after `initialize`. This transport instead:

  1. GETs `{base}/sse` (or `:sse_path`) with `Accept: text/event-stream`
  2. Reads the first `endpoint` event (POST URI + `sessionId`)
  3. POSTs JSON-RPC to that advertised URI
  4. Receives responses and server requests on the GET SSE stream

  ## Options

  - `:url` - server origin or mount prefix, e.g. `"http://localhost:4000"`
  - `:sse_path` - GET path (default `"/sse"`)
  - `:post_path` - optional override of the advertised POST path; `sessionId`
    from the `endpoint` event is preserved
  - `:headers` - extra HTTP headers
  - `:timeout` / `:request_timeout` / `:stream_handshake_timeout`

  ## Example

      {:ok, client} =
        ExMCP.Client.start_link(
          transport: :sse,
          url: "http://localhost:4000"
        )
  """

  @behaviour ExMCP.Transport

  require Logger

  alias ExMCP.Internal.{DNSResolver, LogSummary, Options}
  alias ExMCP.Transport.HTTP.{BoundedClient, TargetPolicy}
  alias ExMCP.Transport.SSEClient

  defstruct [
    :base_url,
    :sse_url,
    :post_url,
    :session_id,
    :sse_pid,
    :headers,
    :timeouts,
    :max_response_bytes,
    :max_stream_buffer_bytes,
    :max_request_bytes,
    :dns_timeout_ms,
    :dns_resolver,
    :allowed_private_hosts
  ]

  @type t :: %__MODULE__{
          base_url: String.t(),
          sse_url: String.t(),
          post_url: String.t(),
          session_id: String.t(),
          sse_pid: pid(),
          headers: [{String.t(), String.t()}],
          timeouts: map(),
          max_response_bytes: pos_integer(),
          max_stream_buffer_bytes: pos_integer(),
          max_request_bytes: pos_integer(),
          dns_timeout_ms: pos_integer(),
          dns_resolver: module() | function(),
          allowed_private_hosts: [String.t()]
        }

  @default_sse_path "/sse"
  @default_max_response_bytes 8 * 1_024 * 1_024
  @default_max_stream_buffer_bytes 1 * 1_024 * 1_024
  @default_max_request_bytes 8 * 1_024 * 1_024

  @impl true
  def connect(opts) do
    raw_url = Keyword.fetch!(opts, :url)
    sse_path = normalize_path(path_opt(opts, :sse_path, :legacy_http_sse_path, @default_sse_path))
    post_path = optional_path(path_opt(opts, :post_path, :legacy_http_sse_post_path, nil))
    headers = Keyword.get(opts, :headers, [])

    timeouts = %{
      connect: Keyword.get(opts, :timeout, 5_000),
      request: Keyword.get(opts, :request_timeout, 30_000),
      stream_handshake: Keyword.get(opts, :stream_handshake_timeout, 15_000),
      stream_idle: Keyword.get(opts, :stream_idle_timeout, 60_000)
    }

    max_response_bytes =
      Options.positive_integer(opts, :max_response_bytes, @default_max_response_bytes)

    max_stream_buffer_bytes =
      Options.positive_integer(opts, :max_stream_buffer_bytes, @default_max_stream_buffer_bytes)

    max_request_bytes =
      Options.positive_integer(opts, :max_request_bytes, @default_max_request_bytes)

    dns_timeout_ms = Options.positive_integer(opts, :dns_timeout_ms, 1_000)
    dns_resolver = Keyword.get(opts, :dns_resolver, DNSResolver)
    allowed_private_hosts = Keyword.get(opts, :allowed_private_hosts, [])

    base_url = origin(raw_url)
    sse_url = join_url(raw_url, sse_path)

    network_opts = [
      dns_timeout_ms: dns_timeout_ms,
      dns_resolver: dns_resolver,
      allowed_private_hosts: allowed_private_hosts
    ]

    with :ok <- TargetPolicy.validate_options(network_opts),
         {:ok, sse_pid} <-
           start_sse(
             sse_url,
             headers,
             timeouts,
             max_response_bytes,
             max_stream_buffer_bytes,
             network_opts
           ),
         {:ok, advertised_url, session_id} <-
           await_endpoint(sse_pid, timeouts.stream_handshake, base_url) do
      post_url = maybe_override_post_path(advertised_url, post_path, base_url)

      Logger.debug("Legacy HTTP+SSE connected",
        endpoint_hash: LogSummary.fingerprint(sse_url),
        session_id_hash: LogSummary.fingerprint(session_id)
      )

      :telemetry.execute([:ex_mcp, :transport, :connection, :opened], %{}, %{
        transport: :sse,
        endpoint_hash: LogSummary.fingerprint(sse_url)
      })

      {:ok,
       %__MODULE__{
         base_url: base_url,
         sse_url: sse_url,
         post_url: post_url,
         session_id: session_id,
         sse_pid: sse_pid,
         headers: headers,
         timeouts: timeouts,
         max_response_bytes: max_response_bytes,
         max_stream_buffer_bytes: max_stream_buffer_bytes,
         max_request_bytes: max_request_bytes,
         dns_timeout_ms: dns_timeout_ms,
         dns_resolver: dns_resolver,
         allowed_private_hosts: allowed_private_hosts
       }}
    else
      {:error, reason} = error ->
        Logger.debug("Legacy HTTP+SSE connect failed", reason: LogSummary.describe(reason))
        error
    end
  end

  @impl true
  def send_message(message, %__MODULE__{} = state) do
    body = encode_body(message)

    headers =
      [
        {"content-type", "application/json"},
        {"accept", "application/json"}
        | state.headers
      ]
      |> Enum.uniq_by(fn {name, _} -> String.downcase(to_string(name)) end)

    result =
      BoundedClient.request(:post, state.post_url, headers, "application/json", body,
        connect_timeout: state.timeouts.connect,
        request_timeout: state.timeouts.request,
        max_request_bytes: state.max_request_bytes,
        max_response_bytes: state.max_response_bytes,
        transport_opts: [],
        dns_timeout_ms: state.dns_timeout_ms,
        dns_resolver: state.dns_resolver,
        allowed_private_hosts: state.allowed_private_hosts
      )

    case result do
      {:ok, {{_, status, _}, _headers, _body}} when status in [200, 202, 204] ->
        {:ok, state}

      {:ok, {{_, status, _}, _headers, body}} ->
        {:error, {:http_error, status, body}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @impl true
  def receive_message(%__MODULE__{} = state), do: receive_message(state, :infinity)

  @doc false
  def receive_message(%__MODULE__{sse_pid: sse_pid} = state, timeout) when is_pid(sse_pid) do
    send(sse_pid, {:change_parent, self()})

    deadline =
      case timeout do
        :infinity -> :infinity
        n when is_integer(n) and n >= 0 -> System.monotonic_time(:millisecond) + n
      end

    do_receive(state, deadline)
  end

  @impl true
  def close(%__MODULE__{sse_pid: sse_pid} = state) do
    :telemetry.execute([:ex_mcp, :transport, :connection, :closed], %{}, %{
      transport: :sse,
      session_id_hash: if(state.session_id, do: LogSummary.fingerprint(state.session_id))
    })

    stop_sse(sse_pid)
    :ok
  end

  @impl true
  def connected?(%__MODULE__{sse_pid: sse_pid}) when is_pid(sse_pid), do: Process.alive?(sse_pid)
  def connected?(_state), do: false

  defp start_sse(url, headers, timeouts, max_response_bytes, max_buffer_bytes, network_opts) do
    opts = [
      url: url,
      headers: headers,
      parent: self(),
      reconnect: false,
      connect_timeout: timeouts.connect,
      handshake_timeout: timeouts.stream_handshake,
      idle_timeout: timeouts.stream_idle,
      max_response_bytes: max_response_bytes,
      max_buffer_bytes: max_buffer_bytes,
      dns_timeout_ms: network_opts[:dns_timeout_ms],
      dns_resolver: network_opts[:dns_resolver],
      allowed_private_hosts: network_opts[:allowed_private_hosts]
    ]

    case SSEClient.start_link(opts) do
      {:ok, pid} -> {:ok, pid}
      {:error, reason} -> {:error, reason}
    end
  end

  defp await_endpoint(sse_pid, timeout, base_url) do
    deadline = System.monotonic_time(:millisecond) + timeout

    try do
      do_await_endpoint(sse_pid, deadline, base_url)
    catch
      :error, reason ->
        stop_sse(sse_pid)
        {:error, reason}
    else
      {:ok, _, _} = ok ->
        ok

      {:error, _reason} = error ->
        stop_sse(sse_pid)
        error
    end
  end

  defp do_await_endpoint(sse_pid, deadline, base_url) do
    receive do
      {:sse_connected, ^sse_pid} ->
        do_await_endpoint(sse_pid, deadline, base_url)

      {:sse_event, ^sse_pid, event} ->
        send(sse_pid, {:sse_event_ack, self()})

        case endpoint_event?(event) do
          true -> parse_endpoint_event(event.data, base_url)
          false -> do_await_endpoint(sse_pid, deadline, base_url)
        end

      {:sse_error, ^sse_pid, reason} ->
        {:error, {:sse_error, reason}}

      {:sse_closed, ^sse_pid} ->
        {:error, :connection_closed}

      {:sse_not_supported, ^sse_pid} ->
        {:error, :sse_not_supported}
    after
      remaining(deadline) ->
        {:error, :endpoint_timeout}
    end
  end

  defp do_receive(state, deadline) do
    receive do
      {:sse_event, pid, event} when pid == state.sse_pid ->
        send(pid, {:sse_event_ack, self()})
        handle_sse_event(event, state, deadline)

      {:sse_error, pid, reason} when pid == state.sse_pid ->
        {:error, {:sse_error, reason}}

      {:sse_closed, pid} when pid == state.sse_pid ->
        {:error, :closed}

      {:sse_not_supported, pid} when pid == state.sse_pid ->
        {:error, :sse_not_supported}
    after
      remaining(deadline) ->
        {:error, :timeout}
    end
  end

  defp handle_sse_event(event, state, deadline) do
    cond do
      endpoint_event?(event) ->
        do_receive(state, deadline)

      skip_event?(event) ->
        do_receive(state, deadline)

      true ->
        decode_message_event(event.data, state, deadline)
    end
  end

  defp decode_message_event(data, state, deadline) do
    case Jason.decode(data) do
      {:ok, %{"type" => "keep-alive"}} ->
        do_receive(state, deadline)

      {:ok, message} ->
        {:ok, message, state}

      {:error, reason} ->
        {:error, {:json_decode_error, reason}}
    end
  end

  defp endpoint_event?(%{type: type}) when type in ["endpoint"], do: true
  defp endpoint_event?(_event), do: false

  defp skip_event?(%{type: type}) when type in ["heartbeat", "connected"], do: true
  defp skip_event?(%{data: data}) when data in [nil, ""], do: true
  defp skip_event?(_event), do: false

  defp parse_endpoint_event(data, base_url) when is_binary(data) do
    data = String.trim(data)

    with {:ok, uri} <- parse_uri(data, base_url),
         {:ok, session_id} <- session_id_from_uri(uri) do
      {:ok, URI.to_string(uri), session_id}
    end
  end

  defp parse_endpoint_event(_data, _base_url), do: {:error, :invalid_endpoint_event}

  defp parse_uri(data, base_url) do
    uri = URI.parse(data)

    cond do
      uri.scheme in ["http", "https"] and is_binary(uri.host) and uri.host != "" ->
        {:ok, uri}

      String.starts_with?(data, "/") ->
        {:ok, URI.merge(base_url <> "/", String.trim_leading(data, "/"))}

      true ->
        {:error, :invalid_endpoint_event}
    end
  end

  defp session_id_from_uri(%URI{query: query}) when is_binary(query) do
    case Map.get(URI.decode_query(query), "sessionId") do
      session_id when is_binary(session_id) and session_id != "" -> {:ok, session_id}
      _missing -> {:error, :missing_session_id}
    end
  end

  defp session_id_from_uri(_uri), do: {:error, :missing_session_id}

  defp maybe_override_post_path(advertised_url, nil, _base_url), do: advertised_url

  defp maybe_override_post_path(advertised_url, post_path, base_url) do
    advertised = URI.parse(advertised_url)
    override = URI.parse(join_url(base_url, post_path))

    override
    |> Map.put(:query, advertised.query)
    |> URI.to_string()
  end

  defp encode_body(message) when is_binary(message), do: message
  defp encode_body(message), do: Jason.encode!(message)

  defp path_opt(opts, primary, alias_key, default) do
    Keyword.get(opts, primary, Keyword.get(opts, alias_key, default))
  end

  defp optional_path(nil), do: nil
  defp optional_path(path), do: normalize_path(path)

  defp normalize_path("/" <> _rest = path), do: path
  defp normalize_path(path) when is_binary(path), do: "/" <> path

  defp origin(url) do
    uri = URI.parse(url)
    "#{uri.scheme}://#{uri.host}#{port_suffix(uri)}"
  end

  defp port_suffix(%URI{scheme: "http", port: port}) when port in [80, nil], do: ""
  defp port_suffix(%URI{scheme: "https", port: port}) when port in [443, nil], do: ""
  defp port_suffix(%URI{port: nil}), do: ""
  defp port_suffix(%URI{port: port}), do: ":#{port}"

  defp join_url(base, path) do
    uri = URI.parse(base)
    prefix = uri.path || ""

    joined =
      case {String.trim_trailing(prefix, "/"), String.trim_leading(path, "/")} do
        {"", rest} -> "/" <> rest
        {mount, rest} -> mount <> "/" <> rest
      end

    URI.to_string(%{uri | path: joined, query: nil, fragment: nil})
  end

  defp remaining(:infinity), do: :infinity

  defp remaining(deadline) do
    max(deadline - System.monotonic_time(:millisecond), 0)
  end

  defp stop_sse(pid) when is_pid(pid) do
    GenServer.stop(pid, :normal, 1_000)
  catch
    :exit, _ -> :ok
  end

  defp stop_sse(_pid), do: :ok
end
