defmodule ExMCP.Content.SchemaHTTPClient do
  @moduledoc false

  @type response :: %{
          required(:status) => pos_integer(),
          required(:headers) => [{String.t(), String.t()}],
          required(:body) => binary()
        }

  @spec get(URI.t(), :inet.ip_address(), keyword()) ::
          {:ok, response()} | {:error, :fetch_failed | :response_too_large}
  def get(%URI{} = uri, address, opts) do
    scheme = String.to_existing_atom(uri.scheme)
    port = uri.port || default_port(scheme)
    timeout = opts[:request_timeout_ms]
    max_bytes = opts[:max_response_bytes]

    connect_opts = [
      hostname: uri.host,
      mode: :passive,
      protocols: [:http1],
      transport_opts: transport_opts(address, opts)
    ]

    pinned_address = address |> :inet.ntoa() |> to_string()

    case Mint.HTTP.connect(scheme, pinned_address, port, connect_opts) do
      {:ok, conn} -> request_and_receive(conn, uri, timeout, max_bytes)
      {:error, _reason} -> {:error, :fetch_failed}
    end
  rescue
    _exception -> {:error, :fetch_failed}
  catch
    _kind, _reason -> {:error, :fetch_failed}
  end

  defp request_and_receive(conn, uri, timeout, max_bytes) do
    headers = [
      {"accept", "application/schema+json, application/json"},
      {"accept-encoding", "identity"},
      {"user-agent", "ex_mcp-json-schema-resolver"}
    ]

    result =
      case Mint.HTTP.request(conn, "GET", request_target(uri), headers, nil) do
        {:ok, next_conn, request_ref} ->
          deadline = System.monotonic_time(:millisecond) + timeout
          receive_response(next_conn, request_ref, empty_response(), deadline, max_bytes)

        {:error, _conn, _reason} ->
          {:error, :fetch_failed}
      end

    _ = Mint.HTTP.close(conn)
    result
  end

  defp receive_response(conn, request_ref, response, deadline, max_bytes) do
    timeout = max(deadline - System.monotonic_time(:millisecond), 0)

    case Mint.HTTP.recv(conn, 0, timeout) do
      {:ok, next_conn, events} ->
        case consume_events(events, request_ref, response, max_bytes) do
          {:done, completed} ->
            {:ok, completed}

          {:more, updated} ->
            receive_response(next_conn, request_ref, updated, deadline, max_bytes)

          {:error, reason} ->
            {:error, reason}
        end

      {:error, _next_conn, _reason, _events} ->
        {:error, :fetch_failed}
    end
  end

  defp consume_events(events, request_ref, response, max_bytes) do
    Enum.reduce_while(events, {:more, response}, fn
      {:status, ^request_ref, status}, {:more, acc} ->
        {:cont, {:more, %{acc | status: status}}}

      {:headers, ^request_ref, headers}, {:more, acc} ->
        headers = normalize_headers(headers)

        if content_length_too_large?(headers, max_bytes) do
          {:halt, {:error, :response_too_large}}
        else
          {:cont, {:more, %{acc | headers: acc.headers ++ headers}}}
        end

      {:data, ^request_ref, data}, {:more, acc} ->
        size = acc.size + byte_size(data)

        if size > max_bytes do
          {:halt, {:error, :response_too_large}}
        else
          {:cont, {:more, %{acc | chunks: [data | acc.chunks], size: size}}}
        end

      {:done, ^request_ref}, {:more, acc} ->
        completed = %{
          status: acc.status,
          headers: acc.headers,
          body: IO.iodata_to_binary(Enum.reverse(acc.chunks))
        }

        {:halt, {:done, completed}}

      _event, state ->
        {:cont, state}
    end)
  end

  defp empty_response, do: %{status: nil, headers: [], chunks: [], size: 0}

  defp normalize_headers(headers) do
    Enum.map(headers, fn {name, value} ->
      {String.downcase(to_string(name)), to_string(value)}
    end)
  end

  defp content_length_too_large?(headers, max_bytes) do
    Enum.any?(headers, fn
      {"content-length", value} ->
        case Integer.parse(value) do
          {length, ""} -> length > max_bytes
          _other -> true
        end

      _header ->
        false
    end)
  end

  defp transport_opts(address, opts) do
    family_opts =
      if tuple_size(address) == 8,
        do: [inet4: false, inet6: true],
        else: [inet4: true, inet6: false]

    [{:timeout, opts[:connect_timeout_ms]} | family_opts]
  end

  defp request_target(%URI{path: path, query: nil}), do: path_or_root(path)
  defp request_target(%URI{path: path, query: query}), do: path_or_root(path) <> "?" <> query

  defp path_or_root(path) when path in [nil, ""], do: "/"
  defp path_or_root(path), do: path

  defp default_port(:http), do: 80
  defp default_port(:https), do: 443
end
