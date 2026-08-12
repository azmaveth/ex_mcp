defmodule ExMCP.Authorization.PinnedHTTPClient do
  @moduledoc false

  @type httpc_response ::
          {{charlist(), non_neg_integer(), charlist()}, [{String.t(), String.t()}], binary()}

  @spec request(atom(), URI.t(), :inet.ip_address(), tuple(), keyword(), keyword()) ::
          {:ok, httpc_response()} | {:error, term()}
  def request(method, %URI{} = uri, address, request, http_options, opts)
      when method in [:get, :post, :put, :delete] and is_list(http_options) and is_list(opts) do
    with {:ok, headers, body} <- request_parts(request),
         {:ok, conn} <- connect(uri, address, http_options, opts) do
      request_and_receive(conn, method, uri, headers, body, opts)
    end
  rescue
    _exception -> {:error, :request_failed}
  catch
    _kind, _reason -> {:error, :request_failed}
  end

  defp connect(uri, address, http_options, opts) do
    scheme = String.to_existing_atom(uri.scheme)
    port = uri.port || default_port(scheme)
    pinned_address = address |> :inet.ntoa() |> to_string()

    transport_opts =
      http_options
      |> Keyword.get(:ssl, [])
      |> Keyword.put(:timeout, Keyword.fetch!(opts, :connect_timeout_ms))
      |> Keyword.put(:send_timeout, Keyword.fetch!(opts, :request_timeout_ms))
      |> Keyword.put(:send_timeout_close, true)
      |> Keyword.merge(address_family_options(address))

    Mint.HTTP1.connect(scheme, pinned_address, port,
      hostname: uri.host,
      mode: :passive,
      transport_opts: transport_opts,
      max_header_list_size: Keyword.fetch!(opts, :max_header_bytes),
      optional_responses: [:status_reason]
    )
  end

  defp request_and_receive(conn, method, uri, headers, body, opts) do
    deadline = System.monotonic_time(:millisecond) + Keyword.fetch!(opts, :request_timeout_ms)

    case Mint.HTTP1.request(conn, method_name(method), request_target(uri), headers, body) do
      {:ok, next_conn, request_ref} ->
        {reply, _final_conn} =
          receive_response(
            next_conn,
            request_ref,
            empty_response(),
            deadline,
            Keyword.fetch!(opts, :max_response_bytes)
          )

        reply

      {:error, _next_conn, _reason} ->
        {:error, :request_failed}
    end
  after
    # Mint connections are immutable, but every state returned for this
    # single request owns the same socket. Closing the initial state therefore
    # also covers parser failures and exceptions before a final state exists.
    _ = Mint.HTTP1.close(conn)
  end

  defp receive_response(conn, request_ref, response, deadline, max_bytes) do
    timeout = max(deadline - System.monotonic_time(:millisecond), 0)

    case Mint.HTTP1.recv(conn, 0, timeout) do
      {:ok, next_conn, events} ->
        case consume_events(events, request_ref, response, max_bytes) do
          {:done, completed} ->
            {{:ok, format_response(completed)}, next_conn}

          {:more, updated} ->
            receive_response(next_conn, request_ref, updated, deadline, max_bytes)

          {:error, reason} ->
            {{:error, reason}, next_conn}
        end

      {:error, next_conn, _reason, _events} ->
        {{:error, :request_failed}, next_conn}
    end
  end

  defp consume_events(events, request_ref, response, max_bytes) do
    Enum.reduce_while(events, {:more, response}, fn
      {:status, ^request_ref, status}, {:more, acc} ->
        {:cont, {:more, %{acc | status: status}}}

      {:status_reason, ^request_ref, reason}, {:more, acc} ->
        {:cont, {:more, %{acc | reason: reason}}}

      {:headers, ^request_ref, headers}, {:more, acc} ->
        headers = normalize_headers(headers)

        case validate_response_headers(headers, max_bytes) do
          :ok -> {:cont, {:more, %{acc | headers: acc.headers ++ headers}}}
          {:error, reason} -> {:halt, {:error, reason}}
        end

      {:data, ^request_ref, data}, {:more, acc} ->
        size = acc.size + byte_size(data)

        if size > max_bytes do
          {:halt, {:error, :response_too_large}}
        else
          {:cont, {:more, %{acc | chunks: [data | acc.chunks], size: size}}}
        end

      {:done, ^request_ref}, {:more, %{status: status} = acc} when is_integer(status) ->
        {:halt, {:done, acc}}

      {:done, ^request_ref}, {:more, _acc} ->
        {:halt, {:error, :invalid_response}}

      _event, state ->
        {:cont, state}
    end)
  end

  defp validate_response_headers(headers, max_bytes) do
    cond do
      compressed?(headers) -> {:error, :compressed_response}
      invalid_or_oversized_content_length?(headers, max_bytes) -> {:error, :response_too_large}
      true -> :ok
    end
  end

  defp invalid_or_oversized_content_length?(headers, max_bytes) do
    values = for {"content-length", value} <- headers, do: String.trim(value)

    case values do
      [] ->
        false

      [value] ->
        case Integer.parse(value) do
          {length, ""} when length >= 0 -> length > max_bytes
          _invalid -> true
        end

      _multiple ->
        true
    end
  end

  defp compressed?(headers) do
    Enum.any?(headers, fn
      {"content-encoding", value} -> String.downcase(String.trim(value)) not in ["", "identity"]
      _header -> false
    end)
  end

  defp format_response(response) do
    status_line = {~c"HTTP/1.1", response.status, String.to_charlist(response.reason || "")}
    body = response.chunks |> Enum.reverse() |> IO.iodata_to_binary()
    {status_line, response.headers, body}
  end

  defp empty_response,
    do: %{status: nil, reason: "", headers: [], chunks: [], size: 0}

  defp request_parts({_url, headers}) do
    {:ok, normalize_headers(headers), nil}
  end

  defp request_parts({_url, headers, content_type, body}) do
    headers =
      headers
      |> normalize_headers()
      |> put_header_if_missing("content-type", to_string(content_type))

    {:ok, headers, IO.iodata_to_binary(body)}
  rescue
    _exception -> {:error, :invalid_request}
  end

  defp request_parts(_request), do: {:error, :invalid_request}

  defp normalize_headers(headers) do
    Enum.map(headers, fn {name, value} ->
      {name |> to_string() |> String.downcase(), to_string(value)}
    end)
  end

  defp put_header_if_missing(headers, name, value) do
    if Enum.any?(headers, fn {key, _value} -> key == name end),
      do: headers,
      else: [{name, value} | headers]
  end

  defp request_target(%URI{path: path, query: nil}), do: path_or_root(path)
  defp request_target(%URI{path: path, query: query}), do: path_or_root(path) <> "?" <> query
  defp path_or_root(path) when path in [nil, ""], do: "/"
  defp path_or_root(path), do: path

  defp address_family_options(address) when tuple_size(address) == 8,
    do: [inet4: false, inet6: true]

  defp address_family_options(_address), do: [inet4: true, inet6: false]

  defp method_name(method), do: method |> Atom.to_string() |> String.upcase()
  defp default_port(:http), do: 80
  defp default_port(:https), do: 443
end
