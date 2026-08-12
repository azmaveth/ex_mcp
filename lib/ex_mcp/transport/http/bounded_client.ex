defmodule ExMCP.Transport.HTTP.BoundedClient do
  @moduledoc false

  alias ExMCP.Transport.HTTP.TargetPolicy

  @type response :: {
          {charlist(), pos_integer(), charlist()},
          [{String.t(), String.t()}],
          binary()
        }

  @spec request(atom(), String.t(), list(), binary(), binary(), keyword()) ::
          {:ok, response()} | {:error, term()}
  def request(method, url, headers, content_type, body, opts)
      when method in [:get, :post, :put, :patch, :delete] and is_binary(url) and
             is_binary(content_type) and is_binary(body) do
    with {:ok, uri, address} <- TargetPolicy.resolve(url, opts),
         :ok <- request_within_limit(body, opts),
         {:ok, conn} <- connect(uri, address, opts) do
      do_request(conn, method, uri, headers, content_type, body, opts)
    end
  rescue
    exception -> {:error, {:http_client_error, exception.__struct__}}
  catch
    kind, _reason -> {:error, {:http_client_error, kind}}
  end

  defp request_within_limit(body, opts) do
    if byte_size(body) <= Keyword.fetch!(opts, :max_request_bytes),
      do: :ok,
      else: {:error, :request_too_large}
  end

  defp connect(uri, address, opts) do
    scheme = String.to_existing_atom(uri.scheme)
    port = uri.port || default_port(scheme)
    pinned_address = address |> :inet.ntoa() |> to_string()

    transport_opts =
      opts
      |> Keyword.fetch!(:transport_opts)
      |> Keyword.put(:timeout, Keyword.fetch!(opts, :connect_timeout))
      |> Keyword.merge(address_family_options(address))

    Mint.HTTP1.connect(scheme, pinned_address, port,
      hostname: uri.host,
      mode: :passive,
      transport_opts: transport_opts,
      max_header_list_size: Keyword.get(opts, :max_header_bytes, 65_536),
      optional_responses: [:status_reason]
    )
  end

  defp do_request(conn, method, uri, headers, content_type, body, opts) do
    headers =
      headers
      |> normalize_headers()
      |> delete_header("host")
      |> put_header("content-type", content_type)
      |> put_header("accept-encoding", "identity")

    result =
      case Mint.HTTP1.request(conn, method_name(method), request_target(uri), headers, body) do
        {:ok, next_conn, request_ref} ->
          deadline = System.monotonic_time(:millisecond) + Keyword.fetch!(opts, :request_timeout)

          receive_response(
            next_conn,
            request_ref,
            empty_response(),
            deadline,
            Keyword.fetch!(opts, :max_response_bytes)
          )

        {:error, next_conn, reason} ->
          {{:error, {:http_request_failed, reason}}, next_conn}
      end

    {reply, final_conn} = result
    _ = Mint.HTTP1.close(final_conn)
    reply
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

      {:error, next_conn, reason, _events} ->
        {{:error, {:http_receive_failed, reason}}, next_conn}
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
        all_headers = acc.headers ++ headers

        case validate_headers(all_headers, max_bytes) do
          :ok -> {:cont, {:more, %{acc | headers: all_headers}}}
          {:error, reason} -> {:halt, {:error, reason}}
        end

      {:data, ^request_ref, data}, {:more, acc} ->
        size = acc.size + byte_size(data)

        if size > max_bytes do
          {:halt, {:error, :response_too_large}}
        else
          {:cont, {:more, %{acc | chunks: [data | acc.chunks], size: size}}}
        end

      {:done, ^request_ref}, {:more, acc} ->
        {:halt, {:done, acc}}

      _event, result ->
        {:cont, result}
    end)
  end

  defp format_response(response) do
    status_line = {~c"HTTP/1.1", response.status, to_charlist(response.reason || "")}
    body = response.chunks |> Enum.reverse() |> IO.iodata_to_binary()
    {status_line, response.headers, body}
  end

  defp empty_response,
    do: %{status: nil, reason: "", headers: [], chunks: [], size: 0}

  defp normalize_headers(headers) do
    Enum.map(headers, fn {name, value} ->
      {name |> to_string() |> String.downcase(), to_string(value)}
    end)
  end

  defp put_header(headers, name, value) do
    [{name, value} | Enum.reject(headers, fn {key, _value} -> key == name end)]
  end

  defp delete_header(headers, name), do: Enum.reject(headers, &(elem(&1, 0) == name))

  defp validate_headers(headers, max_bytes) do
    cond do
      compressed?(headers) -> {:error, :compressed_response}
      conflicting_framing?(headers) -> {:error, :invalid_response_framing}
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

  defp conflicting_framing?(headers) do
    Enum.any?(headers, &(elem(&1, 0) == "content-length")) and
      Enum.any?(headers, &(elem(&1, 0) == "transfer-encoding"))
  end

  defp compressed?(headers) do
    Enum.any?(headers, fn
      {"content-encoding", value} -> String.downcase(String.trim(value)) not in ["", "identity"]
      _header -> false
    end)
  end

  defp request_target(%URI{path: path, query: nil}), do: path_or_root(path)
  defp request_target(%URI{path: path, query: query}), do: path_or_root(path) <> "?" <> query
  defp path_or_root(path) when path in [nil, ""], do: "/"
  defp path_or_root(path), do: path

  defp method_name(method), do: method |> Atom.to_string() |> String.upcase()

  defp address_family_options(address) when tuple_size(address) == 8,
    do: [inet4: false, inet6: true]

  defp address_family_options(_address), do: [inet4: true, inet6: false]
  defp default_port(:http), do: 80
  defp default_port(:https), do: 443
end
