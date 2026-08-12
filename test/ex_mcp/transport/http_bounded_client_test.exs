defmodule ExMCP.Transport.HTTP.BoundedClientTest do
  use ExUnit.Case, async: false

  alias ExMCP.Transport.HTTP.BoundedClient

  test "accepts a response exactly at the limit" do
    bypass = Bypass.open()

    Bypass.expect_once(bypass, "POST", "/mcp", fn conn ->
      Plug.Conn.resp(conn, 200, "1234")
    end)

    assert {:ok, {{_, 200, _}, _headers, "1234"}} =
             request(bypass, max_response_bytes: 4)
  end

  test "rejects a response one byte over the limit" do
    bypass = Bypass.open()

    Bypass.expect_once(bypass, "POST", "/mcp", fn conn ->
      Plug.Conn.resp(conn, 200, "12345")
    end)

    assert {:error, :response_too_large} = request(bypass, max_response_bytes: 4)
  end

  test "rejects an oversized request before connecting" do
    bypass = Bypass.open()

    assert {:error, :request_too_large} =
             request(bypass, body: "12345", max_request_bytes: 4)
  end

  test "rejects duplicate content-length framing" do
    {port, server} =
      raw_server([
        "HTTP/1.1 200 OK\r\n",
        "Content-Length: 4\r\n",
        "Content-Length: 4\r\n",
        "Connection: close\r\n\r\n",
        "1234"
      ])

    assert {:error, _reason} = request_port(port)
    assert :ok = Task.await(server)
  end

  test "rejects compressed response bodies" do
    {port, server} =
      raw_server([
        "HTTP/1.1 200 OK\r\n",
        "Content-Length: 4\r\n",
        "Content-Encoding: gzip\r\n",
        "Connection: close\r\n\r\n",
        "1234"
      ])

    assert {:error, :compressed_response} = request_port(port)
    assert :ok = Task.await(server)
  end

  test "overwrites a caller-supplied Host header with the validated URI authority" do
    {port, server} = raw_server("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n", self())

    assert {:ok, {{_, 200, _}, _headers, ""}} =
             request_port(port, [{"Host", "attacker.invalid"}])

    assert_receive {:raw_request, request}
    assert request =~ "\r\nhost: 127.0.0.1:#{port}\r\n"
    refute request =~ "attacker.invalid"
    assert :ok = Task.await(server)
  end

  defp request(bypass, overrides) do
    body = Keyword.get(overrides, :body, "{}")

    opts =
      [
        connect_timeout: 1_000,
        request_timeout: 1_000,
        max_request_bytes: 1_024,
        max_response_bytes: 1_024,
        transport_opts: []
      ]
      |> Keyword.merge(Keyword.drop(overrides, [:body]))

    BoundedClient.request(
      :post,
      "http://127.0.0.1:#{bypass.port}/mcp",
      [],
      "application/json",
      body,
      opts
    )
  end

  defp request_port(port, headers \\ []) do
    BoundedClient.request(
      :post,
      "http://127.0.0.1:#{port}/mcp",
      headers,
      "application/json",
      "{}",
      connect_timeout: 1_000,
      request_timeout: 1_000,
      max_request_bytes: 1_024,
      max_response_bytes: 1_024,
      transport_opts: []
    )
  end

  defp raw_server(response, owner \\ nil) do
    {:ok, listener} =
      :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true, ip: {127, 0, 0, 1}])

    {:ok, port} = :inet.port(listener)

    server =
      Task.async(fn ->
        {:ok, socket} = :gen_tcp.accept(listener, 1_000)
        {:ok, request} = recv_headers(socket, "")
        if owner, do: send(owner, {:raw_request, request})
        :ok = :gen_tcp.send(socket, response)
        :gen_tcp.close(socket)
        :gen_tcp.close(listener)
        :ok
      end)

    {port, server}
  end

  defp recv_headers(socket, acc) do
    if String.contains?(acc, "\r\n\r\n") do
      {:ok, acc}
    else
      case :gen_tcp.recv(socket, 0, 1_000) do
        {:ok, data} -> recv_headers(socket, acc <> data)
        error -> error
      end
    end
  end
end
