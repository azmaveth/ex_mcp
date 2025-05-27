defmodule ExMCP.Test.HTTPServer do
  @moduledoc """
  Simple HTTP server for testing SSE and other HTTP-based transports.
  """
  use GenServer
  require Logger

  defmodule State do
    @moduledoc false
    defstruct [:port, :ref, :requests, :sse_clients]
  end

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  def stop(server) do
    if Process.alive?(server) do
      GenServer.stop(server)
    else
      :ok
    end
  end

  def get_url(server) do
    GenServer.call(server, :get_url)
  end

  def get_requests(server) do
    GenServer.call(server, :get_requests)
  end

  def get_last_request(server) do
    GenServer.call(server, :get_last_request)
  end

  def clear_requests(server) do
    GenServer.call(server, :clear_requests)
  end

  # Server callbacks

  @impl true
  def init(opts) do
    port = opts[:port] || 0
    ref = make_ref()

    # Start the HTTP server
    dispatch =
      :cowboy_router.compile([
        {:_,
         [
           {"/mcp/v1/sse", ExMCP.Test.HTTPServer.SSEHandler, [server: self()]},
           {"/mcp/v1/messages", ExMCP.Test.HTTPServer.MessageHandler, [server: self()]},
           {:_, ExMCP.Test.HTTPServer.NotFoundHandler, []}
         ]}
      ])

    {:ok, _} =
      :cowboy.start_clear(
        ref,
        [{:port, port}],
        %{env: %{dispatch: dispatch}}
      )

    # Get actual port
    actual_port = :ranch.get_port(ref)

    {:ok,
     %State{
       port: actual_port,
       ref: ref,
       requests: [],
       sse_clients: []
     }}
  end

  @impl true
  def terminate(_reason, state) do
    :cowboy.stop_listener(state.ref)
    :ok
  end

  @impl true
  def handle_call(:get_url, _from, state) do
    {:reply, "http://localhost:#{state.port}", state}
  end

  @impl true
  def handle_call(:get_requests, _from, state) do
    {:reply, Enum.reverse(state.requests), state}
  end

  @impl true
  def handle_call(:get_last_request, _from, state) do
    {:reply, List.first(state.requests), state}
  end

  @impl true
  def handle_call(:clear_requests, _from, state) do
    {:reply, :ok, %{state | requests: []}}
  end

  @impl true
  def handle_call({:add_request, request}, _from, state) do
    {:reply, :ok, %{state | requests: [request | state.requests]}}
  end

  @impl true
  def handle_call({:add_sse_client, client_id}, _from, state) do
    {:reply, :ok, %{state | sse_clients: [client_id | state.sse_clients]}}
  end
end

defmodule ExMCP.Test.HTTPServer.MessageHandler do
  @moduledoc false
  require Logger

  def init(req, opts) do
    try do
      server = Keyword.get(opts, :server)

      # Read body
      {:ok, body, req2} = :cowboy_req.read_body(req)

      # Parse JSON body
      case Jason.decode(body) do
        {:ok, json} ->
          # Extract headers
          headers = :cowboy_req.headers(req2)
          auth = headers["authorization"]
          origin = headers["origin"]

          # Store request
          request = %{
            path: :cowboy_req.path(req2),
            method: :cowboy_req.method(req2),
            headers: headers,
            body: json,
            auth: auth,
            origin: origin,
            timestamp: System.system_time(:millisecond)
          }

          GenServer.call(server, {:add_request, request})

          # Send response
          response = %{
            "jsonrpc" => "2.0",
            "id" => Map.get(json, "id"),
            "result" => %{
              "content" => [
                %{"type" => "text", "text" => "Test response"}
              ]
            }
          }

          resp_body = Jason.encode!(response)

          req3 =
            :cowboy_req.reply(
              200,
              %{
                "content-type" => "application/json"
              },
              resp_body,
              req2
            )

          {:ok, req3, opts}

        {:error, reason} ->
          Logger.error("Failed to parse JSON: #{inspect(reason)}, body: #{inspect(body)}")

          req3 =
            :cowboy_req.reply(
              400,
              %{
                "content-type" => "application/json"
              },
              Jason.encode!(%{
                "jsonrpc" => "2.0",
                "error" => %{
                  "code" => -32700,
                  "message" => "Parse error"
                }
              }),
              req2
            )

          {:ok, req3, opts}
      end
    rescue
      e ->
        Logger.error("Error in MessageHandler: #{inspect(e)}")
        Logger.error(Exception.format(:error, e, __STACKTRACE__))

        req3 =
          :cowboy_req.reply(
            500,
            %{
              "content-type" => "text/plain"
            },
            "Internal server error",
            req
          )

        {:ok, req3, opts}
    end
  end
end

defmodule ExMCP.Test.HTTPServer.SSEHandler do
  @moduledoc false

  def init(req, opts) do
    server = Keyword.get(opts, :server)

    # Generate client ID
    client_id = :crypto.strong_rand_bytes(16) |> Base.encode16()

    # Store SSE connection
    headers = :cowboy_req.headers(req)

    request = %{
      type: :sse_connect,
      client_id: client_id,
      headers: headers,
      timestamp: System.system_time(:millisecond)
    }

    GenServer.call(server, {:add_request, request})
    GenServer.call(server, {:add_sse_client, client_id})

    # Send SSE headers
    req2 =
      :cowboy_req.stream_reply(
        200,
        %{
          "content-type" => "text/event-stream",
          "cache-control" => "no-cache",
          "connection" => "keep-alive",
          "access-control-allow-origin" => "*"
        },
        req
      )

    # Send initial event
    :cowboy_req.stream_body(
      ~s(event: connected\ndata: {"clientId":"#{client_id}"}\n\n),
      :nofin,
      req2
    )

    # Keep connection alive
    sse_loop(req2, client_id)
  end

  defp sse_loop(req, client_id) do
    receive do
      {:send_event, event} ->
        :cowboy_req.stream_body(event, :nofin, req)
        sse_loop(req, client_id)

      :close ->
        :cowboy_req.stream_body("", :fin, req)
        {:ok, req, []}
    after
      30_000 ->
        # Send ping
        :cowboy_req.stream_body(":ping\n\n", :nofin, req)
        sse_loop(req, client_id)
    end
  end
end

defmodule ExMCP.Test.HTTPServer.NotFoundHandler do
  @moduledoc false

  def init(req, opts) do
    req2 =
      :cowboy_req.reply(
        404,
        %{
          "content-type" => "text/plain"
        },
        "Not found",
        req
      )

    {:ok, req2, opts}
  end
end
