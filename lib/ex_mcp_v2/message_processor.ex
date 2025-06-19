defmodule ExMCP.MessageProcessor do
  @moduledoc """
  Core message processing abstraction for ExMCP v2.

  The MessageProcessor provides a simple, composable interface for processing MCP messages.
  It follows the Plug specification pattern used throughout the Elixir ecosystem.
  """

  @type t :: module()
  @type opts :: term()
  @type conn :: %__MODULE__.Conn{}

  defmodule Conn do
    @moduledoc """
    Connection struct representing an MCP message processing context.
    """

    defstruct [
      # The incoming MCP request
      :request,
      # The outgoing MCP response (if any)
      :response,
      # Processing state
      :state,
      # User-defined assigns
      :assigns,
      # Transport information
      :transport,
      # Session identifier
      :session_id,
      # Whether processing should stop
      :halted
    ]

    @type t :: %__MODULE__{
            request: map() | nil,
            response: map() | nil,
            state: term(),
            assigns: map(),
            transport: atom(),
            session_id: String.t() | nil,
            halted: boolean()
          }
  end

  @doc """
  Callback for initializing the plug with options.
  """
  @callback init(opts) :: opts

  @doc """
  Callback for processing the connection.
  """
  @callback call(conn, opts) :: conn

  @doc """
  Creates a new connection.
  """
  @spec new(map(), keyword()) :: Conn.t()
  def new(request, opts \\ []) do
    %Conn{
      request: request,
      response: nil,
      state: nil,
      assigns: %{},
      transport: Keyword.get(opts, :transport),
      session_id: Keyword.get(opts, :session_id),
      halted: false
    }
  end

  @doc """
  Assigns a value to the connection.
  """
  @spec assign(Conn.t(), atom(), term()) :: Conn.t()
  def assign(%Conn{} = conn, key, value) do
    %{conn | assigns: Map.put(conn.assigns, key, value)}
  end

  @doc """
  Halts the plug pipeline.
  """
  @spec halt(Conn.t()) :: Conn.t()
  def halt(%Conn{} = conn) do
    %{conn | halted: true}
  end

  @doc """
  Sets the response on the connection.
  """
  @spec put_response(Conn.t(), map()) :: Conn.t()
  def put_response(%Conn{} = conn, response) do
    %{conn | response: response}
  end

  @doc """
  Runs a list of plugs on the connection.
  """
  @spec run([{module(), opts}], Conn.t()) :: Conn.t()
  def run(plugs, %Conn{} = conn) do
    Enum.reduce_while(plugs, conn, fn {plug_module, opts}, acc ->
      if acc.halted do
        {:halt, acc}
      else
        result = plug_module.call(acc, plug_module.init(opts))
        {:cont, result}
      end
    end)
  end

  @doc """
  Process an MCP request using a handler module.

  This is a convenience function that creates a connection, processes it
  through a handler, and returns the response.
  """
  @spec process(Conn.t(), map()) :: Conn.t()
  def process(%Conn{} = conn, opts) do
    handler = Map.get(opts, :handler)
    server_info = Map.get(opts, :server_info, %{})

    case handler do
      nil ->
        error_response = %{
          "jsonrpc" => "2.0",
          "error" => %{
            "code" => -32603,
            "message" => "No handler configured"
          },
          "id" => get_request_id(conn.request)
        }

        put_response(conn, error_response)

      handler_module when is_atom(handler_module) ->
        process_with_handler(conn, handler_module, server_info)
    end
  end

  # Process request using ServerV2 handler
  defp process_with_handler(conn, handler_module, server_info) do
    request = conn.request
    method = Map.get(request, "method")
    params = Map.get(request, "params", %{})
    id = get_request_id(request)

    # Debug logging for tests
    if Mix.env() == :test do
      require Logger
      Logger.debug("Processing method: #{method} with handler: #{handler_module}")
    end

    case method do
      "initialize" ->
        response = %{
          "jsonrpc" => "2.0",
          "result" => %{
            "protocolVersion" => "2024-11-05",
            "capabilities" => handler_module.get_capabilities(),
            "serverInfo" => server_info
          },
          "id" => id
        }

        put_response(conn, response)

      "tools/list" ->
        tools = handler_module.get_tools() |> Map.values()

        response = %{
          "jsonrpc" => "2.0",
          "result" => %{"tools" => tools},
          "id" => id
        }

        put_response(conn, response)

      "tools/call" ->
        tool_name = Map.get(params, "name")
        arguments = Map.get(params, "arguments", %{})

        case handler_module.handle_tool_call(tool_name, arguments, %{}) do
          {:ok, result, _state} ->
            response = %{
              "jsonrpc" => "2.0",
              "result" => result,
              "id" => id
            }

            put_response(conn, response)

          {:error, reason, _state} ->
            error_response = %{
              "jsonrpc" => "2.0",
              "error" => %{
                "code" => -32603,
                "message" => "Tool execution failed",
                "data" => %{"reason" => inspect(reason)}
              },
              "id" => id
            }

            put_response(conn, error_response)
        end

      "resources/list" ->
        resources = handler_module.get_resources() |> Map.values()

        response = %{
          "jsonrpc" => "2.0",
          "result" => %{"resources" => resources},
          "id" => id
        }

        put_response(conn, response)

      "resources/read" ->
        uri = Map.get(params, "uri")

        case handler_module.handle_resource_read(uri, uri, %{}) do
          {:ok, content, _state} ->
            response = %{
              "jsonrpc" => "2.0",
              "result" => %{"contents" => content},
              "id" => id
            }

            put_response(conn, response)

          {:error, reason, _state} ->
            error_response = %{
              "jsonrpc" => "2.0",
              "error" => %{
                "code" => -32603,
                "message" => "Resource read failed",
                "data" => %{"reason" => inspect(reason)}
              },
              "id" => id
            }

            put_response(conn, error_response)
        end

      "prompts/list" ->
        prompts = handler_module.get_prompts() |> Map.values()

        response = %{
          "jsonrpc" => "2.0",
          "result" => %{"prompts" => prompts},
          "id" => id
        }

        put_response(conn, response)

      "prompts/get" ->
        prompt_name = Map.get(params, "name")
        arguments = Map.get(params, "arguments", %{})

        case handler_module.handle_prompt_get(prompt_name, arguments, %{}) do
          {:ok, result, _state} ->
            response = %{
              "jsonrpc" => "2.0",
              "result" => result,
              "id" => id
            }

            put_response(conn, response)

          {:error, reason, _state} ->
            error_response = %{
              "jsonrpc" => "2.0",
              "error" => %{
                "code" => -32603,
                "message" => "Prompt get failed",
                "data" => %{"reason" => inspect(reason)}
              },
              "id" => id
            }

            put_response(conn, error_response)
        end

      _ ->
        # Try custom handler if it exists and module is loaded
        if Code.ensure_loaded?(handler_module) and
             function_exported?(handler_module, :handle_request, 3) do
          case handler_module.handle_request(method, params, %{}) do
            {:reply, result, _state} ->
              response = %{
                "jsonrpc" => "2.0",
                "result" => result,
                "id" => id
              }

              put_response(conn, response)

            {:error, reason, _state} ->
              error_response = %{
                "jsonrpc" => "2.0",
                "error" => %{
                  "code" => -32603,
                  "message" => "Request failed",
                  "data" => %{"reason" => inspect(reason)}
                },
                "id" => id
              }

              put_response(conn, error_response)

            {:noreply, _state} ->
              # No response for this request
              conn

            _ ->
              # Unknown method
              error_response = %{
                "jsonrpc" => "2.0",
                "error" => %{
                  "code" => -32601,
                  "message" => "Method not found"
                },
                "id" => id
              }

              put_response(conn, error_response)
          end
        else
          # Unknown method and no custom handler
          error_response = %{
            "jsonrpc" => "2.0",
            "error" => %{
              "code" => -32601,
              "message" => "Method not found"
            },
            "id" => id
          }

          put_response(conn, error_response)
        end
    end
  end

  defp get_request_id(request) when is_map(request) do
    Map.get(request, "id")
  end

  defp get_request_id(_), do: nil
end
