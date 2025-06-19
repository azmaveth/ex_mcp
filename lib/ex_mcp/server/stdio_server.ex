defmodule ExMCP.Server.StdioServer do
  @moduledoc """
  STDIO transport server for MCP protocol.

  This server reads from stdin and writes to stdout, making it suitable
  for command-line tools and scripting environments.
  """

  use GenServer
  require Logger

  @doc """
  Starts the STDIO server.

  ## Options

  * `:module` - The handler module implementing server callbacks
  * Other options are passed to GenServer.start_link
  """
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl GenServer
  def init(opts) do
    module = Keyword.fetch!(opts, :module)

    # Initialize the handler module state
    initial_state =
      case function_exported?(module, :init, 1) do
        true ->
          case module.init(opts) do
            {:ok, state} -> state
            _ -> %{}
          end

        false ->
          %{}
      end

    state = %{
      handler_module: module,
      handler_state: initial_state,
      request_id: 0
    }

    Logger.info("STDIO MCP server started with handler: #{module}")

    # Start reading from stdin in a separate process
    spawn_link(fn -> read_stdin_loop(self()) end)

    {:ok, state}
  end

  @impl GenServer
  def handle_info({:stdin_line, line}, state) do
    case Jason.decode(line) do
      {:ok, request} ->
        handle_request(request, state)

      {:error, error} ->
        Logger.error("Invalid JSON received: #{inspect(error)}")
        send_error_response(-32700, "Parse error", nil, state)
        {:noreply, state}
    end
  end

  def handle_info({:stdin_closed}, state) do
    Logger.info("STDIN closed, shutting down server")
    {:stop, :normal, state}
  end

  @impl GenServer
  def handle_call(:get_server_info, _from, state) do
    module = state.handler_module

    server_info =
      case function_exported?(module, :__server_info__, 0) do
        true -> module.__server_info__()
        false -> %{name: to_string(module), version: "1.0.0"}
      end

    {:reply, server_info, state}
  end

  def handle_call(request, from, state) do
    # Forward unknown calls to the handler module if it supports them
    module = state.handler_module

    case function_exported?(module, :handle_call, 3) do
      true ->
        case module.handle_call(request, from, state.handler_state) do
          {:reply, reply, new_handler_state} ->
            new_state = %{state | handler_state: new_handler_state}
            {:reply, reply, new_state}

          other ->
            other
        end

      false ->
        {:reply, {:error, {:unknown_call, request}}, state}
    end
  end

  # Handle incoming MCP requests
  defp handle_request(%{"method" => "initialize"} = request, state) do
    id = Map.get(request, "id")

    # Get capabilities from handler module
    capabilities =
      case function_exported?(state.handler_module, :get_capabilities, 0) do
        true -> state.handler_module.get_capabilities()
        false -> %{}
      end

    server_info =
      case function_exported?(state.handler_module, :__server_info__, 0) do
        true -> state.handler_module.__server_info__()
        false -> %{name: to_string(state.handler_module), version: "1.0.0"}
      end

    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{
        "protocolVersion" => "2024-11-05",
        "capabilities" => capabilities,
        "serverInfo" => server_info
      }
    }

    send_response(response, state)
    {:noreply, state}
  end

  defp handle_request(%{"method" => "tools/list"} = request, state) do
    id = Map.get(request, "id")

    tools =
      case function_exported?(state.handler_module, :get_tools, 0) do
        true ->
          state.handler_module.get_tools()
          |> Map.values()
          |> Enum.map(fn tool ->
            %{
              "name" => tool.name,
              "description" => tool.description,
              "inputSchema" => tool.input_schema
            }
          end)

        false ->
          []
      end

    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{"tools" => tools}
    }

    send_response(response, state)
    {:noreply, state}
  end

  defp handle_request(%{"method" => "tools/call", "params" => params} = request, state) do
    id = Map.get(request, "id")
    tool_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    case function_exported?(state.handler_module, :handle_tool_call, 3) do
      true ->
        case state.handler_module.handle_tool_call(tool_name, arguments, state.handler_state) do
          {:ok, result, new_handler_state} ->
            response = %{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => result
            }

            send_response(response, state)
            new_state = %{state | handler_state: new_handler_state}
            {:noreply, new_state}

          {:error, error, new_handler_state} ->
            send_error_response(-32000, "Tool error: #{inspect(error)}", id, state)
            new_state = %{state | handler_state: new_handler_state}
            {:noreply, new_state}
        end

      false ->
        send_error_response(-32601, "Method not found", id, state)
        {:noreply, state}
    end
  end

  defp handle_request(%{"method" => "resources/list"} = request, state) do
    id = Map.get(request, "id")

    resources =
      case function_exported?(state.handler_module, :get_resources, 0) do
        true ->
          state.handler_module.get_resources()
          |> Map.values()
          |> Enum.map(fn resource ->
            %{
              "uri" => resource.uri || "unknown",
              "name" => resource.name,
              "description" => resource.description,
              "mimeType" => resource.mime_type
            }
          end)

        false ->
          []
      end

    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{"resources" => resources}
    }

    send_response(response, state)
    {:noreply, state}
  end

  defp handle_request(%{"method" => method} = request, state) do
    id = Map.get(request, "id")

    # Try to handle unknown methods with the handler module
    case function_exported?(state.handler_module, :handle_request, 3) do
      true ->
        params = Map.get(request, "params", %{})

        case state.handler_module.handle_request(method, params, state.handler_state) do
          {:reply, result, new_handler_state} ->
            response = %{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => result
            }

            send_response(response, state)
            new_state = %{state | handler_state: new_handler_state}
            {:noreply, new_state}

          {:error, error, new_handler_state} ->
            send_error_response(-32000, "Request error: #{inspect(error)}", id, state)
            new_state = %{state | handler_state: new_handler_state}
            {:noreply, new_state}

          {:noreply, new_handler_state} ->
            new_state = %{state | handler_state: new_handler_state}
            {:noreply, new_state}
        end

      false ->
        send_error_response(-32601, "Method not found: #{method}", id, state)
        {:noreply, state}
    end
  end

  # Send a successful response
  defp send_response(response, _state) do
    json = Jason.encode!(response)
    IO.puts(json)
  end

  # Send an error response
  defp send_error_response(code, message, id, _state) do
    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "error" => %{
        "code" => code,
        "message" => message
      }
    }

    json = Jason.encode!(response)
    IO.puts(json)
  end

  # Read from stdin in a loop and send lines to the main process
  defp read_stdin_loop(server_pid) do
    case IO.read(:stdio, :line) do
      :eof ->
        send(server_pid, {:stdin_closed})

      {:error, reason} ->
        Logger.error("STDIN read error: #{inspect(reason)}")
        send(server_pid, {:stdin_closed})

      line when is_binary(line) ->
        line = String.trim(line)

        if line != "" do
          send(server_pid, {:stdin_line, line})
        end

        read_stdin_loop(server_pid)
    end
  end
end
