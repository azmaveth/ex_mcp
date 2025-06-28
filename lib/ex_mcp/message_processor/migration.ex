defmodule ExMCP.MessageProcessor.Migration do
  @moduledoc """
  Migration module to transition MessageProcessor to use the unified dispatcher.

  This module provides a facade that maintains backward compatibility while
  internally using the new Dispatcher and Handlers modules. This allows for
  a gradual migration without breaking existing code.
  """

  alias ExMCP.MessageProcessor.Dispatcher
  alias ExMCP.Protocol.ErrorCodes

  @doc """
  Replaces the three dispatch maps with a single unified dispatcher call.

  This function can be used as a drop-in replacement for the existing
  dispatch map lookups in MessageProcessor.
  """
  def dispatch_request(conn, handler, mode, method, params, id, server_info \\ %{}) do
    # Create a temporary conn with the request data
    request = %{
      "method" => method,
      "params" => params,
      "id" => id
    }

    temp_conn = %{conn | request: request}

    # Use the unified dispatcher
    Dispatcher.dispatch(temp_conn, handler, mode, server_info)
  end

  @doc """
  Converts old dispatch map calls to use the unified dispatcher.

  ## Old pattern:
  ```
  case Map.get(handler_method_dispatch(), method) do
    nil -> handle_handler_custom_method(conn, server_pid, method, params, id)
    handler_fun -> handler_fun.(conn, server_pid, params, id)
  end
  ```

  ## New pattern:
  ```
  Migration.dispatch_request(conn, server_pid, :handler, method, params, id)
  ```
  """
  def migrate_dispatch_calls do
    [
      # Replace handler_method_dispatch usage
      {:handler_method_dispatch, :handler},

      # Replace server_method_dispatch usage
      {:server_method_dispatch, :genserver},

      # Replace handler_direct_dispatch usage
      {:handler_direct_dispatch, :direct}
    ]
  end

  @doc """
  Helper to create backward-compatible response format.
  """
  def success_response(result, id) do
    %{
      "jsonrpc" => "2.0",
      "result" => result,
      "id" => id
    }
  end

  @doc """
  Helper to create backward-compatible error response format.
  """
  def error_response(message, data, id) do
    %{
      "jsonrpc" => "2.0",
      "error" => %{
        "code" => ErrorCodes.internal_error(),
        "message" => message,
        "data" => data
      },
      "id" => id
    }
  end
end
