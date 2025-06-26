defmodule ExMCP.Transport.Stdio do
  @moduledoc """
  This module implements the standard MCP specification.

  stdio transport implementation for MCP.

  This transport communicates with MCP servers over standard input/output,
  typically by spawning a subprocess. This is one of the two official MCP
  transports defined in the specification.

  ## Options

  - `:command` - Command and arguments to spawn (required)
  - `:cd` - Working directory for the process
  - `:env` - Environment variables as a list of {"KEY", "VALUE"} tuples

  ## Example

      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: ["node", "my-mcp-server.js"],
        cd: "/path/to/server",
        env: [{"NODE_ENV", "production"}]
      )
  """

  @behaviour ExMCP.Transport

  require Logger

  alias ExMCP.Internal.SecurityConfig
  alias ExMCP.Transport.{Error, SecurityGuard}

  defstruct [:port, :buffer, :line_buffer]

  @impl true
  def connect(opts) do
    command = Keyword.fetch!(opts, :command)

    port_opts = [
      :binary,
      :exit_status,
      :use_stdio,
      :hide,
      :stream,
      line: 1_000_000,
      args: tl(command)
    ]

    port_opts =
      case Keyword.get(opts, :cd) do
        nil -> port_opts
        dir -> [{:cd, to_charlist(dir)} | port_opts]
      end

    port_opts =
      case Keyword.get(opts, :env) do
        nil -> port_opts
        env -> [{:env, format_env(env)} | port_opts]
      end

    executable = hd(command)

    # Try to find the executable in common locations if it's not a full path
    executable_path =
      if Path.type(executable) == :absolute do
        executable
      else
        case System.find_executable(executable) do
          nil ->
            # Try common locations for node/npm/npx on macOS
            common_paths = [
              "/opt/homebrew/bin/#{executable}",
              "/usr/local/bin/#{executable}",
              "/usr/bin/#{executable}",
              "#{System.get_env("HOME")}/.nvm/versions/node/#{System.get_env("NODE_VERSION", "*")}/bin/#{executable}"
            ]

            Enum.find(common_paths, executable, &File.exists?/1)

          path ->
            path
        end
      end

    try do
      port = Port.open({:spawn_executable, to_charlist(executable_path)}, port_opts)

      state = %__MODULE__{
        port: port,
        buffer: "",
        line_buffer: ""
      }

      {:ok, state}
    catch
      :error, reason ->
        Error.connection_error({:spawn_failed, reason})
    end
  end

  @impl true
  def send_message(message, %__MODULE__{port: port} = state) do
    # Check if message contains external resource requests that need security validation
    case validate_stdio_message(message, state) do
      {:ok, validated_message} ->
        # MCP uses newline-delimited JSON
        data = validated_message <> "\n"

        try do
          Port.command(port, data)
          {:ok, state}
        catch
          :error, reason ->
            Error.transport_error({:send_failed, reason})
        end

      {:error, security_error} ->
        Logger.warning("Stdio message blocked by security policy",
          error: security_error
        )

        Error.security_violation(security_error)
    end
  end

  defp validate_stdio_message(message, state) do
    # Step 1: Validate that message does not contain embedded newlines
    # MCP specification: "Messages are delimited by newlines, and MUST NOT contain embedded newlines"
    case validate_no_embedded_newlines(message) do
      :ok ->
        # Step 2: Validate that message is valid JSON
        case validate_json_format(message) do
          {:ok, parsed_message} ->
            # Step 3: Validate JSON-RPC 2.0 structure
            case validate_jsonrpc_structure(parsed_message) do
              :ok ->
                # Step 4: Check for external resource requests that need security validation
                validate_security_requirements(parsed_message, message, state)

              {:error, validation_error} ->
                Error.validation_error({:invalid_jsonrpc, validation_error})
            end

          {:error, json_error} ->
            Error.validation_error({:invalid_json, json_error})
        end

      {:error, newline_error} ->
        Error.validation_error({:embedded_newline, newline_error})
    end
  end

  defp validate_no_embedded_newlines(message) do
    if String.contains?(message, "\n") do
      {:error,
       "Message contains embedded newlines which violate MCP stdio transport requirements"}
    else
      :ok
    end
  end

  defp validate_json_format(message) do
    case Jason.decode(message) do
      {:ok, parsed} ->
        {:ok, parsed}

      {:error, error} ->
        {:error, "Invalid JSON format: #{inspect(error)}"}
    end
  end

  defp validate_jsonrpc_structure(parsed_message) when is_map(parsed_message) do
    # Validate JSON-RPC 2.0 structure according to specification
    cond do
      not Map.has_key?(parsed_message, "jsonrpc") ->
        {:error, "Missing required 'jsonrpc' field"}

      parsed_message["jsonrpc"] != "2.0" ->
        {:error, "Invalid jsonrpc version, must be '2.0'"}

      # For requests, must have method and optionally id
      Map.has_key?(parsed_message, "method") ->
        validate_jsonrpc_request(parsed_message)

      # For responses, must have id and either result or error
      Map.has_key?(parsed_message, "id") ->
        validate_jsonrpc_response(parsed_message)

      true ->
        {:error, "Invalid JSON-RPC structure: must be request, response, or notification"}
    end
  end

  defp validate_jsonrpc_structure(parsed_message) when is_list(parsed_message) do
    # JSON-RPC batch request - validate each item
    if parsed_message == [] do
      {:error, "Empty batch requests are not allowed"}
    else
      Enum.reduce_while(parsed_message, :ok, fn item, _acc ->
        case validate_jsonrpc_structure(item) do
          :ok -> {:cont, :ok}
          {:error, error} -> {:halt, {:error, "Batch item invalid: #{error}"}}
        end
      end)
    end
  end

  defp validate_jsonrpc_structure(_parsed_message) do
    {:error, "JSON-RPC message must be an object or array"}
  end

  defp validate_jsonrpc_request(request) do
    cond do
      not is_binary(request["method"]) ->
        {:error, "Method must be a string"}

      String.starts_with?(request["method"], "rpc.") ->
        {:error, "Methods starting with 'rpc.' are reserved"}

      Map.has_key?(request, "id") and is_nil(request["id"]) ->
        {:error, "Request id cannot be null"}

      true ->
        :ok
    end
  end

  defp validate_jsonrpc_response(response) do
    has_result = Map.has_key?(response, "result")
    has_error = Map.has_key?(response, "error")

    cond do
      has_result and has_error ->
        {:error, "Response cannot have both result and error"}

      not has_result and not has_error ->
        {:error, "Response must have either result or error"}

      true ->
        :ok
    end
  end

  defp validate_security_requirements(parsed_message, original_message, state) do
    # Check for external resource requests that need security validation
    case parsed_message do
      %{"method" => "resources/read", "params" => %{"uri" => uri}} ->
        validate_resource_access(uri, original_message, state)

      %{"method" => "resources/list", "params" => %{"uri" => uri}} when is_binary(uri) ->
        validate_resource_access(uri, original_message, state)

      _ ->
        # Non-resource request, allow through
        {:ok, original_message}
    end
  end

  defp validate_resource_access(uri, message, state) do
    # Only validate if URI appears to be external (has scheme and host)
    case URI.parse(uri) do
      %URI{scheme: scheme, host: host} when not is_nil(scheme) and not is_nil(host) ->
        # This is an external resource, validate with SecurityGuard
        security_request = %{
          url: uri,
          headers: [],
          method: "GET",
          transport: :stdio,
          user_id: extract_stdio_user_id(state)
        }

        config = SecurityConfig.get_transport_config(:stdio)

        case SecurityGuard.validate_request(security_request, config) do
          {:ok, _sanitized_request} ->
            {:ok, message}

          {:error, security_error} ->
            {:error, security_error}
        end

      _ ->
        # Local/relative URI, allow through
        {:ok, message}
    end
  end

  defp extract_stdio_user_id(_state) do
    # Use system user as default for stdio transport
    System.get_env("USER") || System.get_env("USERNAME") || "stdio_user"
  end

  @impl true
  def receive_message(%__MODULE__{port: port} = state) do
    # Transfer port ownership to this process if needed
    if Port.info(port, :connected) != {:connected, self()} do
      Port.connect(port, self())
    end

    receive_loop(state)
  end

  @impl true
  def close(%__MODULE__{port: port}) do
    Port.close(port)
    :ok
  end

  @impl true
  def connected?(%__MODULE__{port: port}) do
    Port.info(port) != nil
  end

  # V2 compatibility methods
  def send(state, message) do
    send_message(message, state)
  end

  def recv(state, timeout \\ 5_000) do
    task = Task.async(fn -> receive_message(state) end)

    case Task.yield(task, timeout) || Task.shutdown(task) do
      {:ok, {:ok, message, new_state}} ->
        {:ok, message, new_state}

      {:ok, {:error, reason}} ->
        Error.normalize_error({:error, reason})

      nil ->
        Error.timeout_error(:receive_timeout)
    end
  end

  # Testing support - expose process_data for unit tests
  @doc false
  def process_data(data, state), do: do_process_data(data, state)

  # Private functions

  defp receive_loop(state) do
    receive do
      {port, {:data, data}} when port == state.port ->
        do_process_data(data, state)

      {port, {:exit_status, status}} when port == state.port ->
        Error.connection_error({:process_exited, status})

      {port, :eof} when port == state.port ->
        Error.connection_error(:eof)
    end
  end

  defp do_process_data(data, state) do
    # Handle both binary and :eol tuple format from port
    binary_data =
      case data do
        {:eol, line} -> line <> "\n"
        binary when is_binary(binary) -> binary
        _ -> ""
      end

    # Accumulate data until we have a complete line
    new_buffer = state.line_buffer <> binary_data

    case String.split(new_buffer, "\n", parts: 2) do
      [line, rest] ->
        # We have a complete line
        trimmed = String.trim(line)

        cond do
          trimmed == "" ->
            # Empty line, continue
            receive_loop(%{state | line_buffer: rest})

          # Skip non-JSON output like "Secure MCP Filesystem Server..."
          not String.starts_with?(trimmed, "{") and not String.starts_with?(trimmed, "[") ->
            Logger.debug("Skipping non-JSON output: #{inspect(trimmed)}")
            receive_loop(%{state | line_buffer: rest})

          true ->
            # Return the JSON line and update state
            {:ok, trimmed, %{state | line_buffer: rest}}
        end

      [partial] ->
        # No complete line yet, keep buffering
        receive_loop(%{state | line_buffer: partial})
    end
  end

  defp format_env(env) do
    Enum.map(env, fn {key, value} ->
      {to_charlist(key), to_charlist(value)}
    end)
  end
end
