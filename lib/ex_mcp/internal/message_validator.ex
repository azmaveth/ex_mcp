defmodule ExMCP.Internal.MessageValidator do
  @moduledoc false

  # Provides comprehensive MCP message validation as required by the specification.
  #
  # This module implements the validation layer identified as missing in
  # SPEC_ALIGNMENT_PLAN.md. It validates:
  #
  # - Request ID validation (null IDs are rejected per spec)
  # - Request ID uniqueness tracking within sessions
  # - Response format validation (result XOR error requirement)
  # - JSON-RPC 2.0 compliance
  # - Method availability for protocol versions
  # - Parameter validation for specific methods
  #
  # The validator maintains session state to track request IDs and detect
  # duplicates as required by the MCP specification.

  @type validation_result :: {:ok, map()} | {:error, map()}
  @type session_state :: %{
          seen_request_ids: MapSet.t(String.t()),
          protocol_version: String.t() | nil
        }

  # JSON-RPC 2.0 error codes
  @invalid_request -32600
  @method_not_found -32601
  @invalid_params -32602
  @internal_error -32603

  @doc """
  Creates a new validation session state.
  """
  def new_session(protocol_version \\ nil) do
    %{
      seen_request_ids: MapSet.new(),
      protocol_version: protocol_version
    }
  end

  @doc """
  Validates a request message against MCP specification requirements.
  """
  @spec validate_request(map()) :: validation_result()
  def validate_request(request) when is_map(request) do
    with :ok <- validate_jsonrpc_version(request),
         :ok <- validate_request_structure(request),
         :ok <- validate_request_id(request),
         :ok <- validate_method_exists(request) do
      {:ok, request}
    else
      {:error, error_data} -> {:error, error_data}
    end
  end

  def validate_request(_), do: {:error, create_error(@invalid_request, "Invalid request format")}

  @doc """
  Validates a response message against MCP specification requirements.
  """
  @spec validate_response(map()) :: validation_result()
  def validate_response(response) when is_map(response) do
    with :ok <- validate_jsonrpc_version(response),
         :ok <- validate_response_structure(response),
         :ok <- validate_response_format(response) do
      {:ok, response}
    else
      {:error, error_data} -> {:error, error_data}
    end
  end

  def validate_response(_),
    do: {:error, create_error(@invalid_request, "Invalid response format")}

  @doc """
  Validates any MCP message (request, response, or notification) with session tracking.
  """
  @spec validate_message(map() | list(), session_state()) ::
          {validation_result(), session_state()}
  # Special case: Reject batch requests for protocol version 2025-06-18
  def validate_message(messages, %{protocol_version: "2025-06-18"} = state)
      when is_list(messages) do
    {{:error,
      create_error(
        @invalid_request,
        "Batch requests are not supported in protocol version 2025-06-18"
      )}, state}
  end

  def validate_message(messages, state) when is_list(messages) do
    # Handle batch requests
    if Enum.empty?(messages) do
      {{:error, create_error(@invalid_request, "Empty batch array is invalid")}, state}
    else
      # Validate each message in the batch
      {results, final_state} =
        Enum.reduce(messages, {[], state}, fn message, {acc_results, acc_state} ->
          {result, new_state} = validate_message(message, acc_state)
          {[result | acc_results], new_state}
        end)

      # Reverse to maintain original order
      results = Enum.reverse(results)

      # Check if any validation failed and extract validated messages
      case Enum.find(results, fn {status, _} -> status == :error end) do
        nil ->
          # All validations succeeded - extract the validated messages
          validated_messages = Enum.map(results, fn {:ok, msg} -> msg end)
          {{:ok, validated_messages}, final_state}

        {_, error} ->
          {{:error, error}, final_state}
      end
    end
  end

  def validate_message(message, state) when is_map(message) do
    cond do
      # Check if it's missing jsonrpc field entirely - invalid message structure
      not Map.has_key?(message, "jsonrpc") ->
        {{:error, create_error(@invalid_request, "Invalid message structure")}, state}

      # Request: has method and id
      Map.has_key?(message, "method") and Map.has_key?(message, "id") ->
        validate_request_with_state(message, state)

      # Notification: has method but no id
      Map.has_key?(message, "method") ->
        validate_notification_with_state(message, state)

      # Response: has result or error with id
      Map.has_key?(message, "id") and
          (Map.has_key?(message, "result") or Map.has_key?(message, "error")) ->
        validate_response_with_state(message, state)

      # Has id but no result/error - incomplete response
      Map.has_key?(message, "id") ->
        {{:error, create_error(@internal_error, "Response must contain either result or error")},
         state}

      true ->
        {{:error, create_error(@invalid_request, "Invalid message structure")}, state}
    end
  end

  def validate_message(_, state) do
    {{:error, create_error(@invalid_request, "Message must be a JSON object or array")}, state}
  end

  # Private validation functions

  defp validate_jsonrpc_version(%{"jsonrpc" => "2.0"}), do: :ok

  defp validate_jsonrpc_version(%{"jsonrpc" => version}) do
    {:error,
     create_error(@invalid_request, "Invalid JSON-RPC version: #{version}", %{
       received: version,
       expected: "2.0"
     })}
  end

  defp validate_jsonrpc_version(_) do
    {:error, create_error(@invalid_request, "Missing JSON-RPC version field")}
  end

  defp validate_request_structure(request) do
    required_fields = ["jsonrpc", "method", "id"]
    missing_fields = Enum.reject(required_fields, &Map.has_key?(request, &1))

    cond do
      missing_fields != [] ->
        {:error,
         create_error(@invalid_request, "Missing required fields", %{missing: missing_fields})}

      # Validate params field if present
      Map.has_key?(request, "params") and not is_map(Map.get(request, "params")) ->
        {:error, create_error(@invalid_request, "Parameters must be an object")}

      true ->
        :ok
    end
  end

  defp validate_response_structure(response) do
    required_fields = ["jsonrpc", "id"]
    missing_fields = Enum.reject(required_fields, &Map.has_key?(response, &1))

    if missing_fields == [] do
      :ok
    else
      {:error,
       create_error(@invalid_request, "Missing required fields", %{missing: missing_fields})}
    end
  end

  defp validate_request_id(%{"id" => nil}) do
    {:error, create_error(@invalid_request, "Request ID must not be null")}
  end

  defp validate_request_id(%{"id" => id}) when is_binary(id) or is_integer(id) do
    :ok
  end

  defp validate_request_id(%{"id" => id}) do
    {:error,
     create_error(@invalid_request, "Request ID must be string or integer", %{
       received_type: type_of(id)
     })}
  end

  defp validate_request_id(_) do
    {:error, create_error(@invalid_request, "Missing request ID")}
  end

  defp validate_method_exists(%{"method" => method})
       when is_binary(method) and byte_size(method) > 0 do
    :ok
  end

  defp validate_method_exists(%{"method" => method}) do
    {:error,
     create_error(@internal_error, "Method must be non-empty string", %{received: method})}
  end

  defp validate_method_exists(_) do
    {:error, create_error(@invalid_request, "Missing method field")}
  end

  defp validate_response_format(response) do
    has_result = Map.has_key?(response, "result")
    has_error = Map.has_key?(response, "error")

    cond do
      has_result and has_error ->
        {:error, create_error(@internal_error, "Response cannot contain both result and error")}

      not has_result and not has_error ->
        {:error, create_error(@internal_error, "Response must contain either result or error")}

      has_error ->
        # Validate error object structure
        validate_error_object(Map.get(response, "error"))

      true ->
        :ok
    end
  end

  defp validate_error_object(error) when is_map(error) do
    required_fields = ["code", "message"]
    missing_fields = Enum.reject(required_fields, &Map.has_key?(error, &1))

    cond do
      missing_fields != [] ->
        {:error,
         create_error(@internal_error, "Error object missing required fields", %{
           missing: missing_fields
         })}

      # Check if we have code field but it's invalid type - treat as missing field
      Map.has_key?(error, "code") and not is_integer(Map.get(error, "code")) ->
        {:error,
         create_error(@internal_error, "Error object missing required fields", %{
           missing: ["code"],
           note: "code must be integer"
         })}

      Map.has_key?(error, "message") and not is_binary(Map.get(error, "message")) ->
        {:error,
         create_error(@internal_error, "Error object missing required fields", %{
           missing: ["message"],
           note: "message must be string"
         })}

      true ->
        :ok
    end
  end

  defp validate_error_object(_) do
    {:error, create_error(@internal_error, "Error must be an object")}
  end

  defp validate_request_with_state(request, state) do
    case validate_request(request) do
      {:ok, validated_request} ->
        # Check for duplicate request ID
        request_id = Map.get(request, "id")

        if MapSet.member?(state.seen_request_ids, request_id) do
          error =
            create_error(@invalid_request, "Request ID has already been used in this session", %{
              duplicate_id: request_id
            })

          {{:error, error}, state}
        else
          new_state = %{state | seen_request_ids: MapSet.put(state.seen_request_ids, request_id)}
          {{:ok, validated_request}, new_state}
        end

      {:error, error} ->
        {{:error, error}, state}
    end
  end

  defp validate_notification_with_state(notification, state) do
    with :ok <- validate_jsonrpc_version(notification),
         :ok <- validate_method_exists(notification) do
      {{:ok, notification}, state}
    else
      {:error, error} -> {{:error, error}, state}
    end
  end

  defp validate_response_with_state(response, state) do
    case validate_response(response) do
      {:ok, validated_response} -> {{:ok, validated_response}, state}
      {:error, error} -> {{:error, error}, state}
    end
  end

  defp create_error(code, message, data \\ nil) do
    error = %{
      code: code,
      message: message
    }

    if data do
      Map.put(error, :data, data)
    else
      error
    end
  end

  defp type_of(value) when is_binary(value), do: "string"
  defp type_of(value) when is_integer(value), do: "integer"
  defp type_of(value) when is_float(value), do: "float"
  defp type_of(value) when is_boolean(value), do: "boolean"
  defp type_of(value) when is_list(value), do: "array"
  defp type_of(value) when is_map(value), do: "object"
  defp type_of(value) when is_atom(value), do: "atom"
  defp type_of(_), do: "unknown"

  @doc """
  Validates method availability for a given protocol version.
  """
  @spec validate_method_version(String.t(), String.t()) :: :ok | {:error, map()}
  def validate_method_version(method, version) do
    if ExMCP.Internal.Protocol.method_available?(method, version) do
      :ok
    else
      {:error,
       create_error(@method_not_found, "Method not available in protocol version", %{
         method: method,
         version: version
       })}
    end
  end

  @doc """
  Validates that required parameters are present for specific methods.
  """
  @spec validate_method_params(String.t(), map()) :: :ok | {:error, map()}
  def validate_method_params(method, params) when is_map(params) do
    case method do
      "tools/call" -> validate_tool_call_params(params)
      "resources/read" -> validate_resource_read_params(params)
      "prompts/get" -> validate_prompt_get_params(params)
      "resources/subscribe" -> validate_resource_subscribe_params(params)
      "resources/unsubscribe" -> validate_resource_unsubscribe_params(params)
      _ -> :ok
    end
  end

  def validate_method_params(_, _) do
    {:error, create_error(@invalid_params, "Parameters must be an object")}
  end

  defp validate_tool_call_params(params) do
    required = ["name"]
    missing = Enum.reject(required, &Map.has_key?(params, &1))

    if missing == [] do
      validate_tool_name(Map.get(params, "name"))
    else
      {:error, create_error(@invalid_params, "Missing required parameters", %{missing: missing})}
    end
  end

  defp validate_resource_read_params(params) do
    required = ["uri"]
    missing = Enum.reject(required, &Map.has_key?(params, &1))

    if missing == [] do
      validate_uri(Map.get(params, "uri"))
    else
      {:error, create_error(@invalid_params, "Missing required parameters", %{missing: missing})}
    end
  end

  defp validate_prompt_get_params(params) do
    required = ["name"]
    missing = Enum.reject(required, &Map.has_key?(params, &1))

    if missing == [] do
      validate_prompt_name(Map.get(params, "name"))
    else
      {:error, create_error(@invalid_params, "Missing required parameters", %{missing: missing})}
    end
  end

  defp validate_resource_subscribe_params(params) do
    required = ["uri"]
    missing = Enum.reject(required, &Map.has_key?(params, &1))

    if missing == [] do
      validate_uri(Map.get(params, "uri"))
    else
      {:error, create_error(@invalid_params, "Missing required parameters", %{missing: missing})}
    end
  end

  defp validate_resource_unsubscribe_params(params) do
    required = ["uri"]
    missing = Enum.reject(required, &Map.has_key?(params, &1))

    if missing == [] do
      validate_uri(Map.get(params, "uri"))
    else
      {:error, create_error(@invalid_params, "Missing required parameters", %{missing: missing})}
    end
  end

  defp validate_tool_name(name) when is_binary(name) and byte_size(name) > 0, do: :ok

  defp validate_tool_name(_) do
    {:error, create_error(@invalid_params, "Tool name must be non-empty string")}
  end

  defp validate_prompt_name(name) when is_binary(name) and byte_size(name) > 0, do: :ok

  defp validate_prompt_name(_) do
    {:error, create_error(@invalid_params, "Prompt name must be non-empty string")}
  end

  defp validate_uri(uri) when is_binary(uri) and byte_size(uri) > 0 do
    # Basic URI validation - could be enhanced
    if String.contains?(uri, "://") or String.starts_with?(uri, "/") do
      :ok
    else
      {:error, create_error(@invalid_params, "Invalid URI format")}
    end
  end

  defp validate_uri(_) do
    {:error, create_error(@invalid_params, "URI must be non-empty string")}
  end

  @doc """
  Validates JSON-RPC error codes according to the specification.

  Standard codes: -32768 to -32000 (reserved)
  Server-defined codes: -32099 to -32000 (custom application errors)
  """
  @spec validate_error_code(integer()) :: {:ok, integer()} | {:error, map()}
  def validate_error_code(code) when is_integer(code) do
    cond do
      # Standard JSON-RPC codes (always valid)
      code in [-32700, -32600, -32601, -32602, -32603] ->
        {:ok, code}

      # Server-defined application error codes
      code >= -32099 and code <= -32000 ->
        {:ok, code}

      # Other reserved codes (JSON-RPC spec reserves -32768 to -32000)
      code >= -32768 and code <= -32000 ->
        {:ok, code}

      # Invalid codes outside allowed ranges
      true ->
        {:error,
         create_error(@internal_error, "Invalid error code", %{
           code: code,
           valid_ranges: ["-32768 to -32000 (reserved)", "-32099 to -32000 (application)"]
         })}
    end
  end

  def validate_error_code(_) do
    {:error, create_error(@invalid_params, "Error code must be an integer")}
  end
end
