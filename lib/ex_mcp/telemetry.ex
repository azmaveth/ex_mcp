defmodule ExMCP.Telemetry do
  @moduledoc """
  Telemetry integration for ExMCP.

  This module defines telemetry events emitted by ExMCP components and provides
  convenience functions for attaching handlers.

  ## Events

  ### Request Processing

  * `[:ex_mcp, :request, :start]` - Emitted when request processing starts
    * Measurements: `%{system_time: integer()}`
    * Metadata: `%{request_id: String.t(), method: String.t(), server: atom()}`

  * `[:ex_mcp, :request, :stop]` - Emitted when request processing completes
    * Measurements: `%{duration: integer()}`
    * Metadata: `%{request_id: String.t(), method: String.t(), server: atom(), status: :ok | :error}`

  * `[:ex_mcp, :request, :exception]` - Emitted when request processing fails
    * Measurements: `%{duration: integer()}`
    * Metadata: `%{request_id: String.t(), method: String.t(), server: atom(), kind: atom(), error: term(), stacktrace: list()}`

  ### Tool Execution

  * `[:ex_mcp, :tool, :start]` - Emitted when tool execution starts
    * Measurements: `%{system_time: integer()}`
    * Metadata: `%{tool_name: String.t(), request_id: String.t()}`

  * `[:ex_mcp, :tool, :stop]` - Emitted when tool execution completes
    * Measurements: `%{duration: integer()}`
    * Metadata: `%{tool_name: String.t(), request_id: String.t(), status: :ok | :error}`

  ### Resource Operations

  * `[:ex_mcp, :resource, :read, :start]` - Emitted when resource read starts
    * Measurements: `%{system_time: integer()}`
    * Metadata: `%{uri: String.t(), request_id: String.t()}`

  * `[:ex_mcp, :resource, :read, :stop]` - Emitted when resource read completes
    * Measurements: `%{duration: integer(), bytes: integer() | nil}`
    * Metadata: `%{uri: String.t(), request_id: String.t(), status: :ok | :error}`

  ### Connection Events

  * `[:ex_mcp, :connection, :established]` - Emitted when connection is established
    * Measurements: `%{system_time: integer()}`
    * Metadata: `%{transport: atom(), server: atom()}`

  * `[:ex_mcp, :connection, :lost]` - Emitted when connection is lost
    * Measurements: `%{system_time: integer(), uptime: integer()}`
    * Metadata: `%{transport: atom(), server: atom(), reason: term()}`

  ## Usage

      # Attach a simple logger
      ExMCP.Telemetry.attach_default_logger()
      
      # Attach custom handlers
      :telemetry.attach(
        "my-handler",
        [:ex_mcp, :request, :stop],
        &MyApp.handle_event/4,
        nil
      )
  """

  require Logger

  @doc """
  Attaches a default logger that logs all ExMCP events.

  This is useful for debugging and development.
  """
  def attach_default_logger do
    events = [
      [:ex_mcp, :request, :start],
      [:ex_mcp, :request, :stop],
      [:ex_mcp, :request, :exception],
      [:ex_mcp, :tool, :start],
      [:ex_mcp, :tool, :stop],
      [:ex_mcp, :resource, :read, :start],
      [:ex_mcp, :resource, :read, :stop],
      [:ex_mcp, :connection, :established],
      [:ex_mcp, :connection, :lost]
    ]

    :telemetry.attach_many(
      "ex-mcp-default-logger",
      events,
      &handle_event/4,
      nil
    )
  end

  @doc """
  Detaches the default logger.
  """
  def detach_default_logger do
    :telemetry.detach("ex-mcp-default-logger")
  end

  # Default event handler that logs events
  defp handle_event([:ex_mcp, :request, :start], _measurements, metadata, _config) do
    Logger.debug("Starting request #{metadata.request_id} - #{metadata.method}")
  end

  defp handle_event([:ex_mcp, :request, :stop], measurements, metadata, _config) do
    duration_ms = System.convert_time_unit(measurements.duration, :native, :millisecond)

    Logger.info(
      "Request #{metadata.request_id} completed in #{duration_ms}ms - status: #{metadata.status}"
    )
  end

  defp handle_event([:ex_mcp, :request, :exception], measurements, metadata, _config) do
    duration_ms = System.convert_time_unit(measurements.duration, :native, :millisecond)

    Logger.error(
      "Request #{metadata.request_id} failed after #{duration_ms}ms - #{inspect(metadata.error)}"
    )
  end

  defp handle_event([:ex_mcp, :tool | _], _measurements, metadata, _config) do
    Logger.debug("Tool event: #{inspect(metadata)}")
  end

  defp handle_event([:ex_mcp, :resource | _], _measurements, metadata, _config) do
    Logger.debug("Resource event: #{inspect(metadata)}")
  end

  defp handle_event([:ex_mcp, :connection, :established], _measurements, metadata, _config) do
    Logger.info(
      "Connection established - transport: #{metadata.transport}, server: #{metadata.server}"
    )
  end

  defp handle_event([:ex_mcp, :connection, :lost], measurements, metadata, _config) do
    uptime_s = System.convert_time_unit(measurements.uptime, :native, :second)

    Logger.warning(
      "Connection lost after #{uptime_s}s - transport: #{metadata.transport}, reason: #{inspect(metadata.reason)}"
    )
  end

  defp handle_event(_event, _measurements, _metadata, _config) do
    :ok
  end

  @doc """
  Executes a function and emits telemetry events.

  This is a convenience function for wrapping operations with telemetry.

  ## Examples

      ExMCP.Telemetry.span([:ex_mcp, :custom, :operation], %{id: "123"}, fn ->
        # Do some work
        {:ok, result}
      end)
  """
  def span(event_prefix, metadata, fun)
      when is_list(event_prefix) and is_map(metadata) and is_function(fun, 0) do
    start_time = System.monotonic_time()
    start_metadata = Map.put(metadata, :system_time, System.system_time())

    :telemetry.execute(event_prefix ++ [:start], %{system_time: start_time}, start_metadata)

    try do
      result = fun.()
      duration = System.monotonic_time() - start_time

      status =
        case result do
          {:ok, _, _} ->
            :ok

          {:error, _, _} ->
            :error

          {:ok, _} ->
            :ok

          {:error, _} ->
            :error

          {:response, response, _} ->
            # Check if the response contains an error
            if ExMCP.Protocol.ResponseBuilder.error_response?(response), do: :error, else: :ok

          _ ->
            :ok
        end

      stop_metadata = Map.put(metadata, :status, status)
      :telemetry.execute(event_prefix ++ [:stop], %{duration: duration}, stop_metadata)

      result
    rescue
      error ->
        duration = System.monotonic_time() - start_time

        exception_metadata =
          metadata
          |> Map.put(:kind, :error)
          |> Map.put(:error, error)
          |> Map.put(:stacktrace, __STACKTRACE__)

        :telemetry.execute(
          event_prefix ++ [:exception],
          %{duration: duration},
          exception_metadata
        )

        reraise error, __STACKTRACE__
    end
  end
end
