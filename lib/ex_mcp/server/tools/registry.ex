defmodule ExMCP.Server.Tools.Registry do
  @moduledoc """
  Runtime tool registry that manages tool definitions and handlers.

  This module provides a simpler alternative to compile-time metaprogramming
  by using runtime registration of tools.
  """

  use GenServer

  @type tool_definition :: %{
          optional(atom()) => any(),
          name: String.t(),
          description: String.t(),
          inputSchema: map(),
          outputSchema: map() | nil
        }

  @type handler :: (map(), any() -> {:ok, any()} | {:ok, any(), any()} | {:error, any()})

  # Client API

  @doc """
  Starts the tool registry.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, %{}, name: name)
  end

  @doc """
  Registers a tool with its handler.
  """
  @spec register_tool(GenServer.server(), tool_definition(), handler()) :: :ok
  def register_tool(registry \\ __MODULE__, tool_def, handler) do
    GenServer.call(registry, {:register_tool, tool_def, handler})
  end

  @doc """
  Registers multiple tools at once.
  """
  @spec register_tools(GenServer.server(), [{tool_definition(), handler()}]) :: :ok
  def register_tools(registry \\ __MODULE__, tools) do
    GenServer.call(registry, {:register_tools, tools})
  end

  @doc """
  Lists all registered tools.
  """
  @spec list_tools(GenServer.server()) :: [tool_definition()]
  def list_tools(registry \\ __MODULE__) do
    GenServer.call(registry, :list_tools)
  end

  @doc """
  Calls a tool by name.
  """
  @spec call_tool(GenServer.server(), String.t(), map(), any()) ::
          {:ok, any()} | {:ok, any(), any()} | {:error, any()}
  def call_tool(registry \\ __MODULE__, tool_name, arguments, state) do
    GenServer.call(registry, {:call_tool, tool_name, arguments, state})
  end

  @doc """
  Gets a tool's definition and handler.
  """
  @spec get_tool(GenServer.server(), String.t()) ::
          {:ok, {tool_definition(), handler()}} | {:error, :not_found}
  def get_tool(registry \\ __MODULE__, tool_name) do
    GenServer.call(registry, {:get_tool, tool_name})
  end

  # Server callbacks

  @impl GenServer
  def init(_opts) do
    {:ok, %{tools: %{}, compiled_schemas: %{}}}
  end

  @impl GenServer
  def handle_call({:register_tool, tool_def, handler}, _from, state) do
    name = tool_def.name

    # Compile output schema if present
    compiled_schema = compile_output_schema(tool_def[:outputSchema])

    new_tools = Map.put(state.tools, name, {tool_def, handler})

    new_schemas =
      if compiled_schema do
        Map.put(state.compiled_schemas, name, compiled_schema)
      else
        state.compiled_schemas
      end

    {:reply, :ok, %{state | tools: new_tools, compiled_schemas: new_schemas}}
  end

  def handle_call({:register_tools, tools}, _from, state) do
    new_state =
      Enum.reduce(tools, state, fn {tool_def, handler}, acc ->
        name = tool_def.name
        compiled_schema = compile_output_schema(tool_def[:outputSchema])

        new_tools = Map.put(acc.tools, name, {tool_def, handler})

        new_schemas =
          if compiled_schema do
            Map.put(acc.compiled_schemas, name, compiled_schema)
          else
            acc.compiled_schemas
          end

        %{acc | tools: new_tools, compiled_schemas: new_schemas}
      end)

    {:reply, :ok, new_state}
  end

  def handle_call(:list_tools, _from, state) do
    tools =
      state.tools
      |> Map.values()
      |> Enum.map(fn {tool_def, _handler} -> tool_def end)

    {:reply, tools, state}
  end

  def handle_call({:call_tool, tool_name, arguments, call_state}, _from, state) do
    case Map.get(state.tools, tool_name) do
      {_tool_def, handler} ->
        result = handler.(arguments, call_state)
        validated_result = validate_output(result, state.compiled_schemas[tool_name], call_state)
        {:reply, validated_result, state}

      nil ->
        {:reply, {:error, "Unknown tool: #{tool_name}"}, state}
    end
  end

  def handle_call({:get_tool, tool_name}, _from, state) do
    case Map.get(state.tools, tool_name) do
      nil -> {:reply, {:error, :not_found}, state}
      tool_info -> {:reply, {:ok, tool_info}, state}
    end
  end

  # Private helpers

  defp compile_output_schema(nil), do: nil

  defp compile_output_schema(schema) do
    if Code.ensure_loaded?(ExJsonSchema) do
      try do
        ExJsonSchema.Schema.resolve(schema)
      rescue
        _ -> nil
      end
    else
      nil
    end
  end

  defp validate_output({:ok, output}, nil, state), do: {:ok, output, state}
  defp validate_output({:ok, output, new_state}, nil, _state), do: {:ok, output, new_state}

  defp validate_output({:ok, output}, schema, state) when not is_nil(schema) do
    case validate_with_schema(output, schema) do
      :ok -> {:ok, output, state}
      {:error, errors} -> {:error, "Output validation failed: #{inspect(errors)}"}
    end
  end

  defp validate_output({:ok, output, new_state}, schema, _state) when not is_nil(schema) do
    case validate_with_schema(output, schema) do
      :ok -> {:ok, output, new_state}
      {:error, errors} -> {:error, "Output validation failed: #{inspect(errors)}"}
    end
  end

  defp validate_output(other, _schema, _state), do: other

  defp validate_with_schema(data, schema) do
    if Code.ensure_loaded?(ExJsonSchema) do
      case ExJsonSchema.Validator.validate(schema, data) do
        :ok -> :ok
        {:error, errors} -> {:error, errors}
      end
    else
      :ok
    end
  end
end
