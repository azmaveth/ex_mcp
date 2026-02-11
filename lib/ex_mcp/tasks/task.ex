defmodule ExMCP.Tasks.Task do
  @moduledoc """
  Task struct and state machine validation for MCP Tasks (2025-11-25).

  Tasks represent async operations initiated by tool calls. This module
  provides a pure data structure and state transition validation functions.
  It does NOT include any GenServer or process management - users implement
  task lifecycle management themselves via handler callbacks.

  ## State Machine

  Valid states: `:working`, `:input_required`, `:completed`, `:failed`, `:cancelled`

  Valid transitions:
  - `:working` -> `:input_required` | `:completed` | `:failed` | `:cancelled`
  - `:input_required` -> `:working` | `:cancelled`
  - `:completed` -> (terminal state)
  - `:failed` -> (terminal state)
  - `:cancelled` -> (terminal state)

  ## Usage

      task = ExMCP.Tasks.Task.new("my-tool", %{"arg" => "value"})
      {:ok, task} = ExMCP.Tasks.Task.transition(task, :completed)
  """

  @type t :: %__MODULE__{
          id: String.t(),
          state: state(),
          status_message: String.t() | nil,
          tool_name: String.t(),
          arguments: map(),
          created_at: String.t(),
          last_updated_at: String.t() | nil,
          ttl: integer() | nil,
          poll_interval: integer() | nil,
          result: map() | nil,
          metadata: map()
        }

  @type state :: :working | :input_required | :completed | :failed | :cancelled

  @enforce_keys [:id, :state, :tool_name]
  defstruct [
    :id,
    :tool_name,
    :ttl,
    :poll_interval,
    :result,
    :status_message,
    :last_updated_at,
    state: :working,
    arguments: %{},
    created_at: nil,
    metadata: %{}
  ]

  @terminal_states [:completed, :failed, :cancelled]

  @valid_transitions %{
    working: [:input_required, :completed, :failed, :cancelled],
    input_required: [:working, :cancelled]
  }

  @doc """
  Creates a new task in the `:working` state.

  ## Parameters
  - `tool_name` - Name of the tool this task is executing
  - `arguments` - Tool arguments
  - `opts` - Optional fields: `:id`, `:ttl`, `:metadata`
  """
  @spec new(String.t(), map(), keyword()) :: t()
  def new(tool_name, arguments \\ %{}, opts \\ []) do
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    %__MODULE__{
      id: Keyword.get(opts, :id, generate_id()),
      state: :working,
      tool_name: tool_name,
      arguments: arguments,
      created_at: now,
      last_updated_at: now,
      ttl: Keyword.get(opts, :ttl),
      poll_interval: Keyword.get(opts, :poll_interval),
      status_message: Keyword.get(opts, :status_message),
      metadata: Keyword.get(opts, :metadata, %{})
    }
  end

  @doc """
  Attempts a state transition.

  Returns `{:ok, updated_task}` if the transition is valid,
  `{:error, reason}` if invalid.
  """
  @spec transition(t(), state()) :: {:ok, t()} | {:error, String.t()}
  def transition(%__MODULE__{state: current} = task, new_state) do
    if valid_transition?(current, new_state) do
      now = DateTime.utc_now() |> DateTime.to_iso8601()
      {:ok, %{task | state: new_state, last_updated_at: now}}
    else
      {:error, "Invalid transition from #{current} to #{new_state}"}
    end
  end

  @doc """
  Transitions and sets the result (for completed tasks).
  """
  @spec complete(t(), map()) :: {:ok, t()} | {:error, String.t()}
  def complete(%__MODULE__{} = task, result) do
    case transition(task, :completed) do
      {:ok, task} -> {:ok, %{task | result: result}}
      error -> error
    end
  end

  @doc """
  Transitions to failed state with error info.
  """
  @spec fail(t(), map()) :: {:ok, t()} | {:error, String.t()}
  def fail(%__MODULE__{} = task, error_result) do
    case transition(task, :failed) do
      {:ok, task} -> {:ok, %{task | result: error_result}}
      error -> error
    end
  end

  @doc """
  Checks if a transition from one state to another is valid.
  """
  @spec valid_transition?(state(), state()) :: boolean()
  def valid_transition?(from, to) do
    case Map.get(@valid_transitions, from) do
      nil -> false
      valid_targets -> to in valid_targets
    end
  end

  @doc """
  Checks if the task is in a terminal state.
  """
  @spec terminal?(t()) :: boolean()
  def terminal?(%__MODULE__{state: state}), do: state in @terminal_states

  @doc """
  Returns all valid states.
  """
  @spec states() :: [state()]
  def states, do: [:working, :input_required | @terminal_states]

  @doc """
  Returns all terminal states.
  """
  @spec terminal_states() :: [state()]
  def terminal_states, do: @terminal_states

  @doc """
  Converts a task to a map suitable for protocol serialization.
  """
  @spec to_map(t()) :: map()
  def to_map(%__MODULE__{} = task) do
    base = %{
      "taskId" => task.id,
      "status" => Atom.to_string(task.state),
      "toolName" => task.tool_name
    }

    base
    |> maybe_put("arguments", task.arguments, %{})
    |> maybe_put("createdAt", task.created_at)
    |> maybe_put("lastUpdatedAt", task.last_updated_at)
    |> maybe_put("ttl", task.ttl)
    |> maybe_put("pollInterval", task.poll_interval)
    |> maybe_put("statusMessage", task.status_message)
    |> maybe_put("result", task.result)
    |> maybe_put("metadata", task.metadata, %{})
  end

  @doc """
  Parses a state string to a state atom.
  """
  @spec parse_state(String.t()) :: {:ok, state()} | {:error, String.t()}
  def parse_state("working"), do: {:ok, :working}
  def parse_state("input_required"), do: {:ok, :input_required}
  def parse_state("completed"), do: {:ok, :completed}
  def parse_state("failed"), do: {:ok, :failed}
  def parse_state("cancelled"), do: {:ok, :cancelled}
  def parse_state(other), do: {:error, "Unknown task state: #{other}"}

  # Private helpers

  defp generate_id do
    "task_#{:erlang.unique_integer([:positive, :monotonic])}_#{:rand.uniform(999_999)}"
  end

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp maybe_put(map, _key, empty, empty), do: map
  defp maybe_put(map, key, value, _default), do: Map.put(map, key, value)
end
