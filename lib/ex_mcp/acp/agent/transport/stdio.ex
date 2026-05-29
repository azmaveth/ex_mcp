defmodule ExMCP.ACP.Agent.Transport.Stdio do
  @moduledoc """
  Server-side stdio transport for ACP agents.

  This transport reads JSON-RPC lines from this process' stdin and writes
  JSON-RPC lines to stdout. Logs and diagnostics must go to stderr.
  """

  @behaviour ExMCP.ACP.Agent.Transport

  alias ExMCP.Internal.StdioLoggerConfig

  defstruct input: :stdio, output: :stdio, closed?: false

  @impl true
  def connect(opts) do
    StdioLoggerConfig.configure()

    {:ok,
     %__MODULE__{
       input: Keyword.get(opts, :input, :stdio),
       output: Keyword.get(opts, :output, :stdio)
     }}
  end

  @impl true
  def send_message(message, %__MODULE__{output: output} = state) when is_binary(message) do
    IO.puts(output, message)
    {:ok, state}
  end

  @impl true
  def receive_message(%__MODULE__{input: input} = state) do
    case IO.read(input, :line) do
      :eof ->
        {:error, :closed}

      {:error, reason} ->
        {:error, reason}

      line when is_binary(line) ->
        case String.trim(line) do
          "" -> receive_message(state)
          message -> {:ok, message, state}
        end
    end
  end

  @impl true
  def close(%__MODULE__{}), do: :ok

  @impl true
  def connected?(%__MODULE__{closed?: closed?}), do: not closed?
end
