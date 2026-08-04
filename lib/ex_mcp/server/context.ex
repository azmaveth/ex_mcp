defmodule ExMCP.Server.Context do
  @moduledoc """
  Access to the validated context of the currently executing server callback.

  This is primarily useful for MRTR-aware handlers that retain their existing
  callback arity. The value is scoped to the callback invocation and must not
  be read later from a spawned process.
  """

  alias ExMCP.Server.RequestContext

  @key {__MODULE__, :current}

  @spec current() :: RequestContext.t() | nil
  def current, do: Process.get(@key)

  @spec input_responses() :: map() | nil
  def input_responses do
    case current() do
      %RequestContext{input_responses: responses} -> responses
      nil -> nil
    end
  end

  @spec request_state() :: term()
  def request_state do
    case current() do
      %RequestContext{request_state: state} -> state
      nil -> nil
    end
  end

  @doc false
  def with_context(%RequestContext{} = context, fun) when is_function(fun, 0) do
    previous = Process.put(@key, context)

    try do
      fun.()
    after
      restore(previous)
    end
  end

  defp restore(nil), do: Process.delete(@key)
  defp restore(previous), do: Process.put(@key, previous)
end
