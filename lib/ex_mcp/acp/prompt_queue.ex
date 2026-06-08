defmodule ExMCP.ACP.PromptQueue do
  @moduledoc """
  Small functional queue core for ACP adapter prompt backlogs.
  """

  @opaque t(item) :: :queue.queue(item)

  @spec new() :: t(term())
  def new, do: :queue.new()

  @spec from_list([item]) :: t(item) when item: term()
  def from_list(items) when is_list(items), do: :queue.from_list(items)

  @spec empty?(t(term())) :: boolean()
  def empty?(queue), do: :queue.is_empty(queue)

  @spec len(t(term())) :: non_neg_integer()
  def len(queue), do: :queue.len(queue)

  @spec enqueue(t(item), item) :: t(item) when item: term()
  def enqueue(queue, item), do: :queue.in(item, queue)

  @spec pop(t(item)) :: {:value, item, t(item)} | :empty when item: term()
  def pop(queue) do
    case :queue.out(queue) do
      {{:value, item}, rest} -> {:value, item, rest}
      {:empty, _queue} -> :empty
    end
  end

  @spec split(t(item), (item -> as_boolean(term()))) :: {[item], t(item)} when item: term()
  def split(queue, predicate) when is_function(predicate, 1) do
    {matches, remaining} =
      queue
      |> to_list()
      |> Enum.split_with(predicate)

    {matches, from_list(remaining)}
  end

  @spec drain(t(item)) :: {[item], t(item)} when item: term()
  def drain(queue), do: {to_list(queue), new()}

  @spec to_list(t(item)) :: [item] when item: term()
  def to_list(queue), do: :queue.to_list(queue)
end
