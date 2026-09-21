defmodule ExMCP.ACP.Adapters.Pi.PromptFlow do
  @moduledoc false

  # Pure prompt scheduling for the Pi ACP adapter: what happens to the active
  # prompt and the queue behind it. Nothing here writes to the Port — the
  # planners hand the root adapter an NDJSON payload and the updated state,
  # and the root decides how to deliver it.

  alias ExMCP.ACP.Adapters.Pi.RPC
  alias ExMCP.ACP.{AdapterEvents, Envelope, PromptQueue}
  alias ExMCP.Internal.Maps

  @typedoc """
  The subset of the adapter struct the prompt-flow helpers read and update.
  """
  @type state :: %{
          required(:session_id) => String.t() | nil,
          required(:pending_prompt) => map() | nil,
          required(:prompt_queue) => term(),
          required(:text_acc) => [String.t()],
          required(:last_usage) => map(),
          required(:msg_counter) => integer(),
          optional(any()) => any()
        }

  @type queued :: %{acp_id: term(), message: String.t(), images: list(), params: map()}

  @doc """
  Queues a prompt behind the active one and returns the ACP notices that tell
  the client where it landed.
  """
  @spec enqueue(term(), String.t(), list(), map(), state()) :: {[map()], state()}
  def enqueue(acp_id, message, images, params, state) do
    queued = %{acp_id: acp_id, message: message, images: images, params: params}
    queue = PromptQueue.enqueue(state.prompt_queue, queued)

    session_id = params["sessionId"] || state.session_id
    queue_depth = PromptQueue.len(queue)

    notice =
      AdapterEvents.agent_message_chunk(
        session_id,
        "Queued message (position #{queue_depth})."
      )

    {[notice, queue_info_update(session_id, queue_depth, true)], %{state | prompt_queue: queue}}
  end

  @doc """
  Makes a prompt the active one: returns the native `prompt` NDJSON line and
  the state that expects its response.
  """
  @spec start_plan(term(), String.t(), list(), map(), state()) :: {String.t(), state()}
  def start_plan(acp_id, message, images, params, state) do
    {msg_id, next_counter} = RPC.next_prompt_id(state.msg_counter)

    rpc_msg =
      msg_id
      |> RPC.request(RPC.method(:prompt), %{"message" => message})
      |> Maps.put_non_empty("images", images)
      |> Maps.put_present("streamingBehavior", params["streamingBehavior"])

    state = %{
      state
      | session_id: params["sessionId"] || state.session_id,
        pending_prompt: %{acp_id: acp_id, msg_id: msg_id, cancel_requested: false},
        text_acc: [],
        last_usage: %{},
        msg_counter: next_counter
    }

    {RPC.line(rpc_msg), state}
  end

  @doc """
  Pops the next queued prompt, if any, off the front of the queue.
  """
  @spec next_queued(state()) :: {:ok, queued(), state()} | :empty
  def next_queued(state) do
    case PromptQueue.pop(state.prompt_queue) do
      {:value, queued, rest} -> {:ok, queued, %{state | prompt_queue: rest}}
      :empty -> :empty
    end
  end

  @doc """
  The ACP notices emitted once a queued prompt has become the active one.
  Call after `start_plan/5` so the session id and depth are the current ones.
  """
  @spec queue_started_messages(state()) :: [map()]
  def queue_started_messages(state) do
    queue_depth = PromptQueue.len(state.prompt_queue)

    [
      AdapterEvents.agent_message_chunk(
        state.session_id,
        "Starting queued message. (#{queue_depth} remaining)"
      ),
      queue_info_update(state.session_id, queue_depth, true)
    ]
  end

  @doc """
  Drains the queue on cancellation, answering every queued prompt as cancelled.
  """
  @spec cancel_queued(state()) :: {[map()], state()}
  def cancel_queued(state) do
    {queued, queue} = PromptQueue.drain(state.prompt_queue)

    responses =
      Enum.map(queued, fn queued ->
        Envelope.response(queued.acp_id, %{"stopReason" => "cancelled"})
      end)

    {responses, %{state | prompt_queue: queue}}
  end

  @spec mark_cancel_requested(state()) :: state()
  def mark_cancel_requested(%{pending_prompt: nil} = state), do: state

  def mark_cancel_requested(state) do
    put_in(state.pending_prompt[:cancel_requested], true)
  end

  @spec queue_cleared_messages(state(), boolean()) :: [map()]
  def queue_cleared_messages(state, had_queued) do
    if had_queued do
      [
        AdapterEvents.agent_message_chunk(state.session_id, "Cleared queued prompts."),
        queue_info_update(state.session_id, 0, not is_nil(state.pending_prompt))
      ]
    else
      []
    end
  end

  @doc """
  Completes the active prompt when Pi settles: the ACP response carrying the
  accumulated text and usage, plus the state with the prompt cleared.
  """
  @spec settle(state()) :: {map(), state()}
  def settle(state) do
    text = state.text_acc |> Enum.reverse() |> Enum.join("")

    stop_reason =
      if state.pending_prompt && state.pending_prompt.cancel_requested,
        do: "cancelled",
        else: "end_turn"

    acp_id = get_in(state.pending_prompt, [:acp_id])

    response =
      Envelope.response(acp_id, %{
        "stopReason" => stop_reason,
        "usage" => state.last_usage,
        "_meta" => %{"ex_mcp" => %{"text" => text, "sessionId" => state.session_id || "default"}}
      })

    {response, %{state | pending_prompt: nil, text_acc: [], last_usage: %{}}}
  end

  @doc """
  True when a failed native response belongs to the active prompt rather than
  to a tracked control group.
  """
  @spec response_error?(String.t(), map(), state()) :: boolean()
  def response_error?(id, %{"success" => false}, %{pending_prompt: %{msg_id: id}}), do: true
  def response_error?(_id, _event, _state), do: false

  defp queue_info_update(session_id, queue_depth, running?) do
    AdapterEvents.session_info_update(session_id, %{
      "_meta" => %{
        "ex_mcp" => %{"pi" => %{"queueDepth" => queue_depth, "running" => running?}}
      }
    })
  end
end
