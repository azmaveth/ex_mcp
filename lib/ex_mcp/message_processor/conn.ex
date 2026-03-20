defmodule ExMCP.MessageProcessor.Conn do
  @moduledoc """
  Connection struct representing an MCP message processing context.
  """

  defstruct [
    :request,
    :response,
    :state,
    :assigns,
    :transport,
    :session_id,
    :progress_token,
    :halted
  ]

  @type t :: %__MODULE__{
          request: map() | nil,
          response: map() | nil,
          state: term(),
          assigns: map(),
          transport: atom(),
          session_id: String.t() | nil,
          progress_token: String.t() | integer() | nil,
          halted: boolean()
        }
end
