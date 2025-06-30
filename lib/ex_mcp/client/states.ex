defmodule ExMCP.Client.States do
  @moduledoc """
  State-specific data structures for the ExMCP client state machine.

  Each state has its own data structure containing only the fields
  relevant to that state, reducing complexity and making state
  transitions explicit.
  """

  defmodule Common do
    @moduledoc """
    Common data shared across all states.
    """
    defstruct [
      :config,
      :transport_module,
      :callbacks,
      :name
    ]

    @type t :: %__MODULE__{
            config: map(),
            transport_module: module(),
            callbacks: map(),
            name: atom() | nil
          }

    def new(config, opts) do
      %__MODULE__{
        config: config,
        transport_module: config[:transport] || ExMCP.Transport.Stdio,
        callbacks: %{
          on_initialize: opts[:on_initialize],
          on_error: opts[:on_error],
          on_disconnect: opts[:on_disconnect]
        },
        name: opts[:name]
      }
    end
  end

  defmodule Disconnected do
    @moduledoc """
    State data for disconnected state.
    """
    defstruct [
      :common,
      :last_error,
      :retry_count,
      :disconnect_reason
    ]

    @type t :: %__MODULE__{
            common: Common.t(),
            last_error: term() | nil,
            retry_count: non_neg_integer(),
            disconnect_reason: term() | nil
          }

    def new(config, opts) do
      %__MODULE__{
        common: Common.new(config, opts),
        last_error: nil,
        retry_count: 0,
        disconnect_reason: nil
      }
    end
  end

  defmodule Connecting do
    @moduledoc """
    State data for connecting state.
    """
    defstruct [
      :common,
      :supervisor_pid,
      :start_time,
      :attempt_number
    ]

    @type t :: %__MODULE__{
            common: Common.t(),
            supervisor_pid: pid() | nil,
            start_time: integer(),
            attempt_number: pos_integer()
          }
  end

  defmodule Handshaking do
    @moduledoc """
    State data for handshaking state.
    """
    defstruct [
      :common,
      :transport,
      :transport_state,
      :client_info,
      :handshake_start_time,
      :from_reconnecting
    ]

    @type t :: %__MODULE__{
            common: Common.t(),
            transport: pid(),
            transport_state: term(),
            client_info: map(),
            handshake_start_time: integer(),
            from_reconnecting: nil | map()
          }
  end

  defmodule Ready do
    @moduledoc """
    State data for ready state.
    """
    defstruct [
      :common,
      :transport,
      :transport_state,
      :server_info,
      :capabilities,
      :pending_requests,
      :next_request_id,
      :progress_callbacks,
      :initialized_capability
    ]

    @type t :: %__MODULE__{
            common: Common.t(),
            transport: {module(), term(), pid()},
            transport_state: term(),
            server_info: map(),
            capabilities: map(),
            pending_requests: %{integer() => term()},
            next_request_id: integer(),
            progress_callbacks: %{String.t() => fun()},
            initialized_capability: String.t() | nil
          }
  end

  defmodule Reconnecting do
    @moduledoc """
    State data for reconnecting state.
    """
    defstruct [
      :common,
      :last_transport,
      :last_server_info,
      :backoff_ms,
      :attempt_number,
      :max_attempts,
      :reconnect_timer
    ]

    @type t :: %__MODULE__{
            common: Common.t(),
            last_transport: pid() | nil,
            last_server_info: map() | nil,
            backoff_ms: pos_integer(),
            attempt_number: pos_integer(),
            max_attempts: pos_integer() | :infinity,
            reconnect_timer: reference() | nil
          }
  end

  # Helper functions

  @doc """
  Extracts the common data from any state data structure.
  """
  def get_common(%Disconnected{common: common}), do: common
  def get_common(%Connecting{common: common}), do: common
  def get_common(%Handshaking{common: common}), do: common
  def get_common(%Ready{common: common}), do: common
  def get_common(%Reconnecting{common: common}), do: common
end
