defmodule ExMCP.Client.Handler do
  @moduledoc """
  This module implements the standard MCP specification.

  Behaviour for handling server-to-client requests in MCP.

  The MCP protocol supports bi-directional communication where servers can
  make requests to clients. This behaviour defines the callbacks that a
  client handler must implement to respond to these requests.

  ## Example

      defmodule MyClientHandler do
        @behaviour ExMCP.Client.Handler

        @impl true
        def init(args) do
          {:ok, %{roots: [%{uri: "file:///home/user", name: "Home"}]}}
        end

        @impl true
        def handle_ping(state) do
          {:ok, %{}, state}
        end

        @impl true
        def handle_list_roots(state) do
          {:ok, state.roots, state}
        end

        @impl true
        def handle_create_message(params, state) do
          # Show to user for approval, then sample LLM
          case get_user_approval(params) do
            :approved ->
              result = sample_llm(params)
              {:ok, result, state}
            :denied ->
              {:error, "User denied the request", state}
          end
        end
      end
  """

  @type state :: any()
  @type error_info :: String.t() | map()

  @doc """
  Called when the client handler is started.

  Return `{:ok, state}` to initialize the handler state.
  """
  @callback init(args :: any()) :: {:ok, state}

  @doc """
  Handles a ping request from the server.

  The client should respond promptly to indicate it's still alive.

  ## Response

  - `{:ok, result, new_state}` - Success with empty result
  - `{:error, reason, new_state}` - Error occurred
  """
  @callback handle_ping(state) ::
              {:ok, map(), state}
              | {:error, error_info, state}

  @doc """
  Handles a request to list the client's root directories.

  This is called when the server needs to understand what file system
  locations the client has access to.

  ## Response

  The roots should be a list of maps with:
  - `uri` (required) - The URI of the root (must start with "file://")
  - `name` (optional) - Human-readable name for the root

  ## Example

      def handle_list_roots(state) do
        roots = [
          %{uri: "file:///home/user", name: "Home"},
          %{uri: "file:///projects", name: "Projects"}
        ]
        {:ok, roots, state}
      end
  """
  @callback handle_list_roots(state) ::
              {:ok, [map()], state}
              | {:error, error_info, state}

  @doc """
  Handles a request from the server to sample an LLM.

  The client has full discretion over which model to select and should
  inform the user before beginning sampling (human in the loop).

  ## Parameters

  The params map contains:
  - `messages` - List of messages to send to the LLM
  - `modelPreferences` (optional) - Server's model preferences
  - `systemPrompt` (optional) - System prompt to use
  - `includeContext` (optional) - Whether to include MCP context
  - `temperature` (optional) - Sampling temperature
  - `maxTokens` (optional) - Maximum tokens to sample

  ## Response

  The result should contain:
  - `role` - The role of the created message (usually "assistant")
  - `content` - The content of the message
  - `model` - The model that was used

  ## Human-in-the-Loop

  This callback MUST implement human-in-the-loop approval. The handler can
  use the `ExMCP.Approval` behaviour for this, or implement its own approval
  mechanism. The user must be informed about the sampling request and have
  the opportunity to approve or deny it.

  ## Example

      def handle_create_message(params, state) do
        case get_user_approval(params) do
          :approved ->
            result = %{
              role: "assistant",
              content: %{type: "text", text: "Hello!"},
              model: "gpt-4"
            }
            {:ok, result, state}
          :denied ->
            {:error, "User denied sampling request", state}
        end
      end
  """
  @callback handle_create_message(params :: map(), state) ::
              {:ok, map(), state}
              | {:error, error_info, state}

  @doc """
  Handles an elicitation request from the server.

  This is a stable protocol feature available in MCP 2025-06-18 and later.
  The server is requesting additional information from the user through a 
  structured form with JSON schema validation.

  ## Parameters

  - `message` - Human-readable message explaining what information is needed
  - `requested_schema` - JSON schema defining the expected response structure

  ## Response

  The result should contain:
  - `action` - One of "accept", "decline", or "cancel"
  - `content` (optional) - The user's response data (only for "accept")

  ## Example

      def handle_elicitation_create(message, requested_schema, state) do
        # Present the elicitation to the user
        case present_elicitation_to_user(message, requested_schema) do
          {:accept, data} ->
            {:ok, %{action: "accept", content: data}, state}
          :decline ->
            {:ok, %{action: "decline"}, state}
          :cancel ->
            {:ok, %{action: "cancel"}, state}
        end
      end
  """
  @callback handle_elicitation_create(message :: String.t(), requested_schema :: map(), state) ::
              {:ok, map(), state}
              | {:error, error_info, state}

  @doc """
  Called when the handler process is about to terminate.
  """
  @callback terminate(reason :: term(), state) :: :ok

  @optional_callbacks terminate: 2, handle_elicitation_create: 3
end
