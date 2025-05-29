defmodule ExMCP.Approval do
  @moduledoc """
  This module implements the standard MCP specification.

  Behaviour for implementing human-in-the-loop approval flows.

  The MCP specification requires that clients inform users and get approval
  before processing certain sensitive operations like LLM sampling.

  ## Example Implementation

      defmodule MyApprovalHandler do
        @behaviour ExMCP.Approval
        
        @impl true
        def request_approval(:sampling_request, params, _opts) do
          IO.puts("Server wants to sample LLM with:")
          IO.inspect(params, pretty: true)
          
          case IO.gets("Approve? (y/n): ") do
            "y\\n" -> {:approved, params}
            _ -> {:denied, "User denied the request"}
          end
        end
        
        @impl true
        def request_approval(:sampling_response, response, _opts) do
          IO.puts("LLM returned:")
          IO.inspect(response, pretty: true)
          
          case IO.gets("Send to server? (y/n): ") do
            "y\\n" -> {:approved, response}
            _ -> {:denied, "User blocked the response"}
          end
        end
      end

  ## Using with Client

      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        handler: MyClientHandler,
        approval_handler: MyApprovalHandler
      )
  """

  @type approval_type :: :sampling_request | :sampling_response | atom()
  @type approval_result :: {:approved, any()} | {:denied, String.t()} | {:modified, any()}

  @doc """
  Requests user approval for an operation.

  ## Parameters

  - `type` - The type of approval being requested
  - `data` - The data to be approved (request params or response)
  - `opts` - Additional options (e.g., timeout, context)

  ## Return Values

  - `{:approved, data}` - User approved, proceed with original data
  - `{:modified, new_data}` - User approved with modifications
  - `{:denied, reason}` - User denied, operation should be cancelled

  ## Approval Types

  - `:sampling_request` - Approve before sending request to LLM
  - `:sampling_response` - Approve before sending response to server
  """
  @callback request_approval(type :: approval_type(), data :: any(), opts :: keyword()) ::
              approval_result()
end
