defmodule ExMCP.Client.Configuration do
  @moduledoc """
  Configuration options for ExMCP.Client implementation switching.

  ExMCP provides two client implementations:

  1. **Legacy Implementation** (`ExMCP.Client.LegacyAdapter`) - The original GenServer-based client
  2. **State Machine Implementation** (`ExMCP.Client.StateMachineAdapter`) - New GenStateMachine-based client

  ## Application-Level Configuration

  Set the default adapter for all clients in your application:

      # config/config.exs
      config :ex_mcp, :client_adapter, ExMCP.Client.StateMachineAdapter

  Available adapters:
  - `ExMCP.Client.LegacyAdapter` (default) - Stable, battle-tested implementation  
  - `ExMCP.Client.StateMachineAdapter` - New implementation with formal state management

  ## Per-Client Configuration

  Override the adapter for specific clients:

      # Use state machine for this client
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: "mcp-server",
        adapter: ExMCP.Client.StateMachineAdapter
      )

      # Use legacy implementation
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio, 
        command: "mcp-server",
        adapter: ExMCP.Client.LegacyAdapter
      )

  ## Configuration Priority

  The adapter is selected in this order of precedence:

  1. `:adapter` option in `start_link/2` opts
  2. `:adapter` key in config map passed to `start_link/2`  
  3. Application environment `:client_adapter` setting
  4. Default (`ExMCP.Client.LegacyAdapter`)

  ## Implementation Differences

  ### State Machine Implementation Benefits

  - **Formal state management**: Uses GenStateMachine for explicit state transitions
  - **Reduced complexity**: State-specific data structures instead of 21 monolithic fields
  - **Better error handling**: Clear state transition guards prevent invalid operations
  - **Enhanced observability**: Comprehensive telemetry events for monitoring
  - **Progress tracking integration**: Built-in ExMCP.ProgressTracker integration
  - **Improved reconnection**: Exponential backoff with configurable limits

  ### Legacy Implementation

  - **Stability**: Proven in production environments
  - **Backward compatibility**: Maintains exact API compatibility  
  - **Simple architecture**: Single GenServer process
  - **Resource efficient**: Lower memory footprint

  ## Migration Strategy

  ### Phase 1: Opt-in Testing

  Test the state machine implementation on non-critical clients:

      # Test clients only
      {:ok, test_client} = ExMCP.Client.start_link(
        config,
        adapter: ExMCP.Client.StateMachineAdapter
      )

  ### Phase 2: Application Default

  Once tested, set as application default:

      # config/config.exs  
      config :ex_mcp, :client_adapter, ExMCP.Client.StateMachineAdapter

  ### Phase 3: Migration Complete

  Remove explicit adapter configurations to use the new default.

  ## Telemetry Events

  The state machine implementation emits telemetry events for observability:

      # State transitions
      [:ex_mcp, :client, :state_transition] 
      
      # Request lifecycle  
      [:ex_mcp, :client, :request, :start]
      [:ex_mcp, :client, :request, :success] 
      [:ex_mcp, :client, :request, :error]
      
      # Connection events
      [:ex_mcp, :client, :connection, :success]
      [:ex_mcp, :client, :transport, :error]
      [:ex_mcp, :client, :transport, :closed]
      
      # Handshake events
      [:ex_mcp, :client, :handshake, :start]
      [:ex_mcp, :client, :handshake, :success] 
      [:ex_mcp, :client, :handshake, :error]
      
      # Reconnection events
      [:ex_mcp, :client, :reconnect, :attempt]
      [:ex_mcp, :client, :reconnect, :success]
      [:ex_mcp, :client, :reconnect, :error]
      [:ex_mcp, :client, :reconnect, :timeout]
      
      # Progress tracking events
      [:ex_mcp, :client, :progress, :update]
      [:ex_mcp, :client, :progress, :unknown_token]
      [:ex_mcp, :client, :progress, :rate_limited]
      [:ex_mcp, :client, :progress, :not_increasing] 
      [:ex_mcp, :client, :progress, :error]
      [:ex_mcp, :client, :progress, :untracked]

  Set up telemetry handlers to monitor client behavior:

      :telemetry.attach_many(
        "ex_mcp_client_metrics",
        [
          [:ex_mcp, :client, :state_transition],
          [:ex_mcp, :client, :request, :success], 
          [:ex_mcp, :client, :request, :error]
        ],
        &MyApp.Telemetry.handle_client_event/4,
        %{}
      )

  ## Configuration Examples

  ### Production: High Reliability

      config :ex_mcp, :client_adapter, ExMCP.Client.StateMachineAdapter

      # Client with enhanced monitoring
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: "critical-mcp-server",
        max_reconnect_attempts: 10,
        reconnect_backoff_ms: 1000,
        callbacks: %{
          on_disconnect: &MyApp.Monitoring.handle_disconnect/1,
          on_initialize: &MyApp.Callbacks.on_initialize/1
        }
      )

  ### Development: Legacy Compatibility

      # Use legacy for development/testing
      config :ex_mcp, :client_adapter, ExMCP.Client.LegacyAdapter

  ### Hybrid: Mixed Environment

      # Most clients use state machine
      config :ex_mcp, :client_adapter, ExMCP.Client.StateMachineAdapter

      # Specific legacy client for compatibility
      {:ok, legacy_client} = ExMCP.Client.start_link(
        config,
        adapter: ExMCP.Client.LegacyAdapter
      )

  ## Troubleshooting

  ### Check Current Adapter

      adapter = ExMCP.Client.Wrapper.get_adapter(config, opts)

  ### Verify Configuration

      Application.get_env(:ex_mcp, :client_adapter)

  ### Monitor State Transitions

      :telemetry.attach(
        "debug_states",
        [:ex_mcp, :client, :state_transition],
        fn event, measurements, metadata, _config ->
          IO.inspect({event, measurements, metadata}, label: "State Transition")
        end,
        %{}
      )
  """

  @doc """
  Returns the default client adapter module.
  """
  def default_adapter do
    ExMCP.Client.LegacyAdapter
  end

  @doc """
  Returns all available client adapter modules.
  """
  def available_adapters do
    [
      ExMCP.Client.LegacyAdapter,
      ExMCP.Client.StateMachineAdapter
    ]
  end

  @doc """
  Validates that an adapter module implements the required behavior.
  """
  def valid_adapter?(module) when is_atom(module) do
    module in available_adapters()
  end

  def valid_adapter?(_), do: false

  @doc """
  Returns telemetry events emitted by the state machine adapter.
  """
  def telemetry_events do
    [
      # State transitions
      [:ex_mcp, :client, :state_transition],

      # Request lifecycle
      [:ex_mcp, :client, :request, :start],
      [:ex_mcp, :client, :request, :success],
      [:ex_mcp, :client, :request, :error],

      # Connection events
      [:ex_mcp, :client, :connection, :success],
      [:ex_mcp, :client, :transport, :error],
      [:ex_mcp, :client, :transport, :closed],

      # Handshake events
      [:ex_mcp, :client, :handshake, :start],
      [:ex_mcp, :client, :handshake, :success],
      [:ex_mcp, :client, :handshake, :error],

      # Reconnection events
      [:ex_mcp, :client, :reconnect, :attempt],
      [:ex_mcp, :client, :reconnect, :success],
      [:ex_mcp, :client, :reconnect, :error],
      [:ex_mcp, :client, :reconnect, :timeout],

      # Progress tracking events
      [:ex_mcp, :client, :progress, :update],
      [:ex_mcp, :client, :progress, :unknown_token],
      [:ex_mcp, :client, :progress, :rate_limited],
      [:ex_mcp, :client, :progress, :not_increasing],
      [:ex_mcp, :client, :progress, :error],
      [:ex_mcp, :client, :progress, :untracked]
    ]
  end
end
