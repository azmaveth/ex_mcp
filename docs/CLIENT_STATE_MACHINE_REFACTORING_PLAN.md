# ExMCP Client State Management Refactoring Plan

## Overview

This plan addresses the critical finding from the architecture analysis: ExMCP.Client's complex state management with 21 fields, implicit transitions, and duplicate error handling. The refactoring will introduce GenStateMachine to formalize state transitions and improve maintainability.

## Phase 1: Foundation Setup

### 1. Create State Machine Module Structure

```
lib/ex_mcp/client/
├── state_machine.ex       # Core GenStateMachine implementation
├── states.ex              # State definitions and data structures
├── transitions.ex         # Transition logic and guards
└── middleware.ex          # Middleware pipeline for cross-cutting concerns
```

**Key Components:**
- Define formal states: `:disconnected`, `:connecting`, `:handshaking`, `:ready`, `:reconnecting`
- Create state-specific data structures instead of one monolithic struct
- Implement transition guards to prevent invalid state changes

### 2. Design State Data Architecture

**Current Approach (21 fields):**
```elixir
defstruct [
  :transport, :transport_state, :transport_module, :config,
  :pending_requests, :request_id, :server_info, :client_info,
  :capabilities, :initialized_capability, :pending_batches,
  :cancellation_requests, :progress_callbacks, :last_error,
  :reconnect_attempts, :reconnect_timer, :connection_supervisor,
  :initialization_callback, :error_callback, :transport_error_buffer,
  :max_retries
]
```

**New Approach (State-Specific Data):**
```elixir
# Base data present in all states
defmodule BaseData do
  defstruct [:config, :transport_module, :callbacks]
end

# State-specific extensions
defmodule ConnectingData do
  defstruct [:base, :connection_supervisor, :attempt_count]
end

defmodule ReadyData do
  defstruct [:base, :transport, :server_info, :capabilities, 
             :pending_requests, :request_id, :progress_callbacks]
end
```

## Phase 2: Core State Machine Implementation

### 3. Implement State Transitions

```
State Flow Diagram:

    ┌─────────────┐
    │ disconnected│◄─────────────────┐
    └──────┬──────┘                  │
           │ connect                  │ disconnect/error
           ▼                          │
    ┌─────────────┐                  │
    │ connecting  │──────────────────┤
    └──────┬──────┘                  │
           │ transport_ready          │
           ▼                          │
    ┌─────────────┐                  │
    │ handshaking │──────────────────┤
    └──────┬──────┘                  │
           │ initialized              │
           ▼                          │
    ┌─────────────┐                  │
    │    ready    │──────────────────┘
    └──────┬──────┘
           │ connection_lost
           ▼
    ┌─────────────┐
    │reconnecting │
    └─────────────┘
```

**Transition Implementation:**
- Each transition explicitly handles data transformation
- Guards ensure transitions only occur when valid
- Side effects (callbacks, cleanup) are clearly defined

### 4. Handle Request Management

**Request Lifecycle in State Machine:**
1. Requests only accepted in `:ready` state
2. Pending requests tracked with monotonic IDs
3. Request cleanup on state transitions
4. Batch request coordination simplified

## Phase 3: Integration Architecture

### 5. Middleware Pipeline Design

```
Request Flow Through Middleware:

Request ──► [Telemetry] ──► [RateLimit] ──► [CircuitBreaker] ──► [StateMachine]
               │                │                  │                     │
               ▼                ▼                  ▼                     ▼
          emit_start      check_limits      check_health          handle_call

Response ◄── [Telemetry] ◄── [RateLimit] ◄── [CircuitBreaker] ◄── [StateMachine]
               │                │                  │                     │
               ▼                ▼                  ▼                     ▼
           emit_stop      update_window      record_success        state_reply
```

**Middleware Benefits:**
- Separates cross-cutting concerns from core logic
- Easy to add/remove/reorder middleware
- Testable in isolation
- Consistent error handling

## Phase 4: Migration Strategy

### 6. Backward Compatibility Layer

```elixir
defmodule ExMCP.Client do
  # Existing public API preserved
  def connect(config, opts \\ []) do
    if use_state_machine?(config) do
      ExMCP.Client.StateMachine.connect(config, opts)
    else
      # Original implementation
      LegacyClient.connect(config, opts)
    end
  end
  
  # API delegation pattern for all public functions
  defdelegate request(client, method, params, opts), 
    to: implementation_module(client)
end
```

**Migration Approach:**
- Feature flag controls implementation selection
- Both implementations share same test suite
- Gradual rollout by transport type or user opt-in

## Phase 5: Testing & Validation

### 7. Comprehensive Test Strategy

**Test Categories:**

1. **Unit Tests**
   - Each state transition tested in isolation
   - Guard conditions verified
   - Data transformations validated

2. **Property-Based Tests**
   ```elixir
   property "state machine never enters invalid state" do
     check all initial_state <- state_generator(),
               events <- list_of(event_generator()) do
       # Verify invariants hold after event sequence
     end
   end
   ```

3. **Integration Tests**
   - All transport types tested with state machine
   - Error scenarios and recovery paths verified
   - Performance benchmarks ensure no regression

4. **Migration Tests**
   - Side-by-side comparison of old/new implementations
   - Identical behavior verification
   - Load testing under various conditions

## Phase 6: Rollout Plan

### 8. Phased Deployment Strategy

```
Rollout Timeline:

Week 1-2:  ┌─────────────────┐
           │ Alpha Testing   │ - Internal testing with feature flag
           │ (Flag: false)   │ - Performance validation
           └─────────────────┘
                    │
Week 3-4:  ┌─────────────────┐
           │ Beta Testing    │ - Opt-in for early adopters
           │ (Flag: opt-in)  │ - Monitor production metrics
           └─────────────────┘
                    │
Week 5-6:  ┌─────────────────┐
           │ Gradual Rollout │ - 10% → 50% → 100% traffic
           │ (Flag: percent) │ - A/B testing for validation
           └─────────────────┘
                    │
Week 7-8:  ┌─────────────────┐
           │ General Avail.  │ - Default to state machine
           │ (Flag: true)    │ - Legacy marked deprecated
           └─────────────────┘
```

**Safety Measures:**
- Instant rollback capability via feature flag
- Comprehensive monitoring and alerting
- Clear migration documentation
- Support for gradual adoption

## Implementation Checklist

**Immediate Actions:**
- [ ] Add `gen_state_machine` dependency to mix.exs
- [ ] Create initial state machine module structure
- [ ] Define formal state specifications
- [ ] Implement first state transition (disconnected → connecting)

**Short-term Goals:**
- [ ] Complete all state transitions
- [ ] Integrate with existing transport layer
- [ ] Create compatibility test suite
- [ ] Document state machine architecture

**Long-term Goals:**
- [ ] Implement middleware pipeline
- [ ] Add comprehensive telemetry
- [ ] Complete migration of all client functionality
- [ ] Deprecate legacy implementation

## Benefits Summary

1. **Reduced Complexity:** From 21 fields to focused state-specific data
2. **Explicit Behavior:** Clear state transitions with guards
3. **Better Testing:** Each state testable in isolation
4. **Improved Debugging:** State history and transition logs
5. **Extensibility:** Middleware pipeline for new features
6. **Maintainability:** Clear separation of concerns

This refactoring positions ExMCP for long-term success while maintaining backward compatibility and ensuring a smooth transition for users.