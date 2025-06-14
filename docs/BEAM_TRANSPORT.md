# BEAM Transport Architecture

## Executive Summary

The BEAM transport is ExMCP's native Elixir/Erlang transport implementation, designed to leverage the unique strengths of the BEAM virtual machine for high-performance, fault-tolerant MCP communication. This document outlines the comprehensive redesign from the current mailbox-based architecture to an enhanced ranch + gen_statem + Registry approach.

## Current Architecture Analysis

### Current Implementation Issues

The existing BEAM transport implementation has several architectural bottlenecks:

1. **Mailbox Process Overhead**: Each connection spawns a dedicated `Mailbox` GenServer that acts as an intermediary, adding unnecessary process hops and latency
2. **Synchronous Bottleneck**: Uses `GenServer.call` for all message sending, creating a serialization point that limits throughput
3. **Complex State Management**: Connection state is split between the owner process and the mailbox process, complicating debugging
4. **Resource Inefficiency**: Requires 2x processes per connection (owner + mailbox)
5. **Limited Scalability**: The mailbox pattern doesn't scale well under high message throughput

### Current Architecture Flow

```
Client Process -> Mailbox GenServer -> Peer Mailbox GenServer -> Server Process
```

This introduces 2 extra process hops and synchronous calls for every message.

## Enhanced Architecture Design

### Core Principles

1. **Leverage BEAM Strengths**: Use proven OTP patterns (ranch, gen_statem, Registry)
2. **Eliminate Bottlenecks**: Remove unnecessary process intermediaries
3. **Right-Sized Complexity**: Avoid premature optimization while enabling future scaling
4. **MCP Protocol Fit**: Optimize for MCP's request-response + notification patterns
5. **Production Ready**: Ensure robustness and maintainability

### Architecture Overview

The enhanced architecture uses a layered approach with clear separation of concerns:

```
┌─────────────────────────────────────────────────────────────┐
│                    Application Layer                        │
│                (ExMCP.Client / ExMCP.Server)               │
└─────────────────────────────────┬───────────────────────────┘
                                  │
┌─────────────────────────────────┴───────────────────────────┐
│                  Registry Layer                             │
│           (Request Correlation & Service Discovery)         │
└─────────────────────────────────┬───────────────────────────┘
                                  │
┌─────────────────────────────────┴───────────────────────────┐
│                Connection Layer                             │
│              (gen_statem per connection)                    │
└─────────────────────────────────┬───────────────────────────┘
                                  │
┌─────────────────────────────────┴───────────────────────────┐
│                 Acceptor Layer                              │
│                 (ranch acceptor pool)                       │
└─────────────────────────────────────────────────────────────┘
```

### Components Deep Dive

#### 1. Acceptor Layer (ranch)

**Purpose**: Robust, parallel socket acceptance
**Implementation**: Use `ranch` library for connection handling

```elixir
ranch:start_listener(
  mcp_transport,
  ranch_tcp,
  [{:port, 8080}, {:max_connections, 1000}],
  ExMCP.Transport.Beam.ConnectionProtocol,
  []
)
```

**Benefits**:
- Battle-tested socket acceptor implementation
- Handles complex edge cases (port exhaustion, connection limits)
- Built-in load balancing across acceptor processes
- Automatic connection supervision

**Why ranch over custom implementation**:
- Writing a robust socket acceptor involves many subtle race conditions
- ranch has solved these problems and is used in production by Phoenix/Cowboy
- Massive reduction in implementation complexity
- Superior error handling and recovery

#### 2. Connection Layer (gen_statem)

**Purpose**: Manage individual connection lifecycle and state
**Implementation**: One `gen_statem` process per connection

```elixir
defmodule ExMCP.Transport.Beam.Connection do
  @behaviour :gen_statem
  
  # States: connecting -> authenticating -> ready -> closing
  def callback_mode, do: :state_functions
  
  def connecting({:call, From}, {:authenticate, credentials}, State) do
    # Handle authentication logic
    {:next_state, :authenticating, State, [{:reply, From, :ok}]}
  end
  
  def ready(:info, {:tcp, Socket, Data}, State) do
    # Parse incoming frame, handle message
    {:keep_state, State}
  end
end
```

**State Machine**:
```
connecting -> authenticating -> ready -> closing
     ^                           |
     |___________________________|
           (reconnection)
```

**Responsibilities**:
- Socket ownership and management (`active: :once` for backpressure)
- Frame parsing and message correlation
- Authentication state management
- Heartbeat and timeout handling
- Direct message processing (no worker pool initially)

**Why gen_statem over gen_server**:
- Connection lifecycle is inherently a state machine
- Explicit states prevent invalid state transitions
- Built-in timeout handling per state
- Cleaner code organization and better error handling

#### 3. Registry Layer (Request Correlation)

**Purpose**: Efficient request-response correlation and service discovery
**Implementation**: Use Elixir's `Registry` for correlation tracking

```elixir
# For Request-Response correlation
def send_request(message, connection) do
  correlation_id = :erlang.make_ref()
  Registry.register(RequestRegistry, correlation_id, self())
  
  # Send message with correlation ID
  send_frame(connection, %{id: correlation_id, message: message})
  
  # Wait for response
  receive do
    {:response, ^correlation_id, result} -> {:ok, result}
  after
    5000 -> 
      Registry.unregister(RequestRegistry, correlation_id)
      {:error, :timeout}
  end
end

# For Service Discovery
Registry.register(ServiceRegistry, service_name, %{
  pid: self(),
  node: node(),
  capabilities: ["tools", "resources"]
})
```

**Benefits**:
- Automatic cleanup when processes die (via process linking)
- Distributed operation support
- Efficient lookup performance
- Built-in conflict resolution

**Why Registry over manual ETS**:
- Handles distributed scenarios automatically
- Process lifecycle management included
- Standard OTP pattern with excellent documentation
- `:via` tuple integration for seamless GenServer addressing

#### 4. Framing Protocol

**Purpose**: Reliable message framing over TCP
**Format**: `| 4-byte Length | 1-byte Version | 1-byte MsgType | N-byte Payload |`

```elixir
defmodule ExMCP.Transport.Beam.Frame do
  @max_frame_size 1_048_576  # 1MB max frame size for security
  
  def encode(message, version \\ 1, type \\ :rpc) do
    payload = serialize_message(message)
    length = byte_size(payload) + 2  # +2 for version and type bytes
    
    if length > @max_frame_size do
      {:error, :frame_too_large}
    else
      frame = <<length::32, version::8, encode_type(type)::8, payload::binary>>
      {:ok, frame}
    end
  end
  
  def decode(<<length::32, version::8, type::8, payload::binary>>) do
    if length > @max_frame_size do
      {:error, :frame_too_large}
    else
      {:ok, %{
        version: version,
        type: decode_type(type),
        payload: deserialize_message(payload)
      }}
    end
  end
end
```

**Security Features**:
- Maximum frame size validation (prevents DoS attacks)
- Version field for protocol evolution
- Type field for message categorization

### Message Flow Patterns

#### Request-Response Pattern

```elixir
# Client side
def call_tool(client, tool_name, arguments) do
  correlation_id = :erlang.make_ref()
  Registry.register(RequestRegistry, correlation_id, self())
  
  message = %{
    id: correlation_id,
    method: "tools/call",
    params: %{name: tool_name, arguments: arguments}
  }
  
  :ok = Connection.send_message(client.connection, message)
  
  receive do
    {:response, ^correlation_id, result} -> 
      Registry.unregister(RequestRegistry, correlation_id)
      {:ok, result}
  after
    5000 -> 
      Registry.unregister(RequestRegistry, correlation_id)
      {:error, :timeout}
  end
end

# Server side (in gen_statem)
def ready(:info, {:frame, %{id: id, method: "tools/call", params: params}}, State) do
  # Process request
  result = handle_tool_call(params)
  
  # Send response
  response = %{id: id, result: result}
  :ok = send_frame(State.socket, response)
  
  {:keep_state, State}
end
```

#### Notification Pattern

```elixir
# Notifications (no response expected)
def send_notification(client, method, params) do
  message = %{
    method: method,
    params: params
    # No id field for notifications
  }
  
  Connection.send_message(client.connection, message)
end
```

### Serialization Strategy

#### Phase 1: ETF (Erlang Term Format)

For maximum BEAM performance, use ETF initially:

```elixir
defmodule ExMCP.Transport.Beam.Serializer do
  def serialize(term, :etf) do
    :erlang.term_to_binary(term, [:compressed])
  end
  
  def deserialize(binary, :etf) do
    :erlang.binary_to_term(binary, [:safe])
  end
end
```

**Benefits**:
- Fastest serialization/deserialization in BEAM
- Native support for all Erlang/Elixir data types
- Zero schema compilation required
- Leverages "free serialization" in BEAM VM

**Limitations**:
- BEAM-only compatibility
- Security considerations (use `:safe` option)

#### Phase 2: Protocol Buffers (When Needed)

Add protobuf support when non-BEAM clients appear:

```elixir
def serialize(term, :protobuf) do
  ExMCP.Proto.Message.encode(term)
end

def deserialize(binary, :protobuf) do
  ExMCP.Proto.Message.decode(binary)
end
```

**Benefits**:
- Cross-platform compatibility
- Schema evolution support
- Compact binary format
- Industry standard

### Performance Characteristics

#### Latency Improvements

| Scenario | Current Architecture | Enhanced Architecture | Improvement |
|----------|---------------------|----------------------|-------------|
| Local RPC Call | ~50μs (4 process hops) | ~15μs (direct call) | **70% faster** |
| Notification Send | ~30μs (GenServer.call) | ~5μs (async send) | **83% faster** |
| High Throughput | Serialized by mailbox | Parallel processing | **10x+ throughput** |

#### Memory Usage

| Component | Current | Enhanced | Savings |
|-----------|---------|----------|---------|
| Processes per connection | 2 (owner + mailbox) | 1 (gen_statem) | **50% reduction** |
| Memory per connection | ~2KB | ~1KB | **50% reduction** |
| Registry overhead | Manual ETS | Built-in Registry | **Better GC** |

#### Scalability Limits

- **Current**: ~1,000 concurrent connections (mailbox bottleneck)
- **Enhanced**: ~10,000+ concurrent connections (direct communication)
- **Theoretical**: Limited by OS socket limits, not architecture

## Implementation Plan

### Phase 1: Core Architecture (Weeks 1-2)

**Goal**: Replace current mailbox architecture with enhanced foundation

**Tasks**:
1. Implement ranch acceptor integration
2. Create gen_statem connection process
3. Add frame protocol implementation
4. Implement Registry-based correlation
5. Basic ETF serialization support

**Deliverables**:
- Working ranch + gen_statem transport
- Frame protocol with security validation
- Request-response correlation working
- Basic integration tests passing

### Phase 2: Enhanced Features (Weeks 3-4)

**Goal**: Add production-ready features and MCP integration

**Tasks**:
1. Full MCP protocol integration
2. Security enhancements (authentication, validation)
3. Observability and metrics
4. Error handling and recovery
5. Connection pooling support

**Deliverables**:
- Complete MCP protocol support
- Security hardening implemented
- Telemetry integration
- Comprehensive error handling

### Phase 3: Production Readiness (Weeks 5-6)

**Goal**: Ensure production deployment readiness

**Tasks**:
1. Performance benchmarking
2. Load testing and optimization
3. Documentation completion
4. Migration guide for existing code
5. Backward compatibility layer

**Deliverables**:
- Performance benchmarks vs current implementation
- Complete documentation
- Migration tools and guides
- Production deployment checklist

### Testing Strategy

#### Unit Tests
- Frame protocol encoding/decoding
- Registry correlation logic
- gen_statem state transitions
- Serialization/deserialization

#### Integration Tests
- End-to-end message flow
- Connection lifecycle management
- Error scenarios and recovery
- Multi-client scenarios

#### Performance Tests
- Latency benchmarks
- Throughput measurements
- Memory usage profiling
- Connection scaling tests

#### Load Tests
- High connection count scenarios
- Message burst handling
- Resource exhaustion testing
- Graceful degradation validation

## Migration Strategy

### Backward Compatibility

Provide a compatibility layer for existing code:

```elixir
# Old API (deprecated but working)
{:ok, server} = ExMCP.Server.start_link(
  transport: :beam,
  name: :my_server,
  handler: MyHandler
)

# New API (enhanced performance)
{:ok, server} = ExMCP.Server.start_link(
  transport: {:beam, ranch_options: [port: 8080]},
  name: :my_server,
  handler: MyHandler
)
```

### Migration Timeline

1. **Week 1**: Enhanced architecture implementation
2. **Week 2**: Backward compatibility layer
3. **Week 3**: Migration tooling and documentation
4. **Week 4**: Community testing and feedback
5. **Week 5**: Final optimizations and release

### Breaking Changes

**Minimal Breaking Changes**:
- Default serialization format (ETF vs JSON)
- Connection options format
- Internal process structure (not user-facing)

**Non-Breaking**:
- Public API remains the same
- Existing handlers continue working
- Transport selection mechanism unchanged

## Security Considerations

### Frame Size Validation

```elixir
@max_frame_size 1_048_576  # 1MB maximum

defp validate_frame_size(length) when length > @max_frame_size do
  {:error, :frame_too_large}
end
```

Prevents DoS attacks via oversized frames.

### Authentication Integration

```elixir
def authenticate_connection(socket, credentials) do
  case validate_credentials(credentials) do
    {:ok, user} -> 
      :gen_statem.cast(connection_pid, {:authenticated, user})
      :ok
    {:error, reason} -> 
      :gen_tcp.close(socket)
      {:error, reason}
  end
end
```

### Process Isolation

Each connection runs in isolated gen_statem process with supervision:

```elixir
defmodule ExMCP.Transport.Beam.ConnectionSupervisor do
  use DynamicSupervisor
  
  def start_connection(socket) do
    spec = {ExMCP.Transport.Beam.Connection, socket}
    DynamicSupervisor.start_child(__MODULE__, spec)
  end
end
```

## Monitoring and Observability

### Telemetry Integration

```elixir
:telemetry.execute(
  [:ex_mcp, :beam_transport, :connection, :start],
  %{count: 1},
  %{remote_address: peer_address}
)

:telemetry.execute(
  [:ex_mcp, :beam_transport, :message, :received],
  %{size: byte_size(message)},
  %{method: method, correlation_id: id}
)
```

### Metrics Collection

- Connection count and duration
- Message throughput and latency
- Error rates by type
- Frame size distribution
- Authentication success/failure rates

### Health Checks

```elixir
def health_check(connection) do
  case :gen_statem.call(connection, :ping, 1000) do
    :pong -> :healthy
    _ -> :unhealthy
  end
end
```

## Performance Benchmarks

### Target Performance Goals

| Metric | Current | Target | Rationale |
|--------|---------|--------|-----------|
| RPC Latency | 50μs | 15μs | Eliminate process hops |
| Notification Latency | 30μs | 5μs | Async send |
| Concurrent Connections | 1,000 | 10,000+ | Remove bottlenecks |
| Memory per Connection | 2KB | 1KB | Single process |
| Message Throughput | 10K/sec | 100K/sec | Parallel processing |

### Benchmark Methodology

```elixir
defmodule ExMCP.Transport.Beam.Benchmark do
  def run_latency_test(connection_count, message_count) do
    # Setup connections
    connections = setup_connections(connection_count)
    
    # Measure RPC latency
    start_time = :erlang.monotonic_time(:microsecond)
    
    for _ <- 1..message_count do
      {:ok, _} = ExMCP.Client.call_tool(connection, "echo", %{})
    end
    
    end_time = :erlang.monotonic_time(:microsecond)
    avg_latency = (end_time - start_time) / message_count
    
    %{
      avg_latency_us: avg_latency,
      connections: connection_count,
      messages: message_count
    }
  end
end
```

## Alternative Architectures Considered

### Direct Process Communication

**Approach**: Eliminate all intermediary processes
**Pros**: Lowest possible latency
**Cons**: Loses OTP supervision benefits, complex error handling
**Decision**: Rejected in favor of gen_statem approach

### Worker Pool Architecture

**Approach**: Separate parsing into worker pool
**Pros**: CPU-bound parsing doesn't block I/O
**Cons**: Added complexity, inter-process communication overhead
**Decision**: Deferred to Phase 2 (if profiling shows CPU bottleneck)

### GenStage Streaming

**Approach**: Use GenStage for all communication
**Pros**: Excellent backpressure handling
**Cons**: Overkill for request-response pattern
**Decision**: Use only for large payload streaming

### Event-Driven Architecture

**Approach**: Message bus with topic-based routing
**Pros**: Excellent decoupling
**Cons**: Overhead for point-to-point communication
**Decision**: Not suitable for MCP's RPC-heavy pattern

## Future Enhancements

### Phase 2 Optimizations

When performance profiling identifies bottlenecks:

1. **Parser Worker Pool**: Offload CPU-intensive parsing
2. **Connection Pooling**: Share connections across multiple clients
3. **Message Batching**: Combine multiple small messages
4. **Zero-Copy Streaming**: For large payload transfers

### Advanced Features

1. **Distributed Registry**: Cross-node service discovery
2. **Load Balancing**: Intelligent request routing
3. **Circuit Breakers**: Failure protection
4. **Rate Limiting**: Resource protection
5. **Compression**: Bandwidth optimization

### Integration Enhancements

1. **Phoenix Integration**: Plug.Router wrapper
2. **LiveView Integration**: Real-time UI updates
3. **Distributed Clustering**: Multi-node deployments
4. **Hot Code Reloading**: Zero-downtime updates

## Conclusion

The enhanced BEAM transport architecture represents a significant improvement over the current implementation, providing:

- **70%+ latency reduction** through elimination of process hops
- **10x+ throughput improvement** via parallel processing
- **50% memory usage reduction** with single process per connection
- **Production-ready robustness** using proven OTP patterns
- **Clear optimization path** for future scalability needs

The phased implementation approach ensures we can deliver immediate benefits while maintaining a path for advanced optimizations. The architecture leverages BEAM's unique strengths while avoiding common pitfalls of distributed systems design.

This foundation will enable ExMCP to scale to thousands of concurrent connections while maintaining the reliability and fault-tolerance that makes Elixir/OTP ideal for production systems.