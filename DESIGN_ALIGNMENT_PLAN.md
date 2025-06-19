# ExMCP v2 Design Alignment Plan

## Executive Summary

This document outlines the specific fixes needed to align the ExMCP v2 implementation with the original DESIGN_PLAN.md while preserving valuable enhancements that exceed the design specifications.

**Status**: Implementation is 70% aligned with design - core objectives achieved but key integration features missing.

**Primary Issues**:
1. ❌ **HTTP Plug integration missing** - Cannot integrate with Phoenix/Cowboy as designed
2. ❌ **DSL syntax deviates from specification** - Breaking API differences
3. ❌ **Transport server startup missing** - No automatic transport configuration
4. ❌ **Missing structured response/error types** - Client API incomplete

**Timeline**: 3-4 weeks to achieve full design compliance while preserving enhancements.

---

## 🎯 **Success Metrics**

- [ ] HTTP servers work with `Plug.Cowboy.http(ExMCP.Plug, opts)`
- [ ] DSL syntax matches design specification exactly
- [ ] Servers start with `MyServer.start_link(transport: :http, port: 8080)`
- [ ] Client responses use structured types as designed
- [ ] All original design examples work without modification
- [ ] Maintain current test coverage (636+ tests passing)
- [ ] Preserve reliability enhancements (circuit breakers, health checks)

---

## 🔥 **Critical Fixes (Week 1-2)**

### **Fix 1: HTTP Plug Integration**

**Issue**: Current `ExMCP.Plug` is custom behavior, not HTTP Plug-compatible

**Required Changes**:

#### 1.1 Create HTTP-Compatible Plug
**File**: `lib/ex_mcp_v2/http_plug.ex` (new)
```elixir
defmodule ExMCP.HttpPlug do
  @moduledoc """
  HTTP Plug for MCP (Model Context Protocol) requests.
  Compatible with Phoenix and Cowboy servers.
  """
  
  @behaviour Plug
  
  import Plug.Conn
  
  def init(opts) do
    %{
      tools: Keyword.get(opts, :tools),
      handler: Keyword.get(opts, :handler),
      server_info: Keyword.get(opts, :server_info, %{name: "ex_mcp_server", version: "1.0.0"}),
      session_manager: Keyword.get(opts, :session_manager, ExMCP.SessionManager)
    }
  end
  
  def call(%Plug.Conn{method: "GET", path_info: ["sse"]} = conn, opts) do
    handle_sse_connection(conn, opts)
  end
  
  def call(%Plug.Conn{method: "POST"} = conn, opts) do
    handle_mcp_request(conn, opts)
  end
  
  def call(conn, _opts) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(404, Jason.encode!(%{error: "Not found"}))
  end
  
  # Implementation functions...
end
```

#### 1.2 Rename Current Plug
**File**: `lib/ex_mcp_v2/plug.ex` → `lib/ex_mcp_v2/message_processor.ex`
- Rename `ExMCP.Plug` → `ExMCP.MessageProcessor`
- Update all references in existing code
- Keep current functionality for message processing

#### 1.3 Add SSE Support
**File**: `lib/ex_mcp_v2/http_plug/sse.ex` (new)
```elixir
defmodule ExMCP.HttpPlug.SSE do
  @moduledoc """
  Server-Sent Events support for ExMCP HTTP Plug.
  """
  
  def handle_sse_connection(conn, opts) do
    session_id = get_session_id(conn)
    
    conn
    |> put_resp_header("content-type", "text/event-stream")
    |> put_resp_header("cache-control", "no-cache")
    |> put_resp_header("connection", "keep-alive")
    |> put_resp_header("access-control-allow-origin", "*")
    |> send_chunked(200)
    |> start_sse_loop(session_id, opts)
  end
  
  # Implementation...
end
```

**Tests Required**:
- `test/ex_mcp_v2/http_plug_test.exs`
- `test/ex_mcp_v2/http_plug/sse_test.exs`
- Integration tests with actual Cowboy server

**Success Criteria**:
```elixir
# This should work
{:ok, _} = Plug.Cowboy.http(ExMCP.HttpPlug, [
  tools: MyApp.ToolDefinitions,
  handler: MyApp.ToolHandler
], port: 4000)
```

### **Fix 2: DSL Syntax Compliance**

**Issue**: DSL keywords don't match design specification

**Required Changes**:

#### 2.1 Fix Tool DSL
**File**: `lib/ex_mcp_v2/dsl/tool.ex`

**Change**:
```elixir
# Current (incorrect)
deftool "say_hello" do
  tool_description("Says hello")
  args do
    field(:name, :string, required: true)
  end
end

# Design-compliant (correct)
deftool "say_hello" do
  description "Says hello to a given name"
  input_schema %{
    type: "object",
    properties: %{name: %{type: "string"}},
    required: ["name"]
  }
end
```

**Implementation**:
- Support both syntax styles for backward compatibility
- Add deprecation warnings for non-compliant syntax
- Update macro to generate correct JSON schema from both formats

#### 2.2 Fix Resource DSL
**File**: `lib/ex_mcp_v2/dsl/resource.ex`

**Change**:
```elixir
# Current (incorrect)
defresource "config://app" do
  resource_name("App Config")
  resource_description("Application configuration")
  mime_type("application/json")
end

# Design-compliant (correct)
defresource "config://app" do
  name "App Config"
  description "Application configuration"
  mime_type "application/json"
end
```

#### 2.3 Fix Prompt DSL  
**File**: `lib/ex_mcp_v2/dsl/prompt.ex`

**Change**:
```elixir
# Current (incorrect)
defprompt "greeting" do
  prompt_name("Greeting Template")
  prompt_description("A greeting template")
  arguments do
    arg(:style, description: "Greeting style")
  end
end

# Design-compliant (correct) 
defprompt "greeting" do
  name "Greeting Template"
  description "A greeting template"
  arguments do
    arg :style, description: "Greeting style"
  end
end
```

**Tests Required**:
- Update all DSL tests to use compliant syntax
- Add backward compatibility tests
- Test JSON schema generation from new syntax

**Success Criteria**:
- All design examples work without modification
- Existing tests pass with updated syntax
- Deprecation warnings guide users to correct syntax

### **Fix 3: Transport Server Startup**

**Issue**: No automatic transport server startup as designed

**Required Changes**:

#### 3.1 Add Server Transport Module
**File**: `lib/ex_mcp_v2/server/transport.ex` (new)
```elixir
defmodule ExMCP.Server.Transport do
  @moduledoc """
  Transport configuration and lifecycle management for ExMCP servers.
  """
  
  def start_stdio_server(module, server_info, tools, opts) do
    ExMCP.Server.StdioServer.start_link([
      module: module,
      server_info: server_info,
      tools: tools
    ] ++ opts)
  end
  
  def start_http_server(module, server_info, tools, opts) do
    port = Keyword.get(opts, :port, 4000)
    sse_enabled = Keyword.get(opts, :sse, false)
    
    plug_opts = [
      handler: module,
      server_info: server_info,
      tools: tools,
      sse_enabled: sse_enabled
    ]
    
    cowboy_opts = [port: port]
    
    Plug.Cowboy.http(ExMCP.HttpPlug, plug_opts, cowboy_opts)
  end
  
  # Implementation...
end
```

#### 3.2 Update ServerV2 with start_link
**File**: `lib/ex_mcp_v2/server_v2.ex`

**Add**:
```elixir
defmacro __using__(opts) do
  quote do
    # ... existing code ...
    
    def start_link(opts \\ []) do
      ExMCP.Server.Transport.start_server(__MODULE__, @server_info, @tools, opts)
    end
    
    def child_spec(opts) do
      %{
        id: __MODULE__,
        start: {__MODULE__, :start_link, [opts]},
        type: :worker,
        restart: :permanent,
        shutdown: 500
      }
    end
  end
end

def start_server(module, server_info, tools, opts) do
  transport = Keyword.get(opts, :transport, :http)
  
  case transport do
    :stdio -> ExMCP.Server.Transport.start_stdio_server(module, server_info, tools, opts)
    :http -> ExMCP.Server.Transport.start_http_server(module, server_info, tools, opts)
    :sse -> ExMCP.Server.Transport.start_http_server(module, server_info, tools, Keyword.put(opts, :sse, true))
  end
end
```

**Tests Required**:
- `test/ex_mcp_v2/server/transport_test.exs`
- Integration tests for each transport type

**Success Criteria**:
```elixir
# This should work
MyServer.start_link(transport: :http, port: 8080)
MyServer.start_link(transport: :stdio)
MyServer.start_link(transport: :sse, port: 8080)
```

---

## 🟡 **Important Fixes (Week 3)**

### **Fix 4: Structured Response Types**

**Issue**: Missing `ExMCP.Response` and `ExMCP.Error` as designed

**Required Changes**:

#### 4.1 Implement Response Module
**File**: `lib/ex_mcp_v2/response.ex` (new)
```elixir
defmodule ExMCP.Response do
  @moduledoc """
  Structured response types for MCP operations.
  """
  
  defstruct [
    :content,
    :meta,
    :tool_name,
    :request_id,
    :server_info
  ]
  
  @type t :: %__MODULE__{
    content: [content_item()],
    meta: map() | nil,
    tool_name: String.t() | nil,
    request_id: String.t() | nil,
    server_info: map() | nil
  }
  
  @type content_item :: %{
    type: String.t(),
    text: String.t() | nil,
    data: any() | nil,
    annotations: map() | nil
  }
  
  def from_raw_response(raw_response, opts \\ []) do
    # Implementation from DESIGN_PLAN.md:1031-1038
  end
  
  # ... rest of implementation
end
```

#### 4.2 Implement Error Module
**File**: `lib/ex_mcp_v2/error.ex` (new)
```elixir
defmodule ExMCP.Error do
  @moduledoc """
  Structured error types for MCP operations.
  """
  
  defexception [:code, :message, :data, :request_id]
  
  @type t :: %__MODULE__{
    code: integer() | atom(),
    message: String.t(),
    data: any(),
    request_id: String.t() | nil
  }
  
  # JSON-RPC Error Codes as designed
  @parse_error -32700
  @invalid_request -32600
  # ... etc
  
  # Implementation from DESIGN_PLAN.md:1106-1137
end
```

#### 4.3 Update Client APIs
**Files**: `lib/ex_mcp_v2/client.ex`, `lib/ex_mcp_v2/convenience_client.ex`

**Change**: Return structured responses instead of raw maps
```elixir
def call_tool(client, tool_name, arguments) do
  case GenServer.call(client, {:call_tool, tool_name, arguments}) do
    {:ok, raw_response} ->
      response = ExMCP.Response.from_raw_response(raw_response, tool_name: tool_name)
      {:ok, response}
    
    {:error, error_data} when is_map(error_data) ->
      error = ExMCP.Error.from_json_rpc_error(error_data)
      {:error, error}
    
    {:error, reason} ->
      error = ExMCP.Error.connection_error(inspect(reason))
      {:error, error}
  end
end
```

### **Fix 5: Convenience Functions**

**Issue**: Missing high-level convenience functions as designed

**Required Changes**:

#### 5.1 Add Top-Level Convenience Functions
**File**: `lib/ex_mcp_v2/convenience.ex` (new)
```elixir
defmodule ExMCP do
  @moduledoc """
  High-level convenience functions for ExMCP operations.
  """
  
  def with_client(connect_opts, fun) when is_function(fun, 1) do
    # Implementation from DESIGN_PLAN.md:864-875
  end
  
  def call(connect_opts, tool_name, arguments) when is_binary(tool_name) do
    # Implementation from DESIGN_PLAN.md:885-889
  end
  
  def list_tools(connect_opts) do
    # Implementation from DESIGN_PLAN.md:898-902
  end
  
  def server_info(connect_opts) do
    # Implementation from DESIGN_PLAN.md:911-915
  end
end
```

#### 5.2 Add Configuration Module
**File**: `lib/ex_mcp_v2/client/config.ex` (new)
```elixir
defmodule ExMCP.Client.Config do
  @moduledoc """
  Client configuration with intelligent defaults and auto-detection.
  """
  
  defstruct [
    :url,
    :transport,
    :headers,
    :timeout,
    :retry_attempts,
    :ssl_opts
  ]
  
  def build_config(opts) do
    # Implementation from DESIGN_PLAN.md:949-991
  end
end
```

---

## 🟢 **Enhancements to Preserve**

### **Keep: Reliability Features**
- ✅ Circuit breakers (`lib/ex_mcp_v2/reliability/circuit_breaker.ex`)
- ✅ Health checks (`lib/ex_mcp_v2/reliability/health_check.ex`)
- ✅ Retry logic (`lib/ex_mcp_v2/reliability/retry.ex`)
- ✅ Supervisor (`lib/ex_mcp_v2/reliability/supervisor.ex`)

**Action**: Document as "Production Enhancements Beyond Design"

### **Keep: Transport Manager**
- ✅ Transport abstraction (`lib/ex_mcp_v2/transport_manager.ex`)
- ✅ Automatic failover
- ✅ Multiple strategies (sequential, parallel, fastest)

**Action**: Integrate with server transport startup

### **Keep: Multiple Client Types**  
- ✅ ClientV2 (synchronous as designed)
- ✅ ConvenienceClient (high-level API)
- ✅ SimpleClient (basic use cases)

**Action**: Document different use cases for each

### **Keep: Enhanced Content System**
- ✅ Content builders (`lib/ex_mcp_v2/content/builders.ex`)
- ✅ Content validation (`lib/ex_mcp_v2/content/validation.ex`)
- ✅ Content protocol (`lib/ex_mcp_v2/content/protocol.ex`)

**Action**: Extend beyond design specification

### **Keep: Comprehensive Testing**
- ✅ 41 test files
- ✅ Integration tests
- ✅ Property-based tests  
- ✅ Performance tests
- ✅ Mock server infrastructure

**Action**: Update tests to match design compliance

---

## 📋 **Implementation Checklist**

### **Week 1: HTTP Integration**
- [ ] Create `ExMCP.HttpPlug` with `Plug` behavior
- [ ] Rename current `ExMCP.Plug` to `ExMCP.MessageProcessor`
- [ ] Implement SSE support (`ExMCP.HttpPlug.SSE`)
- [ ] Add HTTP Plug tests
- [ ] Verify Cowboy integration works

### **Week 2: DSL Compliance**
- [ ] Update tool DSL: `tool_description` → `description`, `args` → `input_schema`
- [ ] Update resource DSL: `resource_name` → `name`, etc.
- [ ] Update prompt DSL: `prompt_name` → `name`, etc.
- [ ] Add backward compatibility with deprecation warnings
- [ ] Update all DSL tests
- [ ] Verify design examples work

### **Week 3: Transport & Response Types**
- [ ] Create `ExMCP.Server.Transport` module
- [ ] Add `start_link` to ServerV2 with transport selection
- [ ] Implement `ExMCP.Response` structured types
- [ ] Implement `ExMCP.Error` structured types
- [ ] Update client APIs to return structured responses
- [ ] Add transport startup tests

### **Week 4: Convenience & Polish**
- [ ] Create top-level `ExMCP` convenience functions
- [ ] Add `ExMCP.Client.Config` module
- [ ] Update documentation to reflect design compliance
- [ ] Create migration guide for syntax changes
- [ ] Final integration testing
- [ ] Performance benchmarking

---

## 🎯 **Success Validation**

### **Acceptance Tests**
Create these exact examples from DESIGN_PLAN.md to verify compliance:

#### 1. HTTP Server Example
```elixir
# Should work exactly as designed
{:ok, _} = Plug.Cowboy.http(ExMCP.HttpPlug, [
  tools: MyApp.ToolDefinitions,
  handler: MyApp.ToolHandler,
  server_info: %{name: "my-app", version: "1.0.0"}
], port: 4000)
```

#### 2. DSL Server Example  
```elixir
# Should work exactly as designed
defmodule SimpleHTTPServer do
  use ExMCP.Server,
    server_info: %{name: "simple-http-server", version: "1.0.0"}

  deftool "say_hello" do
    description "Says hello to a given name"
    input_schema %{
      type: "object", 
      properties: %{name: %{type: "string"}},
      required: ["name"]
    }
  end

  @impl ExMCP.Server
  def handle_tool_call("say_hello", %{"name" => name}, _state) do
    content = [%{type: "text", text: "Hello, #{name}!"}]
    {:ok, %{content: content}}
  end
end

# Start with one line
SimpleHTTPServer.start_link(transport: :http, port: 8321)
```

#### 3. Convenience Client Example
```elixir
# Should work exactly as designed
{:ok, result} = ExMCP.call("http://localhost:8080", "say_hello", %{name: "World"})
{:ok, tools} = ExMCP.list_tools("http://localhost:8080")
```

#### 4. Synchronous Client Example
```elixir
# Should work exactly as designed (no Process.sleep needed)
{:ok, client} = ExMCP.Client.start_link(url: "http://localhost:8080")
{:ok, result} = ExMCP.Client.call_tool(client, "tool", %{})
```

### **Line Count Verification**
- [ ] Verify server examples are <20 lines
- [ ] Measure reduction from 151-line baseline
- [ ] Document line count savings

### **Performance Verification**  
- [ ] Native transport: <50μs latency maintained
- [ ] HTTP transport: reasonable performance
- [ ] Memory usage within acceptable bounds

---

## 📚 **Documentation Updates Required**

### **New Documentation**
1. **Migration Guide**: v1 to v2 API changes
2. **Transport Guide**: When to use stdio vs HTTP vs SSE
3. **DSL Reference**: Complete syntax documentation
4. **Reliability Features**: Circuit breakers, health checks guide
5. **Performance Guide**: Benchmark results and tuning

### **Updated Documentation**
1. **README.md**: Update examples to use compliant syntax
2. **API Documentation**: Update all module docs
3. **Getting Started**: Ensure examples work
4. **Architecture**: Document final design vs implementation

---

## 🚨 **Risk Mitigation**

### **Breaking Changes**
**Risk**: DSL syntax changes will break existing code
**Mitigation**: 
- Maintain backward compatibility with deprecation warnings
- Provide automated migration tool
- Clear migration documentation

### **Integration Issues**
**Risk**: HTTP Plug integration may have unforeseen issues  
**Mitigation**:
- Comprehensive integration testing
- Test with real Phoenix/Cowboy applications
- Fallback to current approach if blocking issues

### **Timeline Pressure**
**Risk**: 4-week timeline may be aggressive
**Mitigation**:
- Prioritize critical fixes first (HTTP, DSL)
- Defer nice-to-have features if needed
- Focus on design compliance over perfection

---

## 📈 **Success Metrics Dashboard**

### **Design Compliance Score**: 70% → 100%
- [x] ~~Core objectives achieved~~ ✅
- [ ] HTTP Plug integration ❌ → ✅  
- [ ] DSL syntax compliant ❌ → ✅
- [ ] Transport server startup ❌ → ✅
- [ ] Structured responses ❌ → ✅
- [ ] Convenience functions ❌ → ✅

### **Feature Coverage**: Current vs Target
- **Designed Features**: 12/17 implemented (71%)
- **Beyond Design**: 8 additional features implemented  
- **Test Coverage**: 636+ tests (excellent)
- **Line Count Reduction**: ✅ Achieved (<20 lines)

### **Quality Metrics**: Maintain Current
- **Test Success Rate**: 98.5% → maintain
- **Performance**: Native <50μs → maintain  
- **Reliability**: Circuit breakers, health checks → preserve

---

This plan provides a clear roadmap to achieve full design compliance while preserving the valuable enhancements that make ExMCP v2 production-ready. The 4-week timeline is ambitious but achievable with focused execution on the critical path items.