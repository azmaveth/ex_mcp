# ExMCP Server Refactoring Baseline

This document captures the current state of ExMCP.Server before refactoring begins.

## Current Architecture

### Module Structure
- **ExMCP.Server** (1,488 lines): Monolithic module mixing multiple concerns
  - DSL macro definitions (`deftool`, `defresource`, `defprompt`)
  - GenServer implementation
  - Transport coordination
  - Protocol message processing
  - Helper function generation
  - Capability detection

### Key Functions
- `generate_helper_functions/0` (300+ lines): Generates content builder helpers
- `generate_genserver_handle_calls/0` (280+ lines): Generates GenServer callbacks
- Multiple `process_request/2` clauses with duplicate patterns

### DSL Users
1. ExMCP.Server.Legacy
2. ExMCP.Server.Tools
3. ExMCP.Server.ToolsRefactored
4. ExMCP.Server.Tools.Simplified
5. ExMCP.Server.StdioServer
6. ExMCP.Server.Handler
7. ExMCP.Compliance.Handlers.Handler20241105
8. ExMCP.Compliance.Handlers.Handler20250326
9. ExMCP.TestServer
10. Mix.Tasks.StdioServer
11. Test modules in integration tests
12. Example servers in documentation
13. User implementations (external)

## Current API Contract

### Public Macros
- `use ExMCP.Server` - Main entry point
- `deftool/2` - Define a tool with metadata and schema
- `defresource/2` - Define a resource with metadata
- `defprompt/2` - Define a prompt template

### Generated Functions
- `get_tools/0` - Returns map of defined tools
- `get_resources/0` - Returns map of defined resources  
- `get_prompts/0` - Returns map of defined prompts
- `get_capabilities/0` - Auto-detects and returns capabilities
- `text/1` - Create text content
- `json/1` - Create JSON content
- `user/1` - Create user message
- `assistant/1` - Create assistant message
- `system/1` - Create system message

### Callbacks
- `handle_tool_call/3` - Process tool invocations
- `handle_resource_read/3` - Process resource reads
- `handle_resource_list/1` - List available resources
- `handle_prompt_get/3` - Get prompt with arguments
- `handle_prompt_list/1` - List available prompts

## Performance Baselines

Actual measurements from benchmarks:
- Simple DSL compilation (1 tool, 1 resource, 1 prompt): 47.55ms
- Large module compilation (50 tools, 30 resources, 20 prompts): 316.591ms
- Capability detection time: 143μs
- Average list_tools response time: 0.82μs
- Average tool call response time: 0.77μs
- Memory growth during 1000 operations: 3KB (0 bytes retained after GC)

## Code Quality Metrics

Current state:
- Module size: 1,488 lines (ExMCP.Server)
- Cyclomatic complexity: 15+ in several functions
- Credo warnings: Multiple disabled checks
- Test coverage: TBD

## Backward Compatibility Requirements

### Must Maintain
1. All public macro APIs must remain identical
2. Generated function signatures must not change
3. Callback signatures must remain the same
4. Module attributes set by DSL must be preserved
5. Capability auto-detection logic must produce same results
6. GenServer message handling must remain compatible

### Can Change (Internal)
1. Internal module structure
2. Private function implementations
3. Macro expansion implementation details
4. Internal state representation (if properly migrated)

## Risk Areas

1. **Macro Expansion**: Complex quote/unquote logic that generates code at compile time
2. **Function Generation**: Dynamic function creation based on DSL usage
3. **Capability Detection**: Auto-detection based on module analysis
4. **State Management**: GenServer state handling across multiple concerns
5. **Transport Coordination**: Integration with various transport types

## Success Criteria

1. All existing tests pass without modification
2. All DSL users compile without changes
3. Performance metrics improve or remain stable
4. Code quality metrics improve significantly
5. Module sizes reduced to <300 lines each
6. Clear separation of concerns achieved