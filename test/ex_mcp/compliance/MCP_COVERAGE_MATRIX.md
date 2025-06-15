# MCP Specification Test Coverage Matrix

This document tracks which MCP specification features are tested in our compliance test suite.

## Legend

- ✅ Fully tested
- ⚠️ Partially tested  
- ❌ Not tested
- N/A Not applicable to this version

## Core Protocol Features

### Initialization

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| initialize request | ✅ | ✅ | ✅ | spec_*_test.exs |
| Protocol version negotiation | ✅ | ✅ | ✅ | spec_*_test.exs |
| Capabilities exchange | ✅ | ✅ | ✅ | spec_*_test.exs |
| Server info metadata | ✅ | ✅ | ✅ | spec_*_test.exs |

### Tools

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| tools/list | ✅ | ✅ | ✅ | tools_compliance_test.exs |
| tools/call | ✅ | ✅ | ✅ | tools_compliance_test.exs |
| Tool input validation | ✅ | ✅ | ✅ | tools_compliance_test.exs |
| Tool annotations | N/A | ✅ | ✅ | spec_2025_03_26_test.exs |
| Structured tool output | N/A | N/A | ⚠️ | spec_draft_test.exs |

### Resources

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| resources/list | ✅ | ✅ | ✅ | resources_compliance_test.exs |
| resources/read | ✅ | ✅ | ✅ | resources_compliance_test.exs |
| resources/subscribe | N/A | ✅ | ✅ | spec_2025_03_26_test.exs |
| resources/unsubscribe | N/A | ✅ | ✅ | spec_2025_03_26_test.exs |
| Resource templates | ✅ | ✅ | ✅ | resources_compliance_test.exs |

### Prompts

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| prompts/list | ✅ | ✅ | ✅ | prompts_compliance_test.exs |
| prompts/get | ✅ | ✅ | ✅ | prompts_compliance_test.exs |
| Prompt arguments | ✅ | ✅ | ✅ | prompts_compliance_test.exs |

### Completion

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| completion/complete | ✅ | ✅ | ✅ | completion_compliance_test.exs |
| Argument validation | ✅ | ✅ | ✅ | completion_compliance_test.exs |

### Logging

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| logging/setLevel | ✅ | ✅ | ✅ | logging_compliance_test.exs |
| notifications/message | ✅ | ✅ | ✅ | logging_compliance_test.exs |
| Log level mapping | ✅ | ✅ | ✅ | logging_compliance_test.exs |

### Sampling

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| sampling/createMessage | ✅ | ✅ | ✅ | spec_*_test.exs |
| Model preferences | ✅ | ✅ | ✅ | spec_*_test.exs |

### Pagination

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| Cursor-based pagination | ✅ | ✅ | ✅ | pagination_compliance_test.exs |
| hasMore flag | ✅ | ✅ | ✅ | pagination_compliance_test.exs |

## Advanced Protocol Features

### Batch Requests

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| JSON-RPC batch support | ✅ | ✅ | ❌ | spec_*_test.exs |
| Batch validation | ✅ | ✅ | N/A | spec_*_test.exs |
| Empty batch handling | ❌ | ❌ | N/A | - |
| Notification-only batches | ❌ | ❌ | N/A | - |

### Bidirectional Communication

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| Server-to-client requests | ✅ | ✅ | ✅ | spec_*_test.exs |
| ping request | ✅ | ✅ | ✅ | spec_*_test.exs |
| createMessage request | ✅ | ✅ | ✅ | spec_*_test.exs |
| listRoots request | ✅ | ✅ | ✅ | spec_*_test.exs |

### Progress Notifications

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| Progress tokens | ✅ | ✅ | ✅ | spec_*_test.exs |
| Progress updates | ⚠️ | ⚠️ | ⚠️ | - |
| Progress percentage | ❌ | ❌ | ❌ | - |

### Cancellation

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| notifications/cancelled | ✅ | ✅ | ✅ | spec_*_test.exs |
| Request cancellation | ✅ | ✅ | ✅ | spec_*_test.exs |

### Roots

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| roots/list | ✅ | ✅ | ✅ | spec_*_test.exs |
| roots/list_changed | ✅ | ✅ | ✅ | spec_*_test.exs |

## Message Format & Validation

### Request Format

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| JSON-RPC 2.0 compliance | ✅ | ✅ | ✅ | protocol_test.exs |
| Request ID uniqueness | ❌ | ❌ | ❌ | - |
| Null ID rejection | ❌ | ❌ | ❌ | - |

### Response Format

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| Result XOR error | ❌ | ❌ | ❌ | - |
| Error code validation | ❌ | ❌ | ❌ | - |

### Meta Fields

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| _meta field support | ⚠️ | ⚠️ | ⚠️ | spec_*_test.exs |
| Progress token in _meta | ✅ | ✅ | ✅ | spec_*_test.exs |

## Transport-Specific Requirements

### stdio Transport

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| Line-delimited JSON | ✅ | ✅ | ✅ | Transport tests |
| Newline rejection | ❌ | ❌ | ❌ | - |
| stdout/stdin isolation | ❌ | ❌ | ❌ | - |

### HTTP+SSE Transport (2024-11-05 only)

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| SSE event format | ✅ | N/A | N/A | Transport tests |
| Origin validation | ✅ | N/A | N/A | Transport tests |
| CORS headers | ✅ | N/A | N/A | Transport tests |

### Streamable HTTP Transport (2025-03-26+)

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| Session management | N/A | ❌ | ❌ | - |
| Session ID handling | N/A | ❌ | ❌ | - |
| DELETE termination | N/A | ❌ | ❌ | - |
| Connection resumability | N/A | ❌ | ❌ | - |
| Last-Event-ID | N/A | ❌ | ❌ | - |

## Security Features

### OAuth 2.1

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| Client credentials flow | N/A | ✅ | ✅ | oauth_*_test.exs |
| Authorization code flow | N/A | ✅ | ✅ | oauth_*_test.exs |
| PKCE support | N/A | ✅ | ✅ | oauth_*_test.exs |
| Token refresh | N/A | ✅ | ✅ | oauth_*_test.exs |
| Metadata discovery | N/A | ⚠️ | ⚠️ | oauth_*_test.exs |
| Dynamic registration | N/A | ⚠️ | ⚠️ | oauth_*_test.exs |

### Security Best Practices

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| Token passthrough | N/A | N/A | ❌ | - |
| Confused deputy | N/A | N/A | ❌ | - |
| User consent | ❌ | ❌ | ❌ | - |
| HTTPS enforcement | ❌ | ❌ | ❌ | - |

## Draft-Specific Features

### Elicitation

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| elicitation/create | N/A | N/A | ⚠️ | spec_draft_test.exs |
| Schema validation | N/A | N/A | ❌ | - |
| Action handling | N/A | N/A | ❌ | - |
| Rate limiting | N/A | N/A | ❌ | - |

### Structured Output

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| structuredContent | N/A | N/A | ⚠️ | spec_draft_test.exs |
| isError flag | N/A | N/A | ❌ | - |
| outputSchema validation | N/A | N/A | ❌ | - |

### Resource Metadata

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| Protected resources | N/A | N/A | ⚠️ | spec_draft_test.exs |
| Scope requirements | N/A | N/A | ❌ | - |

## Content Types

| Feature | 2024-11-05 | 2025-03-26 | Draft | Test Location |
|---------|------------|------------|-------|---------------|
| Text content | ✅ | ✅ | ✅ | spec_*_test.exs |
| Image content | ✅ | ✅ | ✅ | spec_*_test.exs |
| Audio content | N/A | ✅ | ✅ | spec_2025_03_26_test.exs |
| Resource references | ✅ | ✅ | ✅ | spec_*_test.exs |

## Summary Statistics

### 2024-11-05 Base Version

- ✅ Fully tested: 31 features
- ⚠️ Partially tested: 3 features
- ❌ Not tested: 15 features
- **Coverage**: ~63%

### 2025-03-26 Version

- ✅ Fully tested: 42 features
- ⚠️ Partially tested: 6 features
- ❌ Not tested: 21 features
- **Coverage**: ~61%

### Draft Version

- ✅ Fully tested: 37 features
- ⚠️ Partially tested: 8 features
- ❌ Not tested: 24 features
- **Coverage**: ~54%

## High Priority Missing Tests

1. **Critical Security**: Token passthrough, confused deputy, user consent
2. **Session Management**: Streamable HTTP session handling
3. **Message Validation**: Request ID uniqueness, null ID rejection
4. **Transport Requirements**: stdio isolation, newline handling
5. **Error Handling**: Standard error codes, error data validation

## Next Steps

1. Create dedicated test files for missing features
2. Add comprehensive message validation tests
3. Implement security vulnerability tests
4. Add transport-specific requirement tests
5. Complete OAuth 2.1 edge case testing
