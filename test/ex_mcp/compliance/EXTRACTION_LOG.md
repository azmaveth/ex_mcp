# Compliance Test Extraction Log

This document tracks which tests were extracted from non-compliance test files and moved to the compliance directory.

## Extraction Summary

### From `cancellation_test.exs` → `cancellation_compliance_test.exs`

Extracted the `"cancellation protocol validation"` describe block containing:
- `encode_cancelled validates initialize request cannot be cancelled`
- `encode_cancelled allows other requests to be cancelled`
- `encode_cancelled works without reason`
- `encode_cancelled! bypasses validation for backward compatibility`

### From `version_negotiation_test.exs` → `version_negotiation_compliance_test.exs`

Extracted the `"protocol negotiation"` describe block containing:
- `initialize request includes all required fields`
- `client can specify custom capabilities`

### From `roots_simple_test.exs` → `roots_compliance_test.exs`

Extracted:
1. The `"protocol compliance"` describe block containing:
   - `roots request uses correct protocol format`
   - `roots response format matches spec`
   - `roots change notification format`

2. The test `roots follow MCP specification format` from the main describe block

### From `security_origin_validation_test.exs` → `security_compliance_test.exs`

Extracted:
1. The entire `"DNS rebinding attack scenarios"` describe block
2. The entire `"HTTPS enforcement"` describe block
3. The test `security headers have correct values` from the security headers block
4. The entire `"case insensitive header matching"` describe block
5. Several host validation tests that are MCP-specific requirements

## Original Files Status

All original test files now contain only implementation-specific tests:
- `cancellation_test.exs` - Client/server cancellation functionality tests
- `version_negotiation_test.exs` - Version negotiation implementation tests
- `roots_simple_test.exs` - Roots functionality implementation tests  
- `security_origin_validation_test.exs` - General security module tests

## Compliance Directory Structure

The compliance directory now contains 19 test files:
- 3 spec version tests (spec_2024_11_05, spec_2025_03_26, spec_draft)
- 6 feature compliance tests (*_compliance_test.exs files)
- 3 protocol tests (batch, progress, notifications)
- 7 newly created/extracted compliance tests

Total: 241 compliance tests (218 passing, 23 skipped)