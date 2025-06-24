# MCP Compliance Test Structure

This document describes the new MCP (Model Context Protocol) compliance test structure implemented to provide comprehensive, maintainable, and version-aware testing across all MCP specification versions.

## Overview

The compliance test structure is designed to:
- ✅ Support multiple MCP specification versions (2024-11-05, 2025-03-26, 2025-06-18)
- ✅ Provide clear failure reporting showing which spec version(s) are affected
- ✅ Reduce code duplication through feature-based organization
- ✅ Maintain separation from unit and integration tests
- ✅ Enable easy addition of new MCP versions and features

## Architecture

### Feature-Based Organization

Tests are organized around MCP features rather than versions:

