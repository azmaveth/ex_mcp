#!/bin/bash

# ExMCP v2 Transport Demo Script
# Starts all servers, runs the universal client, then cleans up

set -e  # Exit on any error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

# Change to project root so _build directory is available
cd "$PROJECT_ROOT"

# But run scripts from the getting_started directory
EXAMPLES_DIR="$PROJECT_ROOT/examples/v2/getting_started"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# PIDs to track for cleanup
NATIVE_PID=""
HTTP_PID=""
SSE_PID=""

# Cleanup function
cleanup() {
    echo -e "\n${YELLOW}🧹 Cleaning up servers...${NC}"
    
    if [ ! -z "$NATIVE_PID" ]; then
        echo "  Stopping Native server (PID: $NATIVE_PID)"
        kill $NATIVE_PID 2>/dev/null || true
    fi
    
    if [ ! -z "$HTTP_PID" ]; then
        echo "  Stopping HTTP server (PID: $HTTP_PID)"
        kill $HTTP_PID 2>/dev/null || true
    fi
    
    if [ ! -z "$SSE_PID" ]; then
        echo "  Stopping SSE server (PID: $SSE_PID)"
        kill $SSE_PID 2>/dev/null || true
    fi
    
    # Wait a moment for graceful shutdown
    sleep 2
    
    # Force kill if still running
    [ ! -z "$NATIVE_PID" ] && kill -9 $NATIVE_PID 2>/dev/null || true
    [ ! -z "$HTTP_PID" ] && kill -9 $HTTP_PID 2>/dev/null || true
    [ ! -z "$SSE_PID" ] && kill -9 $SSE_PID 2>/dev/null || true
    
    echo -e "${GREEN}✓ Cleanup complete${NC}"
}

# Set up cleanup on exit
trap cleanup EXIT INT TERM

echo -e "${BLUE}==========================================
ExMCP v2 Complete Transport Demo
==========================================${NC}"

echo -e "\nThis script will:
1. Start all transport servers
2. Run the universal client
3. Stop all servers and clean up"

echo -e "\n${YELLOW}🚀 Starting servers...${NC}"

# Start Native server
echo "  Starting Native server..."
elixir --name server@127.0.0.1 --cookie hello_mcp_demo_cookie "$EXAMPLES_DIR/hello_server_native.exs" --distributed > /dev/null 2>&1 &
NATIVE_PID=$!
echo "    PID: $NATIVE_PID"

# Start HTTP server
echo "  Starting HTTP server..."
elixir "$EXAMPLES_DIR/hello_server_http.exs" > /dev/null 2>&1 &
HTTP_PID=$!
echo "    PID: $HTTP_PID"

# Start SSE server  
echo "  Starting SSE server..."
elixir "$EXAMPLES_DIR/hello_server_sse.exs" > /dev/null 2>&1 &
SSE_PID=$!
echo "    PID: $SSE_PID"

echo -e "${GREEN}✓ All servers started${NC}"

# Wait for servers to initialize
echo -e "\n⏳ Waiting for servers to initialize..."
sleep 5

# Check if servers are still running
if ! kill -0 $NATIVE_PID 2>/dev/null; then
    echo -e "${RED}✗ Native server failed to start${NC}"
    exit 1
fi

if ! kill -0 $HTTP_PID 2>/dev/null; then
    echo -e "${RED}✗ HTTP server failed to start${NC}"
    exit 1
fi

if ! kill -0 $SSE_PID 2>/dev/null; then
    echo -e "${RED}✗ SSE server failed to start${NC}"
    exit 1
fi

# Test HTTP servers are responding
echo "  Testing HTTP server..."
if ! curl -s http://localhost:3000 > /dev/null; then
    echo -e "${RED}✗ HTTP server not responding${NC}"
    exit 1
fi

echo "  Testing SSE server..."
if ! curl -s http://localhost:3001 > /dev/null; then
    echo -e "${RED}✗ SSE server not responding${NC}"
    exit 1
fi

echo -e "${GREEN}✓ All servers ready${NC}"

# Run the client demo
echo -e "\n${YELLOW}🔄 Running universal client demo...${NC}"
echo -e "${BLUE}----------------------------------------${NC}"

if elixir --name client@127.0.0.1 --cookie hello_mcp_demo_cookie "$EXAMPLES_DIR/hello_client_all.exs"; then
    echo -e "${BLUE}----------------------------------------${NC}"
    echo -e "${GREEN}✓ Client demo completed successfully${NC}"
else
    echo -e "${BLUE}----------------------------------------${NC}"
    echo -e "${RED}✗ Client demo failed${NC}"
    exit 1
fi

echo -e "\n${GREEN}✅ Demo complete!${NC}"

# Cleanup will be called automatically by the trap