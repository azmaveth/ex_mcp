#!/bin/bash

# Script to clean up stray test processes and ports
# Usage: ./scripts/cleanup_tests.sh

echo "🧹 Cleaning up test processes and ports..."

# Common test ports used in ExMCP
TEST_PORTS=(8080 8081 8082 8083 8084 8085 9000 9001 9002)

# Kill processes using test ports
echo "🔌 Checking for processes using test ports..."
for port in "${TEST_PORTS[@]}"; do
    pids=$(lsof -ti :$port 2>/dev/null)
    if [ ! -z "$pids" ]; then
        echo "  Killing processes on port $port: $pids"
        kill -9 $pids 2>/dev/null
    fi
done

# Kill stray beam.smp processes from tests
echo "🔎 Looking for stray test-related beam processes..."
ps aux | grep -E "beam.smp.*(MIX_ENV=test|_build/test|ex_mcp.*test)" | grep -v grep | awk '{print $2}' | while read pid; do
    if [ ! -z "$pid" ] && [ "$pid" != "$$" ]; then
        echo "  Killing stray beam process: $pid"
        kill -9 $pid 2>/dev/null
    fi
done

# Kill any processes with ExMCP or test_server in their command
echo "📡 Looking for named test processes..."
ps aux | grep -E "(ExMCP\.(Test|Testing)|test_server|test_http|test_sse)" | grep -v grep | awk '{print $2}' | while read pid; do
    if [ ! -z "$pid" ] && [ "$pid" != "$$" ]; then
        echo "  Killing test process: $pid"
        kill -9 $pid 2>/dev/null
    fi
done

# Clean up any orphaned epmd (Erlang Port Mapper Daemon) if no beam processes exist
beam_count=$(ps aux | grep -c "[b]eam.smp")
if [ "$beam_count" -eq 0 ]; then
    echo "🗑️  No beam processes found, cleaning up epmd..."
    epmd -kill 2>/dev/null
fi

echo "✅ Cleanup complete!"

# Optional: Show any remaining processes on test ports
echo ""
echo "📊 Port status check:"
for port in "${TEST_PORTS[@]}"; do
    if lsof -i :$port >/dev/null 2>&1; then
        echo "  ⚠️  Port $port is still in use!"
    else
        echo "  ✓ Port $port is free"
    fi
done