#!/bin/bash

# First connect to SSE
echo "Connecting to SSE..."
(
  curl -N http://localhost:8081/sse \
    -H "Accept: text/event-stream" \
    -H "Mcp-Session-Id: test-init-2" \
    -H "Cache-Control: no-cache" 2>&1 | while read line
  do
    echo "[SSE] $line"
  done
) &

SSE_PID=$!
sleep 1

# Send initialize request
echo -e "\nSending initialize request..."
curl -X POST http://localhost:8081 \
  -H "Content-Type: application/json" \
  -H "Mcp-Session-Id: test-init-2" \
  -d '{"jsonrpc":"2.0","id":"init-session-test","method":"initialize","params":{"clientInfo":{"name":"ExMCP","version":"0.1.0"},"protocolVersion":"2025-06-18"}}' \
  -v

# Wait a bit for SSE response
echo -e "\nWaiting for SSE response..."
sleep 3

# Clean up
kill $SSE_PID 2>/dev/null