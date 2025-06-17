#!/usr/bin/env python3
"""
Python MCP HTTP Server

A HTTP-based calculator server using the Python MCP SDK that ExMCP clients can connect to.
This demonstrates HTTP transport for distributed MCP services.

Prerequisites:
    pip install mcp httpx uvicorn fastapi

Usage:
    python http_server.py

The server will start on http://localhost:8000 and accept MCP-over-HTTP requests.
"""

import asyncio
import json
import uvicorn
from fastapi import FastAPI, HTTPException
from fastapi.responses import JSONResponse
from pydantic import BaseModel
from typing import Dict, Any, List, Optional
import time
import sys
import logging

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class MCPRequest(BaseModel):
    """MCP JSON-RPC request model."""
    jsonrpc: str = "2.0"
    id: Optional[int] = None
    method: str
    params: Optional[Dict[str, Any]] = None


class MCPResponse(BaseModel):
    """MCP JSON-RPC response model."""
    jsonrpc: str = "2.0"
    id: Optional[int] = None
    result: Optional[Dict[str, Any]] = None
    error: Optional[Dict[str, Any]] = None


class PythonHTTPCalculator:
    """HTTP-based Python MCP Calculator Server."""
    
    def __init__(self):
        self.app = FastAPI(title="Python MCP HTTP Calculator", version="1.0.0")
        self.operation_count = 0
        self.history = []
        self.start_time = time.time()
        
        # Set up routes
        self._setup_routes()
    
    def _setup_routes(self):
        """Set up HTTP routes for MCP communication."""
        
        @self.app.get("/health")
        async def health_check():
            """Health check endpoint."""
            return {
                "status": "healthy",
                "server": "Python MCP HTTP Calculator",
                "uptime_seconds": int(time.time() - self.start_time)
            }
        
        @self.app.post("/mcp")
        async def mcp_endpoint(request: MCPRequest):
            """Main MCP JSON-RPC endpoint."""
            try:
                response_data = await self._handle_mcp_request(request)
                
                return MCPResponse(
                    id=request.id,
                    result=response_data
                )
                
            except Exception as e:
                logger.error(f"MCP request failed: {e}")
                
                return MCPResponse(
                    id=request.id,
                    error={
                        "code": -32603,
                        "message": f"Internal error: {str(e)}"
                    }
                )
        
        @self.app.get("/")
        async def root():
            """Root endpoint with server information."""
            return {
                "server": "Python MCP HTTP Calculator",
                "version": "1.0.0",
                "endpoints": {
                    "health": "/health",
                    "mcp": "/mcp (POST)"
                },
                "documentation": "/docs"
            }
    
    async def _handle_mcp_request(self, request: MCPRequest) -> Dict[str, Any]:
        """Handle MCP JSON-RPC requests."""
        
        method = request.method
        params = request.params or {}
        
        if method == "initialize":
            return await self._handle_initialize(params)
        elif method == "list_tools":
            return await self._handle_list_tools()
        elif method == "tools/call":
            return await self._handle_tool_call(params)
        else:
            raise HTTPException(
                status_code=400,
                detail=f"Unknown MCP method: {method}"
            )
    
    async def _handle_initialize(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """Handle MCP initialize request."""
        return {
            "protocolVersion": "2024-11-05",
            "capabilities": {
                "tools": {},
                "resources": {},
                "prompts": {}
            },
            "serverInfo": {
                "name": "python-http-calculator",
                "version": "1.0.0",
                "description": "HTTP-based calculator server using Python MCP SDK"
            }
        }
    
    async def _handle_list_tools(self) -> Dict[str, Any]:
        """Handle list_tools request."""
        tools = [
            {
                "name": "add",
                "description": "Add two numbers via HTTP",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "a": {"type": "number", "description": "First number"},
                        "b": {"type": "number", "description": "Second number"}
                    },
                    "required": ["a", "b"]
                }
            },
            {
                "name": "subtract",
                "description": "Subtract two numbers via HTTP",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "a": {"type": "number", "description": "First number"},
                        "b": {"type": "number", "description": "Second number"}
                    },
                    "required": ["a", "b"]
                }
            },
            {
                "name": "multiply",
                "description": "Multiply two numbers via HTTP",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "a": {"type": "number", "description": "First number"},
                        "b": {"type": "number", "description": "Second number"}
                    },
                    "required": ["a", "b"]
                }
            },
            {
                "name": "divide",
                "description": "Divide two numbers via HTTP",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "a": {"type": "number", "description": "Dividend"},
                        "b": {"type": "number", "description": "Divisor (cannot be zero)"}
                    },
                    "required": ["a", "b"]
                }
            },
            {
                "name": "power",
                "description": "Raise a number to a power via HTTP",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "base": {"type": "number", "description": "Base number"},
                        "exponent": {"type": "number", "description": "Exponent"}
                    },
                    "required": ["base", "exponent"]
                }
            },
            {
                "name": "get_stats",
                "description": "Get HTTP server statistics",
                "inputSchema": {
                    "type": "object",
                    "properties": {}
                }
            },
            {
                "name": "get_history",
                "description": "Get calculation history",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "limit": {"type": "integer", "description": "Maximum history items", "default": 10}
                    }
                }
            }
        ]
        
        return {"tools": tools}
    
    async def _handle_tool_call(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """Handle tools/call request."""
        
        tool_name = params.get("name")
        arguments = params.get("arguments", {})
        
        try:
            if tool_name == "add":
                result = await self._add(arguments["a"], arguments["b"])
            elif tool_name == "subtract":
                result = await self._subtract(arguments["a"], arguments["b"])
            elif tool_name == "multiply":
                result = await self._multiply(arguments["a"], arguments["b"])
            elif tool_name == "divide":
                result = await self._divide(arguments["a"], arguments["b"])
            elif tool_name == "power":
                result = await self._power(arguments["base"], arguments["exponent"])
            elif tool_name == "get_stats":
                result = await self._get_stats()
            elif tool_name == "get_history":
                limit = arguments.get("limit", 10)
                result = await self._get_history(limit)
            else:
                raise ValueError(f"Unknown tool: {tool_name}")
            
            # Record operation in history (except for get_stats and get_history)
            if tool_name not in ["get_stats", "get_history"]:
                self.operation_count += 1
                self.history.append({
                    "operation": tool_name,
                    "arguments": arguments,
                    "result": result,
                    "timestamp": self._get_timestamp(),
                    "transport": "HTTP"
                })
                
                # Keep only last 50 operations
                if len(self.history) > 50:
                    self.history = self.history[-50:]
            
            return {
                "content": [
                    {
                        "type": "text",
                        "text": f"HTTP Calculator: {result}"
                    }
                ]
            }
            
        except Exception as e:
            return {
                "content": [
                    {
                        "type": "text",
                        "text": f"Error: {str(e)}"
                    }
                ],
                "isError": True
            }
    
    async def _add(self, a: float, b: float) -> str:
        """Add two numbers."""
        result = a + b
        return f"{a} + {b} = {result}"
    
    async def _subtract(self, a: float, b: float) -> str:
        """Subtract two numbers."""
        result = a - b
        return f"{a} - {b} = {result}"
    
    async def _multiply(self, a: float, b: float) -> str:
        """Multiply two numbers."""
        result = a * b
        return f"{a} × {b} = {result}"
    
    async def _divide(self, a: float, b: float) -> str:
        """Divide two numbers."""
        if b == 0:
            raise ValueError("Division by zero is not allowed")
        result = a / b
        return f"{a} ÷ {b} = {result}"
    
    async def _power(self, base: float, exponent: float) -> str:
        """Raise a number to a power."""
        result = base ** exponent
        return f"{base}^{exponent} = {result}"
    
    async def _get_stats(self) -> str:
        """Get HTTP server statistics."""
        uptime = int(time.time() - self.start_time)
        
        stats = {
            "server_type": "Python MCP HTTP Calculator",
            "transport": "HTTP",
            "port": 8000,
            "uptime_seconds": uptime,
            "total_operations": self.operation_count,
            "history_entries": len(self.history),
            "available_operations": ["add", "subtract", "multiply", "divide", "power"],
            "python_version": sys.version.split()[0],
            "http_features": [
                "RESTful endpoints",
                "JSON-RPC over HTTP",
                "Health check endpoint",
                "FastAPI documentation",
                "Async request handling"
            ],
            "performance": {
                "concurrent_requests": "Supported",
                "connection_pooling": "Client-side",
                "load_balancing": "Ready"
            }
        }
        
        return f"HTTP Server Statistics:\n{json.dumps(stats, indent=2)}"
    
    async def _get_history(self, limit: int) -> str:
        """Get calculation history."""
        if not self.history:
            return "No calculations performed yet via HTTP"
        
        recent_history = self.history[-limit:]
        history_lines = []
        
        for i, entry in enumerate(recent_history, 1):
            op = entry["operation"]
            args = entry["arguments"]
            result = entry["result"]
            timestamp = entry["timestamp"]
            
            history_lines.append(f"{i}. [{timestamp}] {op}({args}) → {result}")
        
        return f"HTTP Calculation History:\n" + "\n".join(history_lines)
    
    def _get_timestamp(self) -> str:
        """Get current timestamp."""
        import datetime
        return datetime.datetime.now().strftime("%H:%M:%S")
    
    def run(self, host: str = "localhost", port: int = 8000):
        """Run the HTTP server."""
        logger.info(f"Starting Python MCP HTTP Calculator Server on {host}:{port}")
        logger.info("Available endpoints:")
        logger.info(f"  - Health check: http://{host}:{port}/health")
        logger.info(f"  - MCP endpoint: http://{host}:{port}/mcp")
        logger.info(f"  - Documentation: http://{host}:{port}/docs")
        
        uvicorn.run(
            self.app,
            host=host,
            port=port,
            log_level="info"
        )


def main():
    """Main entry point for the Python MCP HTTP Calculator Server."""
    
    calculator = PythonHTTPCalculator()
    
    try:
        calculator.run()
    except KeyboardInterrupt:
        logger.info("Server shutting down...")
    except Exception as e:
        logger.error(f"Server error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()