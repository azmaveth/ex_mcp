@echo off
REM ExMCP v2 Transport Demo Script (Windows)
REM Starts all servers, runs the universal client, then cleans up

setlocal enabledelayedexpansion

echo ==========================================
echo ExMCP v2 Complete Transport Demo
echo ==========================================
echo.
echo This script will:
echo 1. Start all transport servers
echo 2. Run the universal client
echo 3. Stop all servers and clean up
echo.

echo Starting servers...
echo.

REM Start Native server
echo   Starting Native server...
start /B elixir --name server@127.0.0.1 --cookie hello_mcp_demo_cookie hello_server_native.exs --distributed > nul 2>&1

REM Start HTTP server
echo   Starting HTTP server...
start /B elixir hello_server_http.exs > nul 2>&1

REM Start SSE server
echo   Starting SSE server...
start /B elixir hello_server_sse.exs > nul 2>&1

echo   All servers started
echo.

REM Wait for servers to initialize
echo Waiting for servers to initialize...
timeout /t 5 /nobreak > nul

REM Test servers (basic check)
echo Testing servers...
curl -s http://localhost:3000 > nul 2>&1
if errorlevel 1 (
    echo   HTTP server not responding
    goto cleanup
)

curl -s http://localhost:3001 > nul 2>&1
if errorlevel 1 (
    echo   SSE server not responding  
    goto cleanup
)

echo   All servers ready
echo.

REM Run the client demo
echo Running universal client demo...
echo ----------------------------------------

elixir --name client@127.0.0.1 --cookie hello_mcp_demo_cookie hello_client_all.exs

echo ----------------------------------------
echo.

if errorlevel 1 (
    echo Client demo failed
    goto cleanup
) else (
    echo Client demo completed successfully
)

echo.
echo Demo complete!

:cleanup
echo.
echo Cleaning up servers...

REM Kill Elixir processes (this is a bit aggressive but effective for demo)
taskkill /f /im "elixir.exe" > nul 2>&1
taskkill /f /im "erl.exe" > nul 2>&1

echo Cleanup complete
pause