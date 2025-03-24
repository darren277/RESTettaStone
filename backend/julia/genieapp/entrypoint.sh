#!/bin/bash
set -x  # Print all commands

echo "Checking port usage before starting:"
# Install netstat if needed
if ! command -v netstat &> /dev/null; then
    echo "netstat not found, installing net-tools..."
    apt-get update && apt-get install -y net-tools
fi
netstat -tulpn | grep ${GENIEAPP_PORT:-3051} || echo "No process using port ${GENIEAPP_PORT:-3051}"

# Start Julia with explicit control over the startup process
julia --project -e '
using Genie

# Set explicit config before anything else
port = parse(Int, get(ENV, "GENIEAPP_PORT", "3051"))
Genie.config.server_port = port
Genie.config.server_host = "0.0.0.0"
Genie.config.run_as_server = true
Genie.config.websockets_server = false

# Load the routes from app.jl without letting Genie auto-start
include("app.jl")

# Start the server explicitly with only HTTP
println("Starting server on 0.0.0.0:$(port)")
up(port, "0.0.0.0", webpipes_autostart=false, server_handle_static_files=false, async=false)
'
