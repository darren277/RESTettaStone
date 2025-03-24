#!/bin/bash
set -x  # Print all commands

echo "Checking port usage before starting:"
netstat -tulpn | grep 3051 || echo "No process using port 3051"

# Start Julia with explicit control over the startup process
julia --project -e '
using Genie

# Set explicit config before anything else
Genie.config.server_port = 3051
Genie.config.server_host = "0.0.0.0"
Genie.config.run_as_server = true
Genie.config.webpipes_enable = false
Genie.config.websockets_server = false
Genie.config.startup_eval_file = ""

# Load the routes from app.jl without letting Genie auto-start
include("app.jl")

# Start the server explicitly with only HTTP
println("Starting server on 0.0.0.0:3051")
up(3051, "0.0.0.0", webpipes_autostart=false, server_handle_static_files=false, async=false)
'
