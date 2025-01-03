#!/bin/bash

declare -A LOG_LEVELS=(
    ["DEBUG"]="0"
    ["INFO"]="1"
    ["WARN"]="2"
    ["ERROR"]="3"
)

CURRENT_LOG_LEVEL=${BASH_APP_LOG_LEVEL:-"INFO"}
LOG_FILE="bashapp.log"

log() {
    local level="$1"
    local message="$2"
    local line_number="${3:-${BASH_LINENO[0]}}"
    local timestamp=$(/bin/date '+%Y-%m-%d %H:%M:%S')

    # Convert log levels to numeric for comparison
    local current_level_num=${LOG_LEVELS[$CURRENT_LOG_LEVEL]}
    local msg_level_num=${LOG_LEVELS[$level]}

    # Only log if message level >= current log level
    if [[ $msg_level_num -ge $current_level_num ]]; then
        # Format the log message
        local log_message="[$timestamp] [${level}]: (${line_number}) ${message}"

        # Write to log file
        echo "$log_message" >> "$LOG_FILE"

        # Also print to stderr if it's an error
        if [[ "$level" == "ERROR" ]]; then
            echo "$log_message" >&2
        fi
    fi
}

# Convenience wrapper functions
debug() { log "DEBUG" "$1" "${2:-${BASH_LINENO[0]}}"; }
info() { log "INFO" "$1" "${2:-${BASH_LINENO[0]}}"; }
warn() { log "WARN" "$1" "${2:-${BASH_LINENO[0]}}"; }
error() { log "ERROR" "$1" "${2:-${BASH_LINENO[0]}}"; }
