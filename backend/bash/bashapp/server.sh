#!/bin/bash

PORT=3039
DB_CONNECTION="postgresql://myusername:mypassword@172.18.0.21:5432/postgres"

send_response() {
    local STATUS="$1"
    local CONTENT_TYPE="$2"
    local RESPONSE="$3"

    echo -ne "HTTP/1.1 $STATUS\r\nContent-Type: $CONTENT_TYPE\r\nContent-Length: ${#RESPONSE}\r\n\r\n$RESPONSE"
}

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

# Infinite loop to handle multiple connections
while true; do
    echo "Listening on http://0.0.0.0:$PORT..."

    # Create a temporary file for the current request
    TMPFILE=$(/bin/mktemp)

    # Capture the full request including headers and body
    /usr/bin/nc -l -p "$PORT" > "$TMPFILE"

    # Read the request line
    REQUEST_LINE=$(/usr/bin/head -n 1 "$TMPFILE")

    if [[ "$REQUEST_LINE" =~ ^([A-Z]+)\ /(.*)\ HTTP ]]; then
        METHOD="${BASH_REMATCH[1]}"
        PATH="${BASH_REMATCH[2]}"
        echo "Method: $METHOD"
        echo "Path: /$PATH"

        # For POST/PUT requests, get the body
        if [[ "$METHOD" == "POST" || "$METHOD" == "PUT" ]]; then
            # Extract JSON body (assuming Content-Length header is present)
            BODY=$(/usr/bin/awk 'BEGIN{RS="\r\n\r\n"} NR==2' "$TMPFILE")
            # Extract email from JSON (basic parsing)
            ## TODO: Consider using `jq`: EMAIL=$(echo "$BODY" | jq -r '.email // empty')
            EMAIL=$(echo "$BODY" | /bin/grep -o '"email"[[:space:]]*:[[:space:]]*"[^"]*"' | /bin/sed 's/.*"email"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/')
        fi

        # Match the "/users/<id>" pattern
        if [[ "$PATH" =~ ^users/([0-9]+)$ ]]; then
            USER_ID="${BASH_REMATCH[1]}"

            case "$METHOD" in
                "GET")
                    QUERY_RESULT=$(echo "SELECT id, email FROM users WHERE id = $USER_ID;" | /usr/bin/psql "$DB_CONNECTION" -t -A)
                    if [[ $? -eq 0 && -n "$QUERY_RESULT" ]]; then
                        IFS="|" read -r ID EMAIL <<< "$QUERY_RESULT"
                        RESPONSE="{\"id\":$ID,\"email\":\"$EMAIL\"}"
                        send_response "200 OK" "application/json" "$RESPONSE"
                    else
                        send_response "404 Not Found" "application/json" '{"error":"User not found"}'
                    fi
                    ;;

                "PUT")
                    if [[ -n "$EMAIL" ]]; then
                        QUERY_RESULT=$(echo "UPDATE users SET email='$EMAIL' WHERE id=$USER_ID RETURNING id, email;" | /usr/bin/psql "$DB_CONNECTION" -t -A)
                        if [[ $? -eq 0 && -n "$QUERY_RESULT" ]]; then
                            IFS="|" read -r ID EMAIL <<< "$QUERY_RESULT"
                            RESPONSE="{\"id\":$ID,\"email\":\"$EMAIL\"}"
                            send_response "200 OK" "application/json" "$RESPONSE"
                        else
                            send_response "404 Not Found" "application/json" '{"error":"User not found"}'
                        fi
                    else
                        send_response "400 Bad Request" "application/json" '{"error":"Invalid request body"}'
                    fi
                    ;;

                "DELETE")
                    if echo "DELETE FROM users WHERE id=$USER_ID;" | /usr/bin/psql "$DB_CONNECTION" -t -A; then
                        send_response "204 No Content" "application/json" ""
                    else
                        send_response "404 Not Found" "application/json" '{"error":"User not found"}'
                    fi
                    ;;

                *)
                    send_response "405 Method Not Allowed" "application/json" '{"error":"Method not allowed"}'
                    ;;
            esac

        elif [[ "$PATH" == "users" ]]; then
            case "$METHOD" in
                "GET")
                    QUERY_RESULT=$(echo "SELECT array_to_json(array_agg(row_to_json(u))) FROM (SELECT id, email FROM users) u;" | /usr/bin/psql "$DB_CONNECTION" -t -A)
                    send_response "200 OK" "application/json" "${QUERY_RESULT:-[]}"
                    ;;

                "POST")
                    if [[ -n "$EMAIL" ]]; then
                        QUERY_RESULT=$(echo "INSERT INTO users (email) VALUES ('$EMAIL') RETURNING id, email;" | /usr/bin/psql "$DB_CONNECTION" -t -A)
                        if [[ $? -eq 0 ]]; then
                            IFS="|" read -r ID EMAIL <<< "$QUERY_RESULT"
                            RESPONSE="{\"id\":$ID,\"email\":\"$EMAIL\"}"
                            send_response "201 Created" "application/json" "$RESPONSE"
                        else
                            send_response "500 Internal Server Error" "application/json" '{"error":"Failed to create user"}'
                        fi
                    else
                        error "Invalid request body: $BODY"
                        send_response "400 Bad Request" "application/json" '{"error":"Invalid request body"}'
                    fi
                    ;;

                *)
                    send_response "405 Method Not Allowed" "application/json" '{"error":"Method not allowed"}'
                    ;;
            esac
        else
            send_response "404 Not Found" "application/json" '{"error":"Endpoint not found"}'
        fi
    fi

    # Clean up temporary file
    /bin/rm -f "$TMPFILE"
done
