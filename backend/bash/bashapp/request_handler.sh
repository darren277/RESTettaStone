#!/bin/bash

# Source the logger
source /usr/local/bin/logger.sh

send_response() {
    local STATUS="$1"
    local CONTENT_TYPE="$2"
    local RESPONSE="$3"

    info "Sending response: $STATUS"

    #echo -ne "HTTP/1.1 $STATUS\r\nContent-Type: $CONTENT_TYPE\r\nContent-Length: ${#RESPONSE}\r\n\r\n$RESPONSE"
    printf "HTTP/1.1 %s\r\nContent-Type: %s\r\nConnection: close\r\nContent-Length: %d\r\n\r\n%s" \
        "$STATUS" "$CONTENT_TYPE" "${#RESPONSE}" "$RESPONSE"
}

# Read the request line
read -r REQUEST_LINE
echo "Received request: $REQUEST_LINE" >&2
info "Received request: $REQUEST_LINE"

# Read headers
while IFS= read -r line && [ -n "$line" ] && [ "$line" != $'\r' ]; do
    info "Received header: $line"
    if [[ "$line" =~ Content-Length:\ *([0-9]+) ]]; then
        LENGTH="${BASH_REMATCH[1]}"
    fi
done

info "Content-Length: $LENGTH"

# Read body for POST/PUT requests
if [ -n "$LENGTH" ]; then
    read -n "$LENGTH" BODY
    echo "Received body: $BODY" >&2
    info "Received body: $BODY"
fi

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

