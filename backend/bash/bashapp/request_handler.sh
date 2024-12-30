#!/bin/bash

source /usr/local/bin/logger.sh
source /usr/local/bin/request_parser.sh

PG_HOST="172.18.0.21"

send_response() {
    local STATUS="$1"
    local CONTENT_TYPE="$2"
    local RESPONSE="$3"

    info "Sending response: $STATUS"

    #echo -ne "HTTP/1.1 $STATUS\r\nContent-Type: $CONTENT_TYPE\r\nContent-Length: ${#RESPONSE}\r\n\r\n$RESPONSE"
    printf "HTTP/1.1 %s\r\nContent-Type: %s\r\nConnection: close\r\nContent-Length: %d\r\n\r\n%s" \
        "$STATUS" "$CONTENT_TYPE" "${#RESPONSE}" "$RESPONSE"
}

# GET /users
handle_list_users() {
    QUERY_RESULT=$(echo "SELECT array_to_json(array_agg(row_to_json(u))) FROM (SELECT id, email FROM users) u;" | /usr/bin/psql -h $PG_HOST "$DB_CONNECTION" -t -A)
    send_response "200 OK" "application/json" "${QUERY_RESULT:-[]}"
}

# POST /users
handle_create_user() {
    local EMAIL="$1"

    if [[ -n "$EMAIL" ]]; then
        QUERY_RESULT=$(echo "INSERT INTO users (email) VALUES ('$EMAIL') RETURNING id, email;" | /usr/bin/psql -h $PG_HOST "$DB_CONNECTION" -t -A)
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
}

# GET /users/:id
handle_get_user() {
    local USER_ID="$1"

    QUERY_RESULT=$(echo "SELECT id, email FROM users WHERE id = $USER_ID;" | /usr/bin/psql -h $PG_HOST "$DB_CONNECTION" -t -A)
    if [[ $? -eq 0 && -n "$QUERY_RESULT" ]]; then
        IFS="|" read -r ID EMAIL <<< "$QUERY_RESULT"
        RESPONSE="{\"id\":$ID,\"email\":\"$EMAIL\"}"
        send_response "200 OK" "application/json" "$RESPONSE"
    else
        send_response "404 Not Found" "application/json" '{"error":"User not found"}'
    fi
}

# PUT /users/:id
handle_update_user() {
    local USER_ID="$1"
    local EMAIL="$2"

    if [[ -n "$EMAIL" ]]; then
        QUERY_RESULT=$(echo "UPDATE users SET email='$EMAIL' WHERE id=$USER_ID RETURNING id, email;" | /usr/bin/psql -h $PG_HOST "$DB_CONNECTION" -t -A)
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
}

# DELETE /users/:id
handle_delete_user() {
    local USER_ID="$1"

    if echo "DELETE FROM users WHERE id=$USER_ID;" | /usr/bin/psql -h $PG_HOST "$DB_CONNECTION" -t -A; then
        send_response "204 No Content" "application/json" ""
    else
        send_response "404 Not Found" "application/json" '{"error":"User not found"}'
    fi
}

# Method not allowed handler
handle_method_not_allowed() {
    send_response "405 Method Not Allowed" "application/json" '{"error":"Method not allowed"}'
}

# Read the full request, including body
read_full_request() {
    # Read first line (request line)
    read -r first_line
    local request="$first_line"$'\n'

    # Read headers until empty line
    while IFS= read -r line && [[ -n "$line" ]] && [[ "$line" != $'\r' ]]; do
        request+="$line"$'\n'
    done

    # Add the empty line that separates headers from body
    request+=$'\n'

    # If we have Content-Length, read the body
    if [[ "$request" =~ Content-Length:\ *([0-9]+) ]]; then
        local length="${BASH_REMATCH[1]}"
        # Read exact number of bytes for body
        local body=$(dd bs=1 count="$length" 2>/dev/null)
        request+="$body"
    fi

    echo "$request"
}

# Read the complete request
REQUEST=$(read_full_request)
REQUEST_LINE=$(echo "$REQUEST" | head -n1)
echo "[DEBUG] Raw request: $REQUEST" >&2
echo "[DEBUG] Request line: $REQUEST_LINE" >&2

if [[ "$REQUEST_LINE" =~ ^([A-Z]+)\ /(.*)\ HTTP ]]; then
    METHOD="${BASH_REMATCH[1]}"
    PATH="${BASH_REMATCH[2]}"
    echo "Method: $METHOD"
    echo "Path: /$PATH"

    # Parse headers and store them in HEADERS associative array
    declare -A HEADERS
    eval "$(parse_headers "$REQUEST")"

    # Parse the entire request and get all the variables we need
    eval "$(parse_request "$METHOD" "$REQUEST")"

    echo "[DEBUG] Content-Type: $CONTENT_TYPE" >&2
    echo "[DEBUG] Content-Length: $CONTENT_LENGTH" >&2
    echo "[DEBUG] Body: $BODY" >&2

    # For POST/PUT requests, parse the body
    if [[ "$METHOD" == "POST" || "$METHOD" == "PUT" ]]; then
        BODY=$(extract_body "$REQUEST")
        EMAIL=$(parse_body_field "email" "$BODY")

        echo "[DEBUG] Email: $EMAIL" >&2
    fi

    # Match the "/users/<id>" pattern
    if [[ "$PATH" =~ ^users/([0-9]+)$ ]]; then
        USER_ID="${BASH_REMATCH[1]}"

        case "$METHOD" in
            "GET")
                handle_get_user "$USER_ID"
                ;;

            "PUT")
                handle_update_user "$USER_ID" "$EMAIL"
                ;;

            "DELETE")
                handle_delete_user "$USER_ID"
                ;;

            *)
                handle_method_not_allowed
                ;;
        esac

    elif [[ "$PATH" == "users" ]]; then
        case "$METHOD" in
            "GET")
                handle_list_users
                ;;

            "POST")
                handle_create_user "$EMAIL"
                ;;

            *)
                handle_method_not_allowed
                ;;
        esac
    else
        send_response "404 Not Found" "application/json" '{"error":"Endpoint not found"}'
    fi
fi

