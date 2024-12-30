#!/bin/bash

# Function to extract headers from raw request
# Usage: parse_headers "raw_request"
parse_headers() {
    local RAW_REQUEST="$1"
    declare -A HEADERS

    # Read line by line until we hit empty line
    while IFS= read -r line; do
        # Break on empty line or just \r
        [[ -z "$line" || "$line" == $'\r' ]] && break

        # Skip the request line
        [[ "$line" =~ ^[A-Z]+\ /.*\ HTTP ]] && continue

        # Extract header name and value
        if [[ "$line" =~ ^([^:]+):\ *(.*)$ ]]; then
            local header_name="${BASH_REMATCH[1]}"
            local header_value="${BASH_REMATCH[2]}"
            # Trim carriage return if present
            header_value="${header_value%$'\r'}"
            HEADERS["$header_name"]="$header_value"
        fi
    done <<< "$RAW_REQUEST"

    declare -p HEADERS
}

# Function to get a specific header value
# Usage: get_header "header_name"
get_header() {
    local HEADER_NAME="$1"
    echo "${HEADERS[$HEADER_NAME]}"
}

# Function to extract a JSON field value from a request body
# Usage: parse_body_field "field_name" "request_body"
parse_body_field() {
    local FIELD_NAME="$1"
    local REQUEST_BODY="$2"

    if command -v jq >/dev/null 2>&1; then
        echo "$REQUEST_BODY" | jq -r ".$FIELD_NAME // empty"
        return
    fi

    ## TODO: Consider using `jq`: EMAIL=$(echo "$BODY" | jq -r '.email // empty')

    echo "$REQUEST_BODY" | \
        /bin/grep -o "\"$FIELD_NAME\"[[:space:]]*:[[:space:]]*\"[^\"]*\"" | \
        /bin/sed "s/.*\"$FIELD_NAME\"[[:space:]]*:[[:space:]]*\"\([^\"]*\)\".*/\1/"
}

# Function to extract the request body from raw request
# Usage: extract_body "raw_request"
extract_body() {
    local RAW_REQUEST="$1"
    # Get everything after the blank line
    /usr/bin/awk 'BEGIN{RS=""; FS="\n"} NR==2{print}' <<< "$RAW_REQUEST"
}

# Main request parsing function that returns all parsed data
# Usage: eval "$(parse_request "$METHOD" "$RAW_REQUEST")"
parse_request() {
    local METHOD="$1"
    local RAW_REQUEST="$2"

    # Create a new associative array for headers
    echo "declare -A HEADERS"

    # Parse headers and get them into calling scope
    eval "$(parse_headers "$RAW_REQUEST")"

    # Extract Content-Length and Content-Type for convenience
    echo "CONTENT_LENGTH=\"${HEADERS['Content-Length']}\""
    echo "CONTENT_TYPE=\"${HEADERS['Content-Type']}\""

    # Parse body for POST/PUT requests
    if [[ "$METHOD" == "POST" || "$METHOD" == "PUT" ]]; then
        local PARSED_BODY=$(extract_body "$RAW_REQUEST" "${HEADERS['Content-Length']}")
        # If we have a JSON body, parse the email field
        if [[ "${HEADERS['Content-Type']}" == "application/json" ]]; then
            local EMAIL=$(parse_body_field "email" "$PARSED_BODY")
            echo "BODY=\"$PARSED_BODY\""
            echo "EMAIL=\"$EMAIL\""
        fi
    fi
}
