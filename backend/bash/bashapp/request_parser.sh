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

    # Return headers as a formatted string that can be eval'd
    for key in "${!HEADERS[@]}"; do
        echo "HEADERS[\"$key\"]=\"${HEADERS[$key]}\""
    done
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
    local CONTENT_LENGTH

    # Get Content-Length if it exists
    CONTENT_LENGTH=$(get_header "Content-Length")

    if [[ -n "$CONTENT_LENGTH" ]]; then
        # Use awk to get everything after the double newline
        local BODY=$(/usr/bin/awk 'BEGIN{RS="\r\n\r\n"} NR==2' <<< "$RAW_REQUEST")
        echo "$BODY"
    fi
}

# Main request parsing function
# Usage: parse_request "method" "raw_request"
parse_request() {
    local METHOD="$1"
    local RAW_REQUEST="$2"

    # Parse headers first
    declare -A HEADERS
    eval "$(parse_headers "$RAW_REQUEST")"

    # Only parse body for POST/PUT
    if [[ "$METHOD" == "POST" || "$METHOD" == "PUT" ]]; then
        local PARSED_BODY=$(extract_body "$RAW_REQUEST")
        echo "$PARSED_BODY"
    fi
}
