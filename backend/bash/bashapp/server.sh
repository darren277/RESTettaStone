#!/bin/bash

PORT=3039
DB_CONNECTION="postgresql://myusername:mypassword@172.18.0.21:5432/postgres"

# Infinite loop to handle multiple connections
while true; do
    echo "Listening on http://0.0.0.0:$PORT..."

    # Wait for an HTTP request and handle it
    /usr/bin/nc -l -p "$PORT" -q 1 > request.txt &
    wait $! # Wait for the connection to finish

    # Read the request to extract the path
    read -r REQUEST_LINE < request.txt
    echo "Request: $REQUEST_LINE"

    if [[ "$REQUEST_LINE" =~ ^([A-Z]+)\ /(.*)\ HTTP ]]; then
        METHOD="${BASH_REMATCH[1]}"
        PATH="${BASH_REMATCH[2]}"
        echo "Method: $METHOD"
        echo "Path: /$PATH"
        # Match the "/users/<id>" pattern
        if [[ "$PATH" =~ ^users/([0-9]+)$ ]]; then
            if [[ "$METHOD" == "PUT" ]]; then
                RESPONSE='{"error": "Method not yet implemented"}'
                CONTENT_TYPE="application/json"
            elif [[ "$METHOD" == "DELETE" ]]; then
                RESPONSE='{"error": "Method not yet implemented"}'
                CONTENT_TYPE="application/json"
            elif [[ "$METHOD" == "GET" ]]; then
                # Extract the user ID from the path
                USER_ID="${BASH_REMATCH[1]}"
                echo "Looking up user with ID: $USER_ID"

                # Run the SQL query for the specific user
                QUERY_RESULT=$(echo "SELECT id, name, email FROM users WHERE id = $USER_ID;" | /usr/bin/psql "$DB_CONNECTION" -t -A)

                # Check for query errors or no results
                if [[ $? -ne 0 || -z "$QUERY_RESULT" ]]; then
                    RESPONSE='{"error": "User not found or database error"}'
                    CONTENT_TYPE="application/json"
                else
                    # Parse the result into JSON
                    IFS="|" read -r ID NAME EMAIL <<< "$QUERY_RESULT"
                    RESPONSE=$(cat <<EOF
{
  "id": $ID,
  "name": "$(echo $NAME | sed 's/"/\\"/g')",
  "email": "$(echo $EMAIL | sed 's/"/\\"/g')"
}
EOF
)
                    CONTENT_TYPE="application/json"
                fi
            else
                RESPONSE='{"error": "Method not allowed"}'
                CONTENT_TYPE="application/json"
            fi
        elif [[ "$PATH" = "/users" ]]; then
            if [[ "$METHOD" == "POST" ]]; then
                RESPONSE='{"error": "Method not yet implemented"}'
                CONTENT_TYPE="application/json"
            elif [[ "$METHOD" == "GET" ]]; then
                RESPONSE='{"error": "Method not yet implemented"}'
                CONTENT_TYPE="application/json"

                # Run the SQL query to fetch the first user
                QUERY_RESULT=$(echo "SELECT * FROM users LIMIT 1;" | /usr/bin/psql "$DB_CONNECTION" -t -A)

                # Check for errors
                if [[ $? -ne 0 ]]; then
                    RESPONSE='{"error": "Database query failed"}'
                    CONTENT_TYPE="application/json"
                else
                    # Serialize the result into JSON
                    IFS="|" read -r ID NAME EMAIL <<< "$QUERY_RESULT"
                    RESPONSE=$(/bin/cat <<EOF
{
  "id": $ID,
  "name": "$(echo $NAME | /bin/sed 's/"/\\"/g')",
  "email": "$(echo $EMAIL | /bin/sed 's/"/\\"/g')"
}
EOF
)
                    CONTENT_TYPE="application/json"
                fi
            else
                RESPONSE='{"error": "Method not yet implemented"}'
                CONTENT_TYPE="application/json"
            fi
        else
            RESPONSE='{"error": "Endpoint not found"}'
            CONTENT_TYPE="application/json"
        fi
        # Prepare the HTTP response
        HTTP_RESPONSE="HTTP/1.1 200 OK\r
Content-Type: $CONTENT_TYPE\r
Content-Length: ${#RESPONSE}\r
\r
$RESPONSE"

        # Send the response back to the client
        echo -e "$HTTP_RESPONSE" | /usr/bin/nc -l -p "$PORT" -q 1
    fi
done
