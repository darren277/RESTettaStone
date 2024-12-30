#!/bin/bash

PORT=3039
DB_CONNECTION="postgresql://myusername:mypassword@172.18.0.21:5432/postgres"

echo "Starting server on port $PORT..."
/usr/bin/socat TCP-LISTEN:$PORT,reuseaddr,fork EXEC:/usr/local/bin/request_handler.sh
