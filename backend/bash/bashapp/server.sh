#!/bin/bash

PORT="${PORT:-8080}"

export PGUSER="${PG_USER:-myusername}"
export PGPASSWORD="${PG_PASS:-mypassword}"
export PGHOST="${PG_HOST:-172.18.0.21}"
export PGPORT="${PG_PORT:-5432}"
export PGDATABASE="${PG_DB:-postgres}"

# TODO: Likely redundant.
DB_CONNECTION="postgresql://$PGUSER:$PGPASSWORD@$PGHOST:$PGPORT/$PGDATABASE"

echo "Starting server on port $PORT..."
/usr/bin/socat TCP-LISTEN:$PORT,reuseaddr,fork EXEC:/usr/local/bin/request_handler.sh
