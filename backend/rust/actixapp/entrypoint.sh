#!/bin/bash
echo -n "Running migrations..."
diesel database setup
echo -n "done."

env

echo "[print_schema]" > diesel.toml
echo "file = \"src/schema.rs\"" >> diesel.toml

if [[ -z "${DEVELOPMENT}" ]]; then
    echo "Running in production!"
    cargo run
else
    echo "Port = $PORT"
    systemfd --no-pid -s http::0.0.0.0:${PORT} -- cargo watch -x run
fi
