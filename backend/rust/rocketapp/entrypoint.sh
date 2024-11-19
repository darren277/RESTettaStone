#!/bin/sh

echo "\n[default]\naddress = '$ROCKETAPP_IP'\nport = $ROCKETAPP_PORT" > ./Rocket.toml
echo "\n[development]\naddress = '$ROCKETAPP_IP'\nport = $ROCKETAPP_PORT" >> ./Rocket.toml
echo "\n[debug]\naddress = '$ROCKETAPP_IP'\nport = $ROCKETAPP_PORT" >> ./Rocket.toml
echo "\n[staging]\naddress = '127.0.0.1'\nport = $ROCKETAPP_PORT" >> ./Rocket.toml
echo "\n[production]\naddress = '127.0.0.1'\nport = $ROCKETAPP_PORT" >> ./Rocket.toml

# ROCKET_LOG_LEVEL=debug
ROCKET_ENV=development /usr/local/cargo/bin/cargo run
