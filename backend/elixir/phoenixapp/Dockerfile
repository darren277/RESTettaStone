# Use an official Elixir image as the base image
FROM elixir:1.18-alpine

ARG PHOENIXAPP_PORT
ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV PHOENIXAPP_PORT=$PHOENIXAPP_PORT
ENV PG_HOST=$PG_HOST
ENV PG_PORT=$PG_PORT
ENV PG_USER=$PG_USER
ENV PG_PASS=$PG_PASS
ENV PG_DB=$PG_DB

# Set environment variables for Phoenix and Elixir
ENV MIX_ENV=prod \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    ERL_AFLAGS="-kernel inet_dist_listen_min 9100 inet_dist_listen_max 9109"

# Install build tools and dependencies
RUN apk add --no-cache build-base git npm postgresql-client

# Install Hex and Rebar (Elixir build tools)
RUN mix local.hex --force && mix local.rebar --force

# Create and set the application working directory
WORKDIR /app

# Copy the mix files and install dependencies
COPY mix.exs ./
RUN mix deps.get --only prod && mix deps.compile

# Copy the rest of the application code
COPY ./config ./config
COPY ./lib ./lib

# Build the Phoenix application
RUN mix compile && mix release

# Expose the port for the Phoenix server
EXPOSE $PHOENIXAPP_PORT

# Start the application
CMD ["_build/prod/rel/phoenixapp/bin/phoenixapp", "start"]
