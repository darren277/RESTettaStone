# ================================
# Build image
# ================================
FROM swift:5.10-focal as build
# 6.0.1-noble

ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV PG_HOST=$PG_HOST
ENV PG_PORT=$PG_PORT
ENV PG_USER=$PG_USER
ENV PG_PASS=$PG_PASS
ENV PG_DB=$PG_DB

RUN export DEBIAN_FRONTEND=noninteractive DEBCONF_NONINTERACTIVE_SEEN=true && apt-get -q update && apt-get -q dist-upgrade -y && rm -rf /var/lib/apt/lists/*

# Install postgres...
RUN apt-get -q update && apt-get -q install -y libpq-dev && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# First just resolve dependencies: This creates a cached layer that can be reused as long as your Package.swift/Package.resolved files do not change.
COPY ./Package.* ./
RUN swift package resolve

COPY . .

# Build everything, with optimizations and test discovery
RUN swift build --enable-test-discovery -c release

WORKDIR /staging

RUN cp "$(swift build --package-path /build -c release --show-bin-path)/Run" ./

# Uncomment the next line if you need to load resources from the `Public` directory.
# Ensure that by default, neither the directory nor any of its contents are writable.
#RUN mv /build/Public ./Public && chmod -R a-w ./Public

# ================================
# Run image
# ================================
FROM swift:5.10-focal-slim

ARG SWIFTAPP_PORT
ENV PORT=$SWIFTAPP_PORT

# Make sure all system packages are up to date.
RUN export DEBIAN_FRONTEND=noninteractive DEBCONF_NONINTERACTIVE_SEEN=true && apt-get -q update && apt-get -q dist-upgrade -y && rm -r /var/lib/apt/lists/*

RUN useradd --user-group --create-home --system --skel /dev/null --home-dir /app vapor

WORKDIR /app

COPY --from=build --chown=vapor:vapor /staging /app

USER vapor:vapor

EXPOSE $PORT

ENTRYPOINT ./Run serve --env production --hostname 0.0.0.0 --port $PORT
