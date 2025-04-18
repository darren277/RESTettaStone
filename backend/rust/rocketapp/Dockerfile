FROM ubuntu:latest

FROM rustlang/rust:nightly-bullseye as builder

ARG ROCKETAPP_PORT
ARG ROCKETAPP_IP

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

WORKDIR /app
COPY ./Cargo.toml ./

RUN apt update
RUN apt install -y ca-certificates

RUN /usr/local/cargo/bin/rustup default nightly

# Test to make sure rustup and cargo installed properly:
RUN /usr/local/cargo/bin/rustup --version
RUN /usr/local/cargo/bin/cargo --version

COPY ./src ./src

RUN /usr/local/cargo/bin/cargo update
RUN /usr/local/cargo/bin/rustup update
RUN /usr/local/cargo/bin/rustup override set nightly

RUN /usr/local/cargo/bin/cargo fetch
RUN /usr/local/cargo/bin/cargo build --release --locked || true

# Note that for this to work, you will likely need to compile the app during the build process:
#FROM debian:buster-slim as runner

FROM rustlang/rust:nightly-bullseye as runner
WORKDIR /app

RUN mkdir -p /usr/local/cargo/registry/src

COPY --from=builder /usr/local/cargo/registry/src /usr/local/cargo/registry/src
COPY --from=builder /usr/local/cargo/bin /usr/local/cargo/bin
COPY --from=builder /app /app

ARG ROCKETAPP_PORT
ARG ROCKETAPP_IP

ENV ROCKETAPP_IP $ROCKETAPP_IP
ENV ROCKETAPP_PORT $ROCKETAPP_PORT

ENV PORT $ROCKETAPP_PORT

ENV ROCKET_ENV development

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

# Note that the following is required to bypass this ridiculous error: "failed to run custom build command for `pear_codegen v0.1.5`".
# See README (Gotchas) for more details.
#RUN /usr/local/cargo/bin/rustup override set nightly

EXPOSE $ROCKETAPP_PORT

COPY ./entrypoint.sh ./
RUN chmod +x ./entrypoint.sh

ENTRYPOINT ["./entrypoint.sh"]
#ENTRYPOINT tail -f /dev/null
