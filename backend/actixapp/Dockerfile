FROM rust:latest


ARG ACTIXAPP_HOST="0.0.0.0"
ARG ACTIXAPP_PORT

ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV HOST $ACTIXAPP_HOST
ENV PORT $ACTIXAPP_PORT

ENV PG_HOST $PG_HOST
ENV PG_PORT $PG_PORT
ENV PG_USER $PG_USER
ENV PG_PASS $PG_PASS
ENV PG_DB $PG_DB

EXPOSE $PORT


WORKDIR /app

RUN apt-get update

RUN apt-get install -y libpq-dev postgresql postgresql-contrib

RUN cargo install systemfd
RUN cargo install cargo-watch

RUN cargo install diesel_cli --no-default-features --features postgres

COPY ./src ./src
COPY ./Cargo.toml ./Cargo.toml
COPY ./entrypoint.sh ./entrypoint.sh

ENV DATABASE_URL="postgres://${PG_USER}:${PG_PASS}@${PG_HOST}:${PG_PORT}/${PG_DB}"

ENTRYPOINT ["./entrypoint.sh"]
