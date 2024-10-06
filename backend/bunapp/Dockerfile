FROM oven/bun

ARG BUNAPP_PORT
ENV BUNAPP_PORT=$BUNAPP_PORT
ENV PORT=$BUNAPP_PORT

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
COPY package.json package.json
RUN touch bun.lockb
RUN bun install
RUN bun install bunrest
RUN bun install postgres

COPY ./index.js index.js
EXPOSE $PORT

ENTRYPOINT bun index.js --port $PORT
