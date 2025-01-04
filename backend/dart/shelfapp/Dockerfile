FROM dart:stable AS build

ARG SHELFAPP_PORT
ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV SHELFAPP_PORT=$SHELFAPP_PORT
ENV PG_HOST=$PG_HOST
ENV PG_PORT=$PG_PORT
ENV PG_USER=$PG_USER
ENV PG_PASS=$PG_PASS
ENV PG_DB=$PG_DB

WORKDIR /app

# Copy the pubspec files and get dependencies
COPY pubspec.* /app/
RUN dart pub get

COPY ./bin /app/bin

RUN dart compile exe bin/server.dart -o /app/server

# Use a smaller base image (runtime) to reduce image size.
FROM dart:stable AS runtime

WORKDIR /app
COPY --from=build /app/server /app/

EXPOSE $SHELFAPP_PORT

ENTRYPOINT ["./server"]
