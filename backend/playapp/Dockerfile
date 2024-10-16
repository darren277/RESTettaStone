# Step 1: Use a base image with SBT and Java installed
#FROM sbtscala/scala-sbt as builder
FROM sbtscala/scala-sbt:eclipse-temurin-alpine-22_36_1.10.2_3.5.1 as builder

WORKDIR /app

COPY ./app /app/app
COPY ./project /app/project
COPY ./conf /app/conf
COPY ./build.sbt /app

# Build the application using sbt
RUN sbt clean compile stage


RUN apk update
RUN apk add libpq-dev postgresql postgresql-contrib


# Use a lightweight base image with Java to run the built Play app
FROM openjdk:11-jre-slim

ARG PLAYAPP_PORT
ENV PLAYAPP_PORT=$PLAYAPP_PORT

ARG PG_HOST
ARG PG_PORT
ARG PG_DB
ARG PG_USER
ARG PG_PASS

ENV PG_HOST=$PG_HOST
ENV PG_PORT=$PG_PORT
ENV PG_DB=$PG_DB
ENV PG_USER=$PG_USER
ENV PG_PASS=$PG_PASS

ARG SECRET_KEY
ENV SECRET_KEY=$SECRET_KEY

WORKDIR /app

COPY --from=builder /app/target/universal/stage /app

EXPOSE $PLAYAPP_PORT

ENV PG_URL="db.default.url=\"jdbc:postgresql://${PG_HOST}:${PG_PORT}/${PG_DB}\""
ENV PG_USERNAME="db.default.username=${PG_USER}"
ENV PG_PASSWORD="db.default.password=${PG_PASS}"

RUN echo 'db.default.driver=org.postgresql.Driver' >> /app/conf/application.conf
RUN echo $PG_URL >> /app/conf/application.conf
RUN echo $PG_USERNAME >> /app/conf/application.conf
RUN echo $PG_PASSWORD >> /app/conf/application.conf

ENV KEY="play.http.secret.key=${SECRET_KEY}"
RUN echo $KEY >> /app/conf/application.conf

CMD ./bin/playapp -Dhttp.port=$PLAYAPP_PORT
