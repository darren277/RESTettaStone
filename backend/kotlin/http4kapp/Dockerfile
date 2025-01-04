FROM gradle:7.5.1-jdk17-alpine as builder

ARG HTTP4KAPP_PORT
ENV HTTP4KAPP_PORT=$HTTP4KAPP_PORT
ENV PORT=$HTTP4KAPP_PORT

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

COPY ./src /app/src
COPY ./build.gradle.kts /app

# Temporary hacky workaround... I keep getting all kinds of complaints about the environment variable parsing but I'd rather deal with that later.
RUN rm -rf /app/src/main/resources/application.conf
RUN echo "server {" > /app/src/main/resources/application.conf
RUN echo "    port = $HTTP4KAPP_PORT" >> /app/src/main/resources/application.conf
RUN echo "}" >> /app/src/main/resources/application.conf

RUN echo "db {" >> /app/src/main/resources/application.conf
RUN echo "    pg_host = \"$PG_HOST\"" >> /app/src/main/resources/application.conf
RUN echo "    pg_port = $PG_PORT" >> /app/src/main/resources/application.conf
RUN echo "    pg_user = \"$PG_USER\"" >> /app/src/main/resources/application.conf
RUN echo "    pg_pass = \"$PG_PASS\"" >> /app/src/main/resources/application.conf
RUN echo "    pg_db = \"$PG_DB\"" >> /app/src/main/resources/application.conf
RUN echo "}" >> /app/src/main/resources/application.conf

RUN gradle clean build --no-daemon


FROM openjdk:17-alpine
WORKDIR /app

# Copy the built JAR from the build stage into this stage
COPY --from=builder /app/build/libs/*.jar app.jar

ARG HTTP4KAPP_PORT
ENV HTTP4KAPP_PORT=$HTTP4KAPP_PORT
ENV PORT=$HTTP4KAPP_PORT

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

EXPOSE $HTTP4KAPP_PORT
ENTRYPOINT ["java", "-jar", "app.jar"]
