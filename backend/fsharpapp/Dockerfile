# Adapted (and updated significantly) from: https://github.com/Brick-Abode/pldotnet/blob/master/Dockerfile

FROM ubuntu:22.04

ARG DOTNET_VERSION=8.0
ARG UBUNTU_VERSION=22.04
ARG PG_VERSION=9.6
ARG DEBIAN_FRONTEND=noninteractive
ARG RELEASE=jammy-pgdg

ARG FSHARPAPP_PORT
ENV FSHARPAPP_PORT=$FSHARPAPP_PORT
ENV PORT=$FSHARPAPP_PORT

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

RUN apt-get update && apt-get install -y gcc make wget git sudo gnupg

RUN wget -q https://packages.microsoft.com/config/ubuntu/$UBUNTU_VERSION/packages-microsoft-prod.deb -O packages-microsoft-prod.deb \
    && dpkg -i packages-microsoft-prod.deb \
    && apt-get update -qq \
    && apt-get install -y dotnet-sdk-"$DOTNET_VERSION"

RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && sh -c "echo deb http://apt.postgresql.org/pub/repos/apt/ $RELEASE main $PG_VERSION \
              >> /etc/apt/sources.list.d/postgresql.list"

RUN apt-get update -qq \
    && apt-get -y -o Dpkg::Options::=--force-confdef -o Dpkg::Options::="--force-confnew" install \
       postgresql-common \
       libpq-dev \
       postgresql-client-"$PG_VERSION" \
       libpq5 \
       libssl3 \
       libldap-2.5-0

RUN apt-get update -qq \
    && apt-get -y -o Dpkg::Options::=--force-confdef -o Dpkg::Options::="--force-confnew" install \
       postgresql-"$PG_VERSION" \
       postgresql-server-dev-"$PG_VERSION" \
    && chmod 777 /etc/postgresql/"$PG_VERSION"/main/pg_hba.conf

RUN echo "local   all         postgres                          trust" >  /etc/postgresql/"$PG_VERSION"/main/pg_hba.conf \
    && echo "local   all         all                               trust" >> /etc/postgresql/"$PG_VERSION"/main/pg_hba.conf \
    && echo "host    all         all         127.0.0.1/32          trust" >> /etc/postgresql/"$PG_VERSION"/main/pg_hba.conf \
    && echo "host    all         all         ::1/128               trust" >> /etc/postgresql/"$PG_VERSION"/main/pg_hba.conf

RUN /etc/init.d/postgresql restart \
    && createuser -U postgres -s root

WORKDIR /home/app

COPY . .

ENTRYPOINT dotnet run
