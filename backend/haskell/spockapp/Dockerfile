FROM ubuntu:22.04

ENV CABAL_VERSION=3.10.2.1
ENV GHCUP_VERSION=8.8.4

ENV POSTGRES_DISTRO jammy

ARG SPOCKAPP_PORT
ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV PORT $SPOCKAPP_PORT
ENV PG_HOST $PG_HOST
ENV PG_PORT $PG_PORT
ENV PG_USER $PG_USER
ENV PG_PASS $PG_PASS
ENV PG_DB $PG_DB

RUN apt-get update && apt-get install --assume-yes g++ curl gcc libgmp-dev make xz-utils zlib1g-dev

RUN curl -L https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup -o /usr/bin/ghcup && chmod +x /usr/bin/ghcup

RUN ghcup install cabal $CABAL_VERSION && ghcup install ghc $GHCUP_VERSION && ghcup set $GHCUP_VERSION

ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"

WORKDIR /project

COPY spockapp.cabal /project

ENV DEBIAN_FRONTEND=noninteractive
#RUN ln -fs /usr/share/zoneinfo/Etc/UTC /etc/localtime && echo "Vancouver/PST" > /etc/timezone && dpkg-reconfigure -f noninteractive tzdata

## install postgresql version 14
RUN apt-get update && apt-get install -y gnupg wget
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | gpg --dearmor -o /usr/share/keyrings/postgresql.gpg

RUN echo "deb [signed-by=/usr/share/keyrings/postgresql.gpg] http://apt.postgresql.org/pub/repos/apt $POSTGRES_DISTRO-pgdg main" | tee /etc/apt/sources.list.d/pgdg.list
RUN apt-get update && apt-get install -y postgresql-14
RUN apt-get update && apt-get install -y postgresql-client-14
RUN apt-get update && apt-get install -y libpq-dev


RUN cabal update

COPY ./app/Main.hs /project/app/Main.hs
COPY ./src/ /project/src

ENV HOST 0.0.0.0
EXPOSE $PORT

RUN cabal build

CMD cabal run
