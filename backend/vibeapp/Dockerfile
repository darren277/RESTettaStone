FROM ubuntu:latest

ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV PG_HOST=${PG_HOST}
ENV PG_PORT=${PG_PORT}
ENV PG_USER=${PG_USER}
ENV PG_PASS=${PG_PASS}
ENV PG_DB=${PG_DB}

ARG VIBEAPP_PORT
ENV PORT=${VIBEAPP_PORT}

RUN apt-get update && apt-get install -y xz-utils curl wget libsdl2-dev libsdl2-image-dev
RUN curl -fsS https://dlang.org/install.sh | bash -s dmd
RUN wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list

RUN apt-get install -y dub
RUN apt-get install -y libssl-dev libevent-dev
RUN apt-get install -y gnupg

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get -y install libpq-dev

# is this redundant?
RUN sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN apt-get -y install postgresql

RUN apt-get install -y libphobos-dev

# install dmd
COPY install_dmd.sh ./install_dmd.sh
RUN chmod +x ./install_dmd.sh
RUN ./install_dmd.sh

RUN apt-get install -y gcc

#RUN apt-get install -y openssl

COPY src/app.d ./source/app.d
COPY dub.sdl ./dub.sdl

#USER nobody

EXPOSE ${PORT}

#ENTRYPOINT ["dub", "run", "--single", "/app.d"]
ENTRYPOINT ["dub", "run", "-q"]
