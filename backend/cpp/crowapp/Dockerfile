# Adapted from: https://stackoverflow.com/a/73355270/10973023

ARG CROWAPP_PORT
ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

FROM ubuntu:latest

ENV CROWAPP_PORT=$CROWAPP_PORT
ENV PG_HOST=$PG_HOST
ENV PG_PORT=$PG_PORT
ENV PG_USER=$PG_USER
ENV PG_PASS=$PG_PASS
ENV PG_DB=$PG_DB

RUN apt-get update -y
RUN apt-get upgrade -y

# reinstall certificates, otherwise git clone command might result in an error
RUN apt-get install --reinstall ca-certificates -y

# install developer dependencies
RUN apt-get install -y git build-essential cmake --no-install-recommends

# install vcpkg package manager
RUN git clone https://github.com/microsoft/vcpkg
RUN apt-get install -y curl zip
RUN vcpkg/bootstrap-vcpkg.sh

# install crow package
RUN apt-get install -y pkg-config
RUN /vcpkg/vcpkg install crow

RUN apt-get install -y libpqxx-dev
RUN apt-get install -y nano

COPY . /project

WORKDIR /build

# compile with CMake
RUN bash -c "cmake ../project && cmake --build ."

# run executable (name has to match with CMakeLists.txt file)
# and log to file
CMD [ "nohup", "./app", "&" ]
