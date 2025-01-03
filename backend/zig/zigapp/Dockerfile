FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive
WORKDIR /root

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

ARG ZIGAPP_PORT
ENV ZIGAPP_PORT=$ZIGAPP_PORT
ENV PORT=$ZIGAPP_PORT

ARG ZIG_VERSION=0.13.0
ARG ARCH=x86_64

RUN apt update && apt -y upgrade && apt -y install wget ca-certificates
RUN apt -y install xz-utils

# Download prebuilt Zig binary
RUN wget https://ziglang.org/download/${ZIG_VERSION}/zig-linux-${ARCH}-${ZIG_VERSION}.tar.xz && \
    tar xf zig-linux-${ARCH}-${ZIG_VERSION}.tar.xz && \
    mv zig-linux-${ARCH}-${ZIG_VERSION} /usr/local/zig

ENV PATH="/usr/local/zig:${PATH}"

COPY . .

RUN zig fetch --save git+https://github.com/karlseguin/pg.zig#zig-0.13
RUN zig fetch --save git+https://github.com/zigzap/zap.git#v0.9.1

EXPOSE $PORT

RUN ["zig", "build"]

ENTRYPOINT ["./zig-out/bin/zigapp"]
