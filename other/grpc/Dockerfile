FROM golang:latest

ENV PROTOBUF_VERSION 1.47.1

ARG GRPCSERVER_IP
ARG GRPCSERVER_PORT

ENV GRPCSERVER_IP=${GRPCSERVER_IP}
ENV GRPCSERVER_PORT=${GRPCSERVER_PORT}

RUN apt-get update && apt-get install -y protobuf-compiler libprotobuf-dev curl \
    && curl -sSL https://github.com/bufbuild/buf/releases/download/v${PROTOBUF_VERSION}/buf-Linux-x86_64 -o /usr/local/bin/buf \
    && chmod +x /usr/local/bin/buf
    #&& go install google.golang.org/protobuf/cmd/protoc-gen-go@latest

WORKDIR /app

COPY ./go.mod /app/
RUN go mod download
RUN go mod tidy

COPY ./buf.yaml ./buf.gen.yaml /app/
COPY ./server /app/server
COPY ./user/v1/user.proto /app/user/v1/user.proto

RUN go install google.golang.org/protobuf/cmd/protoc-gen-go@latest \
    && go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest

RUN buf generate
# COPY the generated folder to `/protos`
RUN cp -r /app/gen /protos

RUN go mod tidy

WORKDIR /app/server

RUN go build -o /go/bin/grpc2 main.go

#CMD tail -f /dev/null
CMD ["/go/bin/grpc2"]
