#!/bin/bash

go mod vendor

# Note that this may not be ideal (copying the protos to the go src directory).
# Keeping it for testing purposes at least.
mkdir -p /usr/local/go/src/protos/gen/go/user/v1
cp -r /protos/go/user/v1 /usr/local/go/src/protos/gen/go/user/

go build -o grpc-client client.go
