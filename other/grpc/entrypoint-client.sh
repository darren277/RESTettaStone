#!/bin/bash

go mod vendor

# Note that this may not be ideal (copying the protos to the go src directory).
# Keeping it for testing purposes at least.
cp -r /protos /usr/local/go/src/

go build -o grpc-client client.go
