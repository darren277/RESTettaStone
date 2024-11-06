package main

import (
    "context"
    "log"
    "time"

    "google.golang.org/grpc"
    userpb "protos/gen/go/user/v1"
)

func main() {
    conn, err := grpc.Dial("localhost:50051", grpc.WithInsecure())
    if err != nil {
        log.Fatalf("Failed to connect: %v", err)
    }
    defer conn.Close()

    client := userpb.NewUserServiceClient(conn)

    req := &userpb.GetUserRequest{
        //UserId: "123",
        Uuid: "123",
    }

    ctx, cancel := context.WithTimeout(context.Background(), time.Second)
    defer cancel()

    res, err := client.GetUser(ctx, req)
    if err != nil {
        log.Fatalf("Error calling GetUser: %v", err)
    }

    log.Printf("Response from server: %v", res)
}
