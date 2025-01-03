package main

import (
    "os"
    "context"
    "log"
    "time"

    "google.golang.org/grpc"
    userpb "protos/gen/go/user/v1"
)

func main() {
    server_ip := os.Getenv("GRPCSERVER_IP")
    server_port := os.Getenv("GRPCSERVER_PORT")
    server_address := server_ip + ":" + server_port
    conn, err := grpc.Dial(server_address, grpc.WithInsecure())
    if err != nil {
        log.Fatalf("Failed to connect: %v", err)
    }
    defer conn.Close()

    client := userpb.NewUserServiceClient(conn)


    log.Println("About to GET a user...")

    req := &userpb.GetUserRequest{
        Id: 1,
    }

    ctx, cancel := context.WithTimeout(context.Background(), time.Second)
    defer cancel()

    res, err := client.GetUser(ctx, req)
    if err != nil {
        log.Fatalf("Error calling GetUser: %v", err)
    }

    log.Printf("Response from server: %v", res)


    log.Println("About to CREATE a user...")

    createReq := &userpb.CreateUserRequest{
        Email: "testing@mail.com",
    }

    createCtx, createCancel := context.WithTimeout(context.Background(), time.Second)
    defer createCancel()

    createRes, createErr := client.CreateUser(createCtx, createReq)
    if createErr != nil {
        log.Fatalf("Error calling CreateUser: %v", createErr)
    }

    log.Printf("Response from server: %v", createRes)


    log.Println("About to UPDATE a user...")

    updateReq := &userpb.UpdateUserRequest{
        Id: 1,
        Email: "updated@mail.com",
    }

    updateCtx, updateCancel := context.WithTimeout(context.Background(), time.Second)
    defer updateCancel()

    updateRes, updateErr := client.UpdateUser(updateCtx, updateReq)
    if updateErr != nil {
        log.Fatalf("Error calling UpdateUser: %v", updateErr)
    }

    log.Printf("Response from server: %v", updateRes)


    log.Println("About to DELETE a user...")

    deleteReq := &userpb.DeleteUserRequest{
        Id: 1,
    }

    deleteCtx, deleteCancel := context.WithTimeout(context.Background(), time.Second)
    defer deleteCancel()

    deleteRes, deleteErr := client.DeleteUser(deleteCtx, deleteReq)
    if deleteErr != nil {
        log.Fatalf("Error calling DeleteUser: %v", deleteErr)
    }

    log.Printf("Response from server: %v", deleteRes)


    log.Println("About to GET a list of users...")

    listReq := &userpb.ListUsersRequest{}

    listCtx, listCancel := context.WithTimeout(context.Background(), time.Second)
    defer listCancel()

    listRes, listErr := client.ListUsers(listCtx, listReq)
    if listErr != nil {
        log.Fatalf("Error calling ListUsers: %v", listErr)
    }

    log.Printf("Response from server: %v", listRes)
}
