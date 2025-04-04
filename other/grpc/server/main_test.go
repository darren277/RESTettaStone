package main

import (
	"context"
	"log"
	"net"
	"testing"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/test/bufconn"

	userpb "example.com/mygrpcapp/gen/go/user/v1"
)

func TestUserService_GetUser(t *testing.T) {
	lis := bufconn.Listen(1024 * 1024)
	t.Cleanup(func() {
		lis.Close()
	})

	srv := grpc.NewServer()
	t.Cleanup(func() {
		srv.Stop()
	})

	svc := userService{}
	userpb.RegisterUserServiceServer(srv, &svc)

	go func() {
		if err := srv.Serve(lis); err != nil {
			log.Fatalf("srv.Serve %v", err)
		}
	}()

	dialer := func(context.Context, string) (net.Conn, error) {
		return lis.Dial()
	}

	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	t.Cleanup(func() {
		cancel()
	})

	conn, err := grpc.DialContext(ctx, "", grpc.WithContextDialer(dialer), grpc.WithInsecure())
	t.Cleanup(func() {
		conn.Close()
	})
	if err != nil {
		t.Fatalf("grpc.DialContext %v", err)
	}

	client := userpb.NewUserServiceClient(conn)
	res, err := client.GetUser(context.Background(), &userpb.GetUserRequest{Uuid: "123"})
	if err != nil {
		t.Fatalf("client.GetUser %v", err)
	}

	if res.User.Uuid != "123" && res.User.FullName != "User1" {
		t.Fatalf("Unexpected values %v", res.User)
	}
}
