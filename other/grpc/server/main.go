// Adapted from: https://github.com/MarioCarrion/grpc-microservice-example

package main

import (
    "os"
	"context"
	"log"
	"net"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/health"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"
	"google.golang.org/grpc/reflection"

	userpb "example.com/mygrpcapp/gen/go/user/v1"
)

type userService struct {
	userpb.UnimplementedUserServiceServer
}

func (u *userService) GetUser(_ context.Context, req *userpb.GetUserRequest) (*userpb.GetUserResponse, error) {
	return &userpb.GetUserResponse{
		User: &userpb.User{
			Uuid:     req.Uuid,
			FullName: "User1",
		},
	}, nil
}

func main() {
    server_ip := "0.0.0.0"
    server_port := os.Getenv("GRPCSERVER_PORT")
    server_address := server_ip + ":" + server_port
	lis, err := net.Listen("tcp", server_address)
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	grpcServer := grpc.NewServer()
	userServer := &userService{}

	healthServer := health.NewServer()

	go func() {
		for {
			status := healthpb.HealthCheckResponse_SERVING

			if time.Now().Second()%2 == 0 {
				status = healthpb.HealthCheckResponse_NOT_SERVING
			}

			healthServer.SetServingStatus(userpb.UserService_ServiceDesc.ServiceName, status)
			healthServer.SetServingStatus("", status)

			time.Sleep(1 * time.Second)
		}
	}()

	healthServer.SetServingStatus("", healthpb.HealthCheckResponse_SERVING)
	healthServer.SetServingStatus(userpb.UserService_ServiceDesc.ServiceName, healthpb.HealthCheckResponse_SERVING)

	userpb.RegisterUserServiceServer(grpcServer, userServer)
	healthpb.RegisterHealthServer(grpcServer, healthServer)

	reflection.Register(grpcServer)

	grpcServer.Serve(lis)
}
