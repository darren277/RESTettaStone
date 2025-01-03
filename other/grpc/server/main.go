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

	"gorm.io/driver/postgres"
    "gorm.io/gorm"

	userpb "example.com/mygrpcapp/gen/go/user/v1"

	"google.golang.org/grpc/codes"
    "google.golang.org/grpc/status"
)

type userService struct {
	userpb.UnimplementedUserServiceServer
	db *gorm.DB
}

func (s *userService) GetUser(ctx context.Context, req *userpb.GetUserRequest) (*userpb.GetUserResponse, error) {
    var user userpb.User
    idInt := req.GetId()
    id := uint(idInt)

    if err := s.db.Where("id = ?", id).First(&user).Error; err != nil {
        if err == gorm.ErrRecordNotFound {
            return nil, status.Error(codes.NotFound, "User not found")
        }
        return nil, status.Errorf(codes.Internal, "Error getting user: %v", err)
    }

    return &userpb.GetUserResponse{
        User: &userpb.User{
            Id: user.Id,
            Email: user.Email,
        },
    }, nil
}

func (s *userService) CreateUser(ctx context.Context, req *userpb.CreateUserRequest) (*userpb.CreateUserResponse, error) {
    user := userpb.User{
        Email: req.GetEmail(),
    }

    if err := s.db.Create(&user).Error; err != nil {
        return nil, status.Errorf(codes.Internal, "Could not create user: %v", err)
    }

    return &userpb.CreateUserResponse{
        User: &userpb.User{
            Id: user.Id,
            Email: user.Email,
        },
    }, nil
}

func (s *userService) UpdateUser(ctx context.Context, req *userpb.UpdateUserRequest) (*userpb.UpdateUserResponse, error) {
    var user userpb.User
    idInt := req.GetId()
    id := uint(idInt)

    if err := s.db.Where("id = ?", id).First(&user).Error; err != nil {
        if err == gorm.ErrRecordNotFound {
            return nil, status.Error(codes.NotFound, "User not found")
        }
        return nil, status.Errorf(codes.Internal, "Error getting user: %v", err)
    }

    user.Email = req.GetEmail()

    if err := s.db.Save(&user).Error; err != nil {
        return nil, status.Errorf(codes.Internal, "Could not update user: %v", err)
    }

    return &userpb.UpdateUserResponse{
        User: &userpb.User{
            Id: user.Id,
            Email: user.Email,
        },
    }, nil
}

func (s *userService) DeleteUser(ctx context.Context, req *userpb.DeleteUserRequest) (*userpb.DeleteUserResponse, error) {
    var user userpb.User
    idInt := req.GetId()
    id := uint(idInt)

    if err := s.db.Where("id = ?", id).First(&user).Error; err != nil {
        if err == gorm.ErrRecordNotFound {
            return nil, status.Error(codes.NotFound, "User not found")
        }
        return nil, status.Errorf(codes.Internal, "Error getting user: %v", err)
    }

    if err := s.db.Delete(&user).Error; err != nil {
        return nil, status.Errorf(codes.Internal, "Could not delete user: %v", err)
    }

    return &userpb.DeleteUserResponse{
        Success: true,
    }, nil
}

func main() {
    pg_host := os.Getenv("PG_HOST")
    pg_user := os.Getenv("PG_USER")
    pg_pass := os.Getenv("PG_PASS")
    pg_db := os.Getenv("PG_DB")
    pg_port := os.Getenv("PG_PORT")

    dsn := "host=" + pg_host + " user=" + pg_user + " password=" + pg_pass + " dbname=" + pg_db + " port=" + pg_port + " sslmode=disable"

    db, err := gorm.Open(postgres.Open(dsn), &gorm.Config{})
    if err != nil {
        log.Fatalf("Could not connect to Postgres: %v", err)
    }

    server_ip := "0.0.0.0"
    server_port := os.Getenv("GRPCSERVER_PORT")
    server_address := server_ip + ":" + server_port
	lis, err := net.Listen("tcp", server_address)
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	grpcServer := grpc.NewServer()
	userServer := &userService{db: db}

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
