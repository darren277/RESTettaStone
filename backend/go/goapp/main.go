package main

import (
    "log"
    "os"
    "fmt"
    "net/http"
    "github.com/labstack/echo"

    "gorm.io/gorm"
    "gorm.io/driver/postgres"
)

var DB *gorm.DB

type User struct {
    ID    int    `json:"id" gorm:"column:id"`
    Email string `json:"email" gorm:"column:email"`
}

func NewDB() *gorm.DB {
    var err error

    PG_USER := os.Getenv("PG_USER")
    PG_PASS := os.Getenv("PG_PASS")
    PG_HOST := os.Getenv("PG_HOST")
    PG_PORT := os.Getenv("PG_PORT")
    PG_DB := os.Getenv("PG_DB")

    dsn := fmt.Sprintf("postgresql://%s:%s@%s:%s/%s?sslmode=disable", PG_USER, PG_PASS, PG_HOST, PG_PORT, PG_DB)

    DB, err = gorm.Open(postgres.Open(dsn), &gorm.Config{})
    if err != nil {
        log.Fatalf("Failed to connect to database: %v", err)
    }

    return DB
}

func getDBInstance() *gorm.DB {
    if DB == nil {
        DB = NewDB()
    }
    return DB
}

func getAllUsers(c echo.Context) error {
    db := getDBInstance()
    var users []User

    if err := db.Find(&users).Error; err != nil {
        log.Printf("Error fetching users: %v", err)
        return c.JSON(http.StatusInternalServerError, map[string]string{"error": "failed to fetch users"})
    }

    return c.JSON(http.StatusOK, users)
}

func main() {
    PORT := os.Getenv("PORT")

    e := echo.New()

    e.GET("/users", getAllUsers)

    e.Start(":" + PORT)
}
