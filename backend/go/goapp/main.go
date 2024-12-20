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

func getUserById(c echo.Context) error {
    db := getDBInstance()
    var user User
    id := c.Param("id")

    if err := db.First(&user, id).Error; err != nil {
        log.Printf("Error fetching user: %v", err)
        return c.JSON(http.StatusInternalServerError, map[string]string{"error": "failed to fetch user"})
    }

    return c.JSON(http.StatusOK, user)
}

func addUser(c echo.Context) error {
    db := getDBInstance()
    var user User

    if err := c.Bind(&user); err != nil {
        log.Printf("Error binding user: %v", err)
        return c.JSON(http.StatusBadRequest, map[string]string{"error": "failed to bind user"})
    }

    if err := db.Create(&user).Error; err != nil {
        log.Printf("Error creating user: %v", err)
        return c.JSON(http.StatusInternalServerError, map[string]string{"error": "failed to create user"})
    }

    return c.JSON(http.StatusOK, user)
}

func updateUser(c echo.Context) error {
    db := getDBInstance()
    var user User
    id := c.Param("id")

    if err := db.First(&user, id).Error; err != nil {
        log.Printf("Error fetching user: %v", err)
        if err == gorm.ErrRecordNotFound {
            return c.JSON(http.StatusNotFound, map[string]string{"error": "user not found"})
        }
        return c.JSON(http.StatusInternalServerError, map[string]string{"error": "failed to fetch user"})
    }

    if err := c.Bind(&user); err != nil {
        log.Printf("Error binding user: %v", err)
        return c.JSON(http.StatusBadRequest, map[string]string{"error": "failed to bind user"})
    }

    if err := db.Save(&user).Error; err != nil {
        log.Printf("Error updating user: %v", err)
        return c.JSON(http.StatusInternalServerError, map[string]string{"error": "failed to update user"})
    }

    return c.JSON(http.StatusOK, user)
}

func deleteUser(c echo.Context) error {
    db := getDBInstance()
    var user User
    id := c.Param("id")

    if err := db.First(&user, id).Error; err != nil {
        log.Printf("Error fetching user: %v", err)
        if err == gorm.ErrRecordNotFound {
            return c.JSON(http.StatusNotFound, map[string]string{"error": "user not found"})
        }
        return c.JSON(http.StatusInternalServerError, map[string]string{"error": "failed to fetch user"})
    }

    if err := db.Delete(&user).Error; err != nil {
        log.Printf("Error deleting user: %v", err)
        return c.JSON(http.StatusInternalServerError, map[string]string{"error": "failed to delete user"})
    }

    return c.JSON(http.StatusOK, map[string]string{"message": "user deleted"})
}

func main() {
    PORT := os.Getenv("PORT")

    e := echo.New()

    e.GET("/users", getAllUsers)
    e.GET("/users/:id", getUserById)
    e.POST("/users", addUser)
    e.PUT("/users/:id", updateUser)
    e.DELETE("/users/:id", deleteUser)

    e.Start(":" + PORT)
}
