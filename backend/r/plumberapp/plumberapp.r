library(DBI)
library(RPostgres)
library(plumber)

# Global variable to store connection
con <- NULL

#* @filter setup
function(req, res) {
    if (is.null(con)) {
        con <<- dbConnect(RPostgres::Postgres(),
            dbname = Sys.getenv("PG_DB"),
            host = Sys.getenv("PG_HOST"),
            port = as.numeric(Sys.getenv("PG_PORT")),
            user = Sys.getenv("PG_USER"),
            password = Sys.getenv("PG_PASS")
        )
    }
    plumber::forward()
}

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
  as.numeric(a) + as.numeric(b)
}

#* Get all users
#* @get /users
function(req, res) {
    result <- dbGetQuery(con, "SELECT * FROM users")
    res$body <- jsonlite::toJSON(result)
    res
}

#* Add a user
#* @post /users
function(req, res) {
    print(req$body)

    email <- req$body$email

    print("Received email:")
    print(email)

    if (is.null(email) || email == "") {
        res$status <- 400
        res$body <- jsonlite::toJSON(list(error = "Email is required"))
        return(res)
    }

    result <- dbGetQuery(con, "INSERT INTO users (email) VALUES ($1) RETURNING *", params = list(email))

    res$body <- jsonlite::toJSON(result)
    res
}

#* Get a user
#* @get /users/<id>
function(req, res) {
    print("Getting user")
    url_path <- req$PATH_INFO
    id <- as.numeric(strsplit(url_path, "/")[[1]][3])

    result <- dbGetQuery(con, "SELECT * FROM users WHERE id = $1", params = list(id))

    if (nrow(result) == 0) {
        res$status <- 404
        res$body <- jsonlite::toJSON(list(error = "User not found"))
        return(res)
    }

    single_user <- as.list(result[1,])

    res$body <- jsonlite::toJSON(single_user, auto_unbox = TRUE)
    print("Final JSON:")
    print(res$body)

    res
}

#* Update a user
#* @put /users/<id>
function(req, res) {
    url_path <- req$PATH_INFO
    id <- as.numeric(strsplit(url_path, "/")[[1]][3])

    email <- req$body$email

    result <- dbGetQuery(con, "UPDATE users SET email = $1 WHERE id = $2 RETURNING *", params = list(email, id))

    if (nrow(result) == 0) {
        res$status <- 404
        res$body <- jsonlite::toJSON(list(error = "User not found"))
        return(res)
    }

    res$body <- jsonlite::toJSON(result[1,], auto_unbox = TRUE)
    res
}

#* Delete a user
#* @delete /users/<id>
function(req, res) {
    url_path <- req$PATH_INFO
    id <- as.numeric(strsplit(url_path, "/")[[1]][3])

    user <- dbGetQuery(con, "SELECT * FROM users WHERE id = $1", params = list(id))

    if (nrow(user) == 0) {
        res$status <- 404
        res$body <- jsonlite::toJSON(list(error = "User not found"))
        return(res)
    }

    result <- dbGetQuery(con, "DELETE FROM users WHERE id = $1", params = list(id))

    res$body <- jsonlite::toJSON(list(message = "User deleted", id = id))
    res
}
