using Genie
using Genie.Renderer.Json
using LibPQ, Tables

route("/hello") do
    "Welcome to Genie!"
end

route("/json") do
    (:greeting => "Welcome to Genie!") |> Genie.Renderer.Json.json
end

# GET /users - fetch an array of all users
route("/users", method="GET") do
    conn = LibPQ.Connection("dbname=postgres host=172.18.0.21 port=5432 user=myusername password=mypassword")
    result = execute(conn, "SELECT * FROM users")
    data = columntable(result)
    close(conn)
    data |> json
end

