using Genie
using Genie.Renderer.Json
using LibPQ, Tables

include("db.jl")
using .Database

route("/hello") do
    "Welcome to Genie!"
end

route("/json") do
    (:greeting => "Welcome to Genie!") |> Genie.Renderer.Json.json
end

# GET /users - fetch an array of all users
route("/users", method="GET") do
    rows = Database.query_rows("SELECT * FROM users")
    rows |> json
end

