using Genie
using Genie.Renderer.Json
using LibPQ, Tables
using HTTP

include("db.jl")
using .Database

# Helper function for parsing JSON request body
function get_json_payload()
    payload_data = Genie.Requests.jsonpayload()

    # If payload is already a Dict, return it
    if isa(payload_data, Dict)
        return payload_data
    end

    # Otherwise, try to parse it as JSON
    if isa(payload_data, String) && !isempty(payload_data)
        try
            return JSON.parse(payload_data)
        catch e
            return Dict()
        end
    end

    return Dict()
end

route("/hello") do
    "Welcome to Genie!"
end

route("/json") do
    (:greeting => "Welcome to Genie!") |> Genie.Renderer.Json.json
end

# GET /users - fetch an array of all users
route("/users", method="GET") do
    users = Database.get_all_users()
    users |> json
end

# GET /users/:id - fetch a single user
route("/users/:id", method="GET") do
    user_id = parse(Int, params(:id))
    user = Database.get_user_by_id(user_id)

    if user === nothing
        return Genie.Renderer.respond(Genie.Renderer.json(Dict(:error => "User not found")), 404)
    end

    user |> json
end

# POST /users - create a new user
route("/users", method="POST") do
    payload = get_json_payload()

    # Validate required fields
    if !haskey(payload, "name") || !haskey(payload, "email")
        return Genie.Renderer.respond(
            Genie.Renderer.json(Dict(:error => "Name and email are required")),
            400
        )
    end

    user = Database.create_user(payload["name"], payload["email"])

    Genie.Renderer.respond(
        Genie.Renderer.json(user),
        200
    )
end

# PUT /users/:id - update a user
route("/users/:id", method="PUT") do
    user_id = parse(Int, params(:id))
    payload = get_json_payload()

    updated_user = Database.update_user(user_id, payload)

    if updated_user === nothing
        return Genie.Renderer.respond(Genie.Renderer.json(Dict(:error => "User not found")), 404)
    end

    updated_user |> json
end

# DELETE /users/:id - delete a user
route("/users/:id", method="DELETE") do
    user_id = parse(Int, params(:id))

    deleted_user = Database.delete_user(user_id)

    if deleted_user === nothing
        return Genie.Renderer.respond(Genie.Renderer.json(Dict(:error => "User not found")), 404)
    end

    deleted_user |> json
end

#Genie.Router.notfound(error -> "Not found: $(Genie.Router.currentroute())")
