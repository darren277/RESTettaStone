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
    host = get(ENV, "PG_HOST", "localhost")
    port = get(ENV, "PG_PORT", "5432")
    user = get(ENV, "PG_USER", "postgres")
    pass = get(ENV, "PG_PASS", "")
    db = get(ENV, "PG_DB", "postgres")

    conn_string = "dbname=$db host=$host port=$port user=$user password=$pass"
    conn = LibPQ.Connection(conn_string)
    result = execute(conn, "SELECT * FROM users")

    columns = propertynames(result)

    rows = []
    for row in result
        row_dict = Dict{Symbol, Any}()
        for col in columns
            row_dict[col] = row[col]
        end
        push!(rows, row_dict)
    end

    close(conn)
    rows |> json
end

