module Database

open System
open Npgsql.FSharp
open System.Text.Json
open System.Text.Json.Serialization

let pg_host = Environment.GetEnvironmentVariable("PG_HOST")
let pg_port = Environment.GetEnvironmentVariable("PG_PORT") |> int
let pg_user = Environment.GetEnvironmentVariable("PG_USER")
let pg_pass = Environment.GetEnvironmentVariable("PG_PASS")
let pg_db = Environment.GetEnvironmentVariable("PG_DB")

let jsonOptions = JsonSerializerOptions()
jsonOptions.Converters.Add(JsonFSharpConverter())

let serialize whatever = JsonSerializer.Serialize(whatever, jsonOptions)
let deserialize (str : string) =
    JsonSerializer.Deserialize(str, jsonOptions)

type User = {
    Id: int
    Email: string
}

let connection =
    Sql.host pg_host
    |> Sql.port pg_port
    |> Sql.username pg_user
    |> Sql.password pg_pass
    |> Sql.database pg_db
    |> Sql.config "Pooling=true"

let getUser id =
    try
        connection
        |> Sql.connectFromConfig
        |> Sql.query "SELECT * FROM users where id = @userid"
        |> Sql.parameters [ "@userid", Sql.int id ]
        |> Sql.executeRow (fun read ->
        {
          Id = read.int "id"
          Email = read.text "email"
        })
        |> Ok
    with
    | ex ->
        printfn "Database error: \n%A" ex.Message
        Error ()

let getUsers () =
    try
        connection
        |> Sql.connectFromConfig
        |> Sql.query "SELECT * FROM users"
        |> Sql.execute (fun read ->
        {
          Id = read.int "id"
          Email = read.text "email"
        })
        |> Ok
    with
    | ex ->
        printfn "Database error: \n%A" ex.Message
        Error ()
