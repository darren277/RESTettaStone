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
    id: int option
    email: string
}

type DbError =
    | NotFound
    | DatabaseError of string

type DbResult<'T> = Result<'T, DbError>

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
        |> Sql.query "SELECT * FROM users WHERE id = @id"
        |> Sql.parameters [ "@id", Sql.int id ]
        |> Sql.execute (fun read ->
        {
          id = Some (read.int "id")
          email = read.text "email"
        })
        |> Ok
    with
    | ex ->
        printfn "Database error: \n%A" ex.Message
        Error (DatabaseError ex.Message)


let getUsers () =
    try
        connection
        |> Sql.connectFromConfig
        |> Sql.query "SELECT * FROM users"
        |> Sql.execute (fun read ->
        {
          id = Some (read.int "id")
          email = read.text "email"
        })
        |> Ok
    with
    | ex ->
        printfn "Database error: \n%A" ex.Message
        Error (DatabaseError ex.Message)

let addUser user =
    try
        connection
        |> Sql.connectFromConfig
        |> Sql.query "INSERT INTO users (email) VALUES (@email)"
        |> Sql.parameters [ "@email", Sql.text user.email ]
        |> Sql.executeNonQuery
        |> ignore
        Ok ()
    with
    | ex ->
        printfn "Database error: \n%A" ex.Message
        Error (DatabaseError ex.Message)

let updateUser id user =
    try
        let affectedRows =
            connection
            |> Sql.connectFromConfig
            |> Sql.query "UPDATE users SET email = @email WHERE id = @id"
            |> Sql.parameters [ "@email", Sql.text user.email; "@id", Sql.int id ]
            |> Sql.executeNonQuery

        if affectedRows = 0 then
            Error NotFound
        else
            Ok ()
    with
    | ex ->
        printfn "Database error: \n%A" ex.Message
        Error (DatabaseError ex.Message)

let deleteUser id =
    try
        let affectedRows =
            connection
            |> Sql.connectFromConfig
            |> Sql.query "DELETE FROM users WHERE id = @id"
            |> Sql.parameters [ "@id", Sql.int id ]
            |> Sql.executeNonQuery

        if affectedRows = 0 then
            Error NotFound
        else
            Ok ()
    with
    | ex ->
        printfn "Database error: \n%A" ex.Message
        Error (DatabaseError ex.Message)
