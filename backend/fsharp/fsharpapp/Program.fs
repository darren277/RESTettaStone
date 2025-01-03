open System
open System.Net
open System.Text.Json.Serialization

open Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection

open Database

let port = Environment.GetEnvironmentVariable("PORT") |> int

let routeHandlers : HttpHandler =
  choose [
    GET >=> route "/" >=> Successful.OK "Hello, World!"

    GET >=> routef "/users/%i" (fun id ->
        fun next ctx -> task {
            match Database.getUser id with
            | Ok user -> return! json user next ctx
            | Error Database.NotFound -> return! setStatusCode 404 next ctx
            | Error (Database.DatabaseError _) -> return! setStatusCode 500 next ctx
        }
    )

    GET >=> route "/users" >=> fun next ctx ->
        match Database.getUsers() with
        | Ok userList ->
            json userList next ctx
        | Error NotFound ->
            setStatusCode 404 next ctx
        | Error (DatabaseError _) ->
            setStatusCode 500 next ctx

    POST >=> route "/users" >=> fun next ctx -> task {
        try
            let! user = ctx.BindJsonAsync<Database.User>()
            match Database.addUser user with
            | Ok _ -> return! setStatusCode 200 next ctx
            | Error Database.NotFound -> return! setStatusCode 404 next ctx
            | Error (Database.DatabaseError _) -> return! setStatusCode 500 next ctx
        with ex ->
            printfn "Error binding user: %s" ex.Message
            return! setStatusCode 400 next ctx
    }

    PUT >=> routef "/users/%i" (fun id ->
        fun next ctx -> task {
            try
                let! user = ctx.BindJsonAsync<Database.User>()
                match Database.updateUser id user with
                | Ok _ -> return! setStatusCode 200 next ctx
                | Error Database.NotFound -> return! setStatusCode 404 next ctx
                | Error (Database.DatabaseError _) -> return! setStatusCode 500 next ctx
            with ex ->
                printfn "Error binding user: %s" ex.Message
                return! setStatusCode 400 next ctx
        }
    )

    DELETE >=> routef "/users/%i" (fun id ->
        fun next ctx -> task {
            match Database.deleteUser id with
            | Ok _ -> return! setStatusCode 200 next ctx
            | Error Database.NotFound -> return! setStatusCode 404 next ctx
            | Error (Database.DatabaseError _) -> return! setStatusCode 500 next ctx
        }
    )

    setStatusCode 404 >=> text "not found"
  ]

let configureApp (app: IApplicationBuilder) = app.UseGiraffe routeHandlers

let configureServices (services: IServiceCollection) =
    services.AddGiraffe() |> ignore

    let serializationOptions = SystemTextJson.Serializer.DefaultOptions
    serializationOptions.Converters.Add(JsonFSharpConverter(JsonUnionEncoding.FSharpLuLike))

    services.AddSingleton<Json.ISerializer>(SystemTextJson.Serializer(serializationOptions))
    |> ignore

[<EntryPoint>]
let main args =
  let host =
    WebHostBuilder()
        .UseKestrel()
        .UseKestrel(fun options -> options.Listen(IPAddress.Any, port))
        .Configure(configureApp)
        .ConfigureServices(configureServices)
        .Build()
  try
    host.Run()
    0
  with ex ->
    printfn "%A" ex
    1
