open System
open System.Net
open System.Text.Json.Serialization

open Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection

let port = Environment.GetEnvironmentVariable("PORT") |> int

let routeHandlers : HttpHandler =
  choose [
    GET >=> route "/" >=> Successful.OK "Hello, World!"

    GET >=> routef "/user/%i" (fun id ->
        match Database.getUser id with
        | Ok user -> json user
        | Error _ -> setStatusCode 500
    )

    GET >=> route "/users" >=> fun next ctx ->
        let users = Database.getUsers()
        json users next ctx

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
