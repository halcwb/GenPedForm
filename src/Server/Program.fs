module Program

open System
open System.IO
open Saturn
open Giraffe
open Shared
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Microsoft.Extensions.DependencyInjection

open Shared.Api

type ServerApi = ServerApi.ServerApi

let tryGetEnv key = 
    match Environment.GetEnvironmentVariable key with
    | x when String.IsNullOrWhiteSpace x -> None 
    | x -> Some x


let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us


let publicPath = Path.GetFullPath "../Client/public"


let webApi =
    Remoting.createApi()
    |> Remoting.fromContext (fun ctx -> ctx.GetService<ServerApi>().Build())
    |> Remoting.withRouteBuilder routerPaths
    |> Remoting.buildHttpHandler

let webApp = choose [ webApi; GET >=> text "GenPed App. Use localhost: 8080 for the GUI" ]

let serviceConfig (services: IServiceCollection) =
    services
      .AddSingleton<ServerApi>()
      .AddLogging()
      

let application = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    use_static publicPath
    use_gzip
    use_iis
    
    service_config serviceConfig
    host_config Env.configureHost
}

run application