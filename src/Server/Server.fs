module Server

open System

open Giraffe
open Saturn

open Fable.Remoting.Server
open Fable.Remoting.Giraffe

open Shared

module ServerApi =

    let runQuery = 
        fun qry ->
            printfn "running qry"
            async {
                return 
                    qry
                    |> Query.run
//                    |> Ok
            }


    let api : IServerApi = 
        {
            Query = runQuery
        }

let tryGetEnv key = 
    match Env.environmentVars().TryGetValue(key) with
    | true, x when String.IsNullOrWhiteSpace x |> not -> Some x 
    | _ -> None


let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us



let webApi =
    Remoting.createApi ()
    |> Remoting.fromValue ServerApi.api
    |> Remoting.withRouteBuilder Route.routerPaths
    |> Remoting.buildHttpHandler


let webApp =
    choose [
        webApi
        GET >=> text "GenPedForm App. Use localhost: 8080 for the GUI"
    ]

let app =
    application {
        url ("http://*:" + port.ToString() + "/")
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
