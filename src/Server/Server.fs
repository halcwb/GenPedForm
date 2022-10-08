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
            [
                "refresh", qry.Refresh |> string |> Some
                "view", Some $"{qry.ShowMd}"
                "indication", qry.Filter.Indication
                "generic", qry.Filter.Generic
                "shape", qry.Filter.Shape
                "route", qry.Filter.Route
                "patient", qry.Filter.PatientString
                "diagnoses", qry.Filter.Patient.Diagnosis
            ]
            |> List.map (fun (k, v) -> v |> Option.map (sprintf "%s: %s" k))
            |> List.choose id
            |> String.concat ", "
            |> printfn "running qry: %s"
            
            async {
                return 
                    qry
                    |> Query.run qry.Refresh
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
