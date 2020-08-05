module ServerApi

open System.Collections.Generic
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Configuration
open Shared
open Shared.Api


let getProducts connString =
    Doses.getProducts connString
    |> List.map (fun p ->
        p.GenericLabel
    )
    |> List.distinct


let getGenerics connString =
    Doses.getDoses connString
    |> List.map (fun d ->
        d.Generic
    )
    |> List.distinct


let getMarkdown connString generic =
    Doses.getDoses connString
    |> List.filter (fun d ->
        d.Generic =generic
    )
    |> Doses.toMarkdown


/// An implementation of the Shared IServerApi protocol.
/// Can require ASP.NET injected dependencies in the constructor and uses the Build() function to return value of `IServerApi`.
type ServerApi(logger: ILogger<ServerApi>, config: IConfiguration) =

    let connString = 
        match (Env.environmentVars ()).TryGetValue "CONN_STR_AP" with
        | true, s -> s
        | _ -> 
            printfn "cannot find the connections string"
            ""
        
    member this.GetProducts () = 
        async {
            try 
                let products = getProducts connString
                return Ok products
            with 
                | error -> 
                    logger.LogError(error, "Error while retrieving products from database")
                    return Error error.Message
        }        
    member this.GetGenerics () = 
        async {
            try 
                let products = getGenerics connString
                return Ok products
            with 
                | error -> 
                    logger.LogError(error, "Error while retrieving generics from database")
                    return Error error.Message
        }        

    member this.GetMarkdown generic =
        async {
            try
                let markdown = generic |> getMarkdown connString
                return Ok markdown
            with
                | error -> 
                    logger.LogError(error, "Error while retrieving markdown for generic")
                    return Error error.Message                
        }

    member this.Build() : IServerApi =
        {
            GetProducts = this.GetProducts
            GetGenerics = this.GetGenerics
            GetMarkdown = this.GetMarkdown
        }

