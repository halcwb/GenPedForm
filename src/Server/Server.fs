module ServerApi

open System.Collections.Generic
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Configuration
open Shared
open Shared.Api

open Informedica.GenUtils.Lib
open Informedica.GenUtils.Lib.BCL

let getDoses =
    Memoization.memoize Doses.getDoses

let getProducts =
    Memoization.memoize Doses.getProducts

let getGenericLabels connString =
    getProducts connString
    |> List.map (fun p ->
        p.GenericLabel
    )
    |> List.distinct

let getGenerics connString =
    getDoses connString
    |> List.map (fun d ->
        d.Generic
    )
    |> List.distinct

let getIndications connString generic =
    getDoses connString
    |> List.filter (fun d -> d.Generic = generic)
    |> List.map (fun d ->
        d.Indication
    )
    |> List.distinct

let getRoutes connString generic indication =
    getDoses connString
    |> List.filter (fun d -> d.Generic = generic && d.Indication = indication)
    |> List.map (fun d ->
        d.Route
    )
    |> List.distinct


let getMarkdown connString generic indication route =
    let doses = getDoses connString
    match indication, route with
    | Some i, Some r ->
        doses
        |> List.filter (fun d ->
            d.Generic = generic && 
            d.Indication |> String.equalsCapInsens i &&
            d.Route |> String.equalsCapInsens r
        )
    | Some i, None ->
        doses
        |> List.filter (fun d ->
            d.Generic = generic && 
            d.Indication |> String.equalsCapInsens i
        )
    | None, _ ->
        doses
        |> List.filter (fun d ->
            d.Generic = generic
        )
    |> Doses.toMarkdown


/// An implementation of the Shared IServerApi protocol.
/// Can require ASP.NET injected dependencies in the constructor and uses the Build() function to return value of `IServerApi`.
type ServerApi(logger: ILogger<ServerApi>, config: IConfiguration) =

    let connString = 
        match (Env.environmentVars ()).TryGetValue "CONN_STR_AP" with
        | true, s -> s
        | _ -> 
            config.GetValue("DATABASE_CONNECTIONSTRING_AP")
        
    member this.GetProducts () = 
        async {
            try 
                let products = getGenericLabels connString
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

    member this.GetIndications generic =
        async {
            try
                let indications = getIndications connString generic
                return Ok indications
            with
                | error -> 
                    logger.LogError(error, "Error while retrieving indications from database")
                    return Error error.Message
        }

    member this.GetRoutes generic indication =
        async {
            try
                let routes = getRoutes connString generic indication
                return Ok routes
            with
                | error -> 
                    logger.LogError(error, "Error while retrieving routes from database")
                    return Error error.Message
        }


    member this.GetMarkdown (qry : Query) =
        async {
            try
                let markdown = getMarkdown connString qry.Generic qry.Indication qry.Route
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
            GetIndications = this.GetIndications
            GetRoutes = this.GetRoutes
            GetMarkdown = this.GetMarkdown
        }

