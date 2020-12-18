namespace Informedica.Formulary.Server

module ServerApi =

    open System.Collections.Generic
    open Microsoft.Extensions.Logging
    open Microsoft.Extensions.Configuration

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    
    open Informedica.Formulary
    open Informedica.Formulary.Lib
    open Informedica.Formulary.Shared.Api

    open Newtonsoft

    open Categorize

    let getVersions = 
        Memoization.memoize Doses.getVersions

    let getDoses =
        Memoization.memoize Doses.getDoses

    let getProducts =
        Memoization.memoize Doses.getProducts

    let getCategorizedAsString conn =
        fun generic ->
            getDoses conn
            |> List.filter(fun d ->
                generic |> String.isNullOrWhiteSpace ||
                d.Generic = generic
            )
            |> Categorize.mapDoses
//            |> List.map Mapper.mapCategorizedGeneric_
            |> List.map Json.JsonConvert.SerializeObject
        |> Memoization.memoize 

    let getCategorized conn =
        fun generic ->
            getDoses conn
            |> List.filter(fun d ->
                generic |> String.isNullOrWhiteSpace ||
                d.Generic = generic
            )
            |> Categorize.mapDoses
            |> List.map Mapper.mapCategorizedGeneric_
        |> Memoization.memoize 

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


    let getPatients connString generic indication route =
        getDoses connString
        |> List.filter (fun d -> d.Generic = generic && d.Indication = indication && d.Route = route)
        |> List.sortBy Doses.sortPat
        |> List.map (fun d -> d |> Doses.printPat, d)
        |> List.map fst
        |> List.distinct


    let getMarkdown connString generic indication route pat =
        let doses = getDoses connString
        match generic, indication, route, pat with
        | Some g, Some i, Some r, Some p ->
            doses
            |> List.filter (fun d ->
                d.Generic = g && 
                d.Indication |> String.equalsCapInsens i &&
                d.Route |> String.equalsCapInsens r
            )
            |> List.map (fun d -> d |> Doses.printPat, d)
            |> List.filter (fst >> ((=) p))
            |> List.map snd
        | Some g, Some i, Some r, None ->
            doses
            |> List.filter (fun d ->
                d.Generic = g && 
                d.Indication |> String.equalsCapInsens i &&
                d.Route |> String.equalsCapInsens r
            )
        | Some g, Some i, _, _ ->
            doses
            |> List.filter (fun d ->
                d.Generic = g && 
                d.Indication |> String.equalsCapInsens i
            )
        | Some g, None, _, _ ->
            doses
            |> List.filter (fun d ->
                d.Generic = g
            )
        | None, _, _, _ -> []
        |> Doses.toMarkdown


    /// An implementation of the Shared IServerApi protocol.
    /// Can require ASP.NET injected dependencies in the constructor and uses the Build() function to return value of `IServerApi`.
    type ServerApi(logger: ILogger<ServerApi>, config: IConfiguration) =

        let connString = 
            match (Env.environmentVars ()).TryGetValue "CONN_STR_AP" with
            | true, s -> s
            | _ -> 
                config.GetValue("DATABASE_CONNECTIONSTRING_AP")

        member this.GetVersions () =
            async {
                try
                    let versions = getVersions connString
                    return Ok versions
                with
                    | error -> 
                        logger.LogError(error, "Error while retrieving products from database")
                        return Error error.Message               
            }
        
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

        member this.GetPatients generic indication route =
            async {
                try
                    let pats = getPatients connString generic indication route
                    return Ok pats
                with
                    | error -> 
                        logger.LogError(error, "Error while retrieving patients from database")
                        return Error error.Message
            }


        member this.GetMarkdown (qry : Shared.Types.Query) =
            async {
                try
                    let markdown = getMarkdown connString qry.Generic qry.Indication qry.Route qry.Patient
                    return Ok markdown
                with
                    | error -> 
                        logger.LogError(error, "Error while retrieving markdown for generic")
                        return Error error.Message                
            }

        member this.GetCategorized generic =
            async {
                try
                    printfn "request editing %s" generic
                    let categorized = 
                        getCategorized connString generic
                    printfn "send generic list: %i" (categorized |> List.length)
                    return Ok categorized
                with
                    | error ->
                        logger.LogError(error, "Error retrieving categorized")
                        return Error error.Message
            }

        member this.GetCategorizedAsString generic =
            async {
                try
                    printfn "request editing %s" generic
                    let categorized = 
                        getCategorizedAsString connString generic
                    printfn "send generic list: %i" (categorized |> List.length)
                    return Ok categorized
                with
                    | error ->
                        logger.LogError(error, "Error retrieving categorized")
                        return Error error.Message
            }

        member this.Build() : IServerApi =
            {
                GetVersions = this.GetVersions
                GetProducts = this.GetProducts
                GetGenerics = this.GetGenerics
                GetIndications = this.GetIndications
                GetRoutes = this.GetRoutes
                GetPatients = this.GetPatients
                GetMarkdown = this.GetMarkdown
                GetCategorized = this.GetCategorized
                GetCategorizedAsString = this.GetCategorizedAsString
            }

