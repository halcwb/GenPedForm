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

    module Json =
        open Fable.Remoting.Json
        open Newtonsoft.Json
        open System.IO
        open System.Text
        
        let private fableConverter = new FableJsonConverter() :> JsonConverter
        
        let private settings = JsonSerializerSettings(DateParseHandling = DateParseHandling.None)
        
        let private fableSerializer =
            let serializer = JsonSerializer()
            serializer.Converters.Add fableConverter
            serializer
        
        let private jsonEncoding = UTF8Encoding false
        
        let jsonSerialize (o: 'a) (stream: Stream) =
            use sw = new StreamWriter (stream, jsonEncoding, 1024, true)
            use writer = new JsonTextWriter (sw, CloseOutput = false)
            fableSerializer.Serialize (writer, o)
        
        let serialize o =
            use stream = new MemoryStream()
            use reader = new StreamReader(stream)
            jsonSerialize o stream
            stream.Position <- 0L
            reader.ReadToEnd()
        
        

    open Categorize

    let getVersions = 
        Memoization.memoize DoseRecords.getVersions

    let getDoses =
        Memoization.memoize DoseRecords.getDoses

    let getProducts =
        Memoization.memoize DoseRecords.getProducts

    let getCategorizedAsString conn =
        fun generic ->
            getDoses conn
            |> List.filter(fun d ->
                generic |> String.isNullOrWhiteSpace ||
                d.Generic = generic
            )
            |> Categorize.mapDoses
//            |> List.map Mapper.mapCategorizedGeneric_
            |> List.map Json.serialize
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


    let getFrequencies connString =
        connString
        |> DoseRecords.getDoses
        |> List.collect (fun d -> d.Freqs)
        |> List.distinct
        |> List.sortBy (fun f ->
            let time =
                match f.Time with
                | (x, u) when u = "dag" -> 0, x
                | (x, u) when u = "dagen" -> 1, x
                | (x, u) when u = "uur" -> 2, x
                | (x, u) when u = "week" -> 3, x
                | (x, u) when u = "weken" -> 4, x
                | (x, u) when u = "maand" -> 5, x
                | (x, u) when u = "maanden" -> 6, x
                | _ -> 99, 0
            time, f.Count
        )
        |> List.map (fun f ->
            if f.Time |> fst = 1 then f.Time |> snd
            else
                f.Time
                |> snd
                |> sprintf "%i %s" (f.Time |> fst)            
            |> sprintf "%i x / %s" f.Count
        )


    let getPatients connString generic indication route =
        getDoses connString
        |> List.filter (fun d -> d.Generic = generic && d.Indication = indication && d.Route = route)
        |> List.sortBy DoseRecords.sortPat
        |> List.map (fun d -> d |> DoseRecords.printPat, d)
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
            |> List.map (fun d -> d |> DoseRecords.printPat, d)
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
        |> DoseRecords.toMarkdown


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


        member this.GetMarkdown (qry : Shared.Types.QueryTypes.Query) =
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

        member this.GetFrequencies _ =
            async {
                try
                    let freqs = getFrequencies connString
                    return Ok freqs
                with
                | error ->
                    logger.LogError(error, "Error retrieving frequencies")
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
                GetFrequencies = this.GetFrequencies
            }

