namespace Informedica.Formulary.Shared

open System


module Api =

    open Types.QueryTypes
    open Types.DoseTypes
//    open Categorize

    /// Defines how routes are generated on server and mapped from client
    let routerPaths typeName method = sprintf "/api/%s" method

    type IServerApi = {
        GetVersions : unit -> Async<Result<(int * DateTime) list, string>>
        GetGenerics : unit -> Async<Result<Generics, string>>
        GetProducts : unit -> Async<Result<Products, string>>
        GetIndications : string -> Async<Result<string list, string>>
        GetRoutes : string -> string -> Async<Result<string list, string>>
        GetPatients : string -> string -> string -> Async<Result<string list, string>>
        GetMarkdown : Query -> Async<Result<string, string>>
        GetCategorized : string -> Async<Result<CategorizedGeneric list, string>>
        GetCategorizedAsString : string -> Async<Result<string list, string>>
        GetFrequencies : unit -> Async<Result<string list, string>>
    }
