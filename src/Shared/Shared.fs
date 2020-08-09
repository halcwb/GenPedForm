namespace Shared

open System

type Days = int
type Kg = float
type Cm = int
type Gender = Male | Female | Unknown of string

type Patient =
    {
        Age : Days
        Gender : Gender
        GestAge : Days
        PMAge : Days
        Weight : Kg
        Length : Cm
    }

type Products = string list

type Generics = string list

type Query =
    { Generic : string option
      Indication : string option
      Route : string option
      Patient : string option }

module Api =


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
    }
