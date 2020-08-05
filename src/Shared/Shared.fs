namespace Shared

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


module Api =


    /// Defines how routes are generated on server and mapped from client
    let routerPaths typeName method = sprintf "/api/%s" method

    type IServerApi = {
        GetGenerics : unit -> Async<Result<Generics, string>>
        GetProducts : unit -> Async<Result<Products, string>>
        GetIndications : string -> Async<Result<string list, string>>
        GetMarkdown : (string * string option) -> Async<Result<string, string>>
    }
