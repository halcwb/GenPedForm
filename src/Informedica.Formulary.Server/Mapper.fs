namespace Informedica.Formulary.Server

module Mapper =

    open System
    open Newtonsoft

    open Informedica.GenUtils.Lib

    open Informedica.Formulary.Lib
    open Informedica.Formulary


    let inline mapType<'T1, 'T2> (x : 'T1) = 
        x
        |> Json.JsonConvert.SerializeObject
        |> Json.JsonConvert.DeserializeObject<'T2>

    
    let mapCategorizedGeneric_ (cg : Types.CategorizedGeneric) =
        cg
        |> mapType<_, Shared.Types.DoseTypes.CategorizedGeneric>

