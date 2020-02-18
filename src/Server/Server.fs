open System
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared

module Extra = Thoth.Json.Net.Extra

module String =

    let split (d: string) (s: string) = s.Split(d)

let toIntOpt x =
    try
        x
        |> int
        |> fun i ->
            if i = 0 then None
            else
                i
                |> Some
    with | _ -> None

let inline toFloatOpt x =
    try
        x
        |> float
        |> fun f ->
            if f = 0. || f |> Double.IsNaN then None
            else
                f
                |> Some
    with | _ -> None

let inline toFloat x =
    try
        x
        |> float
        |> fun f ->
            if f |> Double.IsNaN then 0. else f
    with | _ -> 0.



let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let resourcesPath = Path.GetFullPath "resources"


type Products = FSharp.Data.CsvProvider<"resources/products.csv">
type Doses = FSharp.Data.CsvProvider<"resources/doses.csv">


let mapDose (d : Doses.Row) =
    let mapDose perKg perM2 q =
        match q |> toFloatOpt with
        | Some q ->
            let perKg = perKg <> ""
            let perM2 = perM2 <> ""
            match perKg, perM2 with
            | false, false -> q |> Quantity |> Some
            | true, _ -> q |> QuantityPerKg |> Some
            | _, true -> q |> QuantityPerM2 |> Some
        | None -> None
    {
        Generic = d.Generic
        Shape = d.Shape
        Route = d.Route
        Indication = d.Indication
        Specialty = d.Specialty
        Gender = Unknown ""
        MinAgeMo = d.MinAgeMo |> toFloatOpt
        MaxAgeMo = d.MaxAgeMo |> toFloatOpt
        MinWeightKg = d.MinWghtKg |> toFloatOpt
        MaxWeightKg = d.MaxWghtKg |> toFloatOpt
        MinGestAgeDays = d.MinGestDays |> toIntOpt
        MaxGestAgeDays = d.MaxGestDays |> toIntOpt
        MinPMAgeDays = d.MinGestDays |> toIntOpt
        MaxPMAgeDays = d.MaxGestDays |> toIntOpt
        Freqs =
            d.Frequencies
            |> String.split "||"
            |> Seq.map (fun s ->
                match s |> String.split " x / " with
                | [| c; t |] ->
                    match t |> String.split " " with
                    | [| u |] ->
                        try
                            { Count = c |> int; Time = (1, u) }
                            |> Some
                        with _ -> None
                    | [| q; u |] ->
                        try
                            let q = q |> int
                            { Count = c |> int; Time = (q, u) }
                            |> Some
                        with _ -> None
                    | _ -> None
                | _ -> None
            )
            |> Seq.filter Option.isSome
            |> Seq.map Option.get
            |> Seq.toList

        Unit = d.Unit
        NormDose = d.NormDose |> mapDose d.DosePerKg d.DosePerM2
        MinDose = d.MinDose |> mapDose d.DosePerKg d.DosePerM2
        MaxDose = d.MaxDose |> mapDose d.DosePerKg d.DosePerM2
        AbsMaxDose = d.AbsMaxDose |> mapDose "" ""
        MaxPerDose = d.MaxPerDose |> mapDose "" ""
        Products = []
    }


let mapProduct (p : Products.Row) =
        {
            GPK = p.GPK
            ATC = p.ATC
            MainGroup = p.MainGroup
            SubGroup = p.SubGroup
            Generic = p.Generic
            GenericLabel = p.Label
            ProductLabel = p.Product
            Shape = p.Shape
            Routes = [ p.Routes ]
            Concentration = p.Concentration |> toFloat
            Unit = p.Unit
            Multiple = p.Multiple |> toFloat
            MultipleUnit = p.MultipeUnit
            HasSolution = false
            IsInStock = true
            Doses = []
        }


let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let webApp = router {
    get "/api/init" (fun next ctx ->
        task {
            let counter = {Value = 42}
            return! json counter next ctx
        })

    get "/api/doses" (fun next ctx ->
        task {
            let products =
                Products.Load(Path.Combine(resourcesPath, "products.csv")).Cache().Rows
            let doses =
                Doses.Load(Path.Combine(resourcesPath, "doses.csv")).Cache().Rows
                |> Seq.map mapDose
                |> Seq.filter (fun d -> d.Freqs |> Seq.length > 0)
                |> Seq.map (fun d ->
                    {
                        d with
                            Products =
                                products
                                |> Seq.filter (fun p ->
                                    p.Generic = d.Generic &&
                                    p.Shape = d.Shape
                                )
                                |> Seq.map mapProduct
                                |> Seq.toList
                    }
                )
                |> Seq.toList

            return! json doses next ctx
        }
    )

    get "/api/products" (fun next ctx ->

        task {
            let doses =
                Doses.Load(Path.Combine(resourcesPath, "doses.csv")).Cache()
            let products =
                Products.Load(Path.Combine(resourcesPath, "products.csv")).Rows
                |> Seq.map (fun p ->
                    let p = p |> mapProduct
                    { p with
                        Doses =
                            doses.Rows
                            |> Seq.filter (fun d ->
                               d.Frequencies |> Seq.length >  0 &&
                               d.Generic = p.Generic &&
                               d.Shape = p.Shape
                            )
                            |> Seq.map mapDose
                            |> Seq.filter (fun d -> d.Freqs |> Seq.length > 0)
                            |> Seq.toList
                    }
                )
                |> Seq.toList

            return! json products next ctx
        }
    )
}

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    use_gzip
}

run app
