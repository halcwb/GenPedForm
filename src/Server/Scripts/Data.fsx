//#load "../../../.paket/load/netstandard2.1/Server/server.group.fsx"
//#r "C:\\Users\\halcw\\.nuget\\packages\\fsharp.data\\3.3.3\\lib\\netstandard2.1\\FSharp.Data.dll"
#r "C:\\Users\\cbollen\\.nuget\\packages\\fsharp.data\\3.3.3\\lib\\netstandard2.0\\FSharp.Data.dll" 
#load "../../Shared/Shared.fs"

open System
open System.IO
open FSharp.Data

open Shared

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Products = FSharp.Data.CsvProvider<"resources/products.csv">
type Doses = FSharp.Data.CsvProvider<"resources/doses.csv">

let dosePath = Path.Combine(Environment.CurrentDirectory, "resources/doses.csv")
let doses =
    Doses.Load(dosePath).Cache()

let productPath = Path.Combine(Environment.CurrentDirectory, "resources/products.csv")
let products =
    Products.Load(productPath).Cache().Rows
    |> Seq.map (fun p ->
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
            Concentration = p.Concentration |> float
            Unit = p.Unit
            Multiple =
                try
                    p.Multiple |> float
                with | _ -> 0.
            MultipleUnit = p.MultipeUnit
            HasSolution = false
            IsInStock = true
            Doses = []
        }
    )

products
|> Seq.iter (printfn "%A")