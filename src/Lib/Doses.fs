module Doses

open System

module Queries =

    let getDoseLatest = "SELECT * FROM [dbo].[GetConfigMedDiscDoseLatest] ()"
    let getProductLatest = "SELECT * FROM [dbo].[GetConfigMedDiscLatest] ()"

open Types
open Lib
open Utils
open Informedica.GenUtils.Lib.BCL

let (|Regex|_|) pattern input =
    let m = (String.regex pattern).Match(input) // Regex.Match(input, pattern)
    if m.Success then 
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else None


let parseFreq s =
    match s with
    | Regex @"(antenoctum)" f -> f
    | Regex @"(\d+)(?: x / )(dag)" f -> f
    | Regex @"(\d+)(?: x / )(week)" f -> f
    | Regex @"(\d+)(?: x / )(\d+)(?: )(uur)" f -> f
    | Regex @"(\d+)(?: x / )(\d+)(?: )(dagen)" f -> f
    | Regex @"(\d+)(?: x / )(\d+)(?: )(weken)" f -> f
    | Regex @"(\d+)(?: x / )(\d+)(?: )(maand)" f -> f
    | _ -> []
    |> function 
    | [f; u] -> 
        { 
            Count = f |> Int32.parse
            Time = (1, u)
        }
        |> Some
    | [f; c; u] -> 
        { 
            Count = f |> Int32.parse
            Time = (c |> Int32.parse, u)
        }
        |> Some
    | _ -> None
    

let getProducts connString =
    connString
    |> Sql.connect
    |> Sql.query Queries.getProductLatest
    |> Sql.execute (fun r ->
        let optStr c = r.stringOrNone c |> Option.defaultValue ""
        let optDbl c = r.doubleOrNone c |> Option.defaultValue 0.
        {
            GPK = r.intOrNone "GPK" |> Option.defaultValue 0
            ATC = optStr "ATC"
            MainGroup = optStr "MainGroup"
            SubGroup = optStr "SubGroup"
            Generic = optStr "Generic"
            GenericLabel = optStr "Label"
            ProductLabel = optStr "Product"
            Shape = optStr "Shape"
            Routes = 
                r.string  "Routes"
                |> String.split "||"
            Concentration = optDbl "GenericQuantity"
            Unit = optStr "GenericUnit"
            Multiple = optDbl "MultipleQuantity"
            MultipleUnit = optStr "MultipleUnit"
            HasSolution = r.bool "HasSolutions"
            IsInStock = r.bool "IsActive"
            Doses = []
        } 
    )
    |> function 
    | Ok ps -> ps
    | _ -> []


let getDoses connString : Dose list =
    connString
    |> Sql.connect
    |> Sql.query Queries.getDoseLatest
    |> Sql.execute (fun r ->
        let optStr c = r.stringOrNone c |> Option.defaultValue ""
        let dose s = 
            let perKg = r.bool "IsDosePerKg"
            let perM2 = r.bool "IsDosePerM2"

            match s |> r.doubleOrNone with
            | Some d -> 
                if d = 0. then None
                else
                    match perKg, perM2 with
                    | true, false -> d |> QuantityPerKg 
                    | false, true -> d |> QuantityPerM2 
                    | _, _ -> d |> Quantity
                    |> Some
            | None -> None

        let doubleOrNone c = 
            r.doubleOrNone c
            |> Option.bind (fun v -> if v = 0. then None else Some v)

        let intOrNone (c : string) = 
            r.intOrNone c
            |> Option.bind (fun v -> if v = 0 then None else Some v)

        {
            Generic = optStr "Generic"
            Shape = optStr "Shape"
            Route = optStr "Route"
            Indication = optStr "Indication" |> String.toLower
            Specialty = ""
            Gender = Unknown (optStr "Gender")
            MinAgeMo = doubleOrNone "MinAge"
            MaxAgeMo = doubleOrNone "MaxAge"
            MinWeightKg = doubleOrNone "MinWeight"
            MaxWeightKg = doubleOrNone "MaxWeight"
            MinGestAgeDays = intOrNone "MinGestAge"
            MaxGestAgeDays = intOrNone "MaxGestAge"
            MinPMAgeDays = intOrNone "MinPMAge"
            MaxPMAgeDays = intOrNone "MaxPMAge"
            Freqs = 
                optStr "Frequencies" 
                |> String.split "||"
                |> List.map parseFreq
                |> List.filter Option.isSome
                |> List.map Option.get
            Unit = optStr "DoseUnit"
            NormDose = dose "NormDose"
            MinDose = dose "MinDose"
            MaxDose = dose "MaxDose"
            AbsMaxDose = 
                r.doubleOrNone "AbsMaxDose" 
                |> Option.bind (fun v -> 
                    if v = 0. then None
                    else v |> Quantity |> Some
                )
            MaxPerDose = 
                r.doubleOrNone "MaxPerDose" 
                |> Option.bind (fun v -> 
                    if v = 0. then None
                    else v |> Quantity |> Some
                )
            Products = []
        }
    )
    |> function 
    | Ok ds ->
        let products = getProducts connString
        ds
        |> List.map (fun d ->
            { d with
                Products = 
                    products
                    |> List.filter (fun p ->
                        p.Generic = d.Generic &&
                        p.Shape = d.Shape &&
                        p.Routes |> List.exists ((=) d.Route)
                    )
                    |> List.map (fun p ->
                        {
                            p with 
                                Doses = 
                                    ds 
                                    |> List.filter (fun d ->
                                        p.Generic = d.Generic &&
                                        p.Shape = d.Shape &&
                                        p.Routes |> List.exists ((=) d.Route)
                                    )
                        }
                    )
            }
        )
    | _ -> []


let sortPat (d: Dose) =
    let inline toInt x =
        match x with
        | Some x -> x |> int
        | None -> 0

    (d.MinAgeMo 
     |> Option.bind (fun x -> x * 30. |> Some)
     |> toInt |> fun s -> if s > 0 then s + 300 else s) +
    (d.MinWeightKg |> toInt) +
    (d.MinGestAgeDays |> toInt) +
    (d.MinPMAgeDays |> toInt) 


let printDose (d : Dose) =
    let format d =
        d 
        |> Double.fixPrecision 3
        |> fun d ->
            if (d |> int |> float) = d then sprintf "%A" (d |> int) 
            else d |> sprintf "%A"

    let printQ u = function
    | Quantity q -> 
        if u = "" then sprintf "%s" (q |> format)
        else 
            sprintf "%s %s" (q |> format) u
    | QuantityPerKg q -> 
        if u = "" then sprintf "%s" (q |> format)
        else 
            sprintf "%s %s/kg" (q |> format) u
    | QuantityPerM2 q -> 
        if u = "" then sprintf "%s" (q |> format)
        else 
            sprintf "%s %s/m2" (q |> format) u

    match d.NormDose, d.MinDose, d.MaxDose with
    | None, None, None -> ""
    | Some norm, Some min, Some max when norm = min ->
        let min = min |> printQ ""
        let max = max |> printQ d.Unit
        sprintf "%s - %s" min max
    | Some norm, Some min, Some max ->
        let norm = norm |> printQ d.Unit
        let min = min |> printQ ""
        let max = max |> printQ d.Unit
        sprintf "%s (%s - %s)" norm min max
    | Some norm, Some min, None when norm = min ->
        let min = min |> printQ d.Unit
        sprintf "tot %s" min
    | Some norm, Some min, None ->
        let norm = norm |> printQ ""
        let min = min |> printQ d.Unit
        sprintf "%s, vanaf %s" norm min
    | Some norm, None, Some max when norm = max ->
        let max = max |> printQ d.Unit
        sprintf "tot %s" max
    | Some norm, None, None ->
        let norm = norm |> printQ d.Unit
        sprintf "%s" norm
    | Some norm, None, Some max ->
        let norm = norm |> printQ  ""
        let max = max |> printQ d.Unit
        sprintf "%s - %s" norm max
    | None, Some min, Some max ->
        let min = min |> printQ ""
        let max = max |> printQ d.Unit
        sprintf "%s - %s" min max
    | None, Some min, None ->
        let min = min |> printQ d.Unit
        sprintf "vanaf %s" min
    | None, None, Some max ->
        let max = max |> printQ d.Unit
        sprintf "tot %s" max
    |> fun s ->
        let s = 
            if s = "" then s 
            else
                sprintf "**%s**" s

        match d.AbsMaxDose, d.MaxPerDose with
        | None, None -> s
        | Some max, None ->
            let max = max |> printQ d.Unit
            let time =
                d.Freqs
                |> Seq.fold (fun _ f ->
                    f.Time |> snd
                ) ""
            if s = "" then sprintf "**max %s per %s**" max time
            else sprintf "%s (*max %s per %s*)" s max time
        | None, Some keer ->
            let keer = keer |> printQ d.Unit
            if s = "" then sprintf "**max %s per keer**" keer
            else sprintf "%s (max %s per keer)" s keer
        | Some max, Some keer ->
            let keer = keer |> printQ d.Unit
            let max = max |> printQ d.Unit
            let time =
                d.Freqs
                |> Seq.fold (fun _ f ->
                    f.Time |> snd
                ) ""
            if s = "" then 
                sprintf "**max %s per %s en max %s per keer**" max time keer
            else sprintf "%s (max %s per %s en max %s per keer)" s max time keer



let printFreqs (fs : Frequency list) =
    fs
    |> List.distinct
    |> List.groupBy (fun f -> f.Time)
    |> List.map (fun (t, fs) ->
        let c =
            fs
            |> List.sortBy (fun f -> f.Count)
            |> List.map (fun f -> f.Count |> sprintf "%A")
            |> String.concat ", "

        match (t |> fst) with
        | x when x = 1 -> 
            sprintf "%s x / %s" c (t |> snd)
        | _ -> 
            sprintf "%s x / %i %s" c (t |> fst) (t |> snd)
    )
    |> String.concat ", "



let printPat p =
    let printDays d =
        (d / 7) |> sprintf "%A weken"

    let printAge a =
        match a with
        | _ when ((a * 31.) / 7.) < 1. ->
            (a * 31.)
            |> int
            |> fun i ->
                if i = 1 then sprintf "%A dag" i
                else sprintf "%A dagen" i
        | _ when a < 1. ->
            ((a * 31.) / 7.)
            |> int
            |> fun i ->
                if i = 1 then sprintf "%A week" i
                else sprintf "%A weken" i
        | _ when a < 12. ->
            a
            |> int
            |> fun i ->
                if i = 1 then sprintf "%A maand" i
                else sprintf "%A maanden" i
        | _ ->
            (a / 12.)
            |> int
            |> fun i ->
                if i = 1 then sprintf "%A jaar" i
                else sprintf "%A jaar" i

    match p.Gender with
    | Male -> "man "
    | Female -> "vrouw "
    | Unknown _ -> ""
    |> fun s ->
        match p.MinAgeMo, p.MaxAgeMo with
        | Some min, Some max ->
            let min = min |> printAge
            let max = max |> printAge
            sprintf "%sleeftijd %s tot %s " s min max
        | Some min, None ->
            let min = min |> printAge
            sprintf "%sleeftijd vanaf %s " s min
        | None, Some max ->
            let max = max |> printAge
            sprintf "%sleeftijd tot %s " s max
        | _ -> ""
        |> fun s ->
            match p.MinGestAgeDays, p.MaxGestAgeDays, p.MinPMAgeDays, p.MaxPMAgeDays with
            | Some min, Some max, _, _ ->
                let min = min |> printDays
                let max = max |> printDays
                sprintf "%sneonaten zwangerschapsduur %s tot %s" s min max
            | Some min, None, _, _ ->
                let min = min |> printDays
                sprintf "%sneonaten zwangerschapsduur vanaf %s" s min
            | None, Some max, _, _ ->
                let max = max |> printDays
                sprintf "%sneonaten zwangerschapsduur tot %s" s max
            | _, _, Some min, Some max ->
                let min = min |> printDays
                let max = max |> printDays
                sprintf "%sneonaten postconceptie leeftijd %s tot %s" s min max
            | _, _, Some min, None ->
                let min = min |> printDays
                sprintf "%sneonaten postconceptie leeftijd vanaf %s" s min
            | _, _, None, Some max ->
                let max = max |> printDays
                sprintf "%sneonaten postconceptie leeftijd tot %s" s max
            | _ -> s
            |> fun s ->
                let toStr v =
                    if (v |> int |> float) = v then v |> int |> sprintf "%i"
                    else v |> sprintf "%A"

                match p.MinWeightKg, p.MaxWeightKg with
                | Some min, Some max -> sprintf "gewicht %s tot %s kg" (min |> toStr) (max |> toStr)
                | Some min, None     -> sprintf "gewicht vanaf %s kg" (min |> toStr)
                | None,     Some max -> sprintf "gewicht tot %s kg" (max |> toStr)
                | None,     None     -> ""
                |> sprintf "%s %s" s
                |> String.trim


/// See for use of anonymous record in 
/// fold: https://github.com/dotnet/fsharp/issues/6699
let toMarkdown (ds : Types.Dose list) =
    let generic_md = """
# {generic}
"""

    let route_md = """
### Route: {route}
#### Producten
{products}
"""

    let product_md =  """
* {product}
"""

    let indication_md = """
## Indicatie: {indication}
"""

    let dose_md = """
#### Doseringen
"""

    let patient_md = """
* Patient: **{patient}**<br>
{dose}<br>
in **{freqs}**
"""
    
    ({| md = ""; doses = [] |}, ds
    |> List.groupBy (fun d -> d.Generic))
    ||> List.fold (fun acc (generic, ds) ->
        {| acc with
            md = generic_md |> String.replace "{generic}" generic
            doses = ds
        |}
        |> fun r -> 
            if r.doses = List.empty then r
            else
                (r, r.doses |> List.groupBy (fun d -> d.Indication))
                ||> List.fold (fun acc (indication, ds) ->
                    {| acc with
                        md = acc.md + (indication_md |> String.replace "{indication}" indication)
                        doses = ds
                    |}
                    |> fun r -> 
                        if r.doses = List.empty then r
                        else
                            (r, r.doses |> List.groupBy (fun r -> r.Route))
                            ||> List.fold (fun acc (route, ds) ->

                                let s = 
                                    ds
                                    |> List.collect (fun d -> d.Products)
                                    |> List.sortBy (fun p -> p.Concentration)
                                    |> List.map (fun p -> product_md |> String.replace "{product}" p.ProductLabel)
                                    |> List.distinct
                                    |> String.concat "\n"
                                {| acc with
                                    md = acc.md + (route_md 
                                                   |> String.replace "{route}" route
                                                   |> String.replace "{products}" s)
                                                + dose_md
                                    doses = ds
                                |}
                                |> fun r ->
                                    if r.doses = List.empty then r
                                    else
                                        (r, r.doses 
                                            |> List.sortBy (fun d -> d |> sortPat)
                                            |> List.groupBy (fun d -> d |> printPat))
                                        ||> List.fold (fun acc (pat, ds) ->

                                            let dose =
                                                ds
                                                |> List.map printDose
                                                |> List.distinct
                                                |> function
                                                | [d] -> d
                                                | _ -> ""


                                            let freqs =
                                                if dose = "" then ""
                                                else
                                                    ds 
                                                    |> List.collect (fun d -> d.Freqs)
                                                    |> printFreqs

                                            {| acc with
                                                md = 
                                                    if dose = "" then acc.md
                                                    else
                                                        acc.md + (patient_md 
                                                                  |> String.replace "{patient}" pat
                                                                  |> String.replace "{dose}" dose
                                                                  |> String.replace "{freqs}" freqs)
                                            |}
                                        )
                            )
                )
    ) 
    |> fun r -> r.md
