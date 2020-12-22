

#load "../../../.paket/load/net472/Library/library.group.fsx"
#load "../../../.paket/load/netcoreapp3.1/Informedica.GenUtils.Lib.fsx"

#time

#load "../Utils.fs"
#load "../Types.fs"
#load "../Doses.fs"
#load "../Categorize.fs"

open System
open System.Collections.Generic
open Informedica.Formulary.Lib

open Types
open Categorize


AppContext.SetSwitch("Switch.Microsoft.Data.SqlClient.UseManagedNetworkingOnWindows", true)

// testing
initCategory
|> addGenderCategory [ RootCategory ]
|> addAgeCategory  [ MaleGender |> Gender ] (minMax |> setMinIncl 10.)
|> addAgeCategory [ MaleGender |> Gender ] (minMax |> setMinIncl 1.) 
|> splitMinMaxCategory [ minMax |> setMinIncl 1. |> Age ] 12. false
|> addGestAgeCategory [minMax |> setMaxExcl 1. |> Age]  (minMax |> setMinIncl 32.)
|> splitMinMaxCategory [ minMax |> setMinIncl 32. |> GestationAge ] 36. false
|> addWeightCategory  [ minMax |> setMinIncl 1. |> setMaxExcl 12. |> Age ] (minMax |> setMaxIncl 10.)
|> addWeightCategory  [ FemaleGender |> Gender ] (minMax |> setMaxIncl 10.) 
|> splitMinMaxCategory [ FemaleGender |> Gender; minMax |> setMinExcl 10. |> Weight] 20. true
//|> fun x -> printfn "Start search"; x
//|> findParent [ RootCategory; Male |> Gender; minMax |> setMaxExcl 1. |> Age; minMax |> setMinIncl 36. |> GestationAge ]
//|> clearCategory [ Male |> Gender ]
// |> addGenderCategory [ RootCategory ]
|> toString ""
|> printfn "%s"


initCategory
|> addAgeCategory [ RootCategory ] (minMax |> setMinIncl 1. |> setMaxExcl 12.) 
|> addGenderCategory [ minMax |> setMinIncl 1. |> setMaxExcl 12. |> Age ]
|> findParent [ MaleGender |> Gender ]


initCategory
|> addAgeCategory  [ RootCategory ] (minMax |> setMaxExcl 1.)
|> addGestAgeCategory  [ minMax |> setMaxExcl 1. |> Age]  (minMax |> setMinIncl 28. |> setMaxIncl 37.)
|> toString ""
|> printfn "%s"

// gentamicine example
initCategory
|> addAgeCategory [ RootCategory ] (minMax |> setMaxExcl (7. / 28.))
|> splitMinMaxCategory [ minMax |> setMinIncl (7./28.) |> Age ] 1. false
|> addGestAgeCategory [ minMax |> setMaxExcl (7. / 28.) |> Age ]  (minMax |> setMaxExcl (32. * 7.))
|> splitMinMaxCategory [ minMax |> setMinIncl (32. * 7.) |> GestationAge ] (37. * 7.) true
|> addGestAgeCategory [ minMax |> setMinIncl (7. / 28.) |> setMaxExcl 1. |> Age ] (minMax |> setMaxExcl (37. * 7.))
|> toString ""
|> printfn "%s"


initCategory
|> addGenderCategory [ RootCategory ]
|> addWeightCategory [ MaleGender |> Gender ] (minMax |> setMaxExcl 10.)
|> addWeightCategory [ FemaleGender |> Gender ] (minMax |> setMaxExcl 10.)
|> splitMinMaxCategory [ minMax |> setMinIncl 10. |> Weight ] 20. true
|> toString ""
|> printfn "%s"



(Environment.environmentVars ()).TryGetValue "CONN_STR_AP"
|> function 
| true, s ->
    printfn "=== Start ==="
    Doses.getDoses s
//    |> List.take 500
    |> List.filter (fun d -> d.Generic = "paracetamol" && d.Route = "iv")
    |> mapDoses
    |> List.take 1
    |> List.iter (fun d ->
        printfn "%s" d.Generic 
        d.Shapes
        |> List.iter (fun d ->
            printfn "\t%s" d.Shape 
            d.Routes
            |> List.iter (fun d ->
                printfn "\t\t%s" d.Route
                d.Indications
                |> List.iter (fun d ->
                    printfn "\t\t\t%s" d.Indication
                    let p = d.Patient |> toString "\t\t\t"
                    printfn "%s" p
                )
            )
        )
    )
| _ -> failwith "cannot access database"


let getPatientDoses (c: Category) = 
    let add (sd, pc) xs =
        if xs |> List.length = 0 then [ sd, [ pc ] ]
        else
            xs
            |> List.map (fun xs' -> sd, (xs' |> snd) @ [ pc ])

    let rec get acc (Category(pc, doc)) =
        doc
        |> function
        | Categories cs when cs |> List.length > 0 ->
            cs
            |> List.collect (get (acc |> add (None, pc)))

        | _ -> 
            match doc with
            | (Dose sd) ->
                acc
                |> add (sd, pc)
            | _ ->
                acc
                |> add (None, pc)

    c
    |> get []


// testing
initCategory
|> addGenderCategory [ RootCategory ]
|> addAgeCategory  [ MaleGender |> Gender ] (minMax |> setMinIncl 10.)
|> getPatientDoses


let mapCategorizedGeneric (g: CategorizedGeneric) =
    let getMin = Option.bind (getMinValue >> Some)
    let getMax = Option.bind (getMaxValue >> Some)

    let get pcs f = 
        pcs
        |> List.fold (fun acc pc ->
            if acc |> Option.isSome then acc
            else pc |> f
        ) None 

    g.Shapes
    |> List.collect(fun s ->
        s.Routes
        |> List.collect (fun r -> 
            r.Indications
            |> List.collect (fun i ->
                i.Patient
                |> getPatientDoses 
                |> List.map (fun (sd, pcs) ->
                    {
                        Doses.dose with
                            Generic = g.Generic
                            Shape = s.Shape
                            Route = r.Route
                            Indication = i.Indication
                            Gender = 
                                pcs
                                |> List.fold (fun acc pc ->
                                    if acc = (Types.Unknown "") then
                                        match pc with
                                        | Gender(MaleGender) -> Types.Male
                                        | Gender(FemaleGender) -> Types.Male
                                        | _ -> acc
                                    else acc
                                ) (Types.Unknown "")
                            MinAgeMo = 
                                fun pc ->
                                    match pc with
                                    | Age mm -> mm.Min |> getMin
                                    | _ -> None
                                |> get pcs
                            MaxAgeMo = 
                                fun pc ->
                                    match pc with
                                    | Age mm -> mm.Max |> getMax
                                    | _ -> None
                                |> get pcs
                            MinGestAgeDays = 
                                fun pc ->
                                    match pc with
                                    | GestationAge mm -> mm.Min |> getMin |> Option.map int
                                    | _ -> None
                                |> get pcs
                            MaxGestAgeDays = 
                                fun pc ->
                                    match pc with
                                    | GestationAge mm -> mm.Max |> getMax |> Option.map int
                                    | _ -> None
                                |> get pcs
                            MinPMAgeDays = 
                                fun pc ->
                                    match pc with
                                    | PostConceptionalAge mm -> mm.Min |> getMin |> Option.map int
                                    | _ -> None
                                |> get pcs
                            MaxPMAgeDays = 
                                fun pc ->
                                    match pc with
                                    | PostConceptionalAge mm -> mm.Max |> getMax |> Option.map int
                                    | _ -> None
                                |> get pcs
                            MinWeightKg = 
                                fun pc ->
                                    match pc with
                                    | Weight mm -> mm.Min |> getMin 
                                    | _ -> None
                                |> get pcs
                            MaxWeightKg = 
                                fun pc ->
                                    match pc with
                                    | Weight mm -> mm.Max |> getMax
                                    | _ -> None
                                |> get pcs
                            Freqs = 
                                match sd with
                                | Some d -> d.Freqs
                                | None   -> []
                            Unit = 
                                match sd with
                                | Some d -> d.Unit
                                | None   -> ""
                            NormDose =
                                match sd with
                                | Some d -> d.NormDose
                                | None   -> None
                            MinDose =
                                match sd with
                                | Some d -> d.MinDose
                                | None   -> None
                            MaxDose =
                                match sd with
                                | Some d -> d.MaxDose
                                | None   -> None
                            AbsMaxDose =
                                match sd with
                                | Some d -> d.AbsMaxDose
                                | None   -> None
                            MaxPerDose =
                                match sd with
                                | Some d -> d.MaxPerDose
                                | None   -> None
                            StartDose =
                                match sd with
                                | Some d -> d.StartDose
                                | None   -> None
                            Products =
                                match sd with
                                | Some d -> d.Products
                                | None   -> []
                    }
                )
            )
        )
    )



(Environment.environmentVars ()).TryGetValue "CONN_STR_AP"
|> function 
| true, s ->
    printfn "=== Start ==="
    Doses.getDoses s
//    |> List.take 500
//    |> List.filter (fun d -> d.Generic = "paracetamol") //&& d.Route = "iv")
    |> mapDoses
    |> List.head
    |> mapCategorizedGeneric
//    |> List.length
| _ -> failwith "cannot access database"