

//#r "../bin/Release/net472/Microsoft.Data.SqlClient.dll"
//#r "../bin/Release/net472/Lib.dll"
#load "../../../.paket/load/netstandard2.1/Library/library.group.fsx"
#load "../../../.paket/load/netcoreapp3.1/Informedica.GenUtils.Lib.fsx"

#time

#load "../Utils.fs"
#load "../Types.fs"
#load "../Doses.fs"
#load "../Categorize.fs"

open System
open System.Collections.Generic
open Utils

AppContext.SetSwitch("Switch.Microsoft.Data.SqlClient.UseManagedNetworkingOnWindows", true)

open Categorize

(Environment.environmentVars ()).TryGetValue "CONN_STR_AP"
|> function 
| true, s ->
    printfn "=== Start ==="
    Doses.getDoses s
    |> List.filter (fun r -> 
        r.Generic = "paracetamol" &&
        r.Route = "iv"
    )
//    |> List.take 1000
    |> List.collect (fun d -> 
        (0, [ patient ])
        |> mapGender d
        |> mapDoseAgeMin d
        |> mapDoseAgeMax d
        |> mapDoseGestAgeMin d
        |> mapDoseGestAgeMax d
        |> mapDosePostAgeMin d
        |> mapDosePostAgeMax d
        |> mapDoseWeightMin d
        |> mapDoseWeightMax d
        |> fun (n, pats) ->
            pats
            |> List.mapi (fun i pc ->
                if i = n then 
                    {| 
                        Generic = d.Generic
                        Shape = d.Shape
                        Route = d.Route
                        Indication = d.Indication
                        PatientCategory = pc
                        Dose = Some d
                    |}
                else 
                    {| 
                        Generic = d.Generic
                        Shape = d.Shape
                        Route = d.Route
                        Indication = d.Indication
                        PatientCategory = pc
                        Dose = None
                    |}
            )
    )
    |> List.iter (fun r ->
        let d =
            match r.Dose with
            | Some d -> 
                d 
                |> Doses.printDose
                |> String.replace "*" ""
            | None -> "Geen dosering"
        printfn "%s %s %s %s" 
            r.Generic 
            r.Shape 
            r.Route 
            r.Indication
        printfn "%s: %s"
            (r.PatientCategory |> print)
            d
    )

    printfn "=== Ready ==="
        
| _ -> failwith "could not get connections string"



// Testing

[ 
    { 
    Doses.dose with 
        MinAgeMo = Some (365. * 2.)
        MinWeightKg = Some 10.
        MaxWeightKg = Some 25.
    }  
    { 
        Doses.dose with 
            MinAgeMo = Some (365. * 2.)
            MinWeightKg = Some 25.
            MaxWeightKg = Some 50.
    }  

]
|> List.map (fun d ->
    (0, [ patient ])
    |> mapGender d
    |> mapDoseAgeMin d
    |> mapDoseAgeMax d
    |> mapDoseGestAgeMin d
    |> mapDoseGestAgeMax d
    |> mapDosePostAgeMin d
    |> mapDosePostAgeMax d
    |> mapDoseWeightMin d
    |> mapDoseWeightMax d
)
|> List.distinct


/// Checks whether `Minimum` **m2** > **m1**
/// Note that the fact that a Minimum is inclusive or exclusive 
/// must be taken into account.
let minLTmin m1 m2 =
    match m2, m1 with
    | MinIncl m2', MinIncl m1'
    | MinExcl m2', MinExcl m1'
    | MinIncl m2', MinExcl m1' -> m2' > m1'
    | MinExcl m2', MinIncl m1' -> m2' >= m1'

/// Checks whether `Minimum` **m2** <= **m1**
let minSTEmin m1 m2 = m2 |> minLTmin m1 |> not


/// Checks whether `Maximum` **m2** > **m1**
/// Note that the fact that a maximum is inclusive or exclusive
/// must be taken into account.
let maxLTmax m1 m2 =
    match m2, m1 with
    | MaxIncl m2', MaxIncl m1'
    | MaxExcl m2', MaxExcl m1'
    | MaxExcl m2', MaxIncl m1' -> m2' > m1'
    | MaxIncl m2', MaxExcl m1' -> m2' >= m1'


/// Checks whether `Maximum` **m2** <= **m1**
let maxSTEmax m1 m2 = m2 |> maxLTmax m1 |> not


let maxLTEmax m1 m2 = m1 = m2 || (m2 |> maxLTmax m1)


/// Checks whether `Minimum` **min** > `Maximum` **max**.
/// Note that inclusivity or exclusivity of a minimum and maximum must be
/// accounted for.
let minLTmax max min =
    match min, max with
    | Minimum.MinIncl min', Maximum.MaxIncl max' -> min' > max'
    | Minimum.MinExcl min', Maximum.MaxIncl max'
    | Minimum.MinExcl min', Maximum.MaxExcl max'
    | Minimum.MinIncl min', Maximum.MaxExcl max' -> min' >= max'


/// Checks whether `Minimum` **min** <= `Maximum` **max**
let minSTEmax max min = min |> minLTmax max |> not


let getMinMaxOverlap mm1 mm2 =
    if mm1 = mm2 then (Some mm1, Some mm2)
    else
        match mm1.Min, mm2.Min, mm1.Max, mm2.Max with
        | Some min1, Some min2, Some max1, Some max2 when min1 |> minSTEmin min2 && max1 |> maxLTEmax max2 -> (Some mm1, None)
        | Some min1, Some min2, Some max1, Some max2 when min2 |> minSTEmin min1 && max2 |> maxLTEmax max1 -> (None, Some mm2)
        | Some min1, Some _,    Some _,    Some max2 when min1 |> minLTmax max2 -> (None, None)
        | Some _,    Some min2, Some max1, Some _    when min2 |> minLTmax max1 -> (None, None)
        | _ -> (Some mm1, Some mm2)


let removeOverlapping (pcs : PatientCategory list) =
    pcs
    |> List.fold (fun acc pc ->
        let xs =
            pcs
            |> List.filter ((<>) pc)
        
    ) []
