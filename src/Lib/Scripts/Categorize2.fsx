

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

// testing
initCategory
|> addGenderCategory [ RootCategory ]
|> addAgeCategory  [ Male |> Gender ] (minMax |> setMinIncl 10.)
|> addAgeCategory [ Male |> Gender ] (minMax |> setMinIncl 1.) 
|> splitMinMaxCategory [ minMax |> setMinIncl 1. |> Age ] 12. false
|> addGestAgeCategory [minMax |> setMaxExcl 1. |> Age]  (minMax |> setMinIncl 32.)
|> splitMinMaxCategory [ minMax |> setMinIncl 32. |> GestationAge ] 36. false
|> addWeightCategory  [ minMax |> setMinIncl 1. |> setMaxExcl 12. |> Age ] (minMax |> setMaxIncl 10.)
|> addWeightCategory  [ Female |> Gender ] (minMax |> setMaxIncl 10.) 
|> splitMinMaxCategory [ Female |> Gender; minMax |> setMinExcl 10. |> Weight] 20. true
//|> fun x -> printfn "Start search"; x
//|> findParent [ RootCategory; Male |> Gender; minMax |> setMaxExcl 1. |> Age; minMax |> setMinIncl 36. |> GestationAge ]
//|> clearCategory [ Male |> Gender ]
// |> addGenderCategory [ RootCategory ]
|> toString ""
|> printfn "%s"


initCategory
|> addAgeCategory [ RootCategory ] (minMax |> setMinIncl 1. |> setMaxExcl 12.) 
|> addGenderCategory [ minMax |> setMinIncl 1. |> setMaxExcl 12. |> Age ]
|> findParent [ Male |> Gender ]


initCategory
|> addAgeCategory  [ RootCategory ] (minMax |> setMaxExcl 1.)
|> addGestAgeCategory  [ minMax |> setMaxExcl 1. |> Age]  (minMax |> setMinIncl 28. |> setMaxIncl 37.)
|> toString ""
|> printfn "%s"


initCategory
|> addAgeCategory [ RootCategory ] (minMax |> setMaxExcl (7. / 28.))
|> splitMinMaxCategory [ minMax |> setMinIncl (7./28.) |> Age ] 1. false
|> addGestAgeCategory [ minMax |> setMaxExcl (7. / 28.) |> Age ]  (minMax |> setMaxExcl 32.)
|> splitMinMaxCategory [ minMax |> setMinIncl 32. |> GestationAge ] 37. true
|> addGestAgeCategory [ minMax |> setMinIncl (7. / 28.) |> setMaxExcl 1. |> Age ] (minMax |> setMaxExcl 37.)
|> toString ""
|> printfn "%s"


initCategory
|> addGenderCategory [ RootCategory ]
|> addWeightCategory [ Male |> Gender ] (minMax |> setMaxExcl 10.)
|> addWeightCategory [ Female |> Gender ] (minMax |> setMaxExcl 10.)
|> splitMinMaxCategory [ minMax |> setMinIncl 10. |> Weight ] 20. true
|> toString ""
|> printfn "%s"



(Environment.environmentVars ()).TryGetValue "CONN_STR_AP"
|> function 
| true, s ->
    printfn "=== Start ==="
    Doses.getDoses s
//    |> List.take 500
//    |> List.filter (fun d -> d.Generic = "paracetamol") //&& d.Route = "iv")
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
    let add x xs =
        if xs |> List.length = 0 then [ [ x ] ]
        else
            xs
            |> List.map (fun xs' -> xs' @ [ x])

    let rec get (acc : PatientCategory list list) (Category(pc, doc)) =
        doc
        |> function
        | Categories cs when cs |> List.length > 0 ->
            cs
            |> List.collect (get (acc |> add pc))

        | _ -> 
            acc
            |> add pc
                
    c
    |> get []


// testing
initCategory
|> addGenderCategory [ RootCategory ]
|> addAgeCategory  [ Male |> Gender ] (minMax |> setMinIncl 10.)
|> getPatientDoses


let mapCategorizedGeneric (g: CategorizedGeneric) =
    g.Shapes
    |> List.collect(fun s ->
        s.Routes
        |> List.collect (fun r -> 
            r.Indications
            |> List.collect (fun i ->
                i.Patient
                |> getPatientDoses 
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