
#load @"Scripts\..\..\..\..\.paket\load\netcoreapp3.1\Server\Newtonsoft.Json.fsx"
#load "../../../.paket/load/net472/Library/library.group.fsx"
#load "../../../.paket/load/netcoreapp3.1/Informedica.GenUtils.Lib.fsx"

#time

#load "../../Informedica.Formulary.Lib/Utils.fs"
#load "../../Informedica.Formulary.Lib/Types.fs"
#load "../../Informedica.Formulary.Lib/Doses.fs"
#load "../../Informedica.Formulary.Lib/Categorize.fs"
#load "../../Informedica.Formulary.Shared/Types.fs"


open Newtonsoft


let inline mapType<'T1, 'T2> (x : 'T1) = 
    x
    |> Json.JsonConvert.SerializeObject
    |> Json.JsonConvert.DeserializeObject<'T2>



open System
open System.Collections.Generic
open Informedica.Formulary.Lib

// AppContext.SetSwitch("Switch.Microsoft.Data.SqlClient.UseManagedNetworkingOnWindows", true)

open Categorize


(Environment.environmentVars ()).TryGetValue "CONN_STR_AP"
|> function 
| true, s ->
    printfn "=== Start ==="
    Doses.getDoses s
//    |> List.take 500
//    |> List.filter (fun d -> d.Generic = "gentamicine" && d.Route = "iv")
    |> mapDoses
    |> List.map (mapType<Types.CategorizedGeneric, Informedica.Formulary.Shared.Types.CategorizedGeneric>)
 //   |> List.take 1
//    |> List.iter (fun d ->
//        printfn "%s" d.Generic 
//        d.Shapes
//        |> List.iter (fun d ->
//            printfn "\t%s" d.Shape 
//            d.Routes
//            |> List.iter (fun d ->
//                printfn "\t\t%s" d.Route
//                d.Indications
//                |> List.iter (fun d ->
//                    printfn "\t\t\t%s" d.Indication
////                    let p = d.Patient |> mapType<Informedica.Formulary.Shared.Types.Category, Types.Category> |> toString "\t\t\t"
//                    let p = d.Patient |> toString "\t\t\t"
//                    printfn "%s" p
//                )
//            )
//        )
//    )
    |> ignore
    printfn "=== Finished ==="
| _ -> failwith "cannot access database"

