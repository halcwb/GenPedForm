
//#r "../bin/Release/net472/Microsoft.Data.SqlClient.dll"
//#r "../bin/Release/net472/Lib.dll"
#load "../../../.paket/load/netstandard2.1/Library/library.group.fsx"
#load "../../../.paket/load/netstandard2.1/Informedica.GenUtils.Lib.fsx"

#time

#load "../Utils.fs"
#load "../Types.fs"
#load "../Doses.fs"

open System
open System.Collections.Generic

open Informedica.Formulary.Lib

fsi.AddPrinter<DateTime> (sprintf "%A")



(Environment.environmentVars ()).TryGetValue "CONN_STR_AP"
|> function 
| true, s ->
    Doses.getDoses s
    |> List.filter (fun d -> 
        d.Generic = "paracetamol" 
    )
    |> Doses.toMarkdown
    |> printfn "%A"
        
| _ -> failwith "could not get connections string"
