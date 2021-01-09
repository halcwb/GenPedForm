
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


(Environment.environmentVars ()).TryGetValue "CONN_STR_AP"
|> function 
| true, s ->
    Doses.getDoses s
    |> List.collect (fun d -> d.Freqs)
    |> List.distinct
    |> List.sortBy (fun f ->
        let time =
            match f.Time with
            | (x, u) when u = "dag" -> 0, x
            | (x, u) when u = "dagen" -> 1, x
            | (x, u) when u = "uur" -> 2, x
            | (x, u) when u = "week" -> 3, x
            | (x, u) when u = "weken" -> 4, x
            | (x, u) when u = "maand" -> 5, x
            | (x, u) when u = "maanden" -> 6, x
            | _ -> 99, 0
        time, f.Count
    )
    |> List.map (fun f ->
        if f.Time |> fst = 1 then f.Time |> snd
        else
            f.Time
            |> snd
            |> sprintf "%i %s" (f.Time |> fst)            
        |> sprintf "%i x / %s" f.Count
    )
        
| _ -> failwith "could not get connections string"
