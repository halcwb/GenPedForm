
//#r "../bin/Release/net472/Microsoft.Data.SqlClient.dll"
//#r "../bin/Release/net472/Lib.dll"
#load "../../../.paket/load/netstandard2.1/Library/library.group.fsx"
#load "../../../.paket/load/netstandard2.1/Library/Informedica.GenUtils.Lib.fsx"

#time

#load "../Utils.fs"
#load "../RowReader.fs"
#load "../Sql.fs"
#load "../Types.fs"
#load "../Doses.fs"

open System
open System.Collections.Generic
open Utils

AppContext.SetSwitch("Switch.Microsoft.Data.SqlClient.UseManagedNetworkingOnWindows", true)

fsi.AddPrinter<DateTime> (sprintf "%A")


/// Returns enviroment variables as a dictionary
let environmentVars() =
    let variables = Dictionary<string, string>()
    let userVariables = Environment.GetEnvironmentVariables(EnvironmentVariableTarget.User)
    let processVariables = Environment.GetEnvironmentVariables(EnvironmentVariableTarget.Process)
    for pair in userVariables do
        let variable = unbox<Collections.DictionaryEntry> pair
        let key = unbox<string> variable.Key
        let value = unbox<string> variable.Value
        if not (variables.ContainsKey(key)) && key <> "PATH" then variables.Add(key, value)
    for pair in processVariables do
        let variable = unbox<Collections.DictionaryEntry> pair
        let key = unbox<string> variable.Key
        let value = unbox<string> variable.Value
        if not (variables.ContainsKey(key)) && key <> "PATH" then variables.Add(key, value)
    variables


//let printPat = Doses.printPat
//let printDose = Doses.printDose
//let printFreq = Doses.printFreq
//let sortPat = Doses.sortPat



(environmentVars ()).TryGetValue "CONN_STR_AP"
|> function 
| true, s ->
    Doses.getDoses s
    |> List.filter (fun d -> 
        d.Generic = "paracetamol" 
    )
    |> Doses.toMarkdown
    |> printfn "%A"
        
| _ -> failwith "could not get connections string"
