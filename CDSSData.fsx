
#r "nuget: Informedica.GenUnits.lib, 1.0.1-beta"
#r "nuget: Informedica.GenUtils.Lib, 0.5.0-beta"

open System
open System.IO


let readAllLines path =
    path
    |> File.ReadAllLines


type DoseRecord =
    {
        Generic : string
    }

open MathNet.Numerics
open Informedica.GenUnits.Lib
open Informedica.GenUtils.Lib
open Informedica.GenUtils.Lib.BCL


let parseFreq s =
    let times =
        [
            "minuten", "minuut"
            "uren", "uur"
            "dagen", "dag"
            "weken", "week"
            "maanden", "maand"
            "jaren", "jaar"
        ]
    
    s
    |> String.trim
    |> String.split "/"
    |> function
    | [v] -> 
        v
        |> String.replace "x" ""
//        |> fun s -> s |> printfn "parsing: %s"; s
        |> Double.tryParse
        |> Option.bind BigRational.fromFloat
        |> function
        | None ->  None
        | Some br -> 
            br
            |> ValueUnit.create ValueUnit.Units.Count.times
            |> Some
    | [v;u] ->
        u
        |> String.trim
        |> String.split " "
        |> function
        | [tv;tu] ->
            v,
            tv 
            |> Double.parse 
            |> BigRational.fromFloat
            |> Option.defaultValue 1N , tu
        | [tu] -> v, 1N, tu
        | _    -> v, 1N, ""
        |> fun (v, tv, tu) ->
            times
            |> List.tryFind (fst >> ((=) tu))
            |> Option.map snd
            |> Option.defaultValue tu 
            |> sprintf "%s[Time]"
            |> ValueUnit.Units.fromString
            |> Option.bind ((ValueUnit.setUnitValue tv) >> Some)
            |> function
            | None   -> None
            | Some u ->
                v
                |> String.replace "x" ""
                |> Double.parse
                |> BigRational.fromFloat
                |> function
                | None ->  None
                | Some br -> 
                    ValueUnit.Units.Count.times
                    |> ValueUnit.per u
                    |> fun u -> ValueUnit.create u br
                    |> Some
    | _ -> None


"2 x / 3 dagen"
|> parseFreq


let url = 
    "https://docs.google.com/spreadsheets/d/{{ID}}/gviz/tq?tqx=out:csv&sheet={{sheet_name}}"
    |> String.replace "{{ID}}" "1LRCEtjvFn7w0zQWeVQETzIFArf42_AFwwdJl2I2yTpE"
    |> String.replace "{{sheet_name}}" "Data"


let url2 = 
    "https://docs.google.com/spreadsheets/d/{{ID}}/export?gid=0&format=tsv"
    |> String.replace "{{ID}}" "1LRCEtjvFn7w0zQWeVQETzIFArf42_AFwwdJl2I2yTpE"
    |> String.replace "{{sheet_name}}" "Data"

printfn "%s" url

open System.Net

let download url =
    async {
        let req = WebRequest.Create(Uri(url))
        use! resp = req.AsyncGetResponse()
        use stream = resp.GetResponseStream()
        use reader = new StreamReader(stream)
        return reader.ReadToEnd()
    }    

url2
|> download
|> Async.RunSynchronously
|> printfn "%s"


let parse url =
    url
    |> download
    |> Async.RunSynchronously
    |> String.split "\n"
    |> Seq.map (String.split "\t")
    |> fun data ->
        let headers = data |> Seq.head
        let getColumn s sl = 
            headers
            |> Seq.tryFindIndex ((=) s)
            |> function
            | None   -> 
                sprintf "cannot find %s" s
                |> failwith
            | Some i -> sl |> Seq.item i

        let parseVU g s =
            if s = "" then None
            else
                s 
                |> String.split " " 
                |> Seq.toList
                |> function
                | [v;u] -> 
                    v 
                    |> String.replace "," "."
                    |> Double.parse
                    |> BigRational.fromFloat
                    |> function
                    | Some br ->
                        sprintf "%s %s[%s]" (br |> string) u g
                        |> Api.eval
                        |> Some 
                    | None -> None
                |  _ -> None


        let parseTime = parseVU "Time"
        
        let parseWeight = parseVU "Weight"

        let parseBSA = parseVU "BSA"

        data
        |> Seq.skip 1
        |> Seq.map (fun r ->
            {|
                Generic = r |> getColumn "Generic"
                Shape = r |> getColumn "Shape"
                Route = r |> getColumn "Route"
                Indication = r |> getColumn "Indication"
                Specialty = r |> getColumn "Specialty"
                Gender = r |> getColumn "Gender"
                MinAge = 
                    r 
                    |> ((getColumn "MinAge") >> parseTime)
                MaxAge = 
                    r 
                    |> ((getColumn "MaxAge") >> parseTime)
                MinGA = 
                    r 
                    |> ((getColumn "MinGA") >> parseTime)
                MaxGA = 
                    r 
                    |> ((getColumn "MaxGA") >> parseTime)
                MinPM = 
                    r 
                    |> ((getColumn "MinPM") >> parseTime)
                MaxPM = 
                    r 
                    |> ((getColumn "MaxPM") >> parseTime)
                MinWeight = 
                    r 
                    |> ((getColumn "MinWeight") >> parseWeight)
                MaxWeight = 
                    r 
                    |> ((getColumn "MaxWeight") >> parseWeight)
                MinBSA = 
                    r 
                    |> ((getColumn "MinBSA") >> parseBSA)
                MaxBSA = 
                    r 
                    |> ((getColumn "MaxBSA") >> parseBSA)
                Name =  r |> getColumn "Name"
                Substance = r |> getColumn "Substance"
                Freqs =
                    r
                    |> getColumn "Freqs"
                    |> String.split "||"
                    |> List.map parseFreq
                    |> List.filter Option.isSome
                    |> List.map Option.get
                MinDuration =
                    r 
                    |> ((getColumn "MinDuration") >> parseTime)
                MaxDuration =
                    r 
                    |> ((getColumn "MaxDuration") >> parseTime)
                MinInterval =
                    r 
                    |> ((getColumn "MinDuration") >> parseTime)
                MaxInterval =
                    r 
                    |> ((getColumn "MaxDuration") >> parseTime)
            |} 
        ) 


//"./resources/CDSSData.tsv"
url2
|> parse
|> Seq.filter (fun r -> r.Generic = "dexamethason")
|> Seq.iter (printfn "%A")


