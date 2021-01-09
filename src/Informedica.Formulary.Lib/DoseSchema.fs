namespace Informedica.Formulary.Lib

module DoseSchema =

    open Types

    
    let printTimeUnit min max = 
        match min, max with
        | None, None        -> ""
        | Some (t, u), None -> sprintf "min %i %s" t u
        | None, Some (t, u)  -> sprintf "max %i %s" t u
        | Some (t1, u), Some (t2, _) ->
            sprintf "%i - %i %s" t1 t2 u


    let printSubstanceDose freqs (d : SubstanceDose) =
        let format d =
            let itostr = Int32.toStringNumberNL
            let dtostr = Double.toStringNumberNLWithoutTrailingZeros
            d 
            |> fun d ->
                if (d |> int |> float) = d then 
                    sprintf "%s" (d |> int |> itostr) 
                else 
                    d 
                    |> dtostr
                    |> sprintf "%s"

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
                    freqs
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
                    freqs
                    |> Seq.fold (fun _ f ->
                        f.Time |> snd
                    ) ""
                if s = "" then 
                    sprintf "**max %s per %s en max %s per keer**" max time keer
                else sprintf "%s (max %s per %s en max %s per keer)" s max time keer


    let printItem (di : DoseItem) =
        di.Frequencies 
        |> DoseRecords.printFreqs
        |> sprintf "in %s"
        |> fun s ->
            printTimeUnit di.MinDuration di.MaxDuration
            |> sprintf "%s\ngedurende %s" s
        |> fun s ->
            di.SubstanceDoses
            |> List.map (printSubstanceDose di.Frequencies)
            |> String.concat "\n"
            |> sprintf "%s\n%s" s
            

    let print (ds : DoseSchema) =
        ds
        |> List.map printItem
        |> String.concat "/n"


    let mapToSchema (ds : DoseRecord list) =
        ds
        |> List.groupBy (fun d -> 
            {|
                Name = d.Name
                Freqs = d.Freqs
                MinDuration = d.MinDuration
                MaxDuration = d.MaxDuration
            |}
        )
        |> List.map (fun (di, ds) ->
            {
                Name = di.Name
                Frequencies = di.Freqs
                MinDuration = di.MinDuration
                MaxDuration = di.MaxDuration
                SubstanceDoses = 
                    ds
                    |> List.map (fun d ->
                        {
                            Substance = ""
                            Unit = d.Unit
                            NormDose = d.NormDose
                            MinDose = d.MinDose
                            MaxDose = d.MaxDose
                            MinDoseRate = None
                            MaxDoseRate = None
                            AbsMaxDose = d.AbsMaxDose
                            MaxPerDose = d.MaxPerDose
                        }
                    )
            }
        )

