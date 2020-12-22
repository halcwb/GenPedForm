namespace Informedica.Formulary.Client



/// Function to perform a safe null check
module NullCheck =

    let nullOrDef f d v =
        if v |> isNull then d
        else v |> f

    let nullOrDef2 f d v1 v2 =
        if (v1 |> isNull) || (v2 |> isNull) then d
        else f v1 v2
        
    let nullOrDef3 f d v1 v2 v3 =
        if (v1 |> isNull) || (v2 |> isNull) || (v2 |> isNull) then d
        else f v1 v2 v3


module String = 

    open System 
    
    let removeTrailing chars (s : String) =
        s
        |> Seq.rev
        |> Seq.map string
        |> Seq.skipWhile (fun c ->
            chars |> Seq.exists ((=) c)
        )
        |> Seq.rev
        |> String.concat ""


    let removeTrailingZerosFromDutchNumber (s : string) =
        s.Split([|","|], StringSplitOptions.None)
        |> function 
        | [|n; d|] -> 
            let d = d |> removeTrailing ["0"]
            n + "," + d     
        | _ -> s


    /// Replace `os` with `ns` in string `s`.
    let replace =
        NullCheck.nullOrDef3 (fun os ns (s : string) -> s.Replace(os, ns)) ""




module Int32 =

    open System
    open System.Globalization

    let parse s = Int32.Parse(s, Globalization.CultureInfo.InvariantCulture)

    let tryParse (s : string) =
        let (b, n) = Int32.TryParse(s)
        if b then n |> Some else None

    let toStringNumberNL (n: int) = n.ToString()



module Double =

    open System
    open System.Globalization

    let toStringNumberNL p (n: float) = n.ToString()

    let toStringNumberNLWithoutTrailingZeros = 
        toStringNumberNL "" >> String.removeTrailingZerosFromDutchNumber


module Utils =

    open System
    open Informedica.Formulary.Shared.Types
    
    
    let printDose (d : Dose) =
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



    let minToStr f = function
        | MinIncl v -> v |> f |> sprintf "van (incl) %s" 
        | MinExcl v -> v |> f |> sprintf "van (excl) %s" 



    let maxToStr f = function
        | MaxIncl v -> v |> f |> sprintf "tot (incl) %s"
        | MaxExcl v -> v |> f |> sprintf "tot (excl) %s"


    let minMaxToStr f mm =
        let minToStr = minToStr f
        let maxToStr = maxToStr f
        match mm.Min, mm.Max with
        | Some min, Some max -> sprintf "%s %s" (min |> minToStr) (max |> maxToStr)
        | Some min, None -> min |> minToStr
        | None, Some max -> max |> maxToStr
        | None, None -> ""



    let genderToStr = function
        | MaleGender   -> "man"
        | FemaleGender -> "vrouw"



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

            
    let printDays d =
        (d / 7) |> sprintf "%A weken"


    let patientToStr pc = 
        let toStr v =
            if (v |> int |> float) = v then v |> int |> sprintf "%i"
            else v |> sprintf "%A"

        match pc with
        | RootCategory -> ""
        | Gender g -> g |> genderToStr
        | GestationAge mm -> mm |> minMaxToStr (int >> printDays) |> sprintf "zwangerschapsduur: %s weken"
        | PostConceptionalAge mm -> mm |> minMaxToStr (int >> printDays) |> sprintf "post Conceptie Leeftijd: %s"
        | Weight mm -> mm |> minMaxToStr toStr |> sprintf "gewicht: %s kg"
        | BodySurfaceArea mm -> mm |> minMaxToStr toStr |> sprintf "lichaamsoppervlak: %s m2"
        | Age mm -> mm |> minMaxToStr printAge |> sprintf "leeftijd: %s"



    let toString indent (Category(_, cod)) =
        let rec toStr cod indent s =
            match cod with
            | Dose d ->
                match d with
                | Some d -> d |> printDose
                | None   -> "geen dosering\n"
                |> String.replace "*" ""
                |> sprintf "%s: %s" s
            
            | Categories cs ->
                cs
                |> List.map (fun x -> (indent + "\t", x))
                |> List.fold (fun acc (indent, (Category(c, cod))) ->
                    sprintf "%s\n%s- %s" acc indent (c |> patientToStr)
                    |> toStr cod indent
                ) s
        indent
        |> sprintf "%s- patient" 
        |> toStr cod indent


