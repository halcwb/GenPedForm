

//#r "../bin/Release/net472/Microsoft.Data.SqlClient.dll"
//#r "../bin/Release/net472/Lib.dll"
#load "../../../.paket/load/netstandard2.1/Library/library.group.fsx"
#load "../../../.paket/load/netcoreapp3.1/Informedica.GenUtils.Lib.fsx"

#time

#load "../Utils.fs"
#load "../Types.fs"
#load "../Doses.fs"

open System
open System.Collections.Generic
open Utils

AppContext.SetSwitch("Switch.Microsoft.Data.SqlClient.UseManagedNetworkingOnWindows", true)


type MinMax =
    {
        Min : Minimum option
        Max : Maximum option
    }
and Minimum = | MinIncl of float | MinExcl of float
and Maximum = | MaxIncl of float | MaxExcl of float


let minmax = { Min = None; Max = None }


let checkMinMax mm =
    match mm.Min, mm.Max with
    | None, None 
    | Some _, None
    | None, Some _ -> true
    | Some min, Some max ->
        match min, max with
        | MinExcl v1, MaxIncl v2 
        | MinIncl v1, MaxExcl v2
        | MinExcl v1, MaxExcl v2 -> v1 < v2
        | MinIncl v1, MaxIncl v2 -> v1 <= v2


let setMin b v mm =
    {
        mm with
            Min = 
                if b then v |> MinIncl else v |> MinExcl
                |> Some
    }
    |> fun x -> if x |> checkMinMax then x else mm


let setMax b v mm =
    {
        mm with
            Max = 
                if b then v |> MaxIncl else v |> MaxExcl
                |> Some
    }
    |> fun x -> if x |> checkMinMax then x else mm


let setMaxMin min =
        match min with
        | MinIncl v -> MaxExcl v
        | MinExcl v -> MaxIncl v
        |> Some
        

let setMinMax max =
    match max with
    | MaxIncl v -> MinExcl v
    | MaxExcl v -> MinIncl v
    |> Some


let evalMinMax mm =
    match mm.Min, mm.Max with
    | None, None -> [ mm ]
    | Some min, None ->
        [
            { minmax with Max = setMaxMin min }
            mm
        ]
    | None, Some max ->
        [
            mm
            { minmax with Min = setMinMax max}
        ]
    | Some min, Some max ->
        [
            { minmax with Max = setMaxMin min }
            mm
            { minmax with Min = setMinMax max}
        ]



type AgeCategory = MinMax


type WeightCategory = MinMax


type GestationalAge = MinMax


type PostConceptionalAge = MinMax


type PatientCategory =
    {
        Gender : Gender option
        Age : AgeCategory
        Weight : WeightCategory
        GestAge : GestationalAge
        PostAge : PostConceptionalAge
    }
and Gender = Male | Female


let categorize get set pat =
    pat
    |> get
    |> evalMinMax
    |> List.map (fun mm ->
        pat |> set mm
    )


let categorizeAge pat =
    let get pat = pat.Age
    let set mm pat = { pat with Age = mm}
    pat
    |> categorize get set


let categorizeGestAge pat =
    let get pat = pat.GestAge
    let set mm pat = { pat with GestAge = mm}
    pat
    |> categorize get set


let categorizePostAge pat =
    let get pat = pat.PostAge
    let set mm pat = { pat with PostAge = mm}
    pat
    |> categorize get set


let categorizeWeight pat =
    let get pat = pat.Weight
    let set mm pat = { pat with Weight = mm}
    pat
    |> categorize get set


let setAgeMin min b pc =
    {
        pc with
            Age = pc.Age |> setMin min b
    }
    |> categorizeAge


let setAgeMax max b pc =
    {
        pc with
            Age = pc.Age |> setMax max b
    }
    |> categorizeAge


let setGestAgeMin min b pc =
    {
        pc with
            GestAge = pc.GestAge |> setMin min b
    }
    |> categorizeGestAge


let setGestAgeMax max b pc =
    {
        pc with
            GestAge = pc.GestAge |> setMax max b
    }
    |> categorizeGestAge


let setPostAgeMin min b pc =
    {
        pc with
            PostAge = pc.PostAge |> setMin min b
    }
    |> categorizePostAge


let setPostAgeMax max b pc =
    {
        pc with
            PostAge = pc.PostAge |> setMax max b
    }
    |> categorizePostAge


let setWeightMin min b pc =
    {
        pc with
            Weight = pc.Weight |> setMin min b
    }
    |> categorizeWeight


let setWeightMax max b pc =
    {
        pc with
            Weight = pc.Weight |> setMax max b
    }
    |> categorizeWeight


let setPatients set n b a pats =
    match pats |> List.tryItem n with
    | Some pat ->
        pats
        |> List.collect (fun x ->
            if x <> pat then [ x ]
            else
                pat |> set b a
        )
        //|> List.filter ((<>) pat)
        //|> List.append (pat |> set b a)
    | None -> pats
    |> List.distinct


let setPatientsAgeMin = setPatients setAgeMin


let setPatientsAgeMax = setPatients setAgeMax


let setPatientsGestAgeMin = setPatients setGestAgeMin


let setPatientsGestAgeMax = setPatients setGestAgeMax


let setPatientsPostAgeMin = setPatients setPostAgeMin


let setPatientsPostAgeMax = setPatients setPostAgeMax


let setPatientsWeightMin = setPatients setWeightMin


let setPatientsWeightMax = setPatients setWeightMax


type PatientCategories = PatientCategory list


type Categorize = PatientCategory -> PatientCategories


open Utils
open Types

let patient =
    {
        Gender = None
        Age = minmax
        Weight = minmax
        GestAge = minmax
        PostAge = minmax
    }


let mapDose get set incr (d : Dose) (n, pats) =
    match d |> get with
    | Some x -> (if incr then n + 1 else n), pats |> set n true x
    | None   -> n, pats


let mapDoseAgeMin = mapDose (fun d -> d.MinAgeMo) setPatientsAgeMin true
let mapDoseAgeMax = mapDose (fun d -> d.MaxAgeMo) setPatientsAgeMax false

let mapDoseWeightMin = mapDose (fun d -> d.MinWeightKg) setPatientsWeightMin true
let mapDoseWeightMax = mapDose (fun d -> d.MaxWeightKg) setPatientsWeightMax false


let mapDoseGestAgeMin = mapDose (fun d -> d.MinGestAgeDays |> Option.map float) setPatientsGestAgeMin true
let mapDoseGestAgeMax = mapDose (fun d -> d.MaxGestAgeDays |> Option.map float) setPatientsGestAgeMax false


let mapDosePostAgeMin = mapDose (fun d -> d.MinPMAgeDays |> Option.map float) setPatientsPostAgeMin true
let mapDosePostAgeMax = mapDose (fun d -> d.MaxPMAgeDays |> Option.map float) setPatientsPostAgeMax false
    
    
let print (pc : PatientCategory) =
    let minMaxToMinFloatOption mm =
        mm.Min |> Option.bind (fun min -> match min with | MinIncl v | MinExcl v -> v |> Some)
    let minMaxToMaxFloatOption mm =
        mm.Max |> Option.bind (fun max -> match max with | MaxIncl v | MaxExcl v -> v |> Some)
    let minMaxToMinIntOption mm =
        mm.Min |> Option.bind (fun min -> match min with | MinIncl v | MinExcl v -> v |> int |> Some)
    let minMaxToMaxIntOption mm =
        mm.Max |> Option.bind (fun max -> match max with | MaxIncl v | MaxExcl v -> v |> int |> Some)

    Doses.printPatCat
        (Unknown "")
        (pc.Age |> minMaxToMinFloatOption)
        (pc.Age |> minMaxToMaxFloatOption)        
        (pc.GestAge |> minMaxToMinIntOption)
        (pc.GestAge |> minMaxToMaxIntOption)
        (pc.PostAge |> minMaxToMinIntOption)
        (pc.PostAge |> minMaxToMaxIntOption)
        (pc.Weight |> minMaxToMinFloatOption)
        (pc.Weight |> minMaxToMaxFloatOption)        


(Environment.environmentVars ()).TryGetValue "CONN_STR_AP"
|> function 
| true, s ->
    Doses.getDoses s
//    |> List.take 1000
    |> List.collect (fun d -> 
        (0, [ patient ])
        |> mapDoseAgeMin d
        |> mapDoseAgeMax d
        |> mapDoseWeightMin d
        |> mapDoseWeightMax d
        |> mapDoseGestAgeMin d
        |> mapDoseGestAgeMax d
        |> mapDosePostAgeMin d
        |> mapDosePostAgeMax d
        |> fun (n, pats) ->
            pats
            |> List.mapi (fun i pc ->
                if i = n then pc, Some d
                else pc, None
            )
    )
    |> List.iter (fun (pc, d) ->
        let s = 
            match d with
            | Some d -> d |> Doses.printPat
            | None -> "Geen dosering"
        
        if s <> (pc |> print) &&
           s <> "Geen dosering" then printfn "%s : %s" (pc |> print) s 
    )
        
| _ -> failwith "could not get connections string"

