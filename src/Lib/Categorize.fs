module Categorize

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
        Gender : GenderCategory option
        Age : AgeCategory
        Weight : WeightCategory
        GestAge : GestationalAge
        PostAge : PostConceptionalAge
    }
and GenderCategory = Male | Female


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


let categorizeGender pat =
    match pat.Gender with
    | Some(Male) -> 
        [
            { pat with Gender = Female |> Some }
            pat
        ]
    | Some(Female) ->
        [
            { pat with Gender = Male |> Some }
            pat
        ]
    | None -> [ pat ]


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


let setGender g pc =
    {
        pc with
            Gender = g
    }
    |> categorizeGender

let setPatients set n b a pats =
    match pats |> List.tryItem n with
    | Some pat ->
        pats
        |> List.collect (fun x ->
            if x <> pat then [ x ]
            else
                pat |> set b a
        )
    | None -> pats
    |> List.distinct


let setPatientsGender n g = setPatients (fun _ _ pc -> pc |> setGender g) n false 0.


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


let mapGender (d: Dose) (n, pats) =
    match d.Gender with
    | Male -> n + 1, pats  |> setPatientsGender n (Some GenderCategory.Male)
    | Female -> n + 1, pats  |> setPatientsGender n (Some GenderCategory.Female)
    | Unknown _ -> n, pats


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

