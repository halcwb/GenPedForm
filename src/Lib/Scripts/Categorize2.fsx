

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


type Minimum = | MinIncl of float | MinExcl of float
type Maximum = | MaxIncl of float | MaxExcl of float


type MinMax = { Min : Minimum option; Max : Maximum option}


type GenderCategory =
     | Male 
     | Female 
type AgeCategory = MinMax 
type GestationAgeCategory = MinMax 
type PostConceptionalAgeCategory = MinMax 
type WeightCategory = MinMax 

type PatientCategory =
    | RootCategory
    | Gender of GenderCategory 
    | Age of AgeCategory
    | GestationAge of GestationAgeCategory
    | PostConceptionalAge of PostConceptionalAgeCategory
    | Weight of WeightCategory


type Category =
    | Category of PatientCategory * Category list


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
    | Male -> "man"
    | Female -> "vrouw"


let patientToStr pc = 
    let toStr v =
        if (v |> int |> float) = v then v |> int |> sprintf "%i"
        else v |> sprintf "%A"

    match pc with
    | RootCategory -> ""
    | Gender g -> g |> genderToStr
    | GestationAge mm -> mm |> minMaxToStr toStr |> sprintf "Zwangerschapsduur: %s weken"
    | PostConceptionalAge mm -> mm |> minMaxToStr toStr |> sprintf "Post Conceptie Leeftijd: %s"
    | Weight mm -> mm |> minMaxToStr toStr |> sprintf "Gewicht: %s kg"
    | Age mm -> mm |> minMaxToStr Doses.printAge |> sprintf "Leeftijd: %s"


let toString (Category(c, cs)) =
    let rec toStr cs s1 s2 =
        cs
        |> List.map (fun x -> (s1 + "\t", x))
        |> List.fold (fun acc (s1, (Category(c, cs))) ->
            sprintf "%s%s- %s\n" acc s1 (c |> patientToStr)
            |> toStr cs s1
        ) s2

    "Categorieen\n" 
    |> toStr cs ""


let createCategory xs c = 
    let eqs (Category(c1, _)) (Category(c2, _)) = 
        match c1, c2 with
        | Gender _, Gender _ 
        | Age _, Age _ 
        | PostConceptionalAge _, PostConceptionalAge _ 
        | Weight _, Weight _
        | GestationAge _, GestationAge _ -> true
        | _ -> false

    match xs with
    | [] -> []
    | h::_ ->
        xs 
        |> List.filter (eqs h) 
    |> fun xs ->
        (c, xs |> List.distinct) |> Category 


let initCategory = RootCategory |> createCategory []


let applyFold f pc cat =
    let rec apply pc (Category(c, xs)) =
        if pc = c then c |> f xs
        else 
            xs
            |> List.fold(fun acc cat ->
                if acc |> Option.isSome then acc
                else apply pc cat
            ) None

    apply pc cat


let findCategory = applyFold (fun cs pc -> pc |> createCategory cs |> Some)


let getCategoryChildren (Category(_, xs)) = xs
let getCategoryPatient (Category(c, _)) = c


let findParent pc cat =
    let rec find pc (Category(parent, xs)) cat =
        if pc = (cat |> getCategoryPatient) then 
            Category(parent, xs) |> Some
        else
            cat
            |> getCategoryChildren
            |> List.fold(fun acc child ->
                if acc |> Option.isSome then acc
                else find pc cat child 
            ) None

    cat
    |> getCategoryChildren
    |> List.fold (fun acc child ->
        if acc |> Option.isSome then acc
        else find pc cat child
    ) None
    

let applyMap f pc cat =
    let rec apply cats pc (Category(pc', xs)) =
        if pc = pc' then pc |> f xs else (Category(pc', xs))
        |> fun (Category(pc', xs)) ->
            if xs |> List.isEmpty then pc' |> createCategory xs
            else 
                xs
                |> List.map (apply cats pc)
                |> fun xs -> pc' |> createCategory xs

    apply f pc cat


let addCategories cats =
    applyMap (fun cs pc -> pc |> createCategory (cs @ cats))


let replaceCategory newCat =
    applyMap (fun cs _ -> newCat |> createCategory cs)


let applyCollect f pc (Category(pc', xs)) =
    let rec apply pc (Category(pc', xs)) =
        if pc = pc' then pc |> f xs else [ (Category(pc', xs)) ]
        |> List.collect (fun (Category(pc', xs)) ->
            if xs |> List.isEmpty then pc' |> createCategory xs
            else 
                xs
                |> List.collect (apply pc)
                |> fun xs -> pc' |> createCategory xs
            |> List.singleton
        )

    xs
    |> List.collect (apply pc)
    |> fun xs -> pc' |> createCategory xs


let addGenderCategory pc cat =
    [ 
        Male   |> Gender
        Female |> Gender  
    ] 
    |> List.map (createCategory [])
    |> fun cats -> cat |> addCategories cats pc


let minMax = { Min = None ; Max = None }

let createMin b f = if b then f |> MinIncl else f |> MinExcl
let createMax b f = if b then f |> MaxIncl else f |> MaxExcl

let setMin min mm = { mm with Min = Some min }
let setMax max mm = { mm with Max = Some max }


let inline setMinMaxValue c s b f mm = 
    f 
    |> c b
    |> fun x -> mm |> s x

let setMinIncl = setMinMaxValue createMin setMin true 
let setMinExcl = setMinMaxValue createMin setMin false 
let setMaxIncl = setMinMaxValue createMax setMax true 
let setMaxExcl = setMinMaxValue createMax setMax false 


let minToMax min =
    match min with
    | MinIncl v -> MaxExcl v
    | MinExcl v -> MaxIncl v


let maxToMin max =
    match max with
    | MaxIncl v -> MinExcl v
    | MaxExcl v -> MinIncl v


let evalMinMax mm =
    match mm.Min, mm.Max with
    | None, None     -> [ mm ]
    | Some min, None ->
        [
            { minMax with Max = minToMax min |> Some }
            mm
        ]
    | None, Some max ->
        [
            mm
            { minMax with Min = maxToMin max |> Some }
        ]
    | Some min, Some max ->
        [
            { minMax with Max = minToMax min |> Some }
            mm
            { minMax with Min = maxToMin max |> Some }
        ]


let splitMinMax f b mm =
    [
        if b then mm |> setMaxIncl f else mm |> setMaxExcl f
        if b then mm |> setMinExcl f else mm |> setMinIncl f
    ]


let addMinMaxCategory c mm pc cat =
    mm
    |> evalMinMax
    |> List.map (c >> (createCategory []))
    |> fun cats -> cat |> addCategories cats pc


let addAgeCategory = addMinMaxCategory Age
let addGestAgeCategory = addMinMaxCategory GestationAge
let addPostAgeCategory = addMinMaxCategory PostConceptionalAge
let addWeightCategory = addMinMaxCategory Weight

let splitMinMaxCategory pc f b cat =
    match pc with
    | Age mm -> (Some mm, Some Age)
    | GestationAge mm -> (Some mm, Some GestationAge)
    | PostConceptionalAge mm -> (Some mm, Some PostConceptionalAge)
    | Weight mm -> (Some mm, Some Weight)
    | _ -> None, None
    |> function
    | Some mm, Some c ->
        mm
        |> splitMinMax f b
        |> List.map (c >> (createCategory []))
        |> fun xs -> 
            let f _ _ = xs
            applyCollect f pc cat
    | _ -> cat



// testing
initCategory
|> addGenderCategory RootCategory
|> addAgeCategory (minMax |> setMinIncl 1.) (Male |> Gender)
|> splitMinMaxCategory (minMax |> setMinIncl 1. |> Age) 12. false
|> addGestAgeCategory  (minMax |> setMinIncl 32.) (minMax |> setMaxExcl 1. |> Age)
|> splitMinMaxCategory (minMax |> setMinIncl 32. |> GestationAge) 36. false
|> addWeightCategory (minMax |> setMaxIncl 10.) (minMax |> setMinIncl 1. |> setMaxExcl 12. |> Age)
|> addWeightCategory (minMax |> setMinIncl 20.) (Female |> Gender)
|> toString
|> printfn "%s"


initCategory
|> addAgeCategory (minMax |> setMinIncl 1. |> setMaxExcl 12.) RootCategory
|> addGenderCategory (minMax |> setMinIncl 1. |> setMaxExcl 12. |> Age)
|> findParent (Male |> Gender)


initCategory
|> addAgeCategory (minMax |> setMaxExcl 1.) RootCategory
|> addGestAgeCategory (minMax |> setMinIncl 28. |> setMaxIncl 37.) (minMax |> setMaxExcl 1. |> Age)
|> toString
|> printfn "%s"
