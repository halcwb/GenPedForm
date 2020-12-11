﻿module Categorize

open System
open System.Collections.Generic
open Utils


type Minimum = | MinIncl of float | MinExcl of float
type Maximum = | MaxIncl of float | MaxExcl of float


type MinMax = { Min : Minimum option; Max : Maximum option}


type GenderCategory =
     | Male 
     | Female 
type AgeCategory = MinMax 
type GestationalAgeCategory = MinMax 
type PostConceptionalAgeCategory = MinMax 
type WeightCategory = MinMax 
type BodySurfaceAreaCategory = MinMax


type PatientCategory =
    | RootCategory
    | Gender of GenderCategory 
    | Age of AgeCategory
    | GestationAge of GestationalAgeCategory
    | PostConceptionalAge of PostConceptionalAgeCategory
    | Weight of WeightCategory
    | BodySurfaceArea of BodySurfaceAreaCategory



type Category =
    | Category of PatientCategory * CategoriesOrDose
and CategoriesOrDose = Categories of Category list | Dose of Types.Dose option


let getMinValue = function
    | MinIncl v | MinExcl v -> v


let getMaxValue = function
    | MaxIncl v | MaxExcl v -> v


let applyMinMaxPatientCategory f = function
    | RootCategory -> RootCategory
    | Gender g -> g |> Gender
    | Age mm -> mm |> f |> Age 
    | GestationAge mm -> mm |> f |> GestationAge
    | PostConceptionalAge mm -> mm |> f |> PostConceptionalAge
    | Weight mm  -> mm |> Weight
    | BodySurfaceArea mm -> mm |> BodySurfaceArea


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
    | GestationAge mm -> mm |> minMaxToStr (int >> Doses.printDays) |> sprintf "zwangerschapsduur: %s weken"
    | PostConceptionalAge mm -> mm |> minMaxToStr (int >> Doses.printDays) |> sprintf "post Conceptie Leeftijd: %s"
    | Weight mm -> mm |> minMaxToStr toStr |> sprintf "gewicht: %s kg"
    | BodySurfaceArea mm -> mm |> minMaxToStr toStr |> sprintf "lichaamsoppervlak: %s m2"
    | Age mm -> mm |> minMaxToStr Doses.printAge |> sprintf "leeftijd: %s"


let toString indent (Category(_, cod)) =
    let rec toStr cod indent s =
        match cod with
        | Dose d ->
            match d with
            | Some d -> d |> Doses.printDose
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


let createCategory cs pc = 
    let eqs (Category(c1, _)) (Category(c2, _)) = 
        match c1, c2 with
        | Gender _, Gender _ 
        | Age _, Age _ 
        | PostConceptionalAge _, PostConceptionalAge _ 
        | Weight _, Weight _
        | GestationAge _, GestationAge _ -> true
        | _ -> false

    match cs with
    | [] -> []
    | h::_ ->
        cs 
        |> List.filter (eqs h) 
        |> List.distinctBy (fun (Category(pc, _)) -> pc)
    |> fun cs ->
        (pc, cs |> Categories) |> Category 


let createCategoryDose d pc = (Category(pc, d |> Dose))


let initCategory = RootCategory |> createCategory []


let findCatIn cs1 cs2 = 
    //printfn "find: %s" (cs2 |> List.map patientToStr |> String.concat ";")
    //printfn "in: %s" (cs1 |> List.map patientToStr |> String.concat ";")
    cs2 |> List.isTailList cs1


let applyFold f pcs cat =
    let rec apply current pcs (Category(pc, cod)) =
        let current = current @ [pc]
        match cod with
        | Dose _ -> None
        | Categories cs ->
            if pcs |> findCatIn current then pc |> f cs
            else 
                cs
                |> List.fold(fun acc c ->
                    if acc |> Option.isSome then acc
                    else apply current pcs c
                ) None

    apply [] pcs cat


let findCategory = applyFold (fun cs pc -> pc |> createCategory cs |> Some)


let getCategoriesOrDose (Category(_, cod)) = cod
let getCategoryPatient (Category(c, _)) = c


let findParent pcs c =
    let rec find current pcs (Category(parent, cod)) c =
        let current = current @ [ (c |> getCategoryPatient) ]
        if pcs |> findCatIn current then 
            Category(parent, cod) |> Some
        else
            c
            |> getCategoriesOrDose
            |> function
            | Dose _        -> None
            | Categories cs ->
                cs
                |> List.fold(fun acc c' ->
                    if acc |> Option.isSome then acc
                    else find current pcs c c'
                ) None

    c
    |> getCategoriesOrDose
    |> function
    | Dose _ -> None
    | Categories cs ->
        cs
        |> List.fold (fun acc child ->
            if acc |> Option.isSome then acc
            else find [ RootCategory ] pcs c child
        ) None
    

let applyMap f pcs c =
    let rec apply current pcs (Category(pc, cod)) =
        let current = current @ [pc]

        if pcs |> findCatIn current then pc |> f cod else (Category(pc, cod))
        |> fun (Category(pc, cod)) ->
            match cod with
            | Dose _ -> (Category(pc, cod))
            | Categories cs ->                
                if cs |> List.isEmpty then pc |> createCategory cs
                else 
                    cs
                    |> List.map (apply current pcs)
                    |> fun cs -> pc |> createCategory cs

    apply [] pcs c


let addCategories cs =
    let f =
        fun cod pc ->
            match cod with
            | Dose _ -> Category(pc, cod)
            | Categories _ -> pc |> createCategory cs
    applyMap f


let replaceCategory pc =
    fun cod _ -> Category(pc, cod)
    |> applyMap


let applyCollect f pcs c =
    let rec apply current pcs (Category(pc', cod)) =
        let current = current @ [pc']
        if pcs |> findCatIn current then f ()
        else [ (Category(pc', cod)) ]
        |> List.collect (fun (Category(pc', cod)) ->
            match cod with
            | Dose _ -> [ Category(pc', cod) ]
            | Categories cs ->
                if cs |> List.isEmpty then pc' |> createCategory cs
                else 
                    cs
                    |> List.collect (apply current pcs)
                    |> fun xs -> pc' |> createCategory xs
                |> List.singleton
        )

    match c |> getCategoriesOrDose with
    | Dose _ -> c
    | Categories cs ->
        cs
        |> List.collect (apply [] pcs)
        |> fun cs -> 
            c
            |> getCategoryPatient
            |> createCategory cs


let addGenderCategory pc c =
    [ 
        Male   |> Gender
        Female |> Gender  
    ] 
    |> List.map (createCategory [])
    |> fun cs -> c |> addCategories cs pc


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


let evalMinMaxCont fOld fNew mm =
    match mm.Min, mm.Max with
    | None, None     -> [ mm |> fOld ]
    | Some min, None ->
        [
            { minMax with Max = minToMax min |> Some } |> fNew
            mm |> fOld
        ]
    | None, Some max ->
        [
            mm |> fOld
            { minMax with Min = maxToMin max |> Some } |> fNew
        ]
    | Some min, Some max ->
        [
            { minMax with Max = minToMax min |> Some } |> fNew
            mm |> fOld
            { minMax with Min = maxToMin max |> Some } |> fNew
        ]


let evalMinMax = evalMinMaxCont id id


let splitMinMax f b mm =
    [
        if b then mm |> setMaxIncl f else mm |> setMaxExcl f
        if b then mm |> setMinExcl f else mm |> setMinIncl f
    ]


let addMinMaxCategory fc pcs mm c =
    mm
    |> evalMinMax
    |> List.map (fc >> (createCategory []))
    |> fun cs -> c |> addCategories cs pcs


let addAgeCategory = addMinMaxCategory Age
let addGestAgeCategory = addMinMaxCategory GestationAge
let addPostAgeCategory = addMinMaxCategory PostConceptionalAge
let addWeightCategory = addMinMaxCategory Weight
let addBSACategory = addMinMaxCategory BodySurfaceArea

let splitMinMaxCategory pcs f b c =
    match pcs |> List.last with
    | Age mm -> (Some mm, Some Age)
    | GestationAge mm -> (Some mm, Some GestationAge)
    | PostConceptionalAge mm -> (Some mm, Some PostConceptionalAge)
    | Weight mm -> (Some mm, Some Weight)
    | _ -> None, None
    |> function
    | Some mm, Some fc ->
        mm
        |> splitMinMax f b
        |> List.map (fc >> (createCategory []))
        |> fun cs -> 
            let f _ = cs
            applyCollect f pcs c
    | _ -> c


let clearCategory pcs c =
    if pcs |> List.isEmpty then c
    else 
        let pc = pcs |> List.last
        match pc with
        | RootCategory -> initCategory
        | _ ->
            let f _ =
                match pc with
                | RootCategory  -> [ ]
                | Gender Male   -> [ Male |> Gender |> createCategory [] ]
                | Gender Female -> [ Female |> Gender |> createCategory [] ]
                | Age mm        -> [ mm |> Age |> createCategory [] ]
                | GestationAge mm -> [ mm |> GestationAge |> createCategory [] ]
                | PostConceptionalAge mm -> [ mm |> PostConceptionalAge |> createCategory [] ]
                | Weight mm -> [ mm |> Weight |> createCategory [] ]
                | BodySurfaceArea mm -> [ mm |> BodySurfaceArea |> createCategory [] ]    
            applyCollect f pcs c


let evalTwoMinMax (mm1 : MinMax) (mm2 : MinMax) =
    match mm1.Max, mm2.Min with
    | Some (MaxExcl v1), Some (MinIncl v2) when v1 < v2 ->
            minMax
            |> setMinIncl v1
            |> setMaxExcl v2
            |> Some
    | _ -> None


let evalCategories (cs : Category list) =
    cs
    |> function 
    | [ c ] -> 
        match c |> getCategoryPatient with
        | Age mm -> mm |> evalMinMaxCont (fun _ -> c) (Age >> (createCategory []))
        | GestationAge mm -> mm |> evalMinMaxCont (fun _ -> c) (GestationAge >> (createCategory []))
        | PostConceptionalAge mm -> mm |> evalMinMaxCont (fun _ -> c) (PostConceptionalAge >> (createCategory []))
        | Weight mm -> mm |> evalMinMaxCont (fun _ -> c) (Weight >> (createCategory []))
        | BodySurfaceArea mm -> mm |> evalMinMaxCont (fun _ -> c) (BodySurfaceArea >> (createCategory []))
        | _ -> cs

    | _ -> cs

    |> List.fold (fun acc c ->
        match acc |> List.rev with
        | [] -> [ c ]
        | h::_ ->
            let pc1 = h |> getCategoryPatient
            let pc2 = c |> getCategoryPatient
            match pc1, pc2 with
            | Age mm1, Age mm2 -> evalTwoMinMax mm1 mm2 |> Option.map Age
            | GestationAge mm1, GestationAge mm2 -> evalTwoMinMax mm1 mm2 |> Option.map GestationAge
            | PostConceptionalAge mm1, PostConceptionalAge mm2 -> evalTwoMinMax mm1 mm2 |> Option.map PostConceptionalAge
            | Weight mm1, Weight mm2 -> evalTwoMinMax mm1 mm2 |> Option.map Weight
            | BodySurfaceArea mm1, BodySurfaceArea mm2 -> evalTwoMinMax mm1 mm2 |> Option.map BodySurfaceArea
            | _ -> None
            |> function
            | None    -> [ c ] |> List.append acc
            | Some pc -> [ Category(pc, [] |> Categories); c ] |> List.append acc
    ) []
    

let mapDosesToCat (ds: Types.Dose list) =
    let mapMinMax min max =
        match min, max with
        | None, None         -> minMax
        | Some min, Some max -> minMax |> setMinIncl min |> setMaxExcl max
        | Some min, None -> minMax |> setMinIncl min
        | None, Some max -> minMax |> setMaxExcl max

    let mapDoses fMinMax fpc mapper (ds : Types.Dose list) : CategoriesOrDose =
        ds
        |> List.sortBy Doses.sortPat
        |> List.forall (fMinMax >> (fun (min, max) -> min |> Option.isNone && max |> Option.isNone))
        |> function 
        | true -> 
            ds
            |> mapper
        | false ->
            ds
            |> List.sortBy Doses.sortPat
            |> List.groupBy fMinMax
            |> List.map (fun ((min, max), ds) ->
                mapMinMax min max
                |> fpc
                |> fun pc -> Category(pc, ds |> mapper) 
            )
            |> evalCategories
            |> Categories

    let mapWeight (ds : Types.Dose list) =
        let fMinMax (d: Types.Dose) =
            d.MinWeightKg,
            d.MaxWeightKg
        
        let mapper (ds: Types.Dose list) =
            match ds with
            | [d] -> d |> Some |> Dose
            | _   -> None |> Dose

        ds
        |> mapDoses fMinMax Weight mapper

    let mapGestAge (ds : Types.Dose list) =
        let fMinMax (d: Types.Dose) =
            d.MinGestAgeDays |> Option.map float, 
            d.MaxGestAgeDays |> Option.map float
        ds
        |> mapDoses fMinMax GestationAge mapWeight


    let mapAge (ds : Types.Dose list) =
        let fMinMax (d: Types.Dose) =
            d.MinAgeMo,
            d.MaxAgeMo
        ds
        |> mapDoses fMinMax Age mapGestAge 

    let mapPostAge (ds : Types.Dose list) =
        let fMinMax (d: Types.Dose) =
            d.MinPMAgeDays |> Option.map float, 
            d.MaxPMAgeDays |> Option.map float
        ds
        |> mapDoses fMinMax PostConceptionalAge mapAge
    
    ds
    |> List.groupBy (fun d -> d.Gender)
    |> function
    | [g] -> 
        Category(RootCategory, g |> snd |> mapPostAge)
    | [g1; g2] ->
        match g1 |> fst, g2 |> fst with
        | Types.Male, Types.Female ->
            [
                Category(Male   |> Gender, g1 |> snd |> mapPostAge)
                Category(Female |> Gender, g2 |> snd |> mapPostAge)
            ]
            |> Categories
            |> fun cod -> Category(RootCategory, cod)
        | Types.Female, Types.Male ->
            [
                Category(Female |> Gender, g1 |> snd |> mapPostAge)
                Category(Male   |> Gender, g2 |> snd |> mapPostAge)
            ]
            |> Categories
            |> fun cod -> Category(RootCategory, cod)

        | _ -> Category(RootCategory, [] |> Categories)

    | _ -> Category(RootCategory, [] |> Categories)



type CategorizedGeneric =
    {
        Generic : string
        Shapes : CategorizedShape list
    }
and CategorizedShape = 
    {
        Shape : string
        Routes : CategorizedRoute list
    }
and CategorizedRoute = 
    {
        Route : string
        Indications : CategorizedIndication list
    }
and CategorizedIndication = 
    {
        Indication : string
        Patient : Category

    }


let mapDoses (ds: Types.Dose list) =
    ds
    |> List.groupBy (fun d -> d.Generic)
    |> List.map (fun (g, ds) ->
        {
            Generic = g
            Shapes = 
                ds
                |> List.groupBy (fun d -> d.Shape)
                |> List.map (fun (s, ds) ->
                    {
                        Shape = s
                        Routes = 
                            ds
                            |> List.groupBy (fun d -> d.Route)
                            |> List.map (fun (r, ds) ->
                                {
                                    Route = r
                                    Indications = 
                                        ds
                                        |> List.groupBy (fun d -> d.Indication)
                                        |> List.map (fun (i, ds) ->
                                            {
                                                Indication = i
                                                Patient = 
                                                    ds
                                                    |> mapDosesToCat
                                            }
                                        )
                                }
                            )
                    }
                )
        }
    )

