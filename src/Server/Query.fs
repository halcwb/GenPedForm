module Query

open System
open MathNet.Numerics

open Shared
open Shared.Query

open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL

type DoseRule = Informedica.GenForm.Lib.Types.DoseRule
type GenFormFilter = Informedica.GenForm.Lib.Types.Filter

module Patient = Informedica.GenForm.Lib.Patient
module DoseRule = Informedica.GenForm.Lib.DoseRule


module Memoization =


    let memoizeWithRefresh f =
        let cache = ref Map.empty
        fun refresh x ->
            if refresh then 
                cache.Value <- Map.empty

            match cache.Value.TryFind(x) with
            | Some r -> r
            | None ->
                let r = f x
                cache.Value <- cache.Value.Add(x, r)
                r
    


let mapFilterToGenForm (filter : Filter) : GenFormFilter = 
    { DoseRule.allFilter with
        Indication = filter.Indication
        Generic = filter.Generic
        Shape = filter.Shape
        Route = filter.Route
        Department = filter.Department
        Diagnosis = filter.Patient.Diagnosis
        Age = filter.Patient.Age |> Option.map (fun v -> BigRational.FromInt(v))
        Weight = filter.Patient.Weight |> Option.map (fun v -> BigRational.FromInt(v))
        BSA = filter.Patient.BSA |> Option.map (fun v -> v |> decimal |> BigRational.FromDecimal)
        GestAge = filter.Patient.GestAge |> Option.map (fun v -> BigRational.FromInt(v))
        PMAge = filter.Patient.PMAge |> Option.map (fun v -> BigRational.FromInt(v))
    }


let getDoseRules = 
    Memoization.memoizeWithRefresh DoseRule.doseRules


let run refresh (qry : Query)  =
    try
        let doseRules =
            getDoseRules refresh ()

        qry.Filter 
        |> mapFilterToGenForm
        |> fun filter -> 
            doseRules
            |> DoseRule.filter filter
            |> Array.filter (fun dr -> 
                match qry.Filter.PatientString with
                | None   -> true
                | Some p -> 
                    dr.Patient |> Patient.toString = p
            )
            |> Array.filter (fun dr -> 
                match qry.Filter.Patient.Diagnosis with
                | None   -> true
                | Some d -> 
                    dr.Patient.Diagnosis |> String.isNullOrWhiteSpace ||
                    dr.Patient.Diagnosis  = d
            )
        |> fun rs ->
            { qry with
                Indications = 
                    rs 
                    |> DoseRule.indications
                    |> Array.sortBy String.toLower
                Generics = rs |> DoseRule.generics
                Shapes = rs |> DoseRule.shapes
                Routes = rs |> DoseRule.routes
                Patients = 
                    rs 
                    |> Array.map (fun dr -> dr.Patient |> Patient.toString) 
                    |> Array.distinct
                    |> Array.sort
                Diagnoses = 
                    rs 
                    |> Array.map (fun dr -> dr.Patient.Diagnosis) 
                    |> Array.filter (String.isNullOrWhiteSpace >> not)
                    |> Array.distinct
                    |> Array.sort
                Markdown = rs |> DoseRule.printGenerics
            }
        |> Ok
    with 
    | e -> e.ToString() |> Error 
