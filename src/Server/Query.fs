module Query

open System
open MathNet.Numerics

open Shared
open Shared.Query

open Informedica.Utils.Lib

type DoseRule = Informedica.GenForm.Lib.Types.DoseRule
type GenFormFilter = Informedica.GenForm.Lib.Types.Filter

module Patient = Informedica.GenForm.Lib.Patient
module DoseRule = Informedica.GenForm.Lib.DoseRule


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
    Memoization.memoize DoseRule.doseRules


let run (qry : Query)  =
    try
        let doseRules = getDoseRules ()

        qry.Filter 
        |> mapFilterToGenForm
        |> fun filter -> 
            doseRules
            |> DoseRule.filter filter
        |> fun rs ->
            { qry with
                Indications = rs |> DoseRule.indications
                Generics = rs |> DoseRule.generics
                Shapes = rs |> DoseRule.shapes
                Routes = rs |> DoseRule.routes
                Patients = rs |> Array.map (fun dr -> dr.Patient |> Patient.toString) 
                Markdown = rs |> DoseRule.printGenerics
            }
        |> Ok
    with 
    | e -> e.ToString() |> Error 
