namespace Informedica.Formulary.Lib

module Types =

    type Count = int

    type DoseQuantity =
        | Quantity of float
        | QuantityPerKg of float
        | QuantityPerM2 of float
    type Unit = string
    type TimeUnit = Count * Unit
    type Frequency = { Count : Count; Time : TimeUnit }
    type Gender = Male | Female | Unknown of string

    type Dose =
        {
            Generic : string
            Shape : string
            Route : string
            Indication : string
            Specialty : string
            Gender : Gender
            MinAgeMo : float option
            MaxAgeMo : float option
            MinWeightKg : float option
            MaxWeightKg : float option
            MinGestAgeDays : int option
            MaxGestAgeDays : int option
            MinPMAgeDays : int option
            MaxPMAgeDays : int option
            Freqs : Frequency list
            Unit : Unit
            NormDose : DoseQuantity option
            MinDose : DoseQuantity option
            MaxDose : DoseQuantity option
            AbsMaxDose : DoseQuantity option
            MaxPerDose : DoseQuantity option
            StartDose : DoseQuantity option
            Products : Product list
        }

    and Product =
        {
            GPK : int
            ATC : string
            MainGroup : string
            SubGroup : string
            Generic : string
            GenericLabel : string
            ProductLabel : string
            Shape : string
            Routes : string list
            Concentration : float
            Unit : string
            Multiple : float
            MultipleUnit : string
            HasSolution : bool
            IsInStock : bool
            Doses : Dose list
        }


    type Minimum = | MinIncl of float | MinExcl of float
    type Maximum = | MaxIncl of float | MaxExcl of float


    type MinMax = { Min : Minimum option; Max : Maximum option}


    type GenderCategory =
         | MaleGender 
         | FemaleGender 
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
    and CategoriesOrDose = Categories of Category list | Dose of Dose option
    
    
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
    