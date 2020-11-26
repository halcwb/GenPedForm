module Types

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
