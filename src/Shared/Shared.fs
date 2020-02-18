namespace Shared

type Counter = { Value : int }

type Days = int
type Kg = float
type Cm = int
type Gender = Male | Female | Unknown of string

type Patient =
    {
        Age : Days
        Gender : Gender
        GestAge : Days
        PMAge : Days
        Weight : Kg
        Length : Cm
    }


type Count = int
type Quantity = float
type DoseQuantity =
    | Quantity of Quantity
    | QuantityPerKg of Quantity
    | QuantityPerM2 of Quantity
type Unit = string
type TimeUnit = Count * Unit
type Frequency = { Count : Count; Time : TimeUnit }

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