namespace Shared


[<AutoOpen>]
module Types =


    type Patient = 
        {
            Diagnosis : string option
            Gender : string option
            Age : int option
            Weight : int option
            BSA : float option
            GestAge : int option
            PMAge : int option
        }


    type Filter =
        {
            Indication : string option
            Generic : string option
            Shape : string option
            Route : string option
            Department : string option
            Patient : Patient
        }


    type Query =
        {
            Filter : Filter
            Indications : string []
            Generics : string []
            Shapes : string []
            Routes : string []
            Departments : string []
            Patients : string []
            Diagnoses : string []
            Markdown : string []
        }


    type IServerApi = {
        Query: Query -> Async<Result<Query, string>>
    }



module Query =


    let patient = 
        {
            Diagnosis = None
            Gender = None
            Age = None
            Weight = None
            BSA = None
            GestAge = None
            PMAge = None

        }


    let filter = 
        {
            Indication = None
            Generic = None
            Shape = None
            Route = None
            Department = None
            Patient = patient
        }


    let query =
        {
            Filter = filter
            Indications = Array.empty
            Generics = Array.empty
            Shapes = Array.empty
            Routes = Array.empty
            Departments = Array.empty
            Diagnoses = Array.empty
            Patients = Array.empty
            Markdown = Array.empty
        }


module Route =

    let hello = "/api/hello"

    
    /// Defines how routes are generated on server and mapped from client
    let routerPaths typeName method = sprintf "/api/%s/%s" typeName method


