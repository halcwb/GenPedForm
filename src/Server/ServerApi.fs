module ServerApi

open Microsoft.Extensions.Logging
open Microsoft.Extensions.Configuration
open Shared
open Shared.Api

/// An implementation of the Shared IServerApi protocol.
/// Can require ASP.NET injected dependencies in the constructor and uses the Build() function to return value of `IServerApi`.
type ServerApi(logger: ILogger<ServerApi>, config: IConfiguration) =

    member this.GetDoses () = 
        async {
            try 
                let doses = Doses.getDoses ()
                return Ok doses
            with 
                | error -> 
                    logger.LogError(error, "Error while retrieving patients from database")
                    return Error error.Message
        }        

    member this.Build() : IServerApi =
        {
            GetDoses = this.GetDoses
        }

