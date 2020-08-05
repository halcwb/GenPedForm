
#load @"Scripts\..\..\..\..\.paket\load\netcoreapp3.1\Server\server.group.fsx"
#load @"Scripts\..\..\..\..\.paket\load\net472\Server\Microsoft.Data.SqlClient.fsx"

#time


open System
open System.IO

fsi.AddPrinter<DateTime>(sprintf "%A")

open DustyTables

module Queries =

    let getLatest = """
SELECT
md.[VersionID]
, md.[VersionUTC]
, md.[VersionDate]
, [GPK]
, [ATC]
, [MainGroup]
, [SubGroup]
, md.[Generic]
, tm.[Tallman]
, [Product]
, [Label]
, md.[Shape]
, [Routes]
, [GenericQuantity]
, [GenericUnit]
, [MultipleQuantity]
, [MultipleUnit]
, [Indications]
, [HasSolutions]
, [IsActive]

, d.[Route]
, d.Indication
, d.Gender
, d.MinAge
, d.MaxAge
, d.MinWeight
, d.MaxWeight
, d.MinGestAge
, d.MaxGestAge
, d.MinPMAge
, d.MaxPMAge
, d.Frequencies
, d.NormDose
, d.MinDose
, d.MaxDose
, d.AbsMaxDose
, d.MaxPerDose
, d.IsDosePerKg
, d.IsDosePerM2

FROM [dbo].[ConfigMedDisc] md
LEFT JOIN [dbo].[GetConfigMedDiscDoseLatest]() d ON
d.[VersionID] = md.[VersionID]
AND d.[Generic] = md.[Generic]
AND d.[Shape] = md.[Shape]
AND CHARINDEX(d.[Route], md.[Routes]) > 0
LEFT JOIN [dbo].[ConfigMedTallMan] tm ON tm.[Generic] = REPLACE(REPLACE(md.[Generic], ' ', '-'), '/', '+')

WHERE
md.[VersionID] = [dbo].GetlatestConfigMedDiscVersion()
    """


let connectionString () = Environment.GetEnvironmentVariable("CONN_STR_AP")


let executeGetLatest () =
    connectionString ()
    |> Sql.connect
    |> Sql.query Queries.getLatest
    |> Sql.execute (fun read -> 
        read.string "MainGroup"
    )
