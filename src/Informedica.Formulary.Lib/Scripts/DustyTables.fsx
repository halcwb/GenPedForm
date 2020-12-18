

//#r "../bin/Release/net472/Microsoft.Data.SqlClient.dll"
//#r "../bin/Release/net472/Lib.dll"
#load "../../../.paket/load/netstandard2.1/Library/library.group.fsx"
#load "../../../.paket/load/netcoreapp3.1/Informedica.GenUtils.Lib.fsx"

#time

#load "../Utils.fs"
#load "../Types.fs"

open System
open System.Collections.Generic

open DustyTables

AppContext.SetSwitch("Switch.Microsoft.Data.SqlClient.UseManagedNetworkingOnWindows", true)

fsi.AddPrinter<DateTime> (sprintf "%A")

