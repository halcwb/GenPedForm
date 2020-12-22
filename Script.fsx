
# load ".paket/load/netstandard2.1/Server/Fable.Remoting.Json.fsx"


open Fable.Remoting.Json
open Newtonsoft.Json
open System.IO
open System.Collections.Concurrent
open System.Text


type MyType =
    | EndType of string
    | MyType of MyType

let testType = 
    "test"
    |> EndType
    |> MyType

let private fableConverter = new FableJsonConverter() :> JsonConverter

let private settings = JsonSerializerSettings(DateParseHandling = DateParseHandling.None)

let private fableSerializer =
    let serializer = JsonSerializer()
    serializer.Converters.Add fableConverter
    serializer

let private jsonEncoding = UTF8Encoding false

let jsonSerialize (o: 'a) (stream: Stream) =
    use sw = new StreamWriter (stream, jsonEncoding, 1024, true)
    use writer = new JsonTextWriter (sw, CloseOutput = false)
    fableSerializer.Serialize (writer, o)

let serialize o =
    use stream = new MemoryStream()
    use reader = new StreamReader(stream)
    jsonSerialize o stream
    stream.Position <- 0L
    reader.ReadToEnd()

testType
|> serialize
