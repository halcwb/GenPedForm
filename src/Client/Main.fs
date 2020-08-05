module Main

open System

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Thoth.Json
open Feliz
open Feliz.MaterialUI
open Fable.MaterialUI.Icons
open Fable.Core.JsInterop


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram App.init App.update App.render
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
