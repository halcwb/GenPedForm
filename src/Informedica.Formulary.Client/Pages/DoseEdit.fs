namespace Informedica.Formulary.Client.Pages


module DoseEdit =

    open System

     open Elmish
     open Feliz
     open Feliz.UseElmish
     open Feliz.MaterialUI
     open Fable.MaterialUI.Icons
     open Fable.Core.JsInterop
     open Feliz.Markdown
 
    open System
    open Informedica.Formulary.Shared
    open Informedica.Formulary.Client.Components

    let private comp =
        React.functionComponent("dosedit", fun () ->
            Html.div []
        )
