module App

open Elmish
open Elmish.React
open Feliz
open Feliz.Markdown
open Feliz.MaterialUI

open Shared
open Components

module Filter = Autocomplete.Filter

let defaultTheme = Styles.createMuiTheme()


type Model =
    { Generics: string list
      SelectedGeneric: string option
      Indications : Deferred<string list>
      Details: Deferred<string> }


type Msg =
    | LoadGenerics of AsyncOperationStatus<Result<Generics, string>>
    | LoadIndications of AsyncOperationStatus<Result<string list, string>>
    | LoadMarkdown of AsyncOperationStatus<Result<string, string>>
    | SelectGeneric of string
    | SelectIndication of string


let loadGenerics =
    async {
        let! generics = Server.api.GetGenerics()
        return LoadGenerics(Finished generics) }

let loadIndications generic =
    async {
        let! indications = Server.api.GetIndications generic
        return LoadIndications(Finished indications) }

let init(): Model * Cmd<Msg> =
    let initialModel =
        { Generics = []
          SelectedGeneric = None
          Indications = HasNotStartedYet
          Details = HasNotStartedYet }

    let loadCmd =
        [ Cmd.fromAsync loadGenerics ]
        |> Cmd.batch

    initialModel, loadCmd


let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | LoadGenerics(Finished(Ok generics)) -> { model with Generics = generics }, Cmd.none
    | SelectGeneric s ->
        if s <> "" then
            let loadMarkdown =
                async {
                    let! details = Server.api.GetMarkdown (s, None)
                    return LoadMarkdown(Finished details) }

            let loadIndications =
                async {
                    let! indications = Server.api.GetIndications s
                    return LoadIndications(Finished indications) }

            let cmds =
                [ Cmd.fromAsync loadMarkdown
                  Cmd.fromAsync loadIndications ]
                |> Cmd.batch
            { model with 
                SelectedGeneric = Some s
                Details = InProgress }, cmds

        else
            { model with 
                Details = HasNotStartedYet
                Indications = HasNotStartedYet }, Cmd.none

    | SelectIndication s ->
        if s <> "" && model.SelectedGeneric |> Option.isSome then

            let loadMarkdown =
                async {
                    let! details = Server.api.GetMarkdown (model.SelectedGeneric |> Option.get, Some s)
                    return LoadMarkdown(Finished details) }
            
            { model with Details = InProgress}, Cmd.fromAsync loadMarkdown
        else model, Cmd.none

    | LoadMarkdown(Finished(Ok s)) -> { model with Details = Resolved s }, Cmd.none

    | LoadIndications(Finished(Ok indications)) ->
        { model with Indications = Resolved indications }, Cmd.none

    | _ -> model, Cmd.none


let showProducts (products: Products) =
    Mui.list
        [ for p in products do
            Mui.listItemText p ]


let render (model: Model) (dispatch: Msg -> unit) =
    let filter =
        [
            { Autocomplete.props with
                  Dispatch = (SelectGeneric >> dispatch)
                  Options = model.Generics |> List.sort
                  Label = "Zoek een generiek"
                  Filter = Filter.StartsWith }
            |> Autocomplete.render

            match model.Indications with
            | Resolved indications ->
                Html.div [
                    prop.style [ style.marginTop 20 ]
                    prop.children [
                        { Autocomplete.props with 
                            Dispatch = SelectIndication >> dispatch
                            Options = indications |> List.sort
                            Label = "Kies een indicatie"
                            Filter = Filter.ContainsCaseSensitive
                        }
                        |> Autocomplete.render
                    ]
                ]
                
            | _ -> ()
        ]


    let details =
        match model.Details with
        | HasNotStartedYet -> "## Geen generiek gekozen"
        | InProgress -> "## Doseringen worden opgehaald ..."
        | Resolved s ->
            s
        |> markdown.source
        |> List.singleton
        |> List.append [ markdown.escapeHtml false ]
        |> Markdown.markdown
        |> Html.div


    Mui.themeProvider
        [ themeProvider.children
            [ Html.div
                [ Mui.appBar
                    [ prop.style
                        [ style.display.flex
                          style.flexDirection.row
                          style.padding 10 ]
                      appBar.variant.elevation
                      appBar.children
                          [ Mui.typography
                              [ prop.style
                                  [ style.flexGrow 1
                                    style.padding 10 ]
                                typography.variant.h6
                                prop.text "Afspraken Programma Formularium" ]
                            Mui.iconButton
                                [ prop.style [ style.color "white" ]
                                  prop.children [ Fable.MaterialUI.Icons.menuIcon "" ] ] ] ]
                  Mui.container
                      [ prop.style [ style.marginTop 80 ]
                        prop.children
                            [ // search
                              Html.div
                                  [ prop.style [ style.padding 10 ]
                                    paper.children filter ]
                              // details
                              Mui.paper
                                  [ prop.style
                                      [ style.marginTop 10
                                        style.padding 10 ]
                                    prop.children [ details ] ] ] ] ] ] ]
