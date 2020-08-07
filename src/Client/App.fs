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

type State =
    { SelectedGeneric: string option
      SelectedIndication: string option
      SelectedRoute: string option
      Generics: Deferred<string list>
      Indications: Deferred<string list>
      Routes: Deferred<string list>
      Details: Deferred<string> }


type Msg =
    | LoadGenerics of AsyncOperationStatus<Result<Generics, string>>
    | LoadIndications of AsyncOperationStatus<Result<string list, string>>
    | LoadRoutes of AsyncOperationStatus<Result<string list, string>>
    | LoadMarkdown of AsyncOperationStatus<Result<string, string>>
    | SelectGeneric of string
    | SelectIndication of string
    | SelectRoute of string

let qry: Query =
    { Generic = ""
      Indication = None
      Route = None
      Patient = None }

let loadGenerics =
    async {
        let! generics = Server.api.GetGenerics()
        return LoadGenerics(Finished generics) }

let loadIndications generic =
    async {
        let! indications = Server.api.GetIndications generic
        return LoadIndications(Finished indications) }

let loadRoutes generic indication =
    async {
        let! routes = Server.api.GetRoutes generic indication
        return LoadRoutes(Finished routes) }

let init(): State * Cmd<Msg> =
    let initialState =
        { SelectedGeneric = None
          SelectedIndication = None
          SelectedRoute = None
          Generics = InProgress
          Indications = HasNotStartedYet
          Routes = HasNotStartedYet
          Details = HasNotStartedYet }

    let loadCmd = [ Cmd.fromAsync loadGenerics ] |> Cmd.batch

    initialState, loadCmd


let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | LoadGenerics(Finished(Ok generics)) -> { state with Generics = Resolved generics }, Cmd.none
    | SelectGeneric s ->
        if s <> "" then
            let qry = { qry with Generic = s }
            let loadMarkdown =
                async {
                    let! details = Server.api.GetMarkdown qry
                    return LoadMarkdown(Finished details) }

            let loadIndications =
                async {
                    let! indications = Server.api.GetIndications s
                    return LoadIndications(Finished indications) }

            let cmds =
                [ Cmd.fromAsync loadMarkdown
                  Cmd.fromAsync loadIndications ]
                |> Cmd.batch
            { state with
                  SelectedGeneric = Some s
                  SelectedIndication = None
                  Indications = InProgress
                  Routes = HasNotStartedYet
                  Details = InProgress }, cmds

        else
            { state with
                  SelectedGeneric = None
                  SelectedIndication = None
                  Indications = HasNotStartedYet
                  Routes = HasNotStartedYet
                  Details = HasNotStartedYet }, Cmd.none

    | SelectIndication s ->
        match state.SelectedGeneric with
        | Some generic when s = "" ->
            let qry = { qry with Generic = generic }
            let load =
                async {
                    let! details = Server.api.GetMarkdown qry
                    return LoadMarkdown(Finished details) }
            { state with
                  SelectedIndication = None
                  Routes = HasNotStartedYet
                  Details = InProgress }, Cmd.fromAsync load
        | Some generic ->
            let qry =
                { qry with
                      Generic = generic
                      Indication = Some s }

            let load =
                async {
                    let! details = Server.api.GetMarkdown qry
                    return LoadMarkdown(Finished details) }

            let cmds =
                [ Cmd.fromAsync load
                  Cmd.fromAsync (loadRoutes generic s) ]
                |> Cmd.batch

            { state with
                  SelectedIndication = Some s
                  Details = InProgress }, cmds
        | None ->
            { state with
                  SelectedIndication = None
                  SelectedRoute = None
                  Indications = HasNotStartedYet
                  Routes = HasNotStartedYet }, Cmd.none

    | SelectRoute s ->
        match state.SelectedGeneric with
        | Some generic when s = "" ->
            let qry =
                { qry with
                      Generic = generic
                      Indication = state.SelectedIndication }

            let load =
                async {
                    let! details = Server.api.GetMarkdown qry
                    return LoadMarkdown(Finished details) }
            { state with
                  SelectedRoute = None
                  Details = InProgress }, Cmd.fromAsync load
        | Some generic ->
            let qry =
                { qry with
                      Generic = generic
                      Indication = state.SelectedIndication
                      Route = Some s }

            let load =
                async {
                    let! details = Server.api.GetMarkdown qry
                    return LoadMarkdown(Finished details) }
            { state with
                  SelectedRoute = Some s
                  Details = InProgress }, Cmd.fromAsync load
        | None -> state, Cmd.none


    | LoadMarkdown(Finished(Ok s)) -> { state with Details = Resolved s }, Cmd.none

    | LoadIndications(Finished(Ok indications)) -> { state with Indications = Resolved indications }, Cmd.none

    | LoadRoutes(Finished(Ok routes)) -> { state with Routes = Resolved routes }, Cmd.none

    | _ -> state, Cmd.none


let render (state: State) (dispatch: Msg -> unit) =
    let filter =
        [ match state.Generics with
          | Resolved generics ->
              { Autocomplete.props with
                    Dispatch = (SelectGeneric >> dispatch)
                    Options = generics |> List.sort
                    Label = sprintf "Zoek een generiek (van %i totaal)" (generics |> List.length)
                    Filter = Filter.StartsWith }
              |> Autocomplete.render
          | _ -> ()

          match state.Indications with
          | Resolved indications ->
              Html.div
                  [ prop.style [ style.marginTop 20 ]
                    prop.children
                        [ { Autocomplete.props with
                                Dispatch = SelectIndication >> dispatch
                                Options = indications |> List.sort
                                Label = sprintf "Kies een indicatie (van %i totaal)" (indications |> List.length)
                                Filter = Filter.ContainsCaseSensitive }
                          |> Autocomplete.render ] ]

          | _ -> ()

          match state.Routes with
          | Resolved routes ->
              Html.div
                  [ prop.style [ style.marginTop 20 ]
                    prop.children
                        [ { Autocomplete.props with
                                Dispatch = SelectRoute >> dispatch
                                Options = routes |> List.sort
                                Label = sprintf "Kies een route (van %i totaal)" (routes |> List.length)
                                Filter = Filter.StartsWith }
                          |> Autocomplete.render ] ]
          | _ -> () ]


    let details =
        match state.Details with
        | HasNotStartedYet -> "## Geen generiek gekozen"
        | InProgress -> "## Doseringen worden opgehaald ..."
        | Resolved s -> s
        |> markdown.source
        |> List.singleton
        |> List.append [ markdown.escapeHtml false ]
        |> Markdown.markdown
    //        |> Html.div


    Mui.themeProvider
        [ themeProvider.theme defaultTheme
          themeProvider.children
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
