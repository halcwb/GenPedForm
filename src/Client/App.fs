module App

open Elmish
open Elmish.React
open Feliz
open Feliz.Markdown
open Feliz.MaterialUI

open System
open Shared
open Components

module Filter = Autocomplete.Filter

let defaultTheme = Styles.createMuiTheme()

type State =
    { SelectedGeneric: string option
      SelectedIndication: string option
      SelectedRoute: string option
      SelectedPatient: string option
      Versions: Deferred<(int * DateTime) list>
      Generics: Deferred<string list>
      Indications: Deferred<string list>
      Routes: Deferred<string list>
      Patients: Deferred<string list>
      Details: Deferred<string> }


type Msg =
    | LoadVersions of AsyncOperationStatus<Result<(int * DateTime) list, string>>
    | LoadGenerics of AsyncOperationStatus<Result<Generics, string>>
    | LoadIndications of AsyncOperationStatus<Result<string list, string>>
    | LoadRoutes of AsyncOperationStatus<Result<string list, string>>
    | LoadPatients of AsyncOperationStatus<Result<string list, string>>
    | LoadMarkdown of AsyncOperationStatus<Result<string, string>>
    | SelectGeneric of string
    | SelectIndication of string
    | SelectRoute of string
    | SelectPatient of string

let qry: Query =
    { Generic = ""
      Indication = None
      Route = None
      Patient = None }

let loadVersions =
    async {
        let! versions = Server.api.GetVersions()
        return LoadVersions(Finished versions) }

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

let loadPatients generic indication route =
    async {
        let! pats = Server.api.GetPatients generic indication route
        return LoadPatients(Finished pats) }

let loadMarkdown qry =
    async {
        let! details = Server.api.GetMarkdown qry
        return LoadMarkdown(Finished details) }


let initialState =
    { SelectedGeneric = None
      SelectedIndication = None
      SelectedRoute = None
      SelectedPatient = None
      Versions = InProgress
      Generics = InProgress
      Indications = HasNotStartedYet
      Routes = HasNotStartedYet
      Patients = HasNotStartedYet
      Details = HasNotStartedYet }

let init(): State * Cmd<Msg> =

    let loadCmd =
        [ Cmd.fromAsync loadGenerics
          Cmd.fromAsync loadVersions ]
        |> Cmd.batch

    initialState, loadCmd


let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | LoadVersions(Finished(Ok versions)) -> { state with Versions = Resolved versions }, Cmd.none

    | LoadGenerics(Finished(Ok generics)) -> { state with Generics = Resolved generics }, Cmd.none

    | SelectGeneric s ->
        if s <> "" then
            let qry = { qry with Generic = s }

            let cmds =
                [ Cmd.fromAsync (loadMarkdown qry)
                  Cmd.fromAsync (loadIndications s) ]
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
                  Patients = HasNotStartedYet
                  Details = HasNotStartedYet }, Cmd.none

    | SelectIndication s ->
        match state.SelectedGeneric with
        | Some generic when s = "" ->
            let qry = { qry with Generic = generic }

            { state with
                  SelectedIndication = None
                  Routes = HasNotStartedYet
                  Patients = HasNotStartedYet
                  Details = InProgress }, Cmd.fromAsync (loadMarkdown qry)

        | Some generic ->
            let qry =
                { qry with
                      Generic = generic
                      Indication = Some s }

            let cmds =
                [ Cmd.fromAsync (loadMarkdown qry)
                  Cmd.fromAsync (loadRoutes generic s) ]
                |> Cmd.batch

            { state with
                  SelectedIndication = Some s
                  Routes = InProgress
                  Patients = HasNotStartedYet
                  Details = InProgress }, cmds
        | None ->
            { state with
                  SelectedIndication = None
                  SelectedRoute = None
                  Indications = HasNotStartedYet
                  Routes = HasNotStartedYet
                  Patients = HasNotStartedYet }, Cmd.none

    | SelectRoute s ->
        match state.SelectedGeneric with
        | Some generic when s = "" ->
            let qry =
                { qry with
                      Generic = generic
                      Indication = state.SelectedIndication }

            { state with
                  SelectedRoute = None
                  Patients = HasNotStartedYet
                  Details = InProgress }, Cmd.fromAsync (loadMarkdown qry)

        | Some generic ->
            let qry =
                { qry with
                      Generic = generic
                      Indication = state.SelectedIndication
                      Route = Some s }

            let cmds =
                [ Cmd.fromAsync (loadMarkdown qry)
                  match state.SelectedIndication with
                  | Some indication -> Cmd.fromAsync (loadPatients generic indication s)
                  | None -> () ]
                |> Cmd.batch

            { state with
                  SelectedRoute = Some s
                  Patients = InProgress
                  Details = InProgress }, cmds

        | None -> state, Cmd.none

    | SelectPatient s ->
        match state.SelectedGeneric with
        | Some generic when s = "" ->
            let qry =
                { qry with
                      Generic = generic
                      Indication = state.SelectedIndication
                      Route = state.SelectedRoute }

            { state with
                  SelectedPatient = None
                  Details = InProgress }, Cmd.fromAsync (loadMarkdown qry)

        | Some generic ->
            let qry =
                { qry with
                      Generic = generic
                      Indication = state.SelectedIndication
                      Route = state.SelectedRoute
                      Patient = Some s }

            { state with
                  SelectedPatient = Some s
                  Details = InProgress }, Cmd.fromAsync (loadMarkdown qry)

        | None -> state, Cmd.none

    | LoadMarkdown(Finished(Ok s)) -> { state with Details = Resolved s }, Cmd.none

    | LoadIndications(Finished(Ok indications)) -> { state with Indications = Resolved indications }, Cmd.none

    | LoadRoutes(Finished(Ok routes)) -> { state with Routes = Resolved routes }, Cmd.none

    | LoadPatients(Finished(Ok pats)) -> { state with Patients = Resolved pats }, Cmd.none

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
          | _ -> ()

          match state.Patients with
          | Resolved pats ->
              Html.div
                  [ prop.style [ style.marginTop 20 ]
                    prop.children
                        [ { Autocomplete.props with
                                Dispatch = SelectPatient >> dispatch
                                Options = pats
                                Label = sprintf "Kies een patient (van %i totaal)" (pats |> List.length)
                                Filter = Filter.StartsWith }
                          |> Autocomplete.render ] ]
          | _ -> () ]


    let details =
        match state.Details with
        | HasNotStartedYet -> "## Geen generiek gekozen"
        | InProgress -> "## Doseringen worden opgehaald ..."
        | Resolved s -> s.Trim()
        |> markdown.source
        |> List.singleton
        |> List.append [ markdown.escapeHtml false ]
        |> Markdown.markdown

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

                                  match state.Versions with
                                  | Resolved versions ->
                                      versions
                                      |> List.sortBy fst
                                      |> List.rev
                                      |> List.head
                                      |> (fun (v, d) -> sprintf " versie: %i (van %s)" v (d.ToString("dd-MM-yyyy")))
                                      |> sprintf "Afspraken Programma Formularium %s"
                                      |> prop.text
                                  | _ -> prop.text "Afspraken Programma Formularium" ]
                              Mui.iconButton
                                  [ prop.style [ style.color "white" ]
                                    prop.children [ Fable.MaterialUI.Icons.menuIcon "" ] ] ] ]
                    Mui.container
                        [ prop.style
                            [ style.marginTop 90
                              style.padding 10 ]
                          prop.children
                              [ // search
                                Html.div
                                    [ prop.style [ style.padding 10 ]
                                      paper.children filter ]
                                match state.Generics with
                                | Resolved _ ->
                                    // details
                                    Mui.paper
                                        [ prop.style
                                            [ style.padding 10
                                              style.color Colors.indigo.``900`` ]
                                          prop.children [ details ] ]
                                | _ -> Html.none ] ] ] ] ]
