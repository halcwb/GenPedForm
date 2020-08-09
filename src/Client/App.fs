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


module LoadCommands =

    let qry: Query =
        { Generic = None
          Indication = None
          Route = None
          Patient = None }

    let loadVersions =
        async {
            let! versions = Server.api.GetVersions()
            return LoadVersions(Finished versions) } |> Cmd.fromAsync

    let loadGenerics =
        async {
            let! generics = Server.api.GetGenerics()
            return LoadGenerics(Finished generics) } |> Cmd.fromAsync

    let loadIndications generic =
        match generic with
        | Some g ->
            async {
                let! indications = Server.api.GetIndications g
                return LoadIndications(Finished indications) } |> Cmd.fromAsync
        | None -> Cmd.none

    let loadRoutes generic indication =
        match generic, indication with
        | Some g, Some i ->
            async {
                let! routes = Server.api.GetRoutes g i
                return LoadRoutes(Finished routes) } |> Cmd.fromAsync
        | _ -> Cmd.none


    let loadPatients generic indication route =
        match generic, indication, route with
        | Some g, Some i, Some r ->
            async {
                let! pats = Server.api.GetPatients g i r
                return LoadPatients(Finished pats) } |> Cmd.fromAsync
        | _ -> Cmd.none

    let loadMarkdown qry =
        async {
            let! details = Server.api.GetMarkdown qry
            return LoadMarkdown(Finished details) } |> Cmd.fromAsync


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

    let loadCmd = [ LoadCommands.loadGenerics; LoadCommands.loadVersions ] |> Cmd.batch

    initialState, loadCmd


let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | LoadVersions(Finished(Ok versions)) -> { state with Versions = Resolved versions }, Cmd.none

    | LoadGenerics(Finished(Ok generics)) ->
        let state =
            { state with
                  SelectedIndication = generics |> List.tryExactlyOne
                  Generics = Resolved generics }

        let state, cmds =
            if state.SelectedGeneric |> Option.isNone then
                state, []
            else
                { state with
                      Indications = InProgress
                      Routes = HasNotStartedYet
                      Patients = HasNotStartedYet
                      Details = InProgress },
                [ { LoadCommands.qry with Generic = state.SelectedGeneric } |> LoadCommands.loadMarkdown

                  LoadCommands.loadIndications state.SelectedGeneric ]
                |> Cmd.batch

        state, cmds

    | LoadIndications(Finished(Ok indications)) ->
        let state =
            { state with
                  SelectedIndication = indications |> List.tryExactlyOne
                  Indications = Resolved indications
                  Routes = InProgress }

        let state, cmds =
            if state.SelectedIndication |> Option.isNone then
                state, Cmd.none
            else
                { state with
                      Routes = InProgress
                      Patients = HasNotStartedYet
                      Details = InProgress },
                [ { LoadCommands.qry with
                        Generic = state.SelectedGeneric
                        Indication = state.SelectedIndication }
                  |> LoadCommands.loadMarkdown

                  LoadCommands.loadRoutes state.SelectedGeneric state.SelectedIndication ]
                |> Cmd.batch

        state, cmds

    | LoadRoutes(Finished(Ok routes)) ->
        let state =
            { state with
                  SelectedRoute = routes |> List.tryExactlyOne
                  Routes = Resolved routes }

        let state, cmds =
            if state.SelectedRoute |> Option.isNone then
                state, Cmd.none
            else
                { state with
                      Patients = InProgress
                      Details = InProgress },
                [ { LoadCommands.qry with
                        Generic = state.SelectedGeneric
                        Indication = state.SelectedIndication
                        Route = state.SelectedRoute }
                  |> LoadCommands.loadMarkdown

                  LoadCommands.loadPatients state.SelectedGeneric state.SelectedIndication state.SelectedRoute ]
                |> Cmd.batch

        state, cmds

    | LoadPatients(Finished(Ok pats)) ->
        let state =
            { state with
                  SelectedPatient = pats |> List.tryExactlyOne
                  Patients = Resolved pats }


        let state, cmds =
            if state.SelectedPatient |> Option.isNone then
                state, Cmd.none
            else
                { state with Details = InProgress },
                [ { LoadCommands.qry with
                        Generic = state.SelectedGeneric
                        Indication = state.SelectedIndication
                        Route = state.SelectedRoute
                        Patient = state.SelectedPatient }
                  |> LoadCommands.loadMarkdown ]
                |> Cmd.batch

        state, cmds

    | LoadMarkdown(Finished(Ok s)) -> { state with Details = Resolved s }, Cmd.none

    | SelectGeneric generic ->
        if generic <> "" then
            let qry = { LoadCommands.qry with Generic = (Some generic) }

            let cmds =
                [ LoadCommands.loadMarkdown qry
                  LoadCommands.loadIndications (Some generic) ]
                |> Cmd.batch

            { state with
                  SelectedGeneric = Some generic
                  SelectedIndication = None
                  SelectedRoute = None
                  SelectedPatient = None
                  Indications = InProgress
                  Routes = HasNotStartedYet
                  Patients = HasNotStartedYet
                  Details = InProgress }, cmds
        else
            { initialState with
                Versions = state.Versions
                Generics = state.Generics }, Cmd.none

    | SelectIndication indication ->
        if indication = "" then
            let qry = { LoadCommands.qry with Generic = state.SelectedGeneric }

            { state with
                  SelectedIndication = None
                  Routes = HasNotStartedYet
                  Patients = HasNotStartedYet
                  Details = InProgress }, LoadCommands.loadMarkdown qry

        else
            let qry =
                { LoadCommands.qry with
                      Generic = state.SelectedGeneric
                      Indication = Some indication }

            let cmds =
                [ LoadCommands.loadMarkdown qry
                  LoadCommands.loadRoutes state.SelectedGeneric (Some indication) ]
                |> Cmd.batch

            { state with
                  SelectedIndication = Some indication
                  Routes = InProgress
                  Patients = HasNotStartedYet
                  Details = InProgress }, cmds

    | SelectRoute route ->
        if route = "" then
            let qry =
                { LoadCommands.qry with
                      Generic = state.SelectedGeneric
                      Indication = state.SelectedIndication }

            { state with
                  SelectedRoute = None
                  Patients = HasNotStartedYet
                  Details = InProgress }, LoadCommands.loadMarkdown qry

        else
            let qry =
                { LoadCommands.qry with
                      Generic = state.SelectedGeneric
                      Indication = state.SelectedIndication
                      Route = Some route }

            let cmds =
                [ LoadCommands.loadMarkdown qry
                  LoadCommands.loadPatients state.SelectedGeneric state.SelectedIndication (Some route) ]
                |> Cmd.batch

            { state with
                  SelectedRoute = Some route
                  Patients = InProgress
                  Details = InProgress }, cmds


    | SelectPatient patient ->
        if patient = "" then
            let qry =
                { LoadCommands.qry with
                      Generic = state.SelectedGeneric
                      Indication = state.SelectedIndication
                      Route = state.SelectedRoute }

            { state with
                  SelectedPatient = None
                  Details = InProgress }, LoadCommands.loadMarkdown qry

        else
            let qry =
                { LoadCommands.qry with
                      Generic = state.SelectedGeneric
                      Indication = state.SelectedIndication
                      Route = state.SelectedRoute
                      Patient = Some patient }

            { state with
                  SelectedPatient = Some patient
                  Details = InProgress }, LoadCommands.loadMarkdown qry

    | _ -> state, Cmd.none


let printTitle (versions: Deferred<(int * DateTime) list>) =
    match versions with
    | Resolved versions ->
        versions
        |> List.sortBy fst
        |> List.rev
        |> List.head
        |> (fun (v, d) -> sprintf " (versie: %i, %s)" v (d.ToString("dd-MM-yyyy")))
        |> sprintf "Afspraken Programma Formularium %s"
    | _ -> "Afspraken Programma Formularium"


let createFilter generics indications routes patients dispatch =
    [ match generics with
      | Resolved generics ->
          { Autocomplete.props with
                Dispatch = (SelectGeneric >> dispatch)
                Options = generics |> List.sort
                Label = sprintf "Zoek een generiek (van %i totaal)" (generics |> List.length)
                Filter = Filter.StartsWith }
          |> Autocomplete.render
      | _ -> ()

      match indications with
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

      match routes with
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

      match patients with
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

let render (state: State) (dispatch: Msg -> unit) =
    let filter = createFilter state.Generics state.Indications state.Routes state.Patients dispatch

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
                  [ [ Fable.MaterialUI.Icons.menuIcon "", ignore ]
                    |> Components.TitleBar.render (state.Versions |> printTitle)

                    Mui.container
                        [ prop.style
                            [ style.marginTop 90
                              style.padding 10 ]
                          container.maxWidth.md
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
