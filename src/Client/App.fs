module App

open System

open Elmish
open Elmish.React
open Feliz
open Feliz.Markdown
open Feliz.MaterialUI

open Shared


type Filter = Components.Autocomplete.Filter


type State =
    { 
        SelectedPatient: string option
        Query: Deferred<Query>
    }


type Msg =
    | RunQuery of AsyncOperationStatus<Result<Query, string>>
    | SelectGeneric of string
    | SelectIndication of string
    | SelectRoute of string
    (*
    | SelectPatient of string
    *)


let initialState =
    { 
        SelectedPatient = None
        Query = HasNotStartedYet
    }


let queryCmd = function
    | Resolved qry ->
        printfn $"cmd: {qry.Filter.Generic}"
        async {
            let! result = Server.api.Query qry
            return RunQuery(Finished result)
        }
        |> Cmd.fromAsync

    | HasNotStartedYet ->
        async {
            let! result = Server.api.Query Query.query
            return RunQuery(Finished result)
        }
        |> Cmd.fromAsync

    | InProgress -> Cmd.none


let init(): State * Cmd<Msg> =
    initialState, initialState.Query |> queryCmd


let update (msg: Msg) (state: State): State * Cmd<Msg> =
    let select s set = 
        let state =
            if s = "" then initialState
            else
                set state s

        state, state.Query |> queryCmd

    match msg with
    | RunQuery Started -> state, Cmd.none

    | RunQuery(Finished(Ok qry)) ->
        { state with Query = Resolved qry } , Cmd.none

    | RunQuery(Finished(Error e)) ->
        printfn "error %s" e
        state, Cmd.none

    | SelectGeneric s ->
        printfn $"selected: {s}"
        fun (state : State) s ->
            { state with
                Query = 
                    state.Query 
                    |> Deferred.map (fun q -> 
                        { q with 
                            Filter = 
                                { q.Filter with
                                    Generic = Some s
                                }
                        }
                    )
            }
        |> select s

    | SelectIndication s ->
        fun (state : State)  s ->
            { state with
                Query = 
                    state.Query 
                    |> Deferred.map (fun q -> 
                        { q with 
                            Filter = 
                                { q.Filter with
                                    Indication = Some s
                                }
                        }
                    )
            }
        |> select s

    | SelectRoute s ->
        fun (state : State)  s ->
            { state with
                Query = 
                    state.Query 
                    |> Deferred.map (fun q -> 
                        { q with 
                            Filter = 
                                { q.Filter with
                                    Route = Some s
                                }
                        }
                    )
            }
        |> select s


let createFilter generics indications routes dispatch =
    let toDiv el =
        Html.div [
            prop.style [ style.marginTop 20 ]
            prop.children [ el ]
        ]
    [ 
        { Components.Autocomplete.props with
            Dispatch = SelectGeneric >> dispatch
            Options = generics |> Array.toList
            Label = sprintf "Zoek een generiek (van %i totaal)" (generics |> Array.length)
            Filter = Filter.StartsWith 
        }
        |> Components.Autocomplete.render
        |> toDiv

        
        { Components.Autocomplete.props with
            Dispatch = SelectIndication >> dispatch
            Options = indications |> Array.toList
            Label = sprintf "Kies een indicatie (van %i totaal)" (indications |> Array.length)
            Filter = Filter.ContainsCaseSensitive 
        }
        |> Components.Autocomplete.render 
        |> toDiv

        { Components.Autocomplete.props with
                Dispatch = SelectRoute >> dispatch
                Options = routes |> Array.toList
                Label = sprintf "Kies een route (van %i totaal)" (routes |> Array.length)
                Filter = Filter.StartsWith
        }
        |> Components.Autocomplete.render 
        |> toDiv

        (*
        match routes with
        | Resolved routes ->
            Html.div [ 
                prop.style [ style.marginTop 20 ]
                prop.children [ 
                    { Autocomplete.props with
                            Dispatch = SelectRoute >> dispatch
                            Options = routes |> List.sort
                            Label = sprintf "Kies een route (van %i totaal)" (routes |> List.length)
                            Filter = Filter.StartsWith }
                    |> Autocomplete.render 
                ] 
            ]
        | _ -> ()

        match patients with
        | Resolved pats ->
            Html.div [ 
                prop.style [ style.marginTop 20 ]
                prop.children [ 
                    { Autocomplete.props with
                        Dispatch = SelectPatient >> dispatch
                        Options = pats
                        Label = sprintf "Kies een patient (van %i totaal)" (pats |> List.length)
                        Filter = Filter.StartsWith }
                    |> Autocomplete.render 
                ] 
            ]
        | _ -> () 
        *)
    ]


let render (state: State) (dispatch: Msg -> unit) =
    let toMarkDown s = 
        printfn $"to markdown: {s}"
        s
        |> markdown.children
        |> List.singleton
        |> List.append [ markdown.escapeHtml false ]
        |> Markdown.markdown

    let titleBar = 
        [ Fable.MaterialUI.Icons.menuIcon "", fun () -> () ]
        |> Components.TitleBar.render "Generieke Pediatrisch Formularium"

    let mainWindow =
        match state.Query with
        | HasNotStartedYet -> "## De boel moet nog worden opgestart" |> toMarkDown
        | InProgress  -> "## Formularium wordt geladen" |> toMarkDown
        | Resolved qry ->
            let filter = 
                createFilter 
                    qry.Generics 
                    qry.Indications 
                    qry.Routes
                    dispatch

            Html.div [
                // search
                Html.div [ 
                    prop.style [ style.padding 10 ]
                    paper.children filter 
                ]
                
                Mui.paper [ 
                    prop.style [ 
                        style.padding 10
                        style.color Colors.indigo.``900`` 
                    ]

                    qry.Markdown
                    |> Array.toList
                    |> List.map toMarkDown
                    |> prop.children 
                ]
            ]

    let theme =
        Styles.createTheme [
            theme.overrides.muiDialogTitle.root [
                style.backgroundColor.lightGray
            ]
            theme.palette.primary Colors.blue
        ]

    Mui.themeProvider [ 
        themeProvider.theme theme
        themeProvider.children [ 
            Html.div [ 
                prop.children [
                    titleBar
                    Mui.container [ 
                        prop.style [ 
                            style.marginTop 90
                            style.padding 10 
                        ]
                        container.maxWidth.md
                        prop.children [ 
                            mainWindow 
                        ] 
                    ]                     
                ]
            ] 
        ] 
    ]




#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update render
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run