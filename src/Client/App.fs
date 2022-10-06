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
        Query: Deferred<Query>
    }


type Msg =
    | RunQuery of AsyncOperationStatus<Result<Query, string>>
    | SelectGeneric of string
    | SelectIndication of string
    | SelectShape of string
    | SelectRoute of string
    | SelectPatient of string
    | SelectDiagnosis of string
    | Refresh


let initialState =
    { 
        Query = HasNotStartedYet
    }


let queryCmd = function
    | Resolved qry ->
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
            if s = "" then set state None
            else
                set state (Some s)

        { state with State.Query = InProgress }, state.Query |> queryCmd

    match msg with
    | RunQuery Started -> state, Cmd.none

    | RunQuery(Finished(Ok qry)) ->
        { state with Query = Resolved qry } , Cmd.none

    | RunQuery(Finished(Error e)) ->
        printfn "error %s" e
        state, Cmd.none

    | Refresh -> 
        let cmd =
            state.Query
            |> Deferred.map (fun qry ->
                { qry with Refresh = true }
            )
            |> queryCmd

        { state with Query = InProgress }, cmd

    | SelectGeneric s ->
        if s = "" then initialState, initialState.Query |> queryCmd
        else
            fun (state: State) gen ->
                { state with
                    Query = 
                        state.Query 
                        |> Deferred.map (fun q -> 
                            { q with 
                                Filter = 
                                    { q.Filter with
                                        Generic = gen
                                    }
                            }
                        )
                }
            |> select s

    | SelectIndication s ->
        fun (state : State) ind ->
            { state with
                Query = 
                    state.Query 
                    |> Deferred.map (fun q -> 
                        { q with 
                            Filter = 
                                { q.Filter with
                                    Indication = ind
                                }
                        }
                    )
            }
        |> select s

    | SelectShape s ->
        fun (state : State) shp ->
            { state with
                Query = 
                    state.Query 
                    |> Deferred.map (fun q -> 
                        { q with 
                            Filter = 
                                { q.Filter with
                                    Shape = shp
                                }
                        }
                    )
            }
        |> select s

    | SelectRoute s ->
        fun (state : State) rte ->
            { state with
                Query = 
                    state.Query 
                    |> Deferred.map (fun q -> 
                        { q with 
                            Filter = 
                                { q.Filter with
                                    Route = rte
                                }
                        }
                    )
            }
        |> select s

    | SelectPatient pat ->
        fun (state : State) s ->
            { state with
                Query = 
                    state.Query 
                    |> Deferred.map (fun q -> 
                        { q with 
                            Filter = 
                                { q.Filter with
                                    PatientString =  s
                                }
                        }
                    )
            }
        |> select pat

    | SelectDiagnosis s ->
        fun (state : State)  diagn ->
            { state with
                Query = 
                    state.Query 
                    |> Deferred.map (fun q -> 
                        { q with 
                            Filter = 
                                { q.Filter with
                                    Patient = 
                                        { q.Filter.Patient with
                                            Diagnosis = diagn
                                        }
                                }
                        }
                    )
            }
        |> select s


let createFilter generics indications shapes routes patients diagnoses dispatch =
    let toDiv el =
        Html.div [
            prop.style [ style.marginTop 20 ]
            prop.children [ el ]
        ]
    [ 
        { Components.Autocomplete.props with
            Dispatch = SelectGeneric >> dispatch
            Options = generics 
            Label = sprintf "Zoek een generiek (van %i totaal)" (generics |> Array.length)
            Filter = Filter.StartsWith 
        }
        |> Components.Autocomplete.render
        |> toDiv

        
        { Components.Autocomplete.props with
            Dispatch = SelectIndication >> dispatch
            Options = indications 
            Label = sprintf "Kies een indicatie (van %i totaal)" (indications |> Array.length)
            Filter = Filter.ContainsCaseSensitive 
        }
        |> Components.Autocomplete.render 
        |> toDiv

        { Components.Autocomplete.props with
                Dispatch = SelectShape >> dispatch
                Options = shapes
                Label = sprintf "Kies een vorm (van %i totaal)" (shapes |> Array.length)
                Filter = Filter.StartsWith
        }
        |> Components.Autocomplete.render 
        |> toDiv

        { Components.Autocomplete.props with
                Dispatch = SelectRoute >> dispatch
                Options = routes
                Label = sprintf "Kies een route (van %i totaal)" (routes |> Array.length)
                Filter = Filter.StartsWith
        }
        |> Components.Autocomplete.render 
        |> toDiv

        { Components.Autocomplete.props with
            Dispatch = SelectPatient >> dispatch
            Options = patients |> Array.distinct
            Label = sprintf "Kies een patient (van %i totaal)" (patients |> Array.length)
            Filter = Filter.StartsWith 
        }
        |> Components.Autocomplete.render 
        |> toDiv

    
        if diagnoses |> Array.length >= 1 then
            { Components.Autocomplete.props with
                Dispatch = SelectDiagnosis >> dispatch
                Options = diagnoses |> Array.distinct 
                Label = sprintf "Kies een diagnose (van %i totaal)" (diagnoses |> Array.length)
                Filter = Filter.StartsWith 
            }
            |> Components.Autocomplete.render 
            |> toDiv
    ]


let render (state: State) (dispatch: Msg -> unit) =
    let toMarkDown s = 
        s
        |> markdown.children
        |> List.singleton
        |> Markdown.markdown

    let titleBar = 
        [ 
            Fable.MaterialUI.Icons.menuIcon "", fun () -> () 
            Fable.MaterialUI.Icons.refreshIcon "", (fun () -> Refresh |> dispatch)
        ]
        |> Components.TitleBar.render "Generiek Pediatrisch Formularium"

    let mainWindow =
        match state.Query with
        | HasNotStartedYet 
        | InProgress  -> 
            Mui.circularProgress [  ]
        | Resolved qry ->
            let filter = 
                createFilter 
                    qry.Generics 
                    qry.Indications 
                    qry.Shapes
                    qry.Routes
                    qry.Patients
                    qry.Diagnoses
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