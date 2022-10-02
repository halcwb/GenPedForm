namespace Components

module Autocomplete =

    open Elmish
    open Feliz
    open Feliz.UseElmish
    open Feliz.MaterialUI

    let private init() = (), Cmd.none

    type private Msg =
        | Select of string
        | Clear

    let private update dispatch msg _ =
        match msg with
        | Select s -> (), Cmd.ofSub (fun _ -> s |> dispatch)
        | Clear -> (), Cmd.ofSub (fun _ -> "" |> dispatch)

    module Filter =

        type Filter =
            | StartsWith
            | Contains
            | StartsWithCaseSensitive
            | ContainsCaseSensitive
            | Exact

        let toString (filter: Filter) = sprintf "%A" filter

        let toFilter s =
            match s with
            | _ when s = (sprintf "%A" StartsWith) -> StartsWith
            | _ when s = (sprintf "%A" Contains) -> Contains
            | _ when s = (sprintf "%A" StartsWithCaseSensitive) -> StartsWithCaseSensitive
            | _ when s = (sprintf "%A" ContainsCaseSensitive) -> ContainsCaseSensitive
            | _ when s = (sprintf "%A" Exact) -> Exact
            | _ ->
                printfn "could not find filter for %s" s
                Contains

    type Filter = Filter.Filter

    type Props =
        { Dispatch: string -> unit
          Options: string list
          Label: string
          Filter: Filter
          SetValueIfOne: bool }

    let props =
        { Dispatch = ignore
          Options = []
          Label = ""
          Filter = Filter.Contains
          SetValueIfOne = true }

    let private useStyles = Styles.makeStyles (fun theme styles -> {|  |})

    // It seems that options need to be an array of objects.
    // Make the objects optional to allow for the case of no
    // selected object
    let mapOptions sl =
        sl
        |> List.map (fun s -> {| label = s |} |> Some)
        |> List.toArray

    let applyToOption def f (o: {| label: string |} option) =
        match o with
        | Some o -> o.label |> f
        | None -> def()

    let private comp =
        React.functionComponent
            ("autocomplete",
             (fun (props: Props) ->
                 let _, dispatch = React.useElmish (init, update props.Dispatch, [||])
                 let classes = useStyles()
                 let opts = props.Options |> mapOptions

                 Mui.autocomplete
                     [ autocomplete.options opts
                       // automatically set the value if there is only one
                       // and disable clear button
                       if ((opts |> Array.length) = 1) && props.SetValueIfOne then
                           autocomplete.disableClearable true
                           autocomplete.value (opts |> Array.head)
                       autocomplete.getOptionLabel (applyToOption (fun _ -> "") id)
                       autocomplete.autoComplete true
                       autocomplete.renderInput (fun pars ->
                           Mui.textField
                               [ yield! pars.felizProps
                                 textField.label props.Label ])

                       autocomplete.onChange (applyToOption (fun _ -> Clear |> dispatch) (Select >> dispatch))
                       autocomplete.getOptionSelected (=)

                       autocomplete.filterOptions (fun (options: {| label: string |} option []) (state: string) ->
                           options
                           |> Array.filter (fun o ->
                               match o with
                               | Some o ->
                                   let label, state =
                                       match props.Filter with
                                       | Filter.StartsWith
                                       | Filter.Contains -> o.label.Trim().ToLower(), state.Trim().ToLower()
                                       | _ -> o.label.Trim(), state.Trim()
                                   match props.Filter with
                                   | Filter.StartsWith
                                   | Filter.StartsWithCaseSensitive -> label.StartsWith(state)
                                   | Filter.Contains
                                   | Filter.ContainsCaseSensitive -> label.Contains(state)
                                   | Filter.Exact -> label = state
                               | None -> false)) ]))

    let render props = comp (props)
