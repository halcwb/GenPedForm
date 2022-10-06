namespace Components


module Autocomplete =

    open Elmish
    open Feliz
    open Feliz.UseElmish
    open Feliz.MaterialUI

    open Utils

    type State = { Selected : string option }


    let init selected = 
        { Selected = selected }, Cmd.none


    type Msg =
        | Select of string
        | Clear


    let update dispatch msg state =
        match msg with
        | Select s -> { state with Selected = Some s }, Cmd.ofSub (fun _ -> s |> dispatch)
        | Clear -> { state with Selected = None }, Cmd.ofSub (fun _ -> "" |> dispatch)


    type Filter =
        | StartsWith
        | Contains
        | StartsWithCaseSensitive
        | ContainsCaseSensitive
        | Exact


    module Filter =

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


    type Props =
        { 
            Dispatch: string -> unit
            Selected : string option
            Options : string []
            Label : string
            Filter : Filter
            SetValueIfOne : bool 
        }


    let props =
        { 
            Dispatch = ignore
            Options = [||]
            Selected = None
            Label = ""
            Filter = Filter.Contains
            SetValueIfOne = true 
        }


    let useStyles = Styles.makeStyles (fun theme styles -> {|  |})


    // It seems that options need to be an array of objects.
    // Make the objects optional to allow for the case of no
    // selected object
    let mapOptions sl =
        sl
        |> Array.sort
        |> Array.map (fun s -> {| label = s |} |> Some)


    let applyToOption def f (o: {| label: string |} option) =
        match o with
        | Some o -> o.label |> f
        | None -> def()


    [<ReactComponent>]
    let View (props : Props) =
        let classes = useStyles ()

        let state, dispatch = React.useElmish (init props.Selected, update props.Dispatch, [| box props.Selected; box props.Options |])
        let opts = props.Options |> mapOptions

        Mui.autocomplete
            [ 
                if ((opts |> Array.length) = 1) && props.SetValueIfOne then
                    prop.custom ("value", opts |> Array.head |> box)
                    //autocomplete.value (opts |> Array.head)
                else 
                    state.Selected
                    |> Option.map (fun s -> {| label = s |})
                    |> function
                    | None -> prop.custom ("value", null)
                    | Some v -> prop.custom ("value", box v)

                autocomplete.options opts

                autocomplete.getOptionLabel (applyToOption (fun _ ->  "") id)
                autocomplete.autoComplete true
                autocomplete.renderInput (fun pars ->
                    Mui.textField [ 
                        yield! pars.felizProps
                        textField.label props.Label 
                    ]
                )

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
                        | None -> false)) 
                ]


    let render props = View (props)
