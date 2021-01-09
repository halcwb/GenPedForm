namespace Informedica.Formulary.Client.Components


/// Numeric input component that keeps track of min, max and step values.
/// Works well with Safari and Chrome. Has a problem with Firefox.
/// The problem is that in Firefox, the numeric input box accepts non numerical entries but totally
/// ignores this, so, also doesn't trigger any events.
module NumberInput =
    
    open Browser.Types
    open Fable.React
    
    open System
    open System.ComponentModel
    
    open Elmish
    open Thoth.Elmish
    open Feliz
    open Feliz.UseElmish
    open Feliz.MaterialUI
    open Fable.MaterialUI.Icons
    open Fable.Core
    open Fable.Core.JsInterop
    open Browser
    
    let private merge (o1: obj) (o2: obj) : obj = import "merge" "./../number-format.js"
    
    [<Erase>]
    type NumberFormat =
    
        static member inline numberformat =
            fun props ->
    
                let props =
                    {|
                        onValueChange =
                            fun values ->
                                printfn "getting value: %A" values?value
                                {| target = {| value = values?value |} |}
                        decimalSeparator = ","
                        thousandSeparator = "."
                    |}
                    |> merge props
    
    
                ofImport "default" "./../number-format.js" props []
    
            |> ReactElementType.ofFunction
        
    type State =
        {
            Error: bool
            UserInput: string option
        }
    
    
    type Msg =
        | ChangeValue of string
        | EndOfInput
    
    
    type Props =
        {| 
            value: float option
            min: float option
            max: float option
            step: float
            label: string
            adorn: string
            dispatch: string -> unit 
        |}
    
    
    let private init =
        {
            Error = false
            UserInput = None
        },
        Cmd.none
    
    
    let private update (props: Props) msg state =
        let tryParse (s : string) =
            match Double.TryParse(s) with
            | true, f   -> f |> Some
            | false, _  -> None
    
        let isErr (s: string) =
            if s.Trim() = "" then
                true
            else
                match s |> tryParse, props.min, props.max with
                | None, _, _ -> false
                | Some _, None, None -> true
                | Some v, Some min, None -> v >= min
                | Some v, None, Some max -> v <= max
                | Some v, Some min, Some max -> v >= min && v <= max
            |> not
    
        match msg with
        | ChangeValue s ->
    
            { state with
                Error = s |> isErr
                UserInput = s |> Some
            },
            Cmd.none
    
        | EndOfInput ->
            let state =
                { state with
                    Error = 
                        state.UserInput 
                        |> Option.defaultValue ""
                        |> isErr
                }
    
            state,
            Cmd.ofSub (fun _ -> state.UserInput |> Option.defaultValue "" |> props.dispatch)
    
    
    let useStyles err =
        Styles.makeStyles (fun styles theme ->
            {|
                field =
                    styles.create [
                        style.minWidth (theme.spacing 14)
                        style.marginTop (theme.spacing 1)
                    ]
                input =
                    styles.create
                        [
                            if not err then
                                style.color (theme.palette.primary.main)
                            else
                                style.color (theme.palette.error.main)
                        ]
                label =
                    styles.create [
                        style.fontSize (theme.typography.fontSize - 15.)
                        style.paddingRight (theme.spacing 2)
                    ]
            |})
    
    
    let defaultProps: Props =
        {|
            value = None
            min = None
            max = None
            step = 1.
            label = ""
            adorn = ""
            dispatch = (fun (s: string) -> ())
        |}
    
    
    let private comp =
        React.functionComponent
            ("numericinput",
                (fun (props: Props) ->
                    let state, dispatch =
                        React.useElmish (init, update props, [||])
    
                    let classes = (useStyles state.Error) ()
    
                    Mui.textField [
                        prop.className classes.field
                        textField.error state.Error
                        textField.label
                            (Mui.typography [
                                typography.variant.body2
                                typography.children [ props.label ]
                            ])

                        match state.UserInput with
                        | Some s -> s
                        | None   -> 
                            props.value 
                            |> Option.defaultValue 0. 
                            |> fun v -> if v = 0. then "" else v |> string
                        |> textField.value

                        textField.onChange (ChangeValue >> dispatch)
    
                        // dirty fix to disable number field typ in FF
                        let isFF =
                            navigator.userAgent.ToLower().Contains("firefox")
                        if not isFF then textField.type' "text"
    
                        textField.size.small
                        textField.InputProps [
                            // uses the react-number-format lib
                            // doesn't work, loses focus after each
                            // key stroke and doesn't detect setting
                            // value
                            // input.inputComponent NumberFormat.numberformat
    
                            input.inputProps [
                                prop.step props.step
                                match props.min with
                                | Some m -> prop.min m
                                | None -> prop.min 0.
                                match props.max with
                                | Some m -> prop.max m
                                | None -> ()
                            ]
                             
                            // sets the color of the input value
                            prop.className classes.input
                            // adds a unit (adornment) to a value
                            input.endAdornment
                                (Mui.inputAdornment [
                                    inputAdornment.position.end'
                                    inputAdornment.children
                                        [
                                            Mui.typography [
                                                typography.color.textSecondary
                                                typography.variant.body2
                                                typography.children [ props.adorn ]
                                            ]
                                        ]
                                ])
                        ]
                    ]))
    
    
    let render label adorn value dispatch =
        comp
            ({| defaultProps with
                    value = value
                    label = label
                    adorn = adorn
                    dispatch = dispatch
             |})
    
    
    let renderWithProps props = comp (props)

