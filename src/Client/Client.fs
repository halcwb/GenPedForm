module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Thoth.Json
open Feliz
open Feliz.MaterialUI

open Shared

let defaultTheme = Styles.createMuiTheme ()

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model =
    {
        Patient : Patient option
        Products: Product []
        Doses : Dose []
    }


// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | ProductsLoaded of Product []
    | DosesLoaded of Dose []

let fetchProducts () = Fetch.fetchAs<Product[], Product []> "/api/products"
let fetchDoses () = Fetch.fetchAs<Dose [], Dose []> "/api/doses"

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Patient = None; Products = Array.empty; Doses = Array.empty }
    let loadCmd =
        [
            Cmd.OfPromise.perform fetchProducts () ProductsLoaded
            Cmd.OfPromise.perform fetchDoses () DosesLoaded
        ] |> Cmd.batch
    initialModel, loadCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | ProductsLoaded prods ->
        { model with Products = prods }, Cmd.none
    | DosesLoaded doses ->
        { model with Doses = doses }, Cmd.none


let showDoses (model : Model) =
    let sortPat (d: Dose) =
        let inline toInt x =
            match x with
            | Some x -> x |> int
            | None -> 0

        (d.MinAgeMo |> toInt) +
//        (d.MaxAgeMo |> toInt) +
        (d.MinWeightKg |> toInt) +
//        (d.MaxWeightKg |> toInt) +
        (d.MinGestAgeDays |> toInt) +
//        (d.MaxGestAgeDays |> toInt) +
        (d.MinPMAgeDays |> toInt) //+
//        (d.MaxPMAgeDays |> toInt)

    let printDose (d : Dose) =
        let printQ u = function
        | Quantity q -> sprintf "%A %s" q u
        | QuantityPerKg q -> sprintf "%A %s/kg" q u
        | QuantityPerM2 q -> sprintf "%A %s/m2" q u

        match d.NormDose with
        | Some q -> q |> printQ d.Unit
        | None -> ""
        |> fun s ->
            match d.MinDose, d.MaxDose with
            | Some min, Some max ->
                let min = min |> printQ d.Unit
                let max = max |> printQ d.Unit
                if s = "" then sprintf "van %s tot %s" min max
                else sprintf "%s, van %s tot %s" s min max
            | Some min, None ->
                let min = min |> printQ d.Unit
                if s = "" then sprintf "van %s" min
                else sprintf "%s, van %s" s min
            | None, Some max ->
                let max = max |> printQ d.Unit
                if s = "" then sprintf "tot %s" max
                else sprintf "%s, tot %s" s max
            | _ -> s
            |> fun s ->
                match d.AbsMaxDose with
                | Some max ->
                    let max = max |> printQ d.Unit
                    let time =
                        d.Freqs
                        |> Seq.fold (fun acc f ->
                            f.Time |> snd
                        ) ""
                    if s = "" then sprintf "max %s per %s" max time
                    else sprintf "%s, max %s per %s" s max time
                | None -> s
                |> fun s ->
                    match d.MaxPerDose with
                    | Some max ->
                        let max = max |> printQ d.Unit
                        if s = "" then sprintf "max per keer %s" max
                        else sprintf "%s, max per keer %s" s max
                    | None -> s

    let printFreq (f : Frequency) =
        match (f.Time |> fst) with
        | x when x = 1 -> sprintf "%A x / %A" f.Count (f.Time |> snd)
        | _ -> sprintf "%A x / %A %A" f.Count (f.Time |> fst) (f.Time |> snd)

    let printPat p =
        let printDays d =
            (d / 7) |> sprintf "%A weken"

        let printAge a =
            match a with
            | _ when a < 1. ->
                (a * 30. / 7.)
                |> int
                |> fun i ->
                    if i = 1 then sprintf "%A week" i
                    else sprintf "%A weken" i
            | _ when a < 12. ->
                a
                |> int
                |> fun i ->
                    if i = 1 then sprintf "%A maand" i
                    else sprintf "%A maanden" i
            | _ ->
                (a / 12.)
                |> int
                |> fun i ->
                    if i = 1 then sprintf "%A jaar" i
                    else sprintf "%A jaar" i

        match p.Gender with
        | Male -> "man "
        | Female -> "vrouw "
        | Unknown _ -> ""
        |> fun s ->
            match p.MinAgeMo, p.MaxAgeMo with
            | Some min, Some max ->
                let min = min |> printAge
                let max = max |> printAge
                sprintf "%sleeftijd %s tot %s " s min max
            | Some min, None ->
                let min = min |> printAge
                sprintf "%sleeftijd vanaf %s " s min
            | None, Some max ->
                let max = max |> printAge
                sprintf "%sleeftijd tot %s " s max
            | _ -> ""
            |> fun s ->
                match p.MinGestAgeDays, p.MaxGestAgeDays, p.MinPMAgeDays, p.MaxPMAgeDays with
                | Some min, Some max, _, _ ->
                    let min = min |> printDays
                    let max = max |> printDays
                    sprintf "%sneonaten zwangerschapsduur %s tot %s" s min max
                | Some min, None, _, _ ->
                    let min = min |> printDays
                    sprintf "%sneonaten zwangerschapsduur vanaf %s" s min
                | None, Some max, _, _ ->
                    let max = max |> printDays
                    sprintf "%sneonaten zwangerschapsduur tot %s" s max
                | _, _, Some min, Some max ->
                    let min = min |> printDays
                    let max = max |> printDays
                    sprintf "%sneonaten postconceptie leeftijd %s tot %s" s min max
                | _, _, Some min, None ->
                    let min = min |> printDays
                    sprintf "%sneonaten postconceptie leeftijd vanaf %s" s min
                | _, _, None, Some max ->
                    let max = max |> printDays
                    sprintf "%sneonaten postconceptie leeftijd tot %s" s max
                | _ -> s
                |> fun s ->
                    match p.MinWeightKg, p.MaxWeightKg with
                    | Some min, Some max -> sprintf "%A tot %A kg" min max
                    | Some min, None -> sprintf "van %A kg" min
                    | None, Some max -> sprintf "tot %A kg" max
                    | None, None -> ""
                    |> sprintf "%s %s" s


    match model with
    | { Doses  = doses } when doses |> Seq.length > 0 ->
        let items =
            doses
            |> Seq.groupBy (fun p -> p.Generic)
            |> Seq.map (fun (g, ds) ->
                g,
                ds
                |> Seq.groupBy (fun d -> d.Indication)
                |> Seq.map (fun (i, ds) ->
                    i,
                    ds
                    |> Seq.groupBy (fun d -> d.Route)
                    |> Seq.map (fun (r, ds) ->
                        r,
                        ds
                        |> Seq.sortBy sortPat
                        |> Seq.groupBy(fun d ->
                            d
                            |> printPat
                        )
                        |> Seq.map (fun (pat, ds) ->
                            pat,
                            ds
                            |> Seq.groupBy (fun d ->
                                d.Freqs
                                |> Seq.map printFreq
                                |> String.concat ", "
                            )
                            |> Seq.map (fun (f, ds) ->
                                f,
                                ds
                                |> Seq.map printDose
                                |> Seq.distinct
                            )
                        )
                    )
                )
            )

        [
            for (s, xs) in items do
                Html.div  [
                    prop.style [
                        style.display.flex
                        style.flexDirection.column
                    ]
                    prop.children [
                            Mui.typography [
                                typography.variant.h6
                                prop.text s
                            ]
                            for (s, xs) in xs do
                                Mui.typography [
                                    typography.variant.subtitle2
                                    typography.color.primary
                                    prop.text s
                                ]
                                for (s, xs) in xs do
                                    Mui.typography [
                                        typography.variant.subtitle2
                                        prop.text (sprintf "route: %s" s)
                                    ]
                                    for (s, xs) in xs do
                                        Mui.typography [
                                            typography.variant.subtitle1
                                            typography.color.textSecondary
                                            prop.text s
                                        ]
                                        for (f, ds) in xs do
                                            for d in ds do
                                            Mui.typography [
                                                typography.variant.body2
                                                prop.text (sprintf "- %s in %s" d f)
                                            ]



                    ]
                ]
                |> List.singleton
                |> Mui.listItem
        ]
    | _ ->
        "Loading..."
        |> fun s ->
            Mui.typography
                [ typography.variant.caption
                  prop.text s ]
            |> Seq.singleton
            |> Mui.listItem
            |> List.singleton
    |> Mui.list

let view (model : Model) (dispatch : Msg -> unit) =
    Mui.themeProvider [
        themeProvider.children [
            Html.div [
                Mui.appBar [
                    prop.style [
                        style.display.flex
                        style.flexDirection.row
                        style.padding 10
                    ]
                    appBar.variant.elevation
                    appBar.children [
                        Mui.typography [
                            prop.style [
                                style.flexGrow 1
                                style.padding 10
                            ]
                            typography.variant.h6
                            prop.text "GenPed Kinderformularium"
                        ]
                        Mui.iconButton [
                            prop.style [
                                style.color "white"
                            ]
                            prop.children [
                               Fable.MaterialUI.Icons.menuIcon ""
                            ]

                        ]
                    ]
                ]
                Html.div [
                    prop.style [
                        style.marginTop 80
                    ]
                    prop.children [
                        Mui.paper [
                            prop.style [
                                style.padding 10
                            ]
                            paper.children [
                                Mui.typography "patient"
                            ]
                        ]
                        Mui.paper [
                            prop.style [
                                style.marginTop 10
                                style.padding 10
                            ]
                            model
                            |> showDoses
                            |> prop.children
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

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
