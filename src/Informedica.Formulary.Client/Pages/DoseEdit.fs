﻿namespace Informedica.Formulary.Client.Pages


module DoseEdit =

    open System

     open Elmish
     open Feliz
     open Feliz.UseElmish
     open Feliz.MaterialUI
     open Fable.MaterialUI.Icons
     open Fable.Core.JsInterop
     open Feliz.Markdown
 
    open System
    open Informedica.Formulary.Shared.Types
    open Informedica.Formulary.Client
    open Informedica.Formulary.Client.Components


    type State = 
        { 
            Selected : string option
            Generics : CategorizedGeneric list
            DoseDetails : string option
        }

    
    let init generics = 
        fun () ->
            {
                Selected = None
                Generics = generics
                DoseDetails = None
            }
            , Cmd.none

    
    type Message = | CategoryClicked of string


    let findTreeItem id (generics : CategorizedGeneric list) =
        let rec find id' i (Category(c, doc)) =
            if c = RootCategory then
                match doc with
                | Dose d -> if id' = id then d else None
                | Categories cs ->
                    cs
                    |> List.mapi (fun i c -> i, c)
                    |> List.fold (fun d (i, c) ->
                        if d |> Option.isSome then d
                        else
                            find id' i c
                    ) None

            else
                let id' = sprintf "%s.%i" id' i
                match doc with
                | Dose d -> 
                    if id'= id then d else None
                | Categories cs ->
                    cs
                    |> List.mapi (fun i c -> i, c)
                    |> List.fold (fun d (i, c) ->
                        if d |> Option.isSome then d
                        else
                            find id' i c
                    ) None

        generics
        |> List.mapi (fun i g -> (i, g))
        |> List.fold (fun d (i1, g) -> 
            let id' = sprintf "%i" i1
            match d with
            | Some _             -> d
            | None when id' = id -> None
            | _ ->
                g.Shapes
                |> List.mapi (fun i s -> i, s)
                |> List.fold (fun d (i2, s) ->
                    let id' = sprintf "%i.%i" i1 i2
                    match d with 
                    | Some _ -> d
                    | None when id' = id -> None
                    | _ ->
                        s.Routes 
                        |> List.mapi (fun i r -> i, r)
                        |> List.fold (fun d (i3, r) ->
                            let id' = sprintf "%i.%i.%i" i1 i2 i3
                            match d with
                            | Some _ -> d
                            | None when  id' = id -> d
                            | _ ->
                                r.Indications 
                                |> List.mapi (fun i4 i -> i4, i)
                                |> List.fold (fun d (i4, i) ->
                                    let id' = sprintf "%i.%i.%i.%i" i1 i2 i3 i4
                                    match d with
                                    | Some _ -> d
                                    | None when  id' = id -> None
                                    | _ ->
                                        i.Patient
                                        |> find id' 0
                                ) None
                        ) None
                ) None
        ) None


    let update msg state =
        match msg with
        | CategoryClicked s ->
            { state with
                DoseDetails =
                    state.Generics 
                    |> findTreeItem s 
                    |> function
                    | Some d -> 
                        d 
                        |> Utils.printDose 
                        |> Some
                    | None   -> None

            }, 
            Cmd.none


    let createItem 
        dispatch 
        hasDose
        variant 
        id 
        (labelText : string) 
        (children : ReactElement seq) =
        
        let labelDiv = 
            Html.div [
                prop.style [
                    style.display.flex
                    style.alignItems.center
                ]
                prop.children [
                    if hasDose then 
                        editIcon []
                    Mui.typography [
                        prop.style [
                            if variant = typography.variant.h5 ||
                               variant = typography.variant.h6 then
                                style.color Colors.indigo.``900`` 
                        ]
                        variant
                        prop.text labelText
                    ]
                ]
            ]

        Mui.treeItem [
            treeItem.nodeId id
            treeItem.label [ labelDiv ]
            prop.onClick (fun _ -> id |> CategoryClicked |> dispatch)
            if id <> "0" then
                prop.draggable true
                prop.onDragStart (fun de ->
                    Browser.Dom.console.log("start drag from", de.fromElement)
                    let text = sprintf "id: %s, label: %s" id labelText
                    de.dataTransfer.setData("text/plain", text)
                    |> ignore
                )
                prop.onDragOver (fun de ->
                    de.preventDefault ()
                )
                prop.onDragEnter (fun de ->
                    de.preventDefault ()
                )
                prop.onDrop (fun de ->
                    printfn "item was dropped"
                    Browser.Dom.console.log("drop data", de.dataTransfer.getData("text/plain"))
                )

            treeItem.children children
        ]
        

    let categoryToTreeItems dispatch id (Category(c, doc)) =
        let createItem b = createItem dispatch b typography.variant.body1 
        let toString = Utils.patientToStr

        let rec toItems id i (Category(c, doc)) =
            let id = sprintf "%s.%i" id i
            match doc with
            | Dose _ -> 
                createItem true id (c |> toString) []
            | Categories cs ->
                cs
                |> List.mapi (fun i x -> toItems id i x)
                |> createItem false id (c |> toString)

        match c with
        | RootCategory ->
            match doc with
            | Dose _ -> []
            | Categories cs ->
                cs
                |> List.mapi (fun i (Category(c, doc)) -> 
                    let id = sprintf "%s.%i" id i
                    match doc with
                    | Dose _ -> createItem true id (c |> toString) []
                    | Categories cs ->
                        cs
                        |> List.mapi (fun i x -> toItems id i x)
                        |> createItem false id (c |> toString)
                )
        | _ -> []


    let private comp =
        React.functionComponent("dosedit", fun (props: {| generics: CategorizedGeneric list |}) ->
            let state, dispatch = React.useElmish ((init props.generics), update, [||])
            
            let createItem = createItem dispatch false
            let categoryToTreeItems = categoryToTreeItems dispatch

            let tree =
                props.generics
                |> List.mapi(fun i1 g -> 
                    g.Shapes
                    |> List.mapi (fun i2 s -> 
                        s.Routes
                        |> List.mapi (fun i3 r ->
                            r.Indications
                            |> List.mapi (fun i4 i ->
                                let id = sprintf "%i.%i.%i.%i" i1 i2 i3 i4
                                i.Patient
                                |> categoryToTreeItems id
                                |> createItem typography.variant.h6 id i.Indication
                            )
                            |> createItem typography.variant.h6 (sprintf "%i.%i.%i" i1 i2 i3) r.Route
                        )
                        |> createItem typography.variant.h6 (sprintf "%i.%i" i1 i2) s.Shape 
                    )
                    |> createItem typography.variant.h5 (i1 |> string) g.Generic

                )
                |> fun items ->
                    Mui.treeView [
    //                    prop.style [ style.width 500; style. height 500]
                        treeView.defaultCollapseIcon (arrowDropDownIcon [])
                        treeView.defaultExpandIcon (arrowRightIcon [])
                        treeView.children items
                    ]

            Mui.container [
                prop.style [
                    style.display.flex
                    style.flexDirection.row
                ]
                container.disableGutters true
                container.children [
                    tree
                    Html.div [ 
                        prop.style [
                            style.marginLeft 40
                            style.width 400
                        ]

                        prop.children [
                            state.DoseDetails 
                            |> Option.defaultValue ""
                            |> markdown.source
                            |> List.singleton
                            |> List.append [ markdown.escapeHtml false ]
                            |> Markdown.markdown

                        ]
                    ]
                ]
            ]
        )


    let render gs = comp({| generics = gs |})