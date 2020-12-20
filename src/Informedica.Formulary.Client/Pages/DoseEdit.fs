namespace Informedica.Formulary.Client.Pages


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
    open Informedica.Formulary.Client.Components


    let createItem id (label : string) (children : ReactElement seq) =
        printfn "creating %s: %s" id label
        Mui.treeItem [
            treeItem.nodeId id
            treeItem.label [
                Mui.typography [
                    prop.text label
                ]
            ]
            if id <> "0" then
                prop.draggable true
                prop.onDragStart (fun de ->
                    Browser.Dom.console.log("start drag from", de.fromElement)
                    let text = sprintf "id: %s, label: %s" id label
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


    let categoryToTreeItems id (Category(c, doc)) =
//        Browser.Dom.console.log("category", c)
        let rec toItems id i (Category(c, doc)) =
            let id = sprintf "%s.%i" id i
            match doc with
            | Dose _ -> createItem id (c |> string) []
            | Categories cs ->
                cs
                |> List.mapi (fun i x -> toItems id i x)
                |> createItem id (c |> string)

        match c with
        | RootCategory ->
            match doc with
            | Dose _ -> []
            | Categories cs ->
                cs
                |> List.mapi (fun i (Category(c, doc)) -> 
                    let id = sprintf "%s.%i" id i
                    match doc with
                    | Dose _ -> createItem id (c |> string) []
                    | Categories cs ->
                        cs
                        |> List.mapi (fun i x -> toItems id i x)
                        |> createItem id (c |> string)
                )
        | _ -> []


    let private comp =
        React.functionComponent("dosedit", fun (props: {| generics: CategorizedGeneric list |}) ->
            printfn "creating tree component with %i items" (props.generics |> List.length)
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
                            |> createItem  id i.Indication
                        )
                        |> createItem (sprintf "%i.%i.%i" i1 i2 i3) r.Route
                    )
                    |> createItem (sprintf "%i.%i" i1 i2) s.Shape 
                )
                |> createItem (i1 |> string) g.Generic

            )
            |> fun items ->
                Mui.treeView [
//                    prop.style [ style.width 500; style. height 500]
                    treeView.defaultCollapseIcon (expandMoreIcon [])
                    treeView.defaultExpandIcon (chevronRightIcon [])
                    treeView.children items
                ]
        )


    let render gs = comp({| generics = gs |})