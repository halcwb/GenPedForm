namespace Components

module MultiSelect =

    open System

    open Elmish
    open Elmish.React
    open Fable.React
    open Fable.React.Props
    open Fetch.Types
    open Thoth.Json
    open Feliz
    open Feliz.UseElmish
    open Feliz.MaterialUI
    open Fable.MaterialUI.Icons
    open Fable.Core.JsInterop


    let createMenuItems items =
        items
        |> List.map (fun (s : string) ->
            Mui.menuItem
                [
                    prop.key s
                    prop.value s
                    menuItem.children [
                        Mui.typography [
                            typography.color.primary
                            typography.variant.body1
                            prop.text s
                        ]
                    ]
                ]
        )


    type Props =
         {|
            value: string list
            items: string list
            label: string
            dispatch : string list -> unit
        |}


    let private comp =
        React.functionComponent("multiselect", fun (props: Props) ->

            Mui.formControl [
                Mui.inputLabel props.label
                Mui.select [
                    select.multiple true
                    select.value (props.value |> Seq.toArray)
                    select.renderValue (String.concat ", ")

                    select.onChange (fun (e : string[]) ->
                        e
                        |> Seq.map (fun s -> 
                            props.items
                            |> List.tryFind ((=) s)
           
                        )
                        |> Seq.filter Option.isSome
                        |> Seq.map (Option.get >> string)
                        |> Seq.toArray
                        |> ignore
                     )

                    props.items
                    |> createMenuItems
                    |> prop.children
                ]
            ]
        )


    let render props = comp props