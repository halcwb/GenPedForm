module App

open Elmish
open Elmish.React
open Feliz
open Feliz.Markdown
open Feliz.MaterialUI

open Shared
open Components

module Filter = Autocomplete.Filter

let defaultTheme = Styles.createMuiTheme()


type Model =
    { Generics: string list
      Products: Products
      Details: Deferred<string>
    }


type Msg =
    | LoadProducts of AsyncOperationStatus<Result<Products, string>>
    | LoadGenerics of AsyncOperationStatus<Result<Generics, string>>
    | LoadMarkdown of AsyncOperationStatus<Result<string, string>>
    | Selected of string


let loadProducts =
    async {
        let! products = Server.api.GetProducts()
        return LoadProducts(Finished products) }


let loadGenerics =
    async {
        let! generics = Server.api.GetGenerics()
        return LoadGenerics(Finished generics) }

let init(): Model * Cmd<Msg> =
    let initialModel =
        { Products = []
          Generics = []
          Details  = HasNotStartedYet
        }

    let loadCmd =
        [ Cmd.fromAsync loadProducts
          Cmd.fromAsync loadGenerics ]
        |> Cmd.batch

    initialModel, loadCmd


let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | LoadProducts(Finished(Ok products)) -> { model with Products = products }, Cmd.none
    | LoadGenerics(Finished(Ok generics)) -> { model with Generics = generics }, Cmd.none
    | Selected s ->
        if s <> "" then
            let load = 
                async {
                    let! details = Server.api.GetMarkdown s
                    return LoadMarkdown(Finished details)
                }
            { model with
                Details = InProgress }, Cmd.fromAsync load
        else 
            {
                model with 
                    Details = HasNotStartedYet
            }, Cmd.none
    | LoadMarkdown(Finished(Ok s)) ->
        { model with
            Details = Resolved s }, Cmd.none
    | _ -> model, Cmd.none


let showProducts (products: Products) =
    Mui.list
        [ for p in products do
            Mui.listItemText p ]


let render (model: Model) (dispatch: Msg -> unit) =
    let autocomplete =
        { Autocomplete.props with
              Dispatch = (Selected >> dispatch)
              Options = model.Generics |> List.sort
              Label = "Zoek een generiek"
              Filter = Filter.StartsWith }
        |> Autocomplete.render

    let details = 
        match model.Details with
        | HasNotStartedYet ->
            "## Geen generiek gekozen"
        | InProgress ->
            "## Doseringen worden opgehaald ..."
        | Resolved s ->
            printfn "%s" s
            s
        |> markdown.source
        |> List.singleton
        |> List.append [
            markdown.escapeHtml false
        ]
        |> Markdown.markdown
        |> Html.div


    Mui.themeProvider
        [ themeProvider.children
            [ Html.div
                [ Mui.appBar
                    [ prop.style
                        [ style.display.flex
                          style.flexDirection.row
                          style.padding 10 ]
                      appBar.variant.elevation
                      appBar.children
                          [ Mui.typography
                              [ prop.style
                                  [ style.flexGrow 1
                                    style.padding 10 ]
                                typography.variant.h6
                                prop.text "Afspraken Programma Formularium" ]
                            Mui.iconButton
                                [ prop.style [ style.color "white" ]
                                  prop.children [ Fable.MaterialUI.Icons.menuIcon "" ] ] ] ]
                  Mui.container
                      [ prop.style [ style.marginTop 80 ]
                        prop.children
                            [ // search
                              Mui.paper
                                  [ prop.style [ style.padding 10 ]
                                    paper.children [ autocomplete ] ]
                              // details
                              Mui.paper
                                  [ prop.style
                                      [ style.marginTop 10
                                        style.padding 10 ]
                                    prop.children [ details ] ] ] ] ] ] ]
