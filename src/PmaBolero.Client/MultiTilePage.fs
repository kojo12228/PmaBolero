module PmaBolero.Client.TilesTemplate

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

type Model<'T> =
    {
        Title: string
        IsLoading: bool
        Data: 'T []
        AuthorisationFailure: bool
        Error: string option
    }

type Message<'T> =
    | InitMessage
    | GetData
    | RecvData of option<'T[]>
    | Error of exn

let update getData message model =
    match message with
    | InitMessage ->
        { model with IsLoading = true }, Cmd.ofMsg GetData
    | GetData ->
        model, Cmd.ofAuthorized getData () RecvData Error
    | RecvData (Some data) ->
        { model with Data = data; IsLoading = false }, Cmd.none
    | RecvData None ->
        { model with AuthorisationFailure = true }, Cmd.none
    | Error e ->
        { model with Error = Some e.Message }, Cmd.none

type MultiTileTemplate = Template<"wwwroot/multitilepage.html">

let view (toTile: 'T -> Node) (model: Model<'T>) dispatch =
    MultiTileTemplate
        .Page()
        .Title(model.Title)
        .Progress(
            cond model.IsLoading <| function
            | false -> empty
            | true ->
                MultiTileTemplate.DisplayProgress().Elt()
        )
        .Tiles(
            forEach model.Data (fun d ->
                MultiTileTemplate
                    .Tile()
                    .TileContent(toTile d)
                    .Elt()
            )
        )
        .Elt()