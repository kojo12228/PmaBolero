module PmaBolero.Client.Pages.ViewGroup.ViewGroup

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Helpers.ErrorNotification
open PmaBolero.Client.Helpers.ProgressBar

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
    | ClearError

let update getData message model =
    match message with
    | InitMessage ->
        { model with IsLoading = true }, Cmd.ofMsg GetData
    | GetData ->
        model, Cmd.ofAuthorized getData () RecvData Error
    | RecvData (Some data) ->
        { model with Data = data; IsLoading = false }, Cmd.none
    | RecvData None ->
        // Should not be a possible state of the application, since the user
        // should not be able to get to this page without signing in
        {
            model with
                AuthorisationFailure = true
                Error = Some "You need to sign in to view this page."
                IsLoading = false
        }, Cmd.none
    | Error e ->
        { model with Error = Some e.Message }, Cmd.none
    | ClearError ->
        { model with Error = None }, Cmd.none

type MultiTileTemplate = Template<"wwwroot/multitilepage.html">

let view (toTile: 'T -> Node) (model: Model<'T>) dispatch =
    MultiTileTemplate
        .Page()
        .Title(model.Title)
        .Progress(
            cond model.IsLoading <| function
            | false -> empty
            | true -> createIndeterminateBar()
        )
        .Tiles(
            forEach model.Data (fun d ->
                MultiTileTemplate
                    .Tile()
                    .TileContent(toTile d)
                    .Elt()
            )
        )
        .ErrorNotification(
            cond model.Error <| function
            | None -> empty
            | Some msg -> errorNotifDanger msg (fun _ -> dispatch ClearError)
        )
        .Elt()