module PmaBolero.Client.ViewSingle

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
        DataType: string
        UrlPrefix: string
        Title: string
        IsLoading: bool
        Data: 'T option
        AuthorisationFailure: bool
        Error: string option
    }

type Message<'T> =
    | InitMessage of int
    | GetData of int
    | RecvData of option<option<'T>>
    | Error of exn
    | ClearError

let update getData message model =
    match message with
    | InitMessage dataId ->
        { model with IsLoading = true }, Cmd.ofMsg (GetData dataId)
    | GetData dataId ->
        model, Cmd.ofAuthorized getData dataId RecvData Error

    // Authorised and valid ID
    | RecvData (Some (Some data)) ->
        { model with Data = Some data; IsLoading = false }, Cmd.none
    // Authorised, but invalid ID
    | RecvData (Some None) ->
        {
            model with
                Error = Some "No department exists with this ID."
                IsLoading = false
        }, Cmd.none
    // Unauthorised
    | RecvData None ->
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

type ViewSingleTemplate = Template<"wwwroot/singletilepage.html">

let view (toTile: 'T -> Node) (pageTitle: 'T option -> string) (model: Model<'T>) dispatch =
    ViewSingleTemplate
        .ViewSingle()
        .UrlSection(model.UrlPrefix)
        .PageDataType(model.DataType)
        .Name(pageTitle model.Data)
        .Progress(
            cond model.IsLoading <| function
            | false -> empty
            | true -> createBar()
        )
        .Tile(
            cond model.Data <| function
            | None -> empty
            | Some d ->
                ViewSingleTemplate
                    .TileContentBox()
                    .TileContent(toTile d)
                    .Elt()
        )
        .ErrorNotification(
            cond model.Error <| function
            | None -> empty
            | Some msg -> errorNotifDanger msg (fun _ -> dispatch ClearError)
        )
        .Elt()