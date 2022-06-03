module PmaBolero.Client.Pages.ViewItem.ViewItem

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Helpers.ErrorNotification
open PmaBolero.Client.Helpers.ProgressBar
open PmaBolero.Client.Helpers

type Model<'T> =
    { DataType: string
      UrlPrefix: string
      IsLoading: bool
      Data: 'T option
      AuthorisationFailure: bool
      Error: string option }

type Message<'T> =
    | InitMessage of int
    | GetData of int
    | RecvData of option<option<'T>>
    | Error of exn
    | ClearError

let update getData message model =
    match message with
    | InitMessage dataId -> { model with IsLoading = true }, Cmd.ofMsg (GetData dataId)
    | GetData dataId -> model, Cmd.OfAuthorized.either getData dataId RecvData Error

    // Authorised and valid ID
    | RecvData (Some (Some data)) ->
        { model with
            Data = Some data
            IsLoading = false },
        Cmd.none
    // Authorised, but invalid ID
    | RecvData (Some None) ->
        { model with
            Error = Some "No department exists with this ID."
            IsLoading = false },
        Cmd.none
    // Unauthorised
    | RecvData None ->
        { model with
            AuthorisationFailure = true
            Error = Some "You need to sign in to view this page."
            IsLoading = false },
        Cmd.none

    | Error e -> { model with Error = Some e.Message }, Cmd.none
    | ClearError -> { model with Error = None }, Cmd.none

type ViewSingleTemplate = Template<"wwwroot/singletilepage.html">

let view (toTile: 'T -> Node) (pageTitle: 'T -> string) (model: Model<'T>) dispatch =
    concat' [] [
        a [ attr.classes [ "button"
                           "is-rounded"
                           "is-dark" ]
            attr.href $"model.UrlPrefix/all" ] [
            span [ attr.``class`` "icon" ] [
                text "&#5130;"
            ]
            span [] [
                text $"Back to All {model.DataType}"
            ]
        ]

        p [ attr.``class`` "title" ] [
            match model.Data with
            | Some d -> pageTitle d
            | None -> "Loading"
            |> text
        ]

        cond model.IsLoading
        <| function
            | false -> empty
            | true -> createIndeterminateBar ()

        cond model.Data
        <| function
            | Some d ->
                div [ attr.``class`` "box" ] [
                    toTile d
                ]
            | None -> empty

        cond model.Error
        <| function
            | None -> empty
            | Some msg -> errorNotifDanger msg (fun _ -> dispatch ClearError)
    ]
