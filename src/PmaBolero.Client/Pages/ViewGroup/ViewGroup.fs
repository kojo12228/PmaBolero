module PmaBolero.Client.Pages.ViewGroup.ViewGroup

open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting.Client

open PmaBolero.Client.Helpers.ErrorNotification
open PmaBolero.Client.Helpers.ProgressBar
open PmaBolero.Client.Helpers

type Model<'T> =
    { Title: string
      IsLoading: bool
      Data: 'T []
      AuthorisationFailure: bool
      Error: string option }

type Message<'T> =
    | InitMessage
    | GetData
    | RecvData of option<'T []>
    | Error of exn
    | ClearError

let update getData message model =
    match message with
    | InitMessage -> { model with IsLoading = true }, Cmd.ofMsg GetData
    | GetData -> model, Cmd.OfAuthorized.either getData () RecvData Error
    | RecvData (Some data) ->
        { model with
            Data = data
            IsLoading = false },
        Cmd.none
    | RecvData None ->
        // Should not be a possible state of the application, since the user
        // should not be able to get to this page without signing in
        { model with
            AuthorisationFailure = true
            Error = Some "You need to sign in to view this page."
            IsLoading = false },
        Cmd.none
    | Error e -> { model with Error = Some e.Message }, Cmd.none
    | ClearError -> { model with Error = None }, Cmd.none

let view (toTile: 'T -> Node) (model: Model<'T>) dispatch =
    concat' [] [
        h1 [ attr.``class`` "title" ] [
            text model.Title
        ]

        cond model.IsLoading
        <| function
            | false -> empty
            | true -> createIndeterminateBar ()

        div [ attr.classes [ "columns"
                             "is-multiline" ] ] [
            forEach model.Data
            <| fun d ->
                div [ attr.classes [ "column"; "is-6" ] ] [
                    div [ attr.``class`` "box" ] [
                        toTile d
                    ]
                ]
        ]

        cond model.Error
        <| function
            | None -> empty
            | Some msg -> errorNotifDanger msg (fun _ -> dispatch ClearError)
    ]
