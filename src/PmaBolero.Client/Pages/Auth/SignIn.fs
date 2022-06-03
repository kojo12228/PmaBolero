module PmaBolero.Client.Pages.Auth.SignIn

open Elmish
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client

open PmaBolero.Shared.Models

open PmaBolero.Client.Models.Auth
open PmaBolero.Client.Helpers
open PmaBolero.Client.Helpers.ErrorNotification
open PmaBolero.Client.Helpers.Forms

type Model =
    { Username: string
      Password: string
      SignInFailed: bool
      Error: string option }

let initModel =
    { Username = ""
      Password = ""
      SignInFailed = false
      Error = None }

type Message =
    | SetUsername of string
    | SetPassword of string
    | SendSignIn
    | RecvSignIn of option<string * Role>
    | Error of exn
    | ClearError
    /// Handled within Main and causes no change in model
    | SignInSuccess of string * Role

let update remote message model =
    match message with
    | SetUsername un -> { model with Username = un }, Cmd.none
    | SetPassword pw -> { model with Password = pw }, Cmd.none

    | SendSignIn -> model, Cmd.OfAsync.either remote.signIn (model.Username, model.Password) RecvSignIn Error
    | RecvSignIn None ->
        { model with
            Error =
                Some
                    "Sign in failed. Try creating a new account at \"Sign Up\" (new accounts are removed on redeployment)." },
        Cmd.none
    | RecvSignIn (Some username) -> model, Cmd.ofMsg (SignInSuccess username)

    | Error RemoteUnauthorizedException -> { model with Error = Some "You have been logged out." }, Cmd.none
    | Error exn -> { model with Error = Some exn.Message }, Cmd.none
    | ClearError -> { model with Error = None }, Cmd.none
    | _ -> model, Cmd.none

let view model dispatch =
    concat' [] [
        h1 [ attr.``class`` "title" ] [
            text "Sign in"
        ]
        form [ on.submit (fun _ -> dispatch SendSignIn) ] [
            inputWithLabel "Username" "text" model.Username (fun un -> dispatch (SetUsername un))
            inputWithLabel "Password" "password" model.Password (fun pw -> dispatch (SetPassword pw))

            div [ attr.``class`` "field" ] [
                div [ attr.``class`` "control" ] [
                    input [ attr.classes [ "button"; "is-primary" ]
                            attr.``type`` "submit"
                            attr.value "Sign in" ]
                ]
            ]
        ]

        cond model.Error
        <| function
            | None -> empty
            | Some msg -> errorNotifWarning msg (fun _ -> dispatch ClearError)
    ]
