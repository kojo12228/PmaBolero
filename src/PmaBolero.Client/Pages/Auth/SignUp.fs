module PmaBolero.Client.Pages.Auth.SignUp

open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client

open PmaBolero.Client.Models.Auth
open PmaBolero.Client.Helpers.ErrorNotification
open PmaBolero.Client.Helpers
open PmaBolero.Client.Helpers.Forms

type Model =
    { Username: string
      Password: string
      PasswordRepeat: string
      Error: string option }

let initModel =
    { Username = ""
      Password = ""
      PasswordRepeat = ""
      Error = None }

type Message =
    | SetUsername of string
    | SetPassword of string
    | SetPasswordRepeat of string
    | SendSignUp
    | RecvSignUp of bool
    | Error of exn
    | ClearError
    /// Handled within Main and causes no change in this model
    | SignUpSuccess

let update remote message model =
    match message with
    | SetUsername un -> { model with Username = un }, Cmd.none
    | SetPassword pw -> { model with Password = pw }, Cmd.none
    | SetPasswordRepeat pwr -> { model with PasswordRepeat = pwr }, Cmd.none

    | SendSignUp -> model, Cmd.OfAsync.either remote.addUser (model.Username, model.Password) RecvSignUp Error
    | RecvSignUp false -> { model with Error = Some "Username already exists." }, Cmd.none
    | RecvSignUp true -> { model with Error = None }, Cmd.ofMsg SignUpSuccess

    | Error RemoteUnauthorizedException -> { model with Error = Some "You have been logged out." }, Cmd.none
    | Error exn -> { model with Error = Some exn.Message }, Cmd.none
    | ClearError -> { model with Error = None }, Cmd.none
    | _ -> model, Cmd.none

type SignUpPage = Template<"wwwroot/signup.html">

let view model dispatch =
    concat' [] [
        h1 [ attr.``class`` "title" ] [
            text "Sign up"
        ]

        form [ on.submit (fun _ -> dispatch SendSignUp) ] [
            inputWithLabel "Username" "input" model.Username (SetUsername >> dispatch)
            inputWithLabel "Password (at least 8 characters)" "password" model.Password (SetPassword >> dispatch)
            inputWithLabel "Re-enter password" "password" model.PasswordRepeat (SetPasswordRepeat >> dispatch)

            div [ attr.``class`` "field" ] [
                div [ attr.``class`` "control" ] [
                    input [ attr.``class`` [ "button"
                                             "is-primary" ]
                            attr.``type`` "submit"
                            attr.value "Sign up"

                            (model.Password.Length > 8
                             && model.Password <> model.PasswordRepeat)
                            |> attr.disabled ]
                ]
            ]

            cond model.Error
            <| function
                | None -> empty
                | Some msg -> errorNotifWarning msg (fun m -> dispatch ClearError)
        ]
    ]
