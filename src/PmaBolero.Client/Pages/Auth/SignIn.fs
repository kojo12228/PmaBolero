module PmaBolero.Client.Pages.Auth.SignIn

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Models.Auth
open PmaBolero.Client.Helpers.ErrorNotification

type Model =
    {
        Username: string
        Password: string
        SignInFailed: bool
        Error: string option
    }

let initModel =
    {
        Username = ""
        Password = ""
        SignInFailed = false
        Error = None
    }

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
    | SetUsername un ->
        { model with Username = un }, Cmd.none
    | SetPassword pw ->
        { model with Password = pw }, Cmd.none

    | SendSignIn ->
        model, Cmd.ofAsync remote.signIn (model.Username, model.Password) RecvSignIn Error
    | RecvSignIn None ->
        { model with Error = Some "Sign in failed." }, Cmd.none
    | RecvSignIn (Some username) ->
        model, Cmd.ofMsg (SignInSuccess username)

    | Error RemoteUnauthorizedException ->
        { model with Error = Some "You have been logged out." }, Cmd.none
    | Error exn ->
        { model with Error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with Error = None }, Cmd.none
    | _ ->
        model, Cmd.none

type SignInPage = Template<"wwwroot/signin.html">

let view model dispatch =
    SignInPage.SignIn()
        .Username(model.Username, fun un -> dispatch (SetUsername un))
        .Password(model.Password, fun pw -> dispatch (SetPassword pw))
        .SignIn(fun _ -> dispatch SendSignIn)
        .ErrorNotification(
            cond model.Error <| function
            | None -> empty
            | Some msg -> errorNotifWarning msg (fun _ -> dispatch ClearError)
        )
        .Elt()