module PmaBolero.Client.Pages.Auth.SignUp

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
        PasswordRepeat: string
        Error: string option
    }

let initModel =
    {
        Username = ""
        Password = ""
        PasswordRepeat = ""
        Error = None
    }

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
    | SetUsername un ->
        { model with Username = un }, Cmd.none
    | SetPassword pw ->
        { model with Password = pw }, Cmd.none
    | SetPasswordRepeat pwr ->
        { model with PasswordRepeat = pwr }, Cmd.none

    | SendSignUp ->
        model, Cmd.OfAsync.either remote.addUser (model.Username, model.Password) RecvSignUp Error
    | RecvSignUp false ->
        { model with Error = Some "Username already exists." }, Cmd.none
    | RecvSignUp true ->
        { model with Error = None }, Cmd.ofMsg SignUpSuccess

    | Error RemoteUnauthorizedException ->
        { model with Error = Some "You have been logged out." }, Cmd.none
    | Error exn ->
        { model with Error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with Error = None }, Cmd.none
    | _ ->
        model, Cmd.none

type SignUpPage = Template<"wwwroot/signup.html">

let view model dispatch =
    // fsharplint:disable CanBeReplacedWithComposition

    SignUpPage.SignUp()
        .Username(model.Username, fun un -> dispatch (SetUsername un))
        .Password(model.Password, fun pw -> dispatch (SetPassword pw))
        .ConfirmPassword(model.PasswordRepeat, fun pw -> dispatch (SetPasswordRepeat pw))
        .SignUp(fun _ -> dispatch SendSignUp)
        .SubmitDisabled(
            model.Password.Length > 8 &&
            model.Password <> model.PasswordRepeat
        )
        .ErrorNotification(
            cond model.Error <| function
            | None -> empty
            | Some msg -> errorNotifWarning msg (fun m -> dispatch ClearError)
        )
        .Elt()