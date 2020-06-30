module PmaBolero.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Json
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Microsoft.JSInterop

open PmaBolero.Client.SignIn
open PmaBolero.Client.Models.Auth

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/login">] SignIn

/// The Elmish application's model.
type Model =
    {
        Page: Page
        NavMenuOpen: bool
        IsSignedInAs: string option
        IsSignedInRole: Role option
        Error: string option
        SignInModel: SignIn.Model
    }

let initModel =
    {
        Page = Home
        NavMenuOpen = false
        IsSignedInAs = None
        IsSignedInRole = None
        Error = None
        SignInModel = SignIn.initModel
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | ToggleBurgerMenu
    | SendSignOut
    | RecvSignOut
    | Error of exn
    | ClearError
    | JSRedirect of string
    | JSRedirectSuccess of obj
    | SignInMessage of SignIn.Message

let update remote js message model =
    match message with
    | SetPage page ->
        { model with Page = page; NavMenuOpen = false; SignInModel = SignIn.initModel }, Cmd.none
    | ToggleBurgerMenu ->
        { model with NavMenuOpen = not model.NavMenuOpen }, Cmd.none

    | SendSignOut ->
        model, Cmd.ofAsync remote.signOut () (fun () -> RecvSignOut) Error
    | RecvSignOut ->
        { model with IsSignedInAs = None; IsSignedInRole = None }, Cmd.none

    | JSRedirect url ->
        let cmd = Cmd.ofJS js "location.replace" [| url |] JSRedirectSuccess Error
        model, cmd
    | JSRedirectSuccess _ ->
        model, Cmd.none

    | Error RemoteUnauthorizedException ->
        {
            model
                with
                    Error = Some "You have been logged out."
                    IsSignedInAs = None
                    IsSignedInRole = None
        }, Cmd.none
    | Error exn ->
        { model with Error = Some exn.Message }, Cmd.none
    | ClearError -> 
        { model with Error = None }, Cmd.none

    | SignInMessage (SignInSuccess username) ->
        { model with IsSignedInAs = Some username; SignInModel = SignIn.initModel }, Cmd.none
    | SignInMessage msg ->
        let signInModel, cmd = SignIn.update remote msg model.SignInModel
        { model with SignInModel = signInModel}, Cmd.map SignInMessage cmd

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.Page)

type Main = Template<"wwwroot/main.html">

let homePage model dispatch =
    Main.Home().Elt()

let navMenu model dispatch =
    Main.Navigation()
        .NavMenuActive(if model.NavMenuOpen then "is-active" else "")
        .ToggleBurger(fun _ -> dispatch ToggleBurgerMenu)
        .SignInSection(
            cond model.IsSignedInAs <| function
            | Some _ ->
                Main.SignOutButton()
                    .SignOutClick(fun _ -> dispatch SendSignOut)
                    .Elt()
            | None ->
                Main.SignInButtons()
                    .JSTest(fun _ -> dispatch (JSRedirect "/login"))
                    .Elt()
        )
        .Elt()

let view model dispatch =
    let mapDispatch msgWrapper = msgWrapper >> dispatch

    Main()
        .NavMenu(
            navMenu model dispatch
        )
        .Body(
            cond model.Page <| function
            | Home -> homePage model dispatch
            | SignIn ->
                SignIn.view model.SignInModel (mapDispatch SignInMessage) 
        )
        .Error(
            cond model.Error <| function
            | Some msg ->
                Main.ErrorNotification()
                    .Text(msg)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
            | None -> empty
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let signInService = this.Remote<Models.Auth.AuthService>()
        let update = update signInService this.JSRuntime
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
