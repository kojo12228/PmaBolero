module PmaBolero.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Json
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/login">] SignIn

/// The Elmish application's model.
type Model =
    {
        Page: Page
        NavMenuOpen: bool
        SignInModel: SignIn.Model
    }

let initModel =
    {
        Page = Home
        NavMenuOpen = false
        SignInModel = SignIn.initModel
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | ToggleBurgerMenu
    | SignInMessage of SignIn.Message

let update remote message model =
    match message with
    | SetPage page ->
        { model with Page = page }, Cmd.none

    | ToggleBurgerMenu ->
        { model with NavMenuOpen = not model.NavMenuOpen }, Cmd.none

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
        .NavMenuActive(
            if model.NavMenuOpen
            then "navbar-menu is-active"
            else "navbar-menu"
        )
        .BurgerActive(
            if model.NavMenuOpen
            then "navbar-burger burger is-active"
            else "navbar-burger burger"
        )
        .ToggleBurger(fun _ -> dispatch ToggleBurgerMenu)
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
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let signInService = this.Remote<Models.Auth.AuthService>()
        let update = update signInService
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
