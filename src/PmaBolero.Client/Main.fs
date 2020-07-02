module PmaBolero.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Json
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.SignIn
open PmaBolero.Client.SignUp
open PmaBolero.Client.Models.Auth
open PmaBolero.Client.Helpers.ErrorNotification
open Microsoft.AspNetCore.Components

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/login">] SignIn
    | [<EndPoint "/signup">] SignUp

let authenticatedPages =
    [ Home ]
    |> Set.ofList

/// The Elmish application's model.
type Model =
    {
        Page: Page
        NavMenuOpen: bool
        IsSignedInAs: (string * Role) option
        Error: string option
        Success: string option
        SignInModel: SignIn.Model
        SignUpModel: SignUp.Model
    }

let initModel =
    {
        Page = Home
        NavMenuOpen = false
        IsSignedInAs = None
        Error = None
        Success = None
        SignInModel = SignIn.initModel
        SignUpModel = SignUp.initModel
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | ToggleBurgerMenu
    | SendSignOut
    | RecvSignOut
    | GetSignedInAs
    | RecvSignedInAs of (string * Role) option
    | Error of exn
    | ClearError
    | ClearSuccess
    | Redirect of string
    | RedirectSuccess of unit
    | SignInMessage of SignIn.Message
    | SignUpMessage of SignUp.Message

let update remote (nm: NavigationManager) message model =
    match message with
    | SetPage page ->
        let cmd =
            if
                Option.isNone model.IsSignedInAs &&
                authenticatedPages |> Set.contains page
            then Cmd.ofMsg (Redirect "/login")
            else Cmd.none
        {
            model with
                Page = page
                NavMenuOpen = false
                SignInModel = SignIn.initModel
                SignUpModel = SignUp.initModel }, cmd
    | ToggleBurgerMenu ->
        { model with NavMenuOpen = not model.NavMenuOpen }, Cmd.none

    | SendSignOut ->
        model, Cmd.ofAsync remote.signOut () (fun () -> RecvSignOut) Error
    | RecvSignOut ->
        { model with IsSignedInAs = None}, Cmd.ofMsg (Redirect "/login")
    | GetSignedInAs ->
        model, Cmd.ofAuthorized remote.getUser () RecvSignedInAs Error
    | RecvSignedInAs user ->
        { model with IsSignedInAs = user }, Cmd.none

    | Redirect url ->
        let cmd = Cmd.performFunc (fun url -> nm.NavigateTo(url, false)) url RedirectSuccess
        model, cmd
    | RedirectSuccess _ ->
        model, Cmd.none

    | Error RemoteUnauthorizedException ->
        {
            model
                with
                    Error = Some "You have been logged out."
                    IsSignedInAs = None
        }, Cmd.none
    | Error exn ->
        { model with Error = Some exn.Message }, Cmd.none
    | ClearError -> 
        { model with Error = None }, Cmd.none
    | ClearSuccess ->
        { model with Success = None}, Cmd.none

    | SignInMessage (SignInSuccess (username, role)) ->
        {
            model with
                IsSignedInAs = Some (username, role);
                SignInModel = SignIn.initModel // No longer necessary on redirect
        }, Cmd.none
    | SignInMessage msg ->
        let signInModel, cmd = SignIn.update remote msg model.SignInModel
        { model with SignInModel = signInModel}, Cmd.map SignInMessage cmd

    | SignUpMessage (SignUpSuccess) ->
        {
            model with
                Success = Some "Successfully created account. Please sign in."
        }, Cmd.ofMsg (Redirect "/login")
    | SignUpMessage msg ->
        let signUpModel, cmd = SignUp.update remote msg model.SignUpModel
        { model with SignUpModel = signUpModel }, Cmd.map SignUpMessage cmd

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
                Main.SignInButtons().Elt()
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
            | SignIn -> SignIn.view model.SignInModel (mapDispatch SignInMessage)
            | SignUp -> SignUp.view model.SignUpModel (mapDispatch SignUpMessage) 
        )
        .MainNotification(
            cond model.Error <| function
            | None ->
                // TODO: Really should replace error message logic
                // with local storage logic
                cond model.Success <| function
                | None -> empty
                | Some msg -> errorNotifSuccess msg (fun _ -> dispatch ClearSuccess)
            | Some msg -> errorNotifWarning msg (fun _ -> dispatch ClearError)
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let signInService = this.Remote<Models.Auth.AuthService>()
        let update = update signInService this.NavigationManager
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg GetSignedInAs) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
