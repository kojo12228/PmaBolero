module PmaBolero.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Microsoft.AspNetCore.Components

open PmaBolero.Client.SignIn
open PmaBolero.Client.SignUp
open PmaBolero.Client.Models.Auth
open PmaBolero.Client.Helpers.ErrorNotification
open PmaBolero.Client.Models.EmployeeData

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/login">] SignIn
    | [<EndPoint "/signup">] SignUp
    | [<EndPoint "/project/all">] ViewProjects
    | [<EndPoint "/employee/all">] ViewEmployees
    | [<EndPoint "/department/all">] ViewDepartment

let pageTitles page =
    match page with
    | Home -> "Home"
    | SignIn -> "Sign In"
    | SignUp -> "Sign Up"
    | ViewProjects -> "View All Projects"
    | ViewEmployees -> "View All Employees"
    | ViewDepartment -> "View All Departments"

let authenticatedPages =
    [ Home; ViewProjects; ViewEmployees; ViewDepartment ]
    |> Set.ofList

type Remotes =
    {
        Auth: AuthService
        Department: DepartmentService
        Employee: EmployeeService
        Project: ProjectService
    }

/// The Elmish application's model.
type Model =
    {
        Page: Page
        NavMenuOpen: bool
        InitialSignInChecked: bool
        IsSignedInAs: (string * Role) option
        Error: string option
        Success: string option
        SignInModel: SignIn.Model
        SignUpModel: SignUp.Model
        ViewDepartmentsModel: ViewDepartments.Model
        ViewEmployeesModel: ViewEmployees.Model
        ViewProjectsModel: ViewProjects.Model
    }

let initModel =
    {
        Page = Home
        NavMenuOpen = false
        InitialSignInChecked = false
        IsSignedInAs = None
        Error = None
        Success = None
        SignInModel = SignIn.initModel
        SignUpModel = SignUp.initModel
        ViewDepartmentsModel = ViewDepartments.initModel
        ViewEmployeesModel = ViewEmployees.initModel
        ViewProjectsModel = ViewProjects.initModel
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | SetTitle of string
    | SetTitleSuccess of string
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
    | ViewDepartmentsMessage of ViewDepartments.Message
    | ViewEmployeesMessage of ViewEmployees.Message
    | ViewProjectsMessage of ViewProjects.Message

let update remotes (nm: NavigationManager) js message model =
#if DEBUG
    printfn "%A" message
#endif

    match message with
    | SetPage page ->
        // TODO: Refactor lengthy logic out of match statement
        let cmd =
            if
                model.InitialSignInChecked &&
                Option.isNone model.IsSignedInAs &&
                authenticatedPages |> Set.contains page
            then Cmd.ofMsg (Redirect "/login")
            else
                let pageTitleMessage =
                    pageTitles page
                    |> sprintf "PMA Bolero - %s"
                    |> SetTitle
                    |> Cmd.ofMsg
                let initMessage =
                    if model.InitialSignInChecked
                    then
                        match page with
                        | ViewDepartment ->
                            Cmd.ofMsg (ViewDepartmentsMessage ViewDepartments.InitMessage)
                        | ViewEmployees ->
                            Cmd.ofMsg (ViewEmployeesMessage MultiTilePageTemplate.InitMessage)
                        | ViewProjects ->
                            Cmd.ofMsg (ViewProjectsMessage ViewProjects.InitMessage)
                        | _ -> Cmd.none
                    else Cmd.none

                Cmd.batch [ pageTitleMessage; initMessage ]
        {
            model with
                Page = page
                NavMenuOpen = false
                SignInModel = SignIn.initModel
                SignUpModel = SignUp.initModel }, cmd
    | SetTitle title ->
        model, Cmd.ofJS js "setTitle" [| title |] SetTitleSuccess Error
    | SetTitleSuccess ->
        model, Cmd.none

    | ToggleBurgerMenu ->
        { model with NavMenuOpen = not model.NavMenuOpen }, Cmd.none

    | SendSignOut ->
        model, Cmd.ofAsync remotes.Auth.signOut () (fun () -> RecvSignOut) Error
    | RecvSignOut ->
        { model with IsSignedInAs = None}, Cmd.ofMsg (Redirect "/login")
    | GetSignedInAs ->
        model, Cmd.ofAuthorized remotes.Auth.getUser () RecvSignedInAs Error
    | RecvSignedInAs user ->
        { model with IsSignedInAs = user; InitialSignInChecked = true }, Cmd.ofMsg (SetPage model.Page)

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
        }, Cmd.ofMsg (Redirect "/project/all")
    | SignInMessage msg ->
        let signInModel, cmd = SignIn.update remotes.Auth msg model.SignInModel
        { model with SignInModel = signInModel}, Cmd.map SignInMessage cmd

    | SignUpMessage (SignUpSuccess) ->
        {
            model with
                Success = Some "Successfully created account. Please sign in."
        }, Cmd.ofMsg (Redirect "/login")
    | SignUpMessage msg ->
        let signUpModel, cmd = SignUp.update remotes.Auth msg model.SignUpModel
        { model with SignUpModel = signUpModel }, Cmd.map SignUpMessage cmd

    | ViewDepartmentsMessage msg ->
        let viewDeptModel, cmd = ViewDepartments.update remotes.Department msg model.ViewDepartmentsModel
        { model with ViewDepartmentsModel = viewDeptModel }, Cmd.map ViewDepartmentsMessage cmd

    | ViewEmployeesMessage msg ->
        let viewEmplsModel, cmd = ViewEmployees.update remotes.Employee msg model.ViewEmployeesModel
        { model with ViewEmployeesModel = viewEmplsModel }, Cmd.map ViewEmployeesMessage cmd

    | ViewProjectsMessage msg ->
        let viewProjsModel, cmd = ViewProjects.update remotes.Project msg model.ViewProjectsModel
        { model with ViewProjectsModel = viewProjsModel }, Cmd.map ViewProjectsMessage cmd

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
            | ViewDepartment -> ViewDepartments.view model.ViewDepartmentsModel (mapDispatch ViewDepartmentsMessage)
            | ViewEmployees -> ViewEmployees.view model.ViewEmployeesModel (mapDispatch ViewEmployeesMessage)
            | ViewProjects -> ViewProjects.view model.ViewProjectsModel (mapDispatch ViewProjectsMessage)
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
        let departmentService = this.Remote<Models.EmployeeData.DepartmentService>()
        let employeeService = this.Remote<Models.EmployeeData.EmployeeService>()
        let projectService = this.Remote<Models.EmployeeData.ProjectService>()

        let remotes =
            {
                Auth = signInService
                Department = departmentService
                Employee = employeeService
                Project = projectService
            }

        let update = update remotes this.NavigationManager this.JSRuntime
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg GetSignedInAs) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
