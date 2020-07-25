module PmaBolero.Client.Pages.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Microsoft.AspNetCore.Components

open PmaBolero.Client
open PmaBolero.Client.Pages.Auth
open PmaBolero.Client.Models.Auth
open PmaBolero.Client.Helpers.ErrorNotification
open PmaBolero.Client.Models.EmployeeData

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/login">] SignIn
    | [<EndPoint "/signup">] SignUp
    | [<EndPoint "/project/all">] ViewProjects
    | [<EndPoint "/project">] ViewProject of int
    | [<EndPoint "/project/add">] CreateProject
    | [<EndPoint "/project/{id}/edit">] EditProject of id: int
    | [<EndPoint "/employee/all">] ViewEmployees
    | [<EndPoint "/employee">] ViewEmployee of int
    | [<EndPoint "/employee/add">] CreateEmployee
    | [<EndPoint "/employee/{id}/edit">] EditEmployee of id: int
    | [<EndPoint "/department/all">] ViewDepartments
    | [<EndPoint "/department">] ViewDepartment of int

let pageTitles page =
    match page with
    | Home -> "Home"
    | SignIn -> "Sign In"
    | SignUp -> "Sign Up"

    | ViewProjects -> "View All Projects"
    | ViewProject _ -> "View Project"
    | CreateProject -> "Create New Project"
    | EditProject _ -> "Edit Project"

    | ViewEmployees -> "View All Employees"
    | ViewEmployee _ -> "View Employee"
    | CreateEmployee -> "Create New Employee"
    | EditEmployee _ -> "Edit Employee"

    | ViewDepartments -> "View All Departments"
    | ViewDepartment _ -> "View Department"

// Change to match statement
let authenticatedPages page =
    match page with
    | Home | ViewProjects | ViewEmployees | ViewDepartments
    | ViewProject _ | ViewEmployee _ | ViewDepartment _
    | CreateProject | CreateEmployee
    | EditProject _ | EditEmployee _ -> true
    | _ -> false

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
        ViewDepartmentsModel: ViewGroup.Department.Model
        ViewDepartmentModel: ViewItem.Department.Model
        ViewEmployeesModel: ViewGroup.Employee.Model
        ViewEmployeeModel: ViewItem.Employee.Model
        CreateEmployeeModel: Create.Employee.Model
        EditEmployeeModel: Edit.Employee.Model
        ViewProjectsModel: ViewGroup.Project.Model
        ViewProjectModel: ViewItem.Project.Model
        CreateProjectModel: Create.Project.Model
        EditProjectModel: Edit.Project.Model
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
        ViewDepartmentsModel = ViewGroup.Department.initModel
        ViewDepartmentModel = ViewItem.Department.initModel
        ViewEmployeesModel = ViewGroup.Employee.initModel
        ViewEmployeeModel = ViewItem.Employee.initModel
        CreateEmployeeModel = Create.Employee.initModel
        EditEmployeeModel = Edit.Employee.initModel
        ViewProjectsModel = ViewGroup.Project.initModel
        ViewProjectModel = ViewItem.Project.initModel
        CreateProjectModel = Create.Project.initModel
        EditProjectModel = Edit.Project.initModel
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

    | ViewDepartmentsMessage of ViewGroup.Department.Message
    | ViewDepartmentMessage of ViewItem.Department.Message

    | ViewEmployeesMessage of ViewGroup.Employee.Message
    | ViewEmployeeMessage of ViewItem.Employee.Message
    | CreateEmployeeMessage of Create.Employee.Message
    | EditEmployeeMessage of Edit.Employee.Message

    | ViewProjectsMessage of ViewGroup.Project.Message
    | ViewProjectMessage of ViewItem.Project.Message
    | CreateProjectMessage of Create.Project.Message
    | EditProjectMessage of Edit.Project.Message

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
                authenticatedPages page
            then Cmd.ofMsg (Redirect "/login")
            elif model.InitialSignInChecked && page = Page.Home
            then Cmd.ofMsg (Redirect "/project/all")
            else
                let pageTitleMessage =
                    pageTitles page
                    |> sprintf "PMA Bolero - %s"
                    |> SetTitle
                    |> Cmd.ofMsg
                let initMessage =
                    if model.InitialSignInChecked
                    then
                        let signInRole = model.IsSignedInAs |> Option.map snd
                        match page with
                        | ViewDepartments ->
                            Cmd.ofMsg (ViewDepartmentsMessage ViewGroup.Department.InitMessage)
                        | ViewDepartment deptId ->
                            Cmd.ofMsg (ViewDepartmentMessage (ViewItem.Department.InitMessage deptId))
                        | ViewEmployees ->
                            Cmd.ofMsg (ViewEmployeesMessage (ViewGroup.Employee.InitMessage signInRole))
                        | ViewEmployee emplId ->
                            Cmd.ofMsg (ViewEmployeeMessage (ViewItem.Employee.InitMessage (emplId, signInRole)))
                        | CreateEmployee ->
                            Cmd.ofMsg (CreateEmployeeMessage (Create.Employee.InitMessage))
                        | EditEmployee emplId ->
                            Cmd.ofMsg (EditEmployeeMessage (Edit.Employee.InitMessage emplId))
                        | ViewProjects ->
                            Cmd.ofMsg (ViewProjectsMessage (ViewGroup.Project.InitMessage signInRole))
                        | ViewProject projId ->
                            Cmd.ofMsg (ViewProjectMessage (ViewItem.Project.InitMessage (projId, signInRole)))
                        | CreateProject ->
                            Cmd.ofMsg (CreateProjectMessage Create.Project.InitMessage)
                        | EditProject projId ->
                            Cmd.ofMsg (EditProjectMessage (Edit.Project.InitMessage (projId, signInRole)))
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

    | SignInMessage (SignIn.SignInSuccess (username, role)) ->
        {
            model with
                IsSignedInAs = Some (username, role);
                SignInModel = SignIn.initModel // No longer necessary on redirect
        }, Cmd.ofMsg (Redirect "/project/all")
    | SignInMessage msg ->
        let signInModel, cmd = SignIn.update remotes.Auth msg model.SignInModel
        { model with SignInModel = signInModel}, Cmd.map SignInMessage cmd

    | SignUpMessage (SignUp.SignUpSuccess) ->
        {
            model with
                Success = Some "Successfully created account. Please sign in."
        }, Cmd.ofMsg (Redirect "/login")
    | SignUpMessage msg ->
        let signUpModel, cmd = SignUp.update remotes.Auth msg model.SignUpModel
        { model with SignUpModel = signUpModel }, Cmd.map SignUpMessage cmd

    | ViewDepartmentsMessage msg ->
        let viewDeptModel, cmd = ViewGroup.Department.update remotes.Department msg model.ViewDepartmentsModel
        { model with ViewDepartmentsModel = viewDeptModel }, Cmd.map ViewDepartmentsMessage cmd
    | ViewDepartmentMessage msg ->
        let viewDeptModel, cmd = ViewItem.Department.update remotes.Department msg model.ViewDepartmentModel
        { model with ViewDepartmentModel = viewDeptModel }, Cmd.map ViewDepartmentMessage cmd

    | ViewEmployeesMessage msg ->
        let viewEmplsModel, cmd = ViewGroup.Employee.update remotes.Employee msg model.ViewEmployeesModel
        { model with ViewEmployeesModel = viewEmplsModel }, Cmd.map ViewEmployeesMessage cmd
    | ViewEmployeeMessage msg ->
        let viewEmplModel, cmd = ViewItem.Employee.update remotes.Employee msg model.ViewEmployeeModel
        { model with ViewEmployeeModel = viewEmplModel }, Cmd.map ViewEmployeeMessage cmd
    | CreateEmployeeMessage (Create.Employee.Redirect url) ->
        model, Cmd.ofMsg (Redirect url)
    | CreateEmployeeMessage msg ->
        let createEmplModel, cmd = Create.Employee.update remotes.Employee remotes.Department msg model.CreateEmployeeModel
        { model with CreateEmployeeModel = createEmplModel }, Cmd.map CreateEmployeeMessage cmd
    | EditEmployeeMessage msg ->
        let editEmplModel, cmd = Edit.Employee.update remotes.Employee remotes.Department msg model.EditEmployeeModel
        { model with EditEmployeeModel = editEmplModel }, Cmd.map EditEmployeeMessage cmd

    | ViewProjectsMessage msg ->
        let viewProjsModel, cmd = ViewGroup.Project.update remotes.Project msg model.ViewProjectsModel
        { model with ViewProjectsModel = viewProjsModel }, Cmd.map ViewProjectsMessage cmd
    | ViewProjectMessage msg ->
        let viewProjModel, cmd = ViewItem.Project.update remotes.Project msg model.ViewProjectModel
        { model with ViewProjectModel = viewProjModel }, Cmd.map ViewProjectMessage cmd
    | CreateProjectMessage (Create.Project.Redirect url) ->
        model, Cmd.ofMsg (Redirect url)
    | CreateProjectMessage msg ->
        let createProjModel, cmd = Create.Project.update remotes.Project remotes.Employee remotes.Department msg model.CreateProjectModel
        { model with CreateProjectModel = createProjModel }, Cmd.map CreateProjectMessage cmd
    | EditProjectMessage msg ->
        let editProjModel, cmd = Edit.Project.update remotes.Project remotes.Employee remotes.Department msg model.EditProjectModel
        { model with EditProjectModel = editProjModel }, Cmd.map EditProjectMessage cmd

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.Page)

type Main = Template<"wwwroot/main.html">

let homePage model dispatch =
    Main.Home().Elt()

let navMenu model dispatch =
    Main.Navigation()
        .NavMenuActive(if model.NavMenuOpen then "is-active" else "")
        .ProjectNavbarItem(
            cond model.IsSignedInAs <| function
            | None -> empty
            | Some (_, Developer) ->
                Main.GenericProjectNavbarItem().Elt()
            | _ -> Main.ElevatedProjectNavbarItem().Elt()
        )
        .EmployeeNavbarItem(
            cond model.IsSignedInAs <| function
            | None -> empty
            | Some (_, Admin) ->
                Main.ElevatedEmployeeNavbarItem().Elt()
            | _ -> Main.GenericEmployeeNavbarItem().Elt()
        )
        .DepartmentNavbarItem(
            cond model.IsSignedInAs <| function
            | None -> empty
            | _ -> Main.GenericDepartmentNavbarItem().Elt()
        )
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

            | ViewDepartments -> ViewGroup.Department.view model.ViewDepartmentsModel (mapDispatch ViewDepartmentsMessage)
            | ViewDepartment _ -> ViewItem.Department.view model.ViewDepartmentModel (mapDispatch ViewDepartmentMessage)

            | ViewEmployees -> ViewGroup.Employee.view model.ViewEmployeesModel (mapDispatch ViewEmployeesMessage)
            | ViewEmployee _ -> ViewItem.Employee.view model.ViewEmployeeModel (mapDispatch ViewEmployeeMessage)
            | CreateEmployee -> Create.Employee.view model.CreateEmployeeModel (mapDispatch CreateEmployeeMessage)
            | EditEmployee _ -> Edit.Employee.view model.EditEmployeeModel (mapDispatch EditEmployeeMessage)

            | ViewProjects -> ViewGroup.Project.view model.ViewProjectsModel (mapDispatch ViewProjectsMessage)
            | ViewProject _ -> ViewItem.Project.view model.ViewProjectModel (mapDispatch ViewProjectMessage)
            | CreateProject -> Create.Project.view model.CreateProjectModel (mapDispatch CreateProjectMessage)
            | EditProject _ -> Edit.Project.view model.EditProjectModel (mapDispatch EditProjectMessage)
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
