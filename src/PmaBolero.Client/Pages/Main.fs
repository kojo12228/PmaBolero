module PmaBolero.Client.Pages.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Microsoft.AspNetCore.Components

open PmaBolero.Shared.Models

open PmaBolero.Client
open PmaBolero.Client.Pages.Auth
open PmaBolero.Client.Models.Auth
open PmaBolero.Client.Helpers.ErrorNotification
open PmaBolero.Client.Models.EmployeeData
open PmaBolero.Client.Helpers

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    // Use PageModel<'T> so no page model persists
    | [<EndPoint "/login">] SignIn of PageModel<SignIn.Model>
    | [<EndPoint "/signup">] SignUp of PageModel<SignUp.Model>
    // Project pages
    | [<EndPoint "/project/all">] ViewProjects of PageModel<ViewGroup.Project.Model>
    | [<EndPoint "/project">] ViewProject of int * PageModel<ViewItem.Project.Model>
    | [<EndPoint "/project/add">] CreateProject of PageModel<Create.Project.Model>
    | [<EndPoint "/project/{id}/edit">] EditProject of id: int * PageModel<Edit.Project.Model>
    // Employee pages
    | [<EndPoint "/employee/all">] ViewEmployees of PageModel<ViewGroup.Employee.Model>
    | [<EndPoint "/employee">] ViewEmployee of int * PageModel<ViewItem.Employee.Model>
    | [<EndPoint "/employee/add">] CreateEmployee of PageModel<Create.Employee.Model>
    | [<EndPoint "/employee/{id}/edit">] EditEmployee of id: int * PageModel<Edit.Employee.Model>
    // Department pages
    | [<EndPoint "/department/all">] ViewDepartments of PageModel<ViewGroup.Department.Model>
    | [<EndPoint "/department">] ViewDepartment of int * PageModel<ViewItem.Department.Model>

let pageTitles page =
    match page with
    | Home -> "Home"
    | SignIn _ -> "Sign In"
    | SignUp _ -> "Sign Up"

    | ViewProjects _ -> "View All Projects"
    | ViewProject _ -> "View Project"
    | CreateProject _ -> "Create New Project"
    | EditProject _ -> "Edit Project"

    | ViewEmployees _ -> "View All Employees"
    | ViewEmployee _ -> "View Employee"
    | CreateEmployee _ -> "Create New Employee"
    | EditEmployee _ -> "Edit Employee"

    | ViewDepartments _ -> "View All Departments"
    | ViewDepartment _ -> "View Department"

/// Check whether a given page should only be reached if the user is
/// signed in.
let authenticatedPages page =
    match page with
    | Home
    | ViewProjects _
    | ViewEmployees _
    | ViewDepartments _
    | ViewProject _
    | ViewEmployee _
    | ViewDepartment _
    | CreateProject _
    | CreateEmployee _
    | EditProject _
    | EditEmployee _ -> true
    | _ -> false

type Remotes =
    { Auth: AuthService
      Department: DepartmentService
      Employee: EmployeeService
      Project: ProjectService }

/// The Elmish application's model.
type Model =
    { Page: Page
      NavMenuOpen: bool
      InitialSignInChecked: bool
      IsSignedInAs: (string * Role) option
      Error: string option
      Success: string option }

let initModel =
    { Page = Home
      NavMenuOpen = false
      InitialSignInChecked = false
      IsSignedInAs = None
      Error = None
      Success = None }

let defaultModel =
    function
    | Home -> Router.definePageModel { Model = () } ()

    | SignIn model -> Router.definePageModel model SignIn.initModel
    | SignUp model -> Router.definePageModel model SignUp.initModel

    | ViewProjects model -> Router.definePageModel model ViewGroup.Project.initModel
    | ViewProject (_, model) -> Router.definePageModel model ViewItem.Project.initModel
    | CreateProject model -> Router.definePageModel model Create.Project.initModel
    | EditProject (_, model) -> Router.definePageModel model Edit.Project.initModel

    | ViewEmployees model -> Router.definePageModel model ViewGroup.Employee.initModel
    | ViewEmployee (_, model) -> Router.definePageModel model ViewItem.Employee.initModel
    | CreateEmployee model -> Router.definePageModel model Create.Employee.initModel
    | EditEmployee (_, model) -> Router.definePageModel model Edit.Employee.initModel

    | ViewDepartments model -> Router.definePageModel model ViewGroup.Department.initModel
    | ViewDepartment (_, model) -> Router.definePageModel model ViewItem.Department.initModel


/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | SetTitle of string
    | SetTitleSuccess of string
    | ToggleBurgerMenu
    | SendSignOut
    | RecvSignOut
    | GetSignedInAs // Called when website is initialised
    | RecvSignedInAs of (string * Role) option
    | Error of exn
    | ClearError
    | ClearSuccess
    | Redirect of string
    | RedirectSuccess of unit

    // Wrapping of messages for each individual page
    | SignInMessage of SignIn.Message
    | SignUpMessage of SignUp.Message

    | ViewProjectsMessage of ViewGroup.Project.Message
    | ViewProjectMessage of ViewItem.Project.Message
    | CreateProjectMessage of Create.Project.Message
    | EditProjectMessage of Edit.Project.Message

    | ViewEmployeesMessage of ViewGroup.Employee.Message
    | ViewEmployeeMessage of ViewItem.Employee.Message
    | CreateEmployeeMessage of Create.Employee.Message
    | EditEmployeeMessage of Edit.Employee.Message

    | ViewDepartmentsMessage of ViewGroup.Department.Message
    | ViewDepartmentMessage of ViewItem.Department.Message

let initMessages model page =
    let signInRole = model.IsSignedInAs |> Option.map snd

    match page with
    | ViewDepartments _ -> Cmd.ofMsg (ViewDepartmentsMessage ViewGroup.Department.InitMessage)
    | ViewDepartment (deptId, _) -> Cmd.ofMsg (ViewDepartmentMessage(ViewItem.Department.InitMessage deptId))
    | ViewEmployees _ -> Cmd.ofMsg (ViewEmployeesMessage(ViewGroup.Employee.InitMessage signInRole))
    | ViewEmployee (emplId, _) -> Cmd.ofMsg (ViewEmployeeMessage(ViewItem.Employee.InitMessage(emplId, signInRole)))
    | CreateEmployee _ -> Cmd.ofMsg (CreateEmployeeMessage(Create.Employee.InitMessage))
    | EditEmployee (emplId, _) -> Cmd.ofMsg (EditEmployeeMessage(Edit.Employee.InitMessage emplId))
    | ViewProjects _ -> Cmd.ofMsg (ViewProjectsMessage(ViewGroup.Project.InitMessage signInRole))
    | ViewProject (projId, _) -> Cmd.ofMsg (ViewProjectMessage(ViewItem.Project.InitMessage(projId, signInRole)))
    | CreateProject _ -> Cmd.ofMsg (CreateProjectMessage Create.Project.InitMessage)
    | EditProject (projId, _) -> Cmd.ofMsg (EditProjectMessage(Edit.Project.InitMessage(projId, signInRole)))
    | _ -> Cmd.none

let update remotes (nm: NavigationManager) js message model =
#if DEBUG
    // Print messages for logging purposes
    // Not a perfect solution as objects aren't very legible
    printfn "%A" message
#endif

    match message, model.Page with
    | SetPage page, _ ->
        let shouldRedirectToLogin =
            // Already checked cookie for signin
            model.InitialSignInChecked
            &&
            // After checking, the user is not signed in
            Option.isNone model.IsSignedInAs
            &&
            // The user is trying to visit a page that requires auth
            authenticatedPages page

        let cmd =
            if shouldRedirectToLogin then
                Cmd.ofMsg (Redirect "/login")
            elif model.InitialSignInChecked && page = Home then
                Cmd.ofMsg (Redirect "/project/all")
            else
                let pageTitleMessage =
                    pageTitles page
                    |> sprintf "PMA Bolero - %s"
                    |> SetTitle
                    |> Cmd.ofMsg

                let initMessage =
                    if model.InitialSignInChecked then
                        initMessages model page
                    else
                        Cmd.none

                Cmd.batch [ pageTitleMessage
                            initMessage ]

        { model with
            Page = page
            NavMenuOpen = false },
        cmd
    | SetTitle title, _ -> model, Cmd.OfJS.either js "setTitle" [| title |] SetTitleSuccess Error
    | SetTitleSuccess _, _ -> model, Cmd.none

    | ToggleBurgerMenu, _ -> { model with NavMenuOpen = not model.NavMenuOpen }, Cmd.none

    | SendSignOut, _ -> model, Cmd.OfAsync.either remotes.Auth.signOut () (fun () -> RecvSignOut) Error
    | RecvSignOut, _ -> { model with IsSignedInAs = None }, Cmd.ofMsg (Redirect "/login")
    | GetSignedInAs, _ -> model, Cmd.OfAuthorized.either remotes.Auth.getUser () RecvSignedInAs Error
    | RecvSignedInAs user, _ ->
        { model with
            IsSignedInAs = user
            InitialSignInChecked = true },
        Cmd.ofMsg (SetPage model.Page)

    | Redirect url, _ ->
        let cmd =
            Cmd.OfFunc.perform (fun url -> nm.NavigateTo(url, false)) url RedirectSuccess

        model, cmd
    | RedirectSuccess _, _ -> model, Cmd.none

    | Error RemoteUnauthorizedException, _ ->
        { model with
            Error = Some "You have been logged out."
            IsSignedInAs = None },
        Cmd.none
    | Error exn, _ ->
#if DEBUG
        { model with Error = Some $"{exn}" }, Cmd.none
#else
        model, Cmd.none
#endif
    | ClearError, _ -> { model with Error = None }, Cmd.none
    | ClearSuccess, _ -> { model with Success = None }, Cmd.none

    | SignInMessage (SignIn.SignInSuccess (username, role)), _ ->
        { model with IsSignedInAs = Some(username, role) }, Cmd.ofMsg (Redirect "/project/all")
    | SignInMessage msg, SignIn pgModel ->
        let signInModel, cmd = SignIn.update remotes.Auth msg pgModel.Model
        { model with Page = SignIn { Model = signInModel } }, Cmd.map SignInMessage cmd

    | SignUpMessage (SignUp.SignUpSuccess), _ ->
        { model with Success = Some "Successfully created account. Please sign in." }, Cmd.ofMsg (Redirect "/login")
    | SignUpMessage msg, SignUp pgModel ->
        let signUpModel, cmd = SignUp.update remotes.Auth msg pgModel.Model
        { model with Page = SignUp { Model = signUpModel } }, Cmd.map SignUpMessage cmd

    | ViewDepartmentsMessage msg, ViewDepartments pgModel ->
        let viewDeptModel, cmd =
            ViewGroup.Department.update remotes.Department msg pgModel.Model

        { model with Page = ViewDepartments { Model = viewDeptModel } }, Cmd.map ViewDepartmentsMessage cmd
    | ViewDepartmentMessage msg, ViewDepartment (deptId, pgModel) ->
        let viewDeptModel, cmd =
            ViewItem.Department.update remotes.Department msg pgModel.Model

        { model with Page = ViewDepartment(deptId, { Model = viewDeptModel }) }, Cmd.map ViewDepartmentMessage cmd

    | ViewEmployeesMessage msg, ViewEmployees pgModel ->
        let viewEmplsModel, cmd =
            ViewGroup.Employee.update remotes.Employee msg pgModel.Model

        { model with Page = ViewEmployees { Model = viewEmplsModel } }, Cmd.map ViewEmployeesMessage cmd
    | ViewEmployeeMessage msg, ViewEmployee (emplId, pgModel) ->
        let viewEmplModel, cmd = ViewItem.Employee.update remotes.Employee msg pgModel.Model
        { model with Page = ViewEmployee(emplId, { Model = viewEmplModel }) }, Cmd.map ViewEmployeeMessage cmd
    | CreateEmployeeMessage (Create.Employee.Redirect url), _ -> model, Cmd.ofMsg (Redirect url)
    | CreateEmployeeMessage msg, CreateEmployee pgModel ->
        let createEmplModel, cmd =
            Create.Employee.update remotes.Employee remotes.Department msg pgModel.Model

        { model with Page = CreateEmployee { Model = createEmplModel } }, Cmd.map CreateEmployeeMessage cmd
    | EditEmployeeMessage msg, EditEmployee (emplId, pgModel) ->
        let editEmplModel, cmd =
            Edit.Employee.update remotes.Employee remotes.Department msg pgModel.Model

        { model with Page = EditEmployee(emplId, { Model = editEmplModel }) }, Cmd.map EditEmployeeMessage cmd

    | ViewProjectsMessage msg, ViewProjects pgModel ->
        let viewProjsModel, cmd = ViewGroup.Project.update remotes.Project msg pgModel.Model
        { model with Page = ViewProjects { Model = viewProjsModel } }, Cmd.map ViewProjectsMessage cmd
    | ViewProjectMessage msg, ViewProject (projId, pgModel) ->
        let viewProjModel, cmd = ViewItem.Project.update remotes.Project msg pgModel.Model
        { model with Page = ViewProject(projId, { Model = viewProjModel }) }, Cmd.map ViewProjectMessage cmd
    | CreateProjectMessage (Create.Project.Redirect url), _ -> model, Cmd.ofMsg (Redirect url)
    | CreateProjectMessage msg, CreateProject pgModel ->
        let createProjModel, cmd =
            Create.Project.update remotes.Project remotes.Employee remotes.Department msg pgModel.Model

        { model with Page = CreateProject { Model = createProjModel } }, Cmd.map CreateProjectMessage cmd
    | EditProjectMessage msg, EditProject (projId, pgModel) ->
        let editProjModel, cmd =
            Edit.Project.update remotes.Project remotes.Employee remotes.Department msg pgModel.Model

        { model with Page = EditProject(projId, { Model = editProjModel }) }, Cmd.map EditProjectMessage cmd
    | msg ->
#if DEBUG
        { model with
            Error =
                msg
                |> string
                |> sprintf "Unhandled message: %s"
                |> Some },
        Cmd.none
#else
        model, Cmd.none
#endif

/// Connects the routing system to the Elmish application, using default
/// models defined in `defaultModel`
let router = Router.inferWithModel SetPage (fun model -> model.Page) defaultModel

let homePage model dispatch = empty

let navMenu model dispatch =
    let navbarId = "navbarId"

    nav [ attr.classes [ "navbar"; "is-primary" ]
          "role" => "navigation"
          attr.aria "label" "menu navigation" ] [
        div [ attr.``class`` "navbar-brand" ] [
            a [ attr.``class`` "navbar-item" ] [
                h1 [ attr.classes [ "title"
                                    "has-text-white" ] ] [
                    text "OJO PMA"
                ]
            ]

            a [ [ Some "navbar-burger"
                  Some "burger"
                  if model.NavMenuOpen then
                      Some "is-active"
                  else
                      None ]
                |> List.choose id
                |> attr.classes

                "role" => "button"
                on.click (fun _ -> dispatch ToggleBurgerMenu)
                attr.aria "label" "menu"
                attr.aria "expanded" "false"
                "data-target" => navbarId ] [
                forEach [ 0..2 ]
                <| fun _ -> span [ attr.aria "hidden" "true" ] []
            ]
        ]

        div [ attr.id navbarId
              [ Some "navbar-menu"
                if model.NavMenuOpen then
                    Some "is-active"
                else
                    None ]
              |> List.choose id
              |> attr.classes ] [
            div [ attr.``class`` "navbar-start" ] [
                cond model.IsSignedInAs
                <| function
                    | None -> empty
                    | Some (_, userType) ->
                        concat' [] [
                            match userType with
                            | Role.Developer ->
                                a [ attr.``class`` "navbar-item"
                                    attr.href "/project/all" ] [
                                    text "Projects"
                                ]
                            | _ ->
                                div [ attr.classes [ "navbar-item"
                                                     "has-dropdown"
                                                     "is-hoverable" ] ] [
                                    a [ attr.``class`` "navbar-link"
                                        attr.href "/project/all" ] [
                                        text "Projects"
                                    ]

                                    div [ attr.``class`` "navbar-dropdown" ] [
                                        a [ attr.``class`` "navbar-item"
                                            attr.href "/project/add" ] [
                                            text "Add New Project"
                                        ]
                                    ]
                                ]

                            match userType with
                            | Role.Admin ->
                                div [ attr.classes [ "navbar-item"
                                                     "has-dropdown"
                                                     "is-hoverable" ] ] [
                                    a [ attr.``class`` "navbar-link"
                                        attr.href "/employee/all" ] [
                                        text "Employees"
                                    ]

                                    div [ attr.``class`` "navbar-dropdown" ] [
                                        a [ attr.``class`` "navbar-item"
                                            attr.href "/employee/add" ] [
                                            text "Add New Employee"
                                        ]
                                    ]
                                ]
                            | _ ->
                                a [ attr.``class`` "navbar-item"
                                    attr.href "/employee/add" ] [
                                    text "Employees"
                                ]

                            a [ attr.``class`` "navbar-item"
                                attr.href "/department/all" ] [
                                text "Departments"
                            ]
                        ]
            ]

            div [ attr.``class`` "navbar-end" ] [
                div [ attr.``class`` "navbar-item" ] [
                    div [ attr.``class`` "buttons" ] [
                        cond model.IsSignedInAs
                        <| function
                            | None ->
                                concat' [] [
                                    a [ attr.classes [ "button"; "is-dark" ]
                                        attr.href "/signup" ] [
                                        strong [] [ text "Sign up" ]
                                    ]

                                    a [ attr.classes [ "button"; "is-light" ]
                                        attr.href "/login" ] [
                                        text "Log in"
                                    ]
                                ]
                            | Some _ ->
                                a [ attr.classes [ "button"; "is-light" ]
                                    on.click (fun _ -> dispatch SendSignOut) ] [
                                    text "Log out"
                                ]
                    ]
                ]
            ]
        ]
    ]

let viewBody model dispatch =
    let mapDispatch msgWrapper = msgWrapper >> dispatch

    cond model.Page
    <| function
        | Home -> homePage model dispatch
        | SignIn pgModel -> SignIn.view pgModel.Model (mapDispatch SignInMessage)
        | SignUp pgModel -> SignUp.view pgModel.Model (mapDispatch SignUpMessage)

        | ViewDepartments pgModel -> ViewGroup.Department.view pgModel.Model (mapDispatch ViewDepartmentsMessage)
        | ViewDepartment (_, pgModel) -> ViewItem.Department.view pgModel.Model (mapDispatch ViewDepartmentMessage)

        | ViewEmployees pgModel -> ViewGroup.Employee.view pgModel.Model (mapDispatch ViewEmployeesMessage)
        | ViewEmployee (_, pgModel) -> ViewItem.Employee.view pgModel.Model (mapDispatch ViewEmployeeMessage)
        | CreateEmployee pgModel -> Create.Employee.view pgModel.Model (mapDispatch CreateEmployeeMessage)
        | EditEmployee (_, pgModel) -> Edit.Employee.view pgModel.Model (mapDispatch EditEmployeeMessage)

        | ViewProjects pgModel -> ViewGroup.Project.view pgModel.Model (mapDispatch ViewProjectsMessage)
        | ViewProject (_, pgModel) -> ViewItem.Project.view pgModel.Model (mapDispatch ViewProjectMessage)
        | CreateProject pgModel -> Create.Project.view pgModel.Model (mapDispatch CreateProjectMessage)
        | EditProject (_, pgModel) -> Edit.Project.view pgModel.Model (mapDispatch EditProjectMessage)

let view model dispatch =

    concat' [] [
        navMenu model dispatch

        div [ attr.``class`` "container" ] [
            div [ attr.``class`` "columns" ] [
                div [ attr.``class`` "column" ] [
                    section [ attr.``class`` "section" ] [
                        viewBody model dispatch

                        cond model.Error
                        <| function
                            | None ->
                                // TODO: Really should replace error message logic
                                // with local storage logic
                                cond model.Success
                                <| function
                                    | None -> empty
                                    | Some msg -> errorNotifSuccess msg (fun _ -> dispatch ClearSuccess)
                            | Some msg -> errorNotifWarning msg (fun _ -> dispatch ClearError)
                    ]
                ]
            ]
        ]
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let signInService = this.Remote<Models.Auth.AuthService>()
        let departmentService = this.Remote<Models.EmployeeData.DepartmentService>()
        let employeeService = this.Remote<Models.EmployeeData.EmployeeService>()
        let projectService = this.Remote<Models.EmployeeData.ProjectService>()

        let remotes =
            { Auth = signInService
              Department = departmentService
              Employee = employeeService
              Project = projectService }

        let update = update remotes this.NavigationManager this.JSRuntime

        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg GetSignedInAs) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
