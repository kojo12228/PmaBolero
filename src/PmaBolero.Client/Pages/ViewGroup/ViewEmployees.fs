module PmaBolero.Client.Pages.ViewGroup.Employee

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Shared.Models

open PmaBolero.Client.Models
open PmaBolero.Client.Models.EmployeeData

type Model =
    { SignInRole: Role option
      TilesModel: ViewGroup.Model<Employee> }

let initModel: Model =
    { SignInRole = None
      TilesModel =
        { Title = "Employees"
          IsLoading = false
          Data = [||]
          AuthorisationFailure = false
          Error = None } }

type Message =
    | InitMessage of Role option
    | DeleteEmployee of int
    | DeleteReturn of option<option<int>>
    | TilesMessage of ViewGroup.Message<Employee>

let update remote (message: Message) (model: Model) =
    match message with
    | InitMessage role -> { model with SignInRole = role }, Cmd.ofMsg (TilesMessage ViewGroup.InitMessage)
    | DeleteEmployee emplId ->
        model, Cmd.OfAuthorized.either remote.deleteEmployee emplId DeleteReturn (TilesMessage << ViewGroup.Error)
    | DeleteReturn _ -> model, Cmd.ofMsg (TilesMessage ViewGroup.InitMessage)
    | TilesMessage msg ->
        let getDataFunc = remote.getEmployees

        let updatedModel, cmd = ViewGroup.update getDataFunc msg model.TilesModel
        { model with TilesModel = updatedModel }, Cmd.map TilesMessage cmd

type ViewEmployeesPage = Template<"wwwroot/viewemployees.html">

let populateSkills (skills: string []) =
    ViewEmployeesPage
        .SkillList()
        .Items(forEach skills (fun skill -> ViewEmployeesPage.SkillItem().Skill(skill).Elt()))
        .Elt()

let populateProjects (projects: (int * string) []) =
    ViewEmployeesPage
        .ProjectList()
        .Items(
            forEach projects (fun (projId, name) ->
                ViewEmployeesPage
                    .ProjectItem()
                    .Id(string projId)
                    .Name(name)
                    .Elt())
        )
        .Elt()

let generateTile signInRole dispatch (employee: Employee) =
    ViewEmployeesPage
        .EmployeeTile()
        .Id(string employee.Id)
        .Name(employee.FullName)
        .Email(employee.Email)
        .Role(string employee.Role)
        .DeptId(employee.DepartmentID |> fst |> string)
        .DepartmentName(employee.DepartmentID |> snd)
        .Skills(
            cond (Array.isEmpty employee.Skills)
            <| function
                | true -> ViewEmployeesPage.NoSkills().Elt()
                | false -> populateSkills employee.Skills
        )
        .Projects(
            cond (Array.isEmpty employee.ProjectIds)
            <| function
                | true -> ViewEmployeesPage.NoProjects().Elt()
                | false -> populateProjects employee.ProjectIds
        )
        .EditDisable(
            match signInRole with
            | Some Admin -> false
            | _ -> true
        )
        .DisableDelete(
            match signInRole with
            | Some Admin -> false
            | _ -> true
        )
        .DeleteClick(fun _ -> dispatch (DeleteEmployee employee.Id))
        .Elt()

let view (model: Model) dispatch =
    let mappedDispatch = TilesMessage >> dispatch
    ViewGroup.view (generateTile model.SignInRole dispatch) model.TilesModel mappedDispatch
