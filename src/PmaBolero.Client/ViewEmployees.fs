module PmaBolero.Client.ViewEmployees

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Models
open PmaBolero.Client.Models.EmployeeData

type Model = TilesTemplate.Model<Employee>

let initModel: Model =
    {
        Title = "Employees"
        IsLoading = false
        Data = [||]
        AuthorisationFailure = false
        Error = None
    }

type Message =
    | InitMessage
    | TilesMessage of TilesTemplate.Message<Employee>

let update remote (message: Message) (model: Model) =
    let getDataFunc = remote.getEmployees

    let tilesMsg =
        match message with
        | InitMessage -> TilesTemplate.InitMessage
        | TilesMessage msg -> msg

    let updatedModel, cmd = TilesTemplate.update getDataFunc tilesMsg model
    updatedModel, Cmd.map TilesMessage cmd

type ViewEmployeesPage = Template<"wwwroot/viewemployees.html">

let populateSkills (skills: string []) =
    ViewEmployeesPage
        .SkillList()
        .Items(
            forEach skills (fun skill ->
                ViewEmployeesPage
                    .SkillItem()
                    .Skill(skill)
                    .Elt()
            )
        )
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
                    .Elt()
            )
        )
        .Elt()

let generateTile (employee: Employee) =
    ViewEmployeesPage
        .EmployeeTile()
        .Id(string employee.Id)
        .Name(employee.FullName)
        .Email(employee.Email)
        .Role(string employee.Role)
        .DeptId(employee.DepartmentID |> fst |> string)
        .DepartmentName(employee.DepartmentID |> snd)
        .Skills(
            cond (Array.isEmpty employee.Skills) <| function
            | true ->
                ViewEmployeesPage
                    .NoSkills()
                    .Elt()
            | false -> populateSkills employee.Skills)
        .Projects(
            cond (Array.isEmpty employee.ProjectIds) <| function
            | true ->
                ViewEmployeesPage
                    .NoProjects()
                    .Elt()
            | false ->
                populateProjects employee.ProjectIds)
        .Elt()

let view (model: Model) dispatch =
    let mappedDispatch = TilesMessage >> dispatch
    TilesTemplate.view generateTile model mappedDispatch