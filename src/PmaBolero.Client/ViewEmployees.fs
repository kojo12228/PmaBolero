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

type Model = MultiTilePageTemplate.Model<Employee>

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
            forEach projects (fun (_, name) ->
                ViewEmployeesPage
                    .ProjectItem()
                    .Name(name)
                    .Elt()
            )
        )
        .Elt()

let viewEmployeeTile (employee: Employee) =
    ViewEmployeesPage
        .Tile()
        .Name(employee.FullName)
        .Email(employee.Email)
        .Role(string employee.Role)
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

let initModel: Model =
    {
        Title = "Department"
        IsLoading = false
        Data = [||]
        ToTile = viewEmployeeTile
        AuthorisationFailure = false
        Error = None
    }

type Message = MultiTilePageTemplate.Message<Employee>

let update remote (message: Message) (model: Model) =
    let getDataFunc = remote.getEmployees

    MultiTilePageTemplate.update getDataFunc message model

let view model dispatch =
    MultiTilePageTemplate.view model dispatch