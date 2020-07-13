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

type Model =
    {
        IsLoading: bool
        Employees: Employee[]
        AuthorisationFailure: bool
        Error: string option
    }

let initModel =
    {
        IsLoading = false
        Employees = [||]
        AuthorisationFailure = true
        Error = None
    }

type Message =
    | InitMessage
    | GetEmployees
    | RecvEmployees of option<Employee[]>
    | Error of exn

let update remote message model =
    match message with
    | InitMessage ->
        { model with IsLoading = true }, Cmd.ofMsg GetEmployees
    | GetEmployees ->
        model, Cmd.ofAuthorized remote.getEmployees () RecvEmployees Error
    | RecvEmployees (Some empls) ->
        { model with Employees = empls; IsLoading = false }, Cmd.none
    | RecvEmployees None ->
        { model with AuthorisationFailure = true }, Cmd.none
    | Error e ->
        { model with Error = Some e.Message }, Cmd.none

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

let view model dispatch =
    ViewEmployeesPage
        .EmplTemplate()
        .Progress(
            cond model.IsLoading <| function
            | false -> empty
            | true ->
                ViewEmployeesPage.DisplayProgress().Elt()
        )
        .Tiles(
            forEach model.Employees viewEmployeeTile
        )
        .Elt()