module PmaBolero.Client.ViewDepartments

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Models
open PmaBolero.Client.Models.EmployeeData

type Model = TilesTemplate.Model<Department>

let initModel: Model =
    {
        Title = "Departments"
        IsLoading = false
        Data = [||]
        AuthorisationFailure = false
        Error = None
    }

type Message =
    | InitMessage
    | TilesMessage of TilesTemplate.Message<Department>

let update remote (message: Message) (model: Model) =
    let getDataFunc = remote.getDepartments

    let tilesMsg =
        match message with
        | InitMessage -> TilesTemplate.InitMessage
        | TilesMessage msg -> msg

    let updatedModel, cmd = TilesTemplate.update getDataFunc tilesMsg model
    updatedModel, Cmd.map TilesMessage cmd

type ViewDepartmentsPage = Template<"wwwroot/viewdepartments.html">

let populateProjects (projects: (int * string) []) =
    ViewDepartmentsPage
        .PopulateProjects()
        .ProjectNames(
            forEach projects (fun (projId, projName) ->
                ViewDepartmentsPage
                    .DepartmentSublisting()
                    .UrlPrefix("project")
                    .Id(string projId)
                    .Name(projName)
                    .Elt()
            )
        )
        .Elt()

let populateEmployees (employees: (int * string) []) =
    ViewDepartmentsPage
        .PopulateEmployees()
        .EmployeeNames(
            forEach employees (fun (emplId, emplName) ->
                ViewDepartmentsPage
                    .DepartmentSublisting()
                    .UrlPrefix("employee")
                    .Id(string emplId)
                    .Name(emplName)
                    .Elt()
            )
        )
        .Elt()

let generateTile (dept: Department) =
    ViewDepartmentsPage
        .DepartmentTile()
        .Id(string dept.Id)
        .DepartmentName(dept.Name)
        .ProjectsList(
            cond (Array.isEmpty dept.Projects) <| function
            | false ->
                populateProjects dept.Projects
            | true ->
                ViewDepartmentsPage
                    .NoProjects()
                    .Elt()
        )
        .EmployeesList(
            cond (Array.isEmpty dept.Employees) <| function
            | false ->
                populateEmployees dept.Employees
            | true ->
                ViewDepartmentsPage
                    .NoEmployees()
                    .Elt()
        )
        .Elt()

let view (model: Model) dispatch =
    let mappedDispatch = TilesMessage >> dispatch
    TilesTemplate.view generateTile model mappedDispatch