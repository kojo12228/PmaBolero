module PmaBolero.Client.Pages.ViewItem.Department

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Models.EmployeeData

type Model = ViewItem.Model<Department>

let initModel: Model =
    {
        DataType = "Departments"
        UrlPrefix = "department"
        IsLoading = true
        Data = None
        AuthorisationFailure = false
        Error = None
    }

type Message =
    | InitMessage of int
    | TileMessage of ViewItem.Message<Department>

let update remote message model =
    let getDataFunc = remote.getDepartment

    let tileMsg =
        match message with
        | InitMessage deptId -> ViewItem.InitMessage deptId
        | TileMessage msg -> msg

    ViewItem.update getDataFunc tileMsg model
    |> fun (model, cmd) -> model, Cmd.map TileMessage cmd

type ViewDepartmentPage = Template<"wwwroot/viewdepartment.html">

let populateProjects (projects: (int * string) []) =
    ViewDepartmentPage
        .ProjectList()
        .ProjectItems(
            forEach projects (fun (projId, projName) ->
                ViewDepartmentPage
                    .ProjectItem()
                    .Id(string projId)
                    .Name(projName)
                    .Elt()
            )
        )
        .Elt()

let populateEmployees (employees: (int * string) []) =
    ViewDepartmentPage
        .EmployeeList()
        .EmployeeItems(
            forEach employees (fun (emplId, emplName) ->
                ViewDepartmentPage
                    .EmployeeItem()
                    .Id(string emplId)
                    .Name(emplName)
                    .Elt()
            )
        )
        .Elt()

let generateTile (dept: Department) =
    ViewDepartmentPage
        .Tile()
        .Projects(
            cond (Array.isEmpty dept.Projects) <| function
            | false -> populateProjects dept.Projects
            | true ->
                ViewDepartmentPage
                    .NoProjects()
                    .Elt()
        )
        .Employees(
            cond (Array.isEmpty dept.Employees) <| function
            | false -> populateEmployees dept.Employees
            | true ->
                ViewDepartmentPage
                    .NoEmployees()
                    .Elt()
        )
        .Elt()

let view (model: Model) dispatch =
    let deptTitle (dept: Department) = dept.Name

    let mappedDispatch = TileMessage >> dispatch
    ViewItem.view generateTile deptTitle model mappedDispatch