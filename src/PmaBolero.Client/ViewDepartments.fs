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

type Model = MultiTilePageTemplate.Model<Department>

let initModel: Model =
    {
        Title = "Departments"
        IsLoading = false
        Data = [||]
        AuthorisationFailure = false
        Error = None
    }

type Message = MultiTilePageTemplate.Message<Department>

let update remote (message: Message) (model: Model) =
    let getDataFunc = remote.getDepartments

    MultiTilePageTemplate.update getDataFunc message model

type ViewDepartmentsPage = Template<"wwwroot/viewdepartments.html">

let populateProjects (projects: (int * string) []) =
    ViewDepartmentsPage
        .PopulateProjects()
        .ProjectNames(
            forEach projects (fun (_, projName) ->
                ViewDepartmentsPage
                    .DepartmentSublisting()
                    .Name(projName)
                    .Elt()
            )
        )
        .Elt()

let populateEmployees (employees: (int * string) []) =
    ViewDepartmentsPage
        .PopulateEmployees()
        .EmployeeNames(
            forEach employees (fun (_, emplName) ->
                ViewDepartmentsPage
                    .DepartmentSublisting()
                    .Name(emplName)
                    .Elt()
            )
        )
        .Elt()

let generateTile (dept: Department) =
    ViewDepartmentsPage
        .DepartmentTile()
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
    MultiTilePageTemplate.view generateTile model dispatch