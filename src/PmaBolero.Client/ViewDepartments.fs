module PmaBolero.Client.ViewDepartments

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Models.EmployeeData

type Model =
    {
        IsLoading: bool
        Departments: DepartmentVis[]
        AuthorisationFailure: bool
        Error: string option
    }

let initModel =
    {
        IsLoading = false
        Departments = [||]
        AuthorisationFailure = false
        Error = None
    }

type Message =
    | InitMessage
    | GetDepartments
    | RecvDepartments of option<DepartmentVis[]>
    | Error of exn

let update remote message model =
    match message with
    | InitMessage ->
        { model with IsLoading = true }, Cmd.ofMsg GetDepartments
    | GetDepartments ->
        model, Cmd.ofAuthorized remote.getDepartmentsVis () RecvDepartments Error
    | RecvDepartments (Some depts) ->
        { model with Departments = depts; IsLoading = false }, Cmd.none
    | RecvDepartments None ->
        { model with AuthorisationFailure = true}, Cmd.none
    | Error e ->
        { model with Error = Some e.Message }, Cmd.none

type ViewDepartmentsPage = Template<"wwwroot/viewdepartments.html">

let renderDepartments (dept: DepartmentVis) =
    ViewDepartmentsPage
        .DepartmentTile()
        .DepartmentName(dept.Name)
        .ProjectNames(
            forEach dept.Projects (fun (_, projName) ->
                ViewDepartmentsPage
                    .DepartmentSublisting()
                    .Name(projName)
                    .Elt()
            )
        )
        .EmployeeNames(
            forEach dept.Employees (fun (_, emplName) ->
                ViewDepartmentsPage
                    .DepartmentSublisting()
                    .Name(emplName)
                    .Elt()
            )
        )
        .Elt()

let view model dispatch =
    ViewDepartmentsPage
        .ViewDepartments()
        .Progress(
            cond model.IsLoading <| function
            | false -> empty
            | true ->
                ViewDepartmentsPage.DisplayProgress().Elt()
        )
        .DepartmentTiles(
            forEach model.Departments renderDepartments
        )
        .Elt()