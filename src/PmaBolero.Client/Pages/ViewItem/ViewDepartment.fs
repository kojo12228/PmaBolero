module PmaBolero.Client.Pages.ViewItem.Department

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Shared.Models

open PmaBolero.Client.Models.EmployeeData
open PmaBolero.Client.Helpers

type Model = ViewItem.Model<Department>

let initModel: Model =
    { DataType = "Departments"
      UrlPrefix = "department"
      IsLoading = true
      Data = None
      AuthorisationFailure = false
      Error = None }

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

let viewProjects (projects: (int * string) []) =
    cond (Array.isEmpty projects)
    <| function
        | false ->
            concat' [] [
                p [] [ strong [] [ text "Projects" ] ]
                div [ attr.``class`` "content" ] [
                    ul [] [
                        forEach projects
                        <| fun (projId, projName) ->
                            li [] [
                                a [ attr.href $"/project/{projId}" ] [
                                    text projName
                                ]
                            ]
                    ]
                ]
            ]
        | true ->
            p [] [
                strong [] [ text "No projects" ]
            ]

let viewEmployees (employees: (int * string) []) =
    cond (Array.isEmpty employees)
    <| function
        | false ->
            concat' [] [
                p [] [ strong [] [ text "Employees" ] ]
                div [ attr.``class`` "content" ] [
                    ul [] [
                        forEach employees
                        <| fun (emplId, emplName) ->
                            li [] [
                                a [ attr.href $"/employee/{emplId}" ] [
                                    text emplName
                                ]
                            ]
                    ]
                ]
            ]
        | true ->
            p [] [
                strong [] [ text "No employees" ]
            ]

let generateTile (dept: Department) =
    concat' [] [
        viewProjects dept.Projects
        viewEmployees dept.Employees
    ]

let view (model: Model) dispatch =
    let deptTitle (dept: Department) = dept.Name

    let mappedDispatch = TileMessage >> dispatch
    ViewItem.view generateTile deptTitle model mappedDispatch
