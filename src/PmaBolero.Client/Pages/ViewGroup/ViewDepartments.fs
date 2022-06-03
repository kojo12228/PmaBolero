module PmaBolero.Client.Pages.ViewGroup.Department

open Elmish
open Bolero.Html
open Bolero.Remoting.Client

open PmaBolero.Shared.Models

open PmaBolero.Client.Models.EmployeeData
open PmaBolero.Client.Helpers

type Model = ViewGroup.Model<Department>

let initModel: Model =
    { Title = "Departments"
      IsLoading = false
      Data = [||]
      AuthorisationFailure = false
      Error = None }

type Message =
    | InitMessage
    | TilesMessage of ViewGroup.Message<Department>

let update remote (message: Message) (model: Model) =
    let getDataFunc = remote.getDepartments

    let tilesMsg =
        match message with
        | InitMessage -> ViewGroup.InitMessage
        | TilesMessage msg -> msg

    let updatedModel, cmd = ViewGroup.update getDataFunc tilesMsg model
    updatedModel, Cmd.map TilesMessage cmd

let sublisting urlPrefix urlId name =
    li [] [
        a [ attr.href $"/%s{urlPrefix}/%d{urlId}" ] [
            text name
        ]
    ]

let viewProjects (projects: (int * string) []) =
    cond (Array.isEmpty projects)
    <| function
        | false ->
            concat' [] [
                p [] [ text "Projects:" ]
                div [ attr.``class`` "content" ] [
                    ul [] [
                        forEach projects
                        <| fun (projId, projName) -> sublisting "project" projId projName
                    ]
                ]
            ]
        | true -> p [] [ text "No projects" ]

let viewEmployees (employees: (int * string) []) =
    cond (Array.isEmpty employees)
    <| function
        | false ->
            concat' [] [
                p [] [ text "Employees:" ]

                div [ attr.``class`` "content" ] [
                    ul [] [
                        forEach employees
                        <| fun (emplId, emplName) -> sublisting "employee" emplId emplName
                    ]
                ]
            ]
        | true -> p [] [ text "No employees" ]

let generateTile (dept: Department) =
    concat' [] [
        a [ attr.classes [ "subtitle"; "link" ]
            attr.href $"/department/{dept.Id}" ] [
            strong [] [ text dept.Name ]
        ]

        hr []

        div [ attr.``class`` "columns" ] [
            div [ attr.``class`` "column" ] [
                viewProjects dept.Projects
            ]

            div [ attr.``class`` "column" ] [
                viewEmployees dept.Employees
            ]
        ]

        div [ attr.``class`` "columns" ] [
            div [ attr.``class`` "column" ] [
                a [ attr.href $"/department/{dept.Id}"
                    attr.classes [ "button"
                                   "is-primary"
                                   "is-fullwidth" ] ] [
                    text "View"
                ]
            ]

            div [ attr.``class`` "column" ] [
                button [ attr.classes [ "button"
                                        "is-warning"
                                        "is-fullwidth" ]
                         attr.disabled true ] [
                    text "Delete"
                ]
            ]
        ]
    ]

let view (model: Model) dispatch =
    let mappedDispatch = TilesMessage >> dispatch
    ViewGroup.view generateTile model mappedDispatch
